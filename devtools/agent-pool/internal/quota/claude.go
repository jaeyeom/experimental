package quota

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/quotapace"
)

const (
	defaultClaudeUsageBinary = "claude-usage"
	defaultRecentPaceSpan    = 6 * time.Hour
	windowFiveHour           = "5h"
	windowSevenDay           = "7d"
)

var _ Guard = (*ClaudeGuard)(nil)

// ClaudeUsageReading matches the JSON output of `claude-usage --json`.
type ClaudeUsageReading struct {
	PlanName        string     `json:"planName"`
	FiveHour        *float64   `json:"fiveHour"`
	SevenDay        *float64   `json:"sevenDay"`
	FiveHourResetAt *time.Time `json:"fiveHourResetAt"`
	SevenDayResetAt *time.Time `json:"sevenDayResetAt"`
}

// UsageReader reads the current Claude subscription usage windows.
type UsageReader interface {
	Read(ctx context.Context) (ClaudeUsageReading, error)
}

// ExecClaudeUsageReader invokes `claude-usage --json` with an isolated
// CLAUDE_CONFIG_DIR environment variable.
type ExecClaudeUsageReader struct {
	Binary          string
	ClaudeConfigDir string
}

// NewExecClaudeUsageReader creates an ExecClaudeUsageReader bound to
// claudeConfigDir.
func NewExecClaudeUsageReader(claudeConfigDir string) *ExecClaudeUsageReader {
	return &ExecClaudeUsageReader{
		Binary:          defaultClaudeUsageBinary,
		ClaudeConfigDir: claudeConfigDir,
	}
}

// Read executes `claude-usage --json` under the isolated CLAUDE_CONFIG_DIR and
// parses stdout as a ClaudeUsageReading.
func (r *ExecClaudeUsageReader) Read(ctx context.Context) (ClaudeUsageReading, error) {
	if strings.TrimSpace(r.ClaudeConfigDir) == "" {
		return ClaudeUsageReading{}, errors.New("isolated claude config dir is required")
	}
	binary := r.Binary
	if binary == "" {
		binary = defaultClaudeUsageBinary
	}

	// Direct execve of the configured claude-usage binary with fixed --json arg (no shell).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, binary, "--json")
	cmd.Env = withEnvOverride(os.Environ(), "CLAUDE_CONFIG_DIR", r.ClaudeConfigDir)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return ClaudeUsageReading{}, fmt.Errorf("run %s --json: %s: %w", binary, msg, err)
		}
		return ClaudeUsageReading{}, fmt.Errorf("run %s --json: %w", binary, err)
	}

	var reading ClaudeUsageReading
	if err := json.Unmarshal(stdout.Bytes(), &reading); err != nil {
		return ClaudeUsageReading{}, fmt.Errorf("decode %s --json output: %w", binary, err)
	}
	return reading, nil
}

func withEnvOverride(environ []string, key, value string) []string {
	prefix := key + "="
	out := make([]string, 0, len(environ)+1)
	for _, entry := range environ {
		if strings.HasPrefix(entry, prefix) {
			continue
		}
		out = append(out, entry)
	}
	return append(out, prefix+value)
}

// ClaudeGuardOption configures optional behavior of ClaudeGuard.
type ClaudeGuardOption func(*ClaudeGuard)

// WithClock overrides the clock used by ClaudeGuard for caching, staleness,
// and quotapace timestamps.
func WithClock(now func() time.Time) ClaudeGuardOption {
	return func(g *ClaudeGuard) {
		if now != nil {
			g.now = now
		}
	}
}

// ClaudeGuard enforces Claude subscription rolling-window caps and quotapace
// pace projections.
type ClaudeGuard struct {
	cfg    Config
	reader UsageReader
	store  quotapace.Store
	now    func() time.Time

	mu          sync.Mutex
	hasReading  bool
	lastReading ClaudeUsageReading
	lastReadAt  time.Time
}

// NewClaudeGuard creates a ClaudeGuard using cfg and reader.
func NewClaudeGuard(cfg Config, reader UsageReader, opts ...ClaudeGuardOption) (*ClaudeGuard, error) {
	if reader == nil {
		return nil, errors.New("usage reader must not be nil")
	}
	cfg = normalizeConfig(cfg)
	if err := quotapace.CheckProfileName(cfg.QuotapaceProfile); err != nil {
		return nil, fmt.Errorf("invalid quotapace profile: %w", err)
	}

	qpDir := cfg.QuotapaceDir
	if qpDir == "" {
		dir, err := quotapace.DataDir(os.Getenv)
		if err != nil {
			return nil, fmt.Errorf("resolve quotapace data dir: %w", err)
		}
		qpDir = dir
	}

	g := &ClaudeGuard{
		cfg:    cfg,
		reader: reader,
		store:  quotapace.Store{Dir: qpDir},
		now:    func() time.Time { return time.Now().UTC() },
	}
	for _, opt := range opts {
		if opt != nil {
			opt(g)
		}
	}
	return g, nil
}

// Refresh forces a fresh read from UsageReader, records valid windows in
// quotapace, and returns the updated QuotaHealth.
func (g *ClaudeGuard) Refresh(ctx context.Context) (protocol.QuotaHealth, error) {
	g.mu.Lock()
	defer g.mu.Unlock()

	return g.refreshLocked(ctx)
}

// CanAccept reports whether the worker may accept a new job right now.
func (g *ClaudeGuard) CanAccept(ctx context.Context) Decision {
	g.mu.Lock()
	defer g.mu.Unlock()

	now := g.now().UTC()
	if !g.hasReading || now.Sub(g.lastReadAt) >= g.cfg.PollInterval {
		if _, err := g.refreshLocked(ctx); err != nil {
			return g.unknownDecision(fmt.Sprintf("quota refresh failed: %v", err))
		}
		now = g.now().UTC()
	}

	age := now.Sub(g.lastReadAt)
	if age > g.cfg.StaleAfter {
		return g.unknownDecision(fmt.Sprintf("quota reading is stale (%s old > %s)", age.Truncate(time.Second), g.cfg.StaleAfter))
	}

	return g.evaluateLocked(now)
}

func (g *ClaudeGuard) refreshLocked(ctx context.Context) (protocol.QuotaHealth, error) {
	reading, err := g.reader.Read(ctx)
	if err != nil {
		return protocol.QuotaHealth{State: StateUnknown}, fmt.Errorf("read claude usage: %w", err)
	}

	now := g.now().UTC()
	if err := g.recordQuotapaceLocked(reading, now); err != nil {
		return protocol.QuotaHealth{State: StateUnknown}, err
	}

	g.hasReading = true
	g.lastReading = reading
	g.lastReadAt = now

	dec := g.evaluateLocked(now)
	return dec.Health, nil
}

func (g *ClaudeGuard) recordQuotapaceLocked(reading ClaudeUsageReading, now time.Time) error {
	if err := g.recordWindowLocked(windowFiveHour, reading.FiveHour, reading.FiveHourResetAt, g.cfg.MaxFiveHourPct, now); err != nil {
		return err
	}
	return g.recordWindowLocked(windowSevenDay, reading.SevenDay, reading.SevenDayResetAt, g.cfg.MaxSevenDayPct, now)
}

func (g *ClaudeGuard) recordWindowLocked(window string, pct *float64, resetsAt *time.Time, target float64, now time.Time) error {
	if pct == nil || resetsAt == nil || resetsAt.IsZero() {
		return nil
	}

	prof, err := g.store.LoadProfile(g.cfg.QuotapaceProfile)
	if err != nil {
		return fmt.Errorf("load quotapace profile %s: %w", g.cfg.QuotapaceProfile, err)
	}
	if prof.Active[window] == nil {
		opts := quotapace.StartOptions{
			Window:   window,
			ResetsAt: resetsAt.UTC(),
			Target:   target,
		}
		if _, err := g.store.StartPlan(g.cfg.QuotapaceProfile, opts, now); err != nil {
			return fmt.Errorf("start quotapace %s plan: %w", window, err)
		}
	}

	rec := quotapace.Reading{
		At:       now,
		Profile:  g.cfg.QuotapaceProfile,
		Window:   window,
		Percent:  *pct,
		ResetsAt: resetsAt.UTC(),
		Source:   g.cfg.Provider,
	}
	if _, err := g.store.Record(rec, true); err != nil {
		return fmt.Errorf("record quotapace %s reading: %w", window, err)
	}
	return nil
}

func (g *ClaudeGuard) evaluateLocked(now time.Time) Decision {
	health := buildQuotaHealth(g.lastReading, quotapace.Green)

	if g.lastReading.FiveHour != nil && *g.lastReading.FiveHour >= g.cfg.MaxFiveHourPct {
		health.State = quotapace.Red
		return Decision{
			Allow:  false,
			Reason: fmt.Sprintf("5h usage at %.1f%% >= cap %.1f%%", *g.lastReading.FiveHour, g.cfg.MaxFiveHourPct),
			Health: health,
		}
	}
	if g.lastReading.SevenDay != nil && *g.lastReading.SevenDay >= g.cfg.MaxSevenDayPct {
		health.State = quotapace.Red
		return Decision{
			Allow:  false,
			Reason: fmt.Sprintf("7d usage at %.1f%% >= cap %.1f%%", *g.lastReading.SevenDay, g.cfg.MaxSevenDayPct),
			Health: health,
		}
	}

	snap, err := g.store.Status(
		[]string{g.cfg.QuotapaceProfile},
		now,
		quotapace.StatusOptions{
			Recent: defaultRecentPaceSpan,
			Stale:  g.cfg.StaleAfter,
		},
	)
	if err != nil {
		return g.unknownDecision(fmt.Sprintf("quotapace status failed: %v", err))
	}

	worstState, worstLine := worstPaceState(snap, now)
	if g.cfg.RequireGreenPace && (worstState == quotapace.Amber || worstState == quotapace.Red) {
		health.State = worstState
		return Decision{
			Allow:  false,
			Reason: fmt.Sprintf("quotapace pace is %s: %s", worstState, worstLine),
			Health: health,
		}
	}

	return Decision{
		Allow:  true,
		Reason: "quota green",
		Health: health,
	}
}

func (g *ClaudeGuard) unknownDecision(reason string) Decision {
	return Decision{
		Allow:  !g.cfg.FailClosed,
		Reason: reason,
		Health: protocol.QuotaHealth{State: StateUnknown},
	}
}

func buildQuotaHealth(reading ClaudeUsageReading, state string) protocol.QuotaHealth {
	health := protocol.QuotaHealth{State: state}
	if reading.FiveHour != nil {
		rem := max(0.0, 100.0-*reading.FiveHour)
		health.FiveHourRemainingPct = &rem
	}
	if reading.SevenDay != nil {
		rem := max(0.0, 100.0-*reading.SevenDay)
		health.SevenDayRemainingPct = &rem
	}
	if reading.FiveHourResetAt != nil {
		t := reading.FiveHourResetAt.UTC()
		health.FiveHourResetsAt = &t
	}
	if reading.SevenDayResetAt != nil {
		t := reading.SevenDayResetAt.UTC()
		health.SevenDayResetsAt = &t
	}
	return health
}

func worstPaceState(snap quotapace.Snapshot, now time.Time) (string, string) {
	worst := quotapace.Green
	worstLine := ""
	for _, ws := range snap.Windows() {
		if paceSeverity(ws.State) > paceSeverity(worst) {
			worst = ws.State
			worstLine = ws.StatusLine(now)
		}
	}
	return worst, worstLine
}

func paceSeverity(state string) int {
	switch state {
	case quotapace.Red:
		return 2
	case quotapace.Amber:
		return 1
	default:
		return 0
	}
}
