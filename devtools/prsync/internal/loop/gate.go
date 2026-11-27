package loop

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

// Loop gate and pause state constants.
const (
	StatePaused     = "paused"
	GateResultPass  = "pass"
	GateResultGated = "gated"

	defaultPreRoundTimeout = 30 * time.Second
	defaultGatePause       = 5 * time.Minute
	defaultGatePauseMax    = time.Hour
)

// GateOutcome describes the result of running loop_pre_round_cmd.
type GateOutcome struct {
	Checked bool
	Gated   bool
	Reason  string
	Pause   time.Duration
}

// CheckPreRoundGate executes cfg.LoopPreRoundCmd directly (never through a
// shell) bounded by cfg.LoopPreRoundTimeout. Exit 0 passes; any non-zero exit,
// missing binary, start failure, or timeout gates the round and logs at
// WARNING when execution fails.
func CheckPreRoundGate(ctx context.Context, exec executor.Executor, cfg config.Config, now time.Time) GateOutcome {
	if len(cfg.LoopPreRoundCmd) == 0 {
		return GateOutcome{}
	}
	timeout := cfg.LoopPreRoundTimeout
	if timeout <= 0 {
		timeout = defaultPreRoundTimeout
	}
	defaultPause := cfg.LoopGatePause
	if defaultPause <= 0 {
		defaultPause = defaultGatePause
	}
	maxPause := cfg.LoopGatePauseMax
	if maxPause <= 0 {
		maxPause = defaultGatePauseMax
	}
	if exec == nil {
		exec = executor.NewBasicExecutor()
	}

	res, err := exec.Execute(ctx, executor.ToolConfig{
		Command: cfg.LoopPreRoundCmd[0],
		Args:    cfg.LoopPreRoundCmd[1:],
		Timeout: timeout,
	})

	if err != nil || res == nil || res.TimedOut || res.Error != "" {
		errMsg := formatGateExecError(err, res, timeout)
		runlog.FromContext(ctx).Warn("loop_pre_round_gate_error", "cmd", cfg.LoopPreRoundCmd[0], "error", errMsg)
		var stdout string
		if res != nil {
			stdout = res.Output
		}
		reason := firstNonEmptyLine(stdout)
		if reason == "" {
			reason = fmt.Sprintf("pre-round gate error: %s", errMsg)
		}
		pause := resolveGatePause(ctx, stdout, now, defaultPause, maxPause)
		return GateOutcome{
			Checked: true,
			Gated:   true,
			Reason:  reason,
			Pause:   pause,
		}
	}

	if res.ExitCode != 0 {
		reason := firstNonEmptyLine(res.Output)
		if reason == "" {
			reason = firstNonEmptyLine(res.Stderr)
		}
		if reason == "" {
			reason = fmt.Sprintf("exit %d", res.ExitCode)
		}
		pause := resolveGatePause(ctx, res.Output, now, defaultPause, maxPause)
		runlog.FromContext(ctx).Info("loop_pre_round_gate_blocked",
			"cmd", cfg.LoopPreRoundCmd[0],
			"exit_code", res.ExitCode,
			"reason", reason,
			"pause", pause.String(),
		)
		return GateOutcome{
			Checked: true,
			Gated:   true,
			Reason:  reason,
			Pause:   pause,
		}
	}

	return GateOutcome{Checked: true, Gated: false}
}

func formatGateExecError(err error, res *executor.ExecutionResult, timeout time.Duration) string {
	switch {
	case err != nil:
		return err.Error()
	case res != nil && res.TimedOut:
		return fmt.Sprintf("timed out after %s", timeout)
	case res != nil && res.Error != "":
		return res.Error
	default:
		return "unknown execution failure"
	}
}

func firstNonEmptyLine(s string) string {
	for _, raw := range strings.Split(s, "\n") {
		if line := strings.TrimSpace(raw); line != "" {
			return line
		}
	}
	return ""
}

func resolveGatePause(ctx context.Context, stdout string, now time.Time, defaultPause, maxPause time.Duration) time.Duration {
	for _, raw := range strings.Split(stdout, "\n") {
		line := strings.TrimSpace(raw)
		val, ok := strings.CutPrefix(line, "retry_after=")
		if !ok {
			continue
		}
		val = strings.TrimSpace(val)
		if d, valid := parseRetryAfter(val, now); valid {
			if maxPause > 0 && d > maxPause {
				return maxPause
			}
			return d
		}
		runlog.FromContext(ctx).Warn("loop_pre_round_gate_invalid_retry_after", "value", val)
		return defaultPause
	}
	return defaultPause
}

func parseRetryAfter(val string, now time.Time) (time.Duration, bool) {
	if val == "" {
		return 0, false
	}
	if ts, err := time.Parse(time.RFC3339Nano, val); err == nil {
		d := ts.Sub(now)
		if d < 0 {
			d = 0
		}
		return d, true
	}
	if d, err := time.ParseDuration(val); err == nil && d >= 0 {
		return d, true
	}
	return 0, false
}

// WakeWaiter waits for an external wake notification (such as an incoming
// agentbus message) starting from since, until ctx is canceled.
type WakeWaiter interface {
	Wait(ctx context.Context, since time.Time) (via string, err error)
}

// AgentbusWakeWaiter subscribes to incoming messages for Name by streaming
// `agentbus log --follow --name <Name> --event send --since <since> --json`.
type AgentbusWakeWaiter struct {
	Bin  string
	Name string
}

// Wait runs `agentbus log --follow` and returns ("agentbus", nil) as soon as a
// non-empty event line is emitted, or returns ctx.Err() / an execution error.
func (w AgentbusWakeWaiter) Wait(ctx context.Context, since time.Time) (string, error) {
	if strings.TrimSpace(w.Name) == "" {
		<-ctx.Done()
		return "", fmt.Errorf("agentbus wake canceled: %w", ctx.Err())
	}
	bin := w.Bin
	if bin == "" {
		bin = "agentbus"
	}
	args := []string{
		"log",
		"--follow",
		"--name", w.Name,
		"--event", "send",
		"--since", since.UTC().Format(time.RFC3339Nano),
		"--json",
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, bin, args...) //nolint:gosec // G204: operator-configured agentbus_bin with fixed flags
	cmd.WaitDelay = 200 * time.Millisecond
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("agentbus wake pipe: %w", err)
	}
	var stderrBuf bytes.Buffer
	cmd.Stderr = &stderrBuf
	if err := cmd.Start(); err != nil {
		return "", fmt.Errorf("start %s: %w", bin, err)
	}

	sc := bufio.NewScanner(stdout)
	for sc.Scan() {
		if strings.TrimSpace(sc.Text()) != "" {
			if cmd.Process != nil {
				_ = cmd.Process.Kill()
			}
			_ = cmd.Wait()
			return "agentbus", nil
		}
	}
	scanErr := sc.Err()
	waitErr := cmd.Wait()
	if ctx.Err() != nil {
		return "", fmt.Errorf("agentbus wake canceled: %w", ctx.Err())
	}
	if scanErr != nil {
		return "", fmt.Errorf("read %s log: %w", bin, scanErr)
	}
	if waitErr != nil {
		if msg := strings.TrimSpace(stderrBuf.String()); msg != "" {
			return "", fmt.Errorf("%s log failed: %w (%s)", bin, waitErr, msg)
		}
		return "", fmt.Errorf("%s log failed: %w", bin, waitErr)
	}
	return "", errors.New("agentbus log exited without wake event")
}
