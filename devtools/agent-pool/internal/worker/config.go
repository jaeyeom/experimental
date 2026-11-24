package worker

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/sandbox"
)

const (
	defaultPollInterval         = 20 * time.Second
	defaultHeartbeatInterval    = 30 * time.Second
	defaultJobHeartbeatInterval = 20 * time.Second
	defaultDrainTimeout         = 60 * time.Second
	defaultWorkerEnvKey         = "AGENT_POOL_WORKER_TOKEN"
)

// WorkerConfig holds core connection, concurrency, and directory settings for
// the consumer worker daemon.
type WorkerConfig struct {
	BrokerURL            string
	TokenEnv             string
	TokenFile            string
	Alias                string
	SupportedRepos       []string
	SupportedSkills      map[string][]string
	MaxConcurrentJobs    int
	GitHubAuth           sandbox.GitHubAuthMode
	IsolatedHome         string
	StateDir             string
	PollInterval         time.Duration
	HeartbeatInterval    time.Duration
	JobHeartbeatInterval time.Duration
	DrainTimeout         time.Duration
}

// JobLimits defines local worker execution caps for jobs or specific skills.
type JobLimits struct {
	MaxTurns            int   `toml:"max_turns"`
	MaxEffectiveTokens  int64 `toml:"max_effective_tokens"`
	TimeoutSeconds      int   `toml:"timeout_seconds"`
	MaxDeliverableBytes int   `toml:"max_deliverable_bytes"`
}

// Config holds the full parsed worker.toml configuration.
type Config struct {
	Worker         WorkerConfig
	Repos          []RepoRemote
	Quota          quota.Config
	JobDefaults    JobLimits
	SkillOverrides map[string]JobLimits
	DryRun         bool
}

type rawConfig struct {
	Worker         rawWorkerConfig      `toml:"worker"`
	Repos          []RepoRemote         `toml:"repos"`
	Quota          rawQuotaConfig       `toml:"quota"`
	JobDefaults    JobLimits            `toml:"job_defaults"`
	SkillOverrides map[string]JobLimits `toml:"skill_overrides"`
}

type rawWorkerConfig struct {
	BrokerURL            string              `toml:"broker_url"`
	TokenEnv             string              `toml:"token_env"`
	TokenFile            string              `toml:"token_file"`
	Alias                string              `toml:"alias"`
	SupportedRepos       []string            `toml:"supported_repos"`
	SupportedSkills      map[string][]string `toml:"supported_skills"`
	MaxConcurrentJobs    int                 `toml:"max_concurrent_jobs"`
	GitHubAuth           string              `toml:"github_auth"`
	IsolatedHome         string              `toml:"isolated_home"`
	StateDir             string              `toml:"state_dir"`
	PollInterval         string              `toml:"poll_interval"`
	HeartbeatInterval    string              `toml:"heartbeat_interval"`
	JobHeartbeatInterval string              `toml:"job_heartbeat_interval"`
	DrainTimeout         string              `toml:"drain_timeout"`
}

type rawQuotaConfig struct {
	Provider         string  `toml:"provider"`
	PollInterval     string  `toml:"poll_interval"`
	StaleAfter       string  `toml:"stale_after"`
	MaxFiveHourPct   float64 `toml:"max_five_hour_pct"`
	MaxSevenDayPct   float64 `toml:"max_seven_day_pct"`
	QuotapaceProfile string  `toml:"quotapace_profile"`
	QuotapaceDir     string  `toml:"quotapace_dir"`
	RequireGreenPace *bool   `toml:"require_green_pace"`
	FailClosed       *bool   `toml:"fail_closed_on_error"`
}

// XDGConfigHome resolves the XDG config directory on both Linux and macOS.
func XDGConfigHome() string {
	return xdgDir("XDG_CONFIG_HOME", ".config")
}

// XDGDataHome resolves the XDG data directory on both Linux and macOS.
func XDGDataHome() string {
	return xdgDir("XDG_DATA_HOME", filepath.Join(".local", "share"))
}

// XDGStateHome resolves the XDG state directory on both Linux and macOS.
func XDGStateHome() string {
	return xdgDir("XDG_STATE_HOME", filepath.Join(".local", "state"))
}

// DefaultConfigPath returns the default worker.toml path under XDG_CONFIG_HOME.
func DefaultConfigPath() string {
	return filepath.Join(XDGConfigHome(), "agent-pool", "worker.toml")
}

// DefaultIsolatedHome returns the default isolated HOME directory under XDG_DATA_HOME.
func DefaultIsolatedHome() string {
	return filepath.Join(XDGDataHome(), "agent-pool", "isolated-home")
}

// DefaultStateDir returns the default worker state directory under XDG_STATE_HOME.
func DefaultStateDir() string {
	return filepath.Join(XDGStateHome(), "agent-pool")
}

// DefaultConfig returns a Config populated with standard defaults and XDG paths.
func DefaultConfig() *Config {
	return &Config{
		Worker: WorkerConfig{
			TokenEnv:             defaultWorkerEnvKey,
			MaxConcurrentJobs:    1,
			GitHubAuth:           sandbox.GitHubAuthCpltGH,
			IsolatedHome:         DefaultIsolatedHome(),
			StateDir:             DefaultStateDir(),
			PollInterval:         defaultPollInterval,
			HeartbeatInterval:    defaultHeartbeatInterval,
			JobHeartbeatInterval: defaultJobHeartbeatInterval,
			DrainTimeout:         defaultDrainTimeout,
		},
		Quota: quota.DefaultConfig(),
		JobDefaults: JobLimits{
			MaxTurns:            catalog.DefaultMaxTurns,
			MaxEffectiveTokens:  catalog.DefaultMaxEffectiveTokens,
			TimeoutSeconds:      catalog.DefaultTimeoutSeconds,
			MaxDeliverableBytes: catalog.DefaultMaxDeliverableBytes,
		},
		SkillOverrides: make(map[string]JobLimits),
	}
}

// LoadConfig reads and parses a worker.toml file from path, applying XDG and
// operational defaults for omitted fields.
func LoadConfig(path string) (*Config, error) {
	if strings.TrimSpace(path) == "" {
		return nil, errors.New("config path must not be empty")
	}
	rawBytes, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read worker config %q: %w", path, err)
	}

	var raw rawConfig
	if err := toml.Unmarshal(rawBytes, &raw); err != nil {
		return nil, fmt.Errorf("parse worker config %q: %w", path, err)
	}

	cfg := DefaultConfig()
	if err := applyRawWorkerConfig(&cfg.Worker, raw.Worker); err != nil {
		return nil, err
	}
	if err := applyRawQuotaConfig(&cfg.Quota, raw.Quota); err != nil {
		return nil, err
	}
	cfg.Repos = raw.Repos
	applyJobLimits(&cfg.JobDefaults, raw.JobDefaults)
	if raw.SkillOverrides != nil {
		cfg.SkillOverrides = raw.SkillOverrides
	}
	return cfg, nil
}

// ResolveToken loads the worker bearer token from TokenEnv or TokenFile.
func (c *Config) ResolveToken() (string, error) {
	if c.Worker.TokenEnv != "" {
		if tok := strings.TrimSpace(os.Getenv(c.Worker.TokenEnv)); tok != "" {
			return tok, nil
		}
	}
	if c.Worker.TokenFile != "" {
		raw, err := os.ReadFile(expandHome(c.Worker.TokenFile))
		if err != nil {
			return "", fmt.Errorf("read worker token file %q: %w", c.Worker.TokenFile, err)
		}
		if tok := strings.TrimSpace(string(raw)); tok != "" {
			return tok, nil
		}
	}
	return "", errors.New("worker bearer token not found in token_env or token_file")
}

// RepoRemotes returns a map keyed by repository slug for WorktreeManager.
func (c *Config) RepoRemotes() map[string]RepoRemote {
	remotes := make(map[string]RepoRemote, len(c.Repos))
	for _, r := range c.Repos {
		if r.Name != "" {
			remotes[r.Name] = r
		}
	}
	return remotes
}

// EffectiveLimits computes the effective execution limits for skill by taking
// the stricter minimum of the repository catalog limits and local worker caps
// for every field.
func (c *Config) EffectiveLimits(skill string, catLimits catalog.Limits) catalog.Limits {
	local := c.localLimitsForSkill(skill)
	return catalog.Limits{
		MaxTurns:            minPositiveInt(catLimits.MaxTurns, local.MaxTurns, catalog.DefaultMaxTurns),
		MaxEffectiveTokens:  minPositiveInt64(catLimits.MaxEffectiveTokens, local.MaxEffectiveTokens, catalog.DefaultMaxEffectiveTokens),
		TimeoutSeconds:      minPositiveInt(catLimits.TimeoutSeconds, local.TimeoutSeconds, catalog.DefaultTimeoutSeconds),
		MaxDeliverableBytes: minPositiveInt(catLimits.MaxDeliverableBytes, local.MaxDeliverableBytes, catalog.DefaultMaxDeliverableBytes),
	}
}

func (c *Config) localLimitsForSkill(skill string) JobLimits {
	local := c.JobDefaults
	if override, ok := c.SkillOverrides[skill]; ok {
		applyJobLimits(&local, override)
	}
	return local
}

func applyRawWorkerConfig(dst *WorkerConfig, src rawWorkerConfig) error {
	if src.BrokerURL != "" {
		dst.BrokerURL = src.BrokerURL
	}
	if src.TokenEnv != "" {
		dst.TokenEnv = src.TokenEnv
	}
	if src.TokenFile != "" {
		dst.TokenFile = expandHome(src.TokenFile)
	}
	if src.Alias != "" {
		dst.Alias = src.Alias
	}
	if len(src.SupportedRepos) > 0 {
		dst.SupportedRepos = src.SupportedRepos
	}
	if len(src.SupportedSkills) > 0 {
		dst.SupportedSkills = src.SupportedSkills
	}
	if src.MaxConcurrentJobs > 0 {
		dst.MaxConcurrentJobs = src.MaxConcurrentJobs
	}
	if src.GitHubAuth != "" {
		mode, err := parseGitHubAuthMode(src.GitHubAuth)
		if err != nil {
			return err
		}
		dst.GitHubAuth = mode
	}
	if src.IsolatedHome != "" {
		dst.IsolatedHome = expandHome(src.IsolatedHome)
	}
	if src.StateDir != "" {
		dst.StateDir = expandHome(src.StateDir)
	}
	return applyRawWorkerDurations(dst, src)
}

func applyRawWorkerDurations(dst *WorkerConfig, src rawWorkerConfig) error {
	var err error
	if dst.PollInterval, err = parseOptionalDuration("poll_interval", src.PollInterval, dst.PollInterval); err != nil {
		return err
	}
	if dst.HeartbeatInterval, err = parseOptionalDuration("heartbeat_interval", src.HeartbeatInterval, dst.HeartbeatInterval); err != nil {
		return err
	}
	if dst.JobHeartbeatInterval, err = parseOptionalDuration("job_heartbeat_interval", src.JobHeartbeatInterval, dst.JobHeartbeatInterval); err != nil {
		return err
	}
	if dst.DrainTimeout, err = parseOptionalDuration("drain_timeout", src.DrainTimeout, dst.DrainTimeout); err != nil {
		return err
	}
	return nil
}

func applyRawQuotaConfig(dst *quota.Config, src rawQuotaConfig) error {
	if src.Provider != "" {
		dst.Provider = src.Provider
	}
	var err error
	if dst.PollInterval, err = parseOptionalDuration("quota.poll_interval", src.PollInterval, dst.PollInterval); err != nil {
		return err
	}
	if dst.StaleAfter, err = parseOptionalDuration("quota.stale_after", src.StaleAfter, dst.StaleAfter); err != nil {
		return err
	}
	if src.MaxFiveHourPct > 0 {
		dst.MaxFiveHourPct = src.MaxFiveHourPct
	}
	if src.MaxSevenDayPct > 0 {
		dst.MaxSevenDayPct = src.MaxSevenDayPct
	}
	if src.QuotapaceProfile != "" {
		dst.QuotapaceProfile = src.QuotapaceProfile
	}
	if src.QuotapaceDir != "" {
		dst.QuotapaceDir = expandHome(src.QuotapaceDir)
	}
	if src.RequireGreenPace != nil {
		dst.RequireGreenPace = *src.RequireGreenPace
	}
	if src.FailClosed != nil {
		dst.FailClosed = *src.FailClosed
	}
	return nil
}

func applyJobLimits(dst *JobLimits, src JobLimits) {
	if src.MaxTurns > 0 {
		dst.MaxTurns = src.MaxTurns
	}
	if src.MaxEffectiveTokens > 0 {
		dst.MaxEffectiveTokens = src.MaxEffectiveTokens
	}
	if src.TimeoutSeconds > 0 {
		dst.TimeoutSeconds = src.TimeoutSeconds
	}
	if src.MaxDeliverableBytes > 0 {
		dst.MaxDeliverableBytes = src.MaxDeliverableBytes
	}
}

func parseGitHubAuthMode(raw string) (sandbox.GitHubAuthMode, error) {
	switch sandbox.GitHubAuthMode(raw) {
	case sandbox.GitHubAuthCpltGH:
		return sandbox.GitHubAuthCpltGH, nil
	case sandbox.GitHubAuthNone:
		return sandbox.GitHubAuthNone, nil
	default:
		return "", fmt.Errorf("invalid github_auth %q: must be %q or %q", raw, sandbox.GitHubAuthCpltGH, sandbox.GitHubAuthNone)
	}
}

func parseOptionalDuration(field, raw string, fallback time.Duration) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" {
		return fallback, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil {
		return 0, fmt.Errorf("invalid %s %q: %w", field, raw, err)
	}
	if d <= 0 {
		return 0, fmt.Errorf("%s must be positive, got %s", field, raw)
	}
	return d, nil
}

func xdgDir(envVar, fallbackRel string) string {
	if v := strings.TrimSpace(os.Getenv(envVar)); v != "" {
		return expandHome(v)
	}
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return fallbackRel
	}
	return filepath.Join(home, fallbackRel)
}

func expandHome(p string) string {
	if p == "~" {
		if home, err := os.UserHomeDir(); err == nil && home != "" {
			return home
		}
		return p
	}
	if strings.HasPrefix(p, "~/") {
		if home, err := os.UserHomeDir(); err == nil && home != "" {
			return filepath.Join(home, p[2:])
		}
	}
	return p
}

func minPositiveInt(a, b, fallback int) int {
	switch {
	case a > 0 && b > 0:
		return min(a, b)
	case a > 0:
		return a
	case b > 0:
		return b
	default:
		return fallback
	}
}

func minPositiveInt64(a, b, fallback int64) int64 {
	switch {
	case a > 0 && b > 0:
		return min(a, b)
	case a > 0:
		return a
	case b > 0:
		return b
	default:
		return fallback
	}
}
