package authwatch

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"time"

	"github.com/pelletier/go-toml/v2"
)

const (
	// DefaultWarnWithin is the default refresh-token threshold for warn.
	DefaultWarnWithin = 72 * time.Hour
	// DefaultCriticalWithin is the default refresh-token threshold for critical.
	DefaultCriticalWithin = 24 * time.Hour
	// DefaultFailingAfter is the default grace period past access expiry before failing.
	DefaultFailingAfter = 10 * time.Minute
	// DefaultCLICommand is the default agent CLI binary for optional status checks.
	DefaultCLICommand = "claude"
)

// Config is the authwatch configuration.
type Config struct {
	WarnWithin     time.Duration
	CriticalWithin time.Duration
	FailingAfter   time.Duration
	Notify         []string
	UseCLIStatus   bool
	CLICommand     string
	Profiles       []ProfileConfig
}

// ProfileConfig configures one agent CLI account profile to watch.
type ProfileConfig struct {
	Name            string
	ConfigDir       string
	CredentialsFile string
	KeychainService string
}

type fileConfig struct {
	WarnWithin     string              `toml:"warn_within"`
	CriticalWithin string              `toml:"critical_within"`
	FailingAfter   string              `toml:"failing_after"`
	Notify         []string            `toml:"notify"`
	UseCLIStatus   bool                `toml:"use_cli_status"`
	CLICommand     string              `toml:"cli_command"`
	Profiles       []fileProfileConfig `toml:"profiles"`
}

type fileProfileConfig struct {
	Name            string `toml:"name"`
	ConfigDir       string `toml:"config_dir"`
	CredentialsFile string `toml:"credentials_file"`
	KeychainService string `toml:"keychain_service"`
}

// DefaultConfig returns a Config with default thresholds and a single "default" profile.
func DefaultConfig() Config {
	return Config{
		WarnWithin:     DefaultWarnWithin,
		CriticalWithin: DefaultCriticalWithin,
		FailingAfter:   DefaultFailingAfter,
		CLICommand:     DefaultCLICommand,
		Profiles: []ProfileConfig{
			{Name: "default"},
		},
	}
}

// ParseConfig parses TOML into a validated Config.
func ParseConfig(data []byte) (Config, error) {
	var raw fileConfig
	if err := toml.Unmarshal(data, &raw); err != nil {
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	cfg := Config{
		WarnWithin:     DefaultWarnWithin,
		CriticalWithin: DefaultCriticalWithin,
		FailingAfter:   DefaultFailingAfter,
		Notify:         raw.Notify,
		UseCLIStatus:   raw.UseCLIStatus,
		CLICommand:     raw.CLICommand,
	}
	if cfg.CLICommand == "" {
		cfg.CLICommand = DefaultCLICommand
	}
	var err error
	if cfg.WarnWithin, err = parseOptionalDuration(raw.WarnWithin, DefaultWarnWithin, "warn_within"); err != nil {
		return Config{}, err
	}
	if cfg.CriticalWithin, err = parseOptionalDuration(raw.CriticalWithin, DefaultCriticalWithin, "critical_within"); err != nil {
		return Config{}, err
	}
	if cfg.FailingAfter, err = parseOptionalDuration(raw.FailingAfter, DefaultFailingAfter, "failing_after"); err != nil {
		return Config{}, err
	}
	if cfg.CriticalWithin >= cfg.WarnWithin {
		return Config{}, errors.New("critical_within must be less than warn_within")
	}
	for _, arg := range cfg.Notify {
		if arg == "" {
			return Config{}, errors.New("notify has an empty argument")
		}
	}
	if len(raw.Profiles) == 0 {
		cfg.Profiles = []ProfileConfig{{Name: "default"}}
		return cfg, nil
	}
	seen := make(map[string]struct{}, len(raw.Profiles))
	for _, p := range raw.Profiles {
		if p.Name == "" {
			return Config{}, errors.New("profile name is empty")
		}
		if _, dup := seen[p.Name]; dup {
			return Config{}, fmt.Errorf("duplicate profile %q", p.Name)
		}
		seen[p.Name] = struct{}{}
		cfg.Profiles = append(cfg.Profiles, ProfileConfig(p))
	}
	return cfg, nil
}

func parseOptionalDuration(raw string, def time.Duration, name string) (time.Duration, error) {
	if raw == "" {
		return def, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil {
		return 0, fmt.Errorf("%s: %w", name, err)
	}
	if d <= 0 {
		return 0, fmt.Errorf("%s must be positive", name)
	}
	return d, nil
}

// LoadConfig reads a TOML config file. If optional is true and the file does not
// exist, DefaultConfig is returned.
func LoadConfig(path string, optional bool) (Config, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: user's own config path
	if err != nil {
		if optional && errors.Is(err, fs.ErrNotExist) {
			return DefaultConfig(), nil
		}
		return Config{}, fmt.Errorf("read config: %w", err)
	}
	return ParseConfig(data)
}
