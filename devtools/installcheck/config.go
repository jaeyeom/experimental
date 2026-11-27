// Package installcheck watches GitHub repositories and notifies a configured
// target when watched issues land on main. It never installs or builds anything;
// it only sends a notification.
package installcheck

import (
	"bytes"
	"errors"
	"fmt"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

// DefaultGHBin is the GitHub CLI binary used when neither gh_bin nor GH_BIN is set.
const DefaultGHBin = "gh"

// Supported notification delivery modes.
const (
	NotifyModeAuto     = ""
	NotifyModeAgentbus = "agentbus"
	NotifyModeCommand  = "command"
	NotifyModeStdout   = "stdout"
)

// Config is the user's install-check config.
type Config struct {
	NotifyMode     string       `toml:"notify_mode"`
	NotifyName     string       `toml:"notify_name"`
	NotifyCommand  []string     `toml:"notify_command"`
	GHBin          string       `toml:"gh_bin"`
	StateFile      string       `toml:"state_file"`
	SkipNotPlanned bool         `toml:"skip_not_planned"`
	Repos          []RepoConfig `toml:"repo"`
}

// RepoConfig is one watched repository.
type RepoConfig struct {
	URL    string `toml:"url"`
	Issues []int  `toml:"issues"`
	Query  string `toml:"query"`
}

type rawConfig struct {
	NotifyMode     string       `toml:"notify_mode"`
	NotifyName     string       `toml:"notify_name"`
	NotifyCommand  *[]string    `toml:"notify_command"`
	GHBin          *string      `toml:"gh_bin"`
	StateFile      string       `toml:"state_file"`
	SkipNotPlanned bool         `toml:"skip_not_planned"`
	Repos          []RepoConfig `toml:"repo"`
}

// ParseConfig parses and validates TOML config. Unknown keys are errors.
func ParseConfig(data []byte) (Config, error) {
	var raw rawConfig
	dec := toml.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&raw); err != nil {
		var strict *toml.StrictMissingError
		if errors.As(err, &strict) {
			return Config{}, fmt.Errorf("parse config: %s", strict.String())
		}
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	cfg := Config{
		NotifyMode:     strings.TrimSpace(raw.NotifyMode),
		NotifyName:     raw.NotifyName,
		StateFile:      raw.StateFile,
		SkipNotPlanned: raw.SkipNotPlanned,
		Repos:          raw.Repos,
	}
	if raw.GHBin != nil {
		if strings.TrimSpace(*raw.GHBin) == "" {
			return Config{}, errors.New("gh_bin: expected a non-empty binary name or path, found empty")
		}
		cfg.GHBin = *raw.GHBin
	}
	if raw.NotifyCommand != nil {
		if len(*raw.NotifyCommand) == 0 {
			return Config{}, errors.New("notify_command must not be empty")
		}
		cfg.NotifyCommand = append([]string(nil), (*raw.NotifyCommand)...)
	}
	if err := validateNotifier(cfg); err != nil {
		return Config{}, err
	}
	if err := validateRepos(cfg.Repos); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

func validateNotifier(cfg Config) error {
	if err := validateNotifyFields(cfg); err != nil {
		return err
	}
	return validateNotifyMode(cfg)
}

func validateNotifyFields(cfg Config) error {
	if cfg.NotifyName != "" && strings.TrimSpace(cfg.NotifyName) == "" {
		return errors.New("notify_name must not be whitespace-only")
	}
	for _, arg := range cfg.NotifyCommand {
		if strings.TrimSpace(arg) == "" {
			return errors.New("notify_command has an empty argument")
		}
	}
	return nil
}

func validateNotifyMode(cfg Config) error {
	switch cfg.NotifyMode {
	case NotifyModeAuto:
		if len(cfg.NotifyCommand) == 0 && strings.TrimSpace(cfg.NotifyName) == "" {
			return errors.New("configure notify_command, notify_name, or notify_mode = \"stdout\"")
		}
	case NotifyModeCommand:
		if len(cfg.NotifyCommand) == 0 {
			return errors.New("notify_command is required when notify_mode = \"command\"")
		}
	case NotifyModeAgentbus:
		if len(cfg.NotifyCommand) > 0 {
			return errors.New("notify_command cannot be set when notify_mode = \"agentbus\"")
		}
		if strings.TrimSpace(cfg.NotifyName) == "" {
			return errors.New("notify_name is required when notify_mode = \"agentbus\"")
		}
	case NotifyModeStdout:
		if len(cfg.NotifyCommand) > 0 {
			return errors.New("notify_command cannot be set when notify_mode = \"stdout\"")
		}
	default:
		return fmt.Errorf("notify_mode %q: want %q, %q, or %q",
			cfg.NotifyMode, NotifyModeAgentbus, NotifyModeCommand, NotifyModeStdout)
	}
	return nil
}

func validateRepos(repos []RepoConfig) error {
	if len(repos) == 0 {
		return errors.New("at least one [[repo]] is required")
	}
	seen := make(map[string]string, len(repos))
	for _, r := range repos {
		if strings.HasPrefix(r.URL, "-") {
			return fmt.Errorf("repo url %q: must not start with '-'", r.URL)
		}
		slug, err := r.Slug()
		if err != nil {
			return err
		}
		if len(r.Issues) == 0 && strings.TrimSpace(r.Query) == "" {
			return fmt.Errorf("repo %s: set issues, query, or both", r.URL)
		}
		for _, n := range r.Issues {
			if n <= 0 {
				return fmt.Errorf("repo %s: issue number %d must be positive", r.URL, n)
			}
		}
		if prevURL, dup := seen[slug]; dup {
			return fmt.Errorf("repo %s (%s) duplicates %s", r.URL, slug, prevURL)
		}
		seen[slug] = r.URL
	}
	return nil
}

// EffectiveNotifyMode returns the resolved notification mode:
// "command", "stdout", or "agentbus".
func (c Config) EffectiveNotifyMode() string {
	switch c.NotifyMode {
	case NotifyModeCommand, NotifyModeStdout, NotifyModeAgentbus:
		return c.NotifyMode
	default:
		if len(c.NotifyCommand) > 0 {
			return NotifyModeCommand
		}
		if strings.TrimSpace(c.NotifyName) != "" {
			return NotifyModeAgentbus
		}
		return NotifyModeStdout
	}
}

// NotifyTargetLabel returns a short human-readable description of the
// configured notification destination.
func (c Config) NotifyTargetLabel() string {
	switch c.EffectiveNotifyMode() {
	case NotifyModeCommand:
		if len(c.NotifyCommand) > 0 {
			return c.NotifyCommand[0]
		}
		return "command"
	case NotifyModeStdout:
		return "stdout"
	default:
		return c.NotifyName
	}
}

// ResolveGHBin picks the gh binary: cfg.GHBin, else GH_BIN from env, else
// DefaultGHBin ("gh"). A leading ~/ expands to HOME from env. An explicit but
// empty GH_BIN environment variable returns an error.
func (c Config) ResolveGHBin(env []string) (string, error) {
	if c.GHBin != "" {
		bin, err := expandHomePath(c.GHBin, env)
		if err != nil {
			return "", fmt.Errorf("gh_bin: %w", err)
		}
		return bin, nil
	}
	val, ok := lookupEnv(env, "GH_BIN")
	if !ok {
		return DefaultGHBin, nil
	}
	if strings.TrimSpace(val) == "" {
		return "", errors.New("GH_BIN: expected a non-empty binary name or path, found empty")
	}
	bin, err := expandHomePath(val, env)
	if err != nil {
		return "", fmt.Errorf("GH_BIN: %w", err)
	}
	return bin, nil
}

// ResolveStateFile returns cfg.StateFile (expanding leading ~/) if non-empty,
// otherwise defaultPath.
func (c Config) ResolveStateFile(defaultPath string, env []string) (string, error) {
	if c.StateFile == "" {
		return defaultPath, nil
	}
	resolved, err := expandHomePath(c.StateFile, env)
	if err != nil {
		return "", fmt.Errorf("state_file: %w", err)
	}
	return resolved, nil
}

// Slug is owner/repo derived from the URL, for example from
// https://github.com/owner/repo.git or git@github.com:owner/repo.git.
func (r RepoConfig) Slug() (string, error) {
	s := strings.TrimSuffix(strings.TrimRight(r.URL, "/"), ".git")
	s = strings.ReplaceAll(s, ":", "/")
	parts := strings.Split(s, "/")
	if r.URL == "" || len(parts) < 2 || parts[len(parts)-1] == "" || parts[len(parts)-2] == "" {
		return "", fmt.Errorf("repo url %q: cannot derive owner/repo", r.URL)
	}
	return parts[len(parts)-2] + "/" + parts[len(parts)-1], nil
}
