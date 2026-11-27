// Package forkkeeper implements the fork-keeper scheduler and deterministic
// tripwire checks for rebasing personal forks onto upstream.
package forkkeeper

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// DefaultGHBin is the GitHub CLI binary used when neither gh_bin nor GH_BIN is set.
const DefaultGHBin = "gh"

// Fork configures a single personal fork managed by fork-keeper.
type Fork struct {
	Name        string   `json:"name"`
	Repo        string   `json:"repo"`
	ForkURL     string   `json:"fork_url,omitempty"` //nolint:tagliatelle // External forks.json snake_case schema.
	UpstreamURL string   `json:"upstream_url"`       //nolint:tagliatelle // External forks.json snake_case schema.
	Branch      string   `json:"branch,omitempty"`
	Agent       []string `json:"agent,omitempty"`
	ReviewerCmd []string `json:"reviewer_cmd,omitempty"` //nolint:tagliatelle // External forks.json snake_case schema.
}

// Config is the root structure of forks.json.
type Config struct {
	GHBin       string   `json:"gh_bin,omitempty"`       //nolint:tagliatelle // External forks.json snake_case schema.
	ReviewerCmd []string `json:"reviewer_cmd,omitempty"` //nolint:tagliatelle // External forks.json snake_case schema.
	Forks       []Fork   `json:"forks"`
}

type rawConfig struct {
	GHBin       *string   `json:"gh_bin"`       //nolint:tagliatelle // External forks.json snake_case schema.
	ReviewerCmd *[]string `json:"reviewer_cmd"` //nolint:tagliatelle // External forks.json snake_case schema.
	Forks       []Fork    `json:"forks"`
}

// DefaultConfigPath returns the configured forks.json path from
// FORK_KEEPER_CONFIG or ~/.config/fork-keeper/forks.json.
func DefaultConfigPath() string {
	if p := os.Getenv("FORK_KEEPER_CONFIG"); p != "" {
		return p
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".config", "fork-keeper", "forks.json")
}

// DefaultCacheDir returns the cache directory from FORK_KEEPER_CACHE or
// ~/.cache/fork-keeper.
func DefaultCacheDir() string {
	if p := os.Getenv("FORK_KEEPER_CACHE"); p != "" {
		return p
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".cache", "fork-keeper")
}

// DefaultScriptsDir returns the optional legacy scripts directory from
// FORK_KEEPER_SCRIPTS. When unset, fork-keeper uses its built-in Go range and
// verdict implementations without needing any external scripts on disk.
func DefaultScriptsDir() string {
	return os.Getenv("FORK_KEEPER_SCRIPTS")
}

// LoadConfig reads, parses, and validates a forks.json configuration file.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return Config{}, fmt.Errorf("read config %s: %w", path, err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		return Config{}, fmt.Errorf("parse config %s: %w", path, err)
	}
	return cfg, nil
}

// ParseConfig parses and validates a forks.json payload.
func ParseConfig(data []byte) (Config, error) {
	var raw rawConfig
	if err := json.Unmarshal(data, &raw); err != nil {
		return Config{}, fmt.Errorf("decode config JSON: %w", err)
	}
	cfg := Config{
		Forks: raw.Forks,
	}
	if raw.GHBin != nil {
		if strings.TrimSpace(*raw.GHBin) == "" {
			return Config{}, errors.New("gh_bin: expected a non-empty binary name or path, found empty")
		}
		cfg.GHBin = *raw.GHBin
	}
	if raw.ReviewerCmd != nil {
		if len(*raw.ReviewerCmd) == 0 {
			return Config{}, errors.New("reviewer_cmd: expected a non-empty command array, found empty")
		}
		if err := validateCommandSlice("reviewer_cmd", *raw.ReviewerCmd); err != nil {
			return Config{}, err
		}
		cfg.ReviewerCmd = append([]string(nil), (*raw.ReviewerCmd)...)
	}
	if err := validateForks(cfg.Forks); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

func validateForks(forks []Fork) error {
	seen := make(map[string]bool, len(forks))
	for i, f := range forks {
		name := strings.TrimSpace(f.Name)
		if name == "" {
			return fmt.Errorf("forks[%d]: name must not be empty", i)
		}
		if seen[name] {
			return fmt.Errorf("forks[%d]: duplicate fork name %q", i, name)
		}
		seen[name] = true
		if err := validateFork(name, f); err != nil {
			return err
		}
	}
	return nil
}

func validateFork(name string, f Fork) error {
	if strings.TrimSpace(f.Repo) == "" {
		return fmt.Errorf("fork %s: repo must not be empty", name)
	}
	if strings.HasPrefix(f.Repo, "-") {
		return fmt.Errorf("fork %s: repo %q must not start with '-'", name, f.Repo)
	}
	if strings.HasPrefix(f.ForkURL, "-") {
		return fmt.Errorf("fork %s: fork_url %q must not start with '-'", name, f.ForkURL)
	}
	if strings.TrimSpace(f.UpstreamURL) == "" {
		return fmt.Errorf("fork %s: upstream_url must not be empty", name)
	}
	if strings.HasPrefix(f.UpstreamURL, "-") {
		return fmt.Errorf("fork %s: upstream_url %q must not start with '-'", name, f.UpstreamURL)
	}
	if strings.HasPrefix(f.Branch, "-") {
		return fmt.Errorf("fork %s: branch %q must not start with '-'", name, f.Branch)
	}
	if len(f.Agent) > 0 && len(f.ReviewerCmd) > 0 {
		return fmt.Errorf("fork %s: set either agent or reviewer_cmd, not both", name)
	}
	if len(f.Agent) > 0 {
		if err := validateCommandSlice("fork "+name+".agent", f.Agent); err != nil {
			return err
		}
	}
	if len(f.ReviewerCmd) > 0 {
		if err := validateCommandSlice("fork "+name+".reviewer_cmd", f.ReviewerCmd); err != nil {
			return err
		}
	}
	return nil
}

func validateCommandSlice(field string, cmd []string) error {
	for i, arg := range cmd {
		if strings.TrimSpace(arg) == "" {
			return fmt.Errorf("%s[%d]: command token must not be empty", field, i)
		}
	}
	return nil
}

// ResolveGHBin resolves the GitHub CLI executable: override (if non-empty),
// else cfg.GHBin, else GH_BIN from env, else DefaultGHBin ("gh"). A leading ~/
// expands to HOME from env. An explicit but empty GH_BIN environment variable
// returns an error.
func (c Config) ResolveGHBin(override string, env []string) (string, error) {
	if override != "" {
		if strings.TrimSpace(override) == "" {
			return "", errors.New("gh_bin: expected a non-empty binary name or path, found empty")
		}
		bin, err := expandHomePath(override, env)
		if err != nil {
			return "", fmt.Errorf("gh_bin: %w", err)
		}
		return bin, nil
	}
	if c.GHBin != "" {
		bin, err := expandHomePath(c.GHBin, env)
		if err != nil {
			return "", fmt.Errorf("gh_bin: %w", err)
		}
		return bin, nil
	}
	if val, ok := lookupEnv(env, "GH_BIN"); ok {
		if strings.TrimSpace(val) == "" {
			return "", errors.New("GH_BIN: expected a non-empty binary name or path, found empty")
		}
		bin, err := expandHomePath(val, env)
		if err != nil {
			return "", fmt.Errorf("GH_BIN: %w", err)
		}
		return bin, nil
	}
	return DefaultGHBin, nil
}

// EffectiveReviewerCommand returns the external reviewer command configured for
// fork f, or nil when fork-keeper should use its built-in standalone mode.
func (f Fork) EffectiveReviewerCommand(cliOverride, cfgReviewer []string, env []string) ([]string, error) {
	if len(cliOverride) > 0 {
		return append([]string(nil), cliOverride...), nil
	}
	if len(f.ReviewerCmd) > 0 {
		return append([]string(nil), f.ReviewerCmd...), nil
	}
	if len(f.Agent) > 0 {
		return append([]string(nil), f.Agent...), nil
	}
	if len(cfgReviewer) > 0 {
		return append([]string(nil), cfgReviewer...), nil
	}
	if raw, ok := lookupEnv(env, "FORK_KEEPER_REVIEWER_CMD"); ok && raw != "" {
		if strings.TrimSpace(raw) == "" {
			return nil, errors.New("FORK_KEEPER_REVIEWER_CMD: expected a non-empty command, found whitespace")
		}
		return SplitCommand(raw)
	}
	return nil, nil
}

func (f Fork) withDefaults() Fork {
	if f.Branch == "" {
		f.Branch = "main"
	}
	if f.ForkURL == "" {
		f.ForkURL = fmt.Sprintf("https://github.com/%s.git", f.Repo)
	}
	return f
}

func lookupEnv(env []string, key string) (string, bool) {
	var val string
	var found bool
	for _, entry := range env {
		if k, v, ok := strings.Cut(entry, "="); ok && k == key {
			val = v
			found = true
		}
	}
	return val, found
}

func expandHomePath(p string, env []string) (string, error) {
	if p != "~" && !strings.HasPrefix(p, "~/") {
		return p, nil
	}
	home, _ := lookupEnv(env, "HOME")
	if home == "" {
		return "", errors.New("HOME is not set")
	}
	if p == "~" {
		return home, nil
	}
	return filepath.Join(home, p[2:]), nil
}
