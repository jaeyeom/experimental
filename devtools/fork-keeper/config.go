// Package forkkeeper implements the fork-keeper scheduler and deterministic
// tripwire checks for rebasing personal forks onto upstream.
package forkkeeper

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// Fork configures a single personal fork managed by fork-keeper.
type Fork struct {
	Name        string   `json:"name"`
	Repo        string   `json:"repo"`
	ForkURL     string   `json:"fork_url"`     //nolint:tagliatelle // External forks.json snake_case schema.
	UpstreamURL string   `json:"upstream_url"` //nolint:tagliatelle // External forks.json snake_case schema.
	Branch      string   `json:"branch"`
	Agent       []string `json:"agent"`
}

// Config is the root structure of forks.json.
type Config struct {
	Forks []Fork `json:"forks"`
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

// DefaultScriptsDir returns the claude-toolbox helper scripts directory from
// FORK_KEEPER_SCRIPTS or ~/.cache/claude-toolbox/plugins/fork-keeper/skills/fork-keeper/scripts.
func DefaultScriptsDir() string {
	if p := os.Getenv("FORK_KEEPER_SCRIPTS"); p != "" {
		return p
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".cache", "claude-toolbox", "plugins", "fork-keeper", "skills", "fork-keeper", "scripts")
}

// LoadConfig reads and parses a forks.json configuration file.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return Config{}, fmt.Errorf("read config %s: %w", path, err)
	}
	var cfg Config
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse config %s: %w", path, err)
	}
	return cfg, nil
}

func (f Fork) withDefaults() Fork {
	if f.Branch == "" {
		f.Branch = "main"
	}
	if f.ForkURL == "" {
		f.ForkURL = fmt.Sprintf("https://github.com/%s.git", f.Repo)
	}
	if len(f.Agent) == 0 {
		f.Agent = []string{"claude", "-p", "/fork-keeper:fork-keeper sync --out {out} -b {branch}"}
	}
	return f
}
