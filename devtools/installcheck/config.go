// Package installcheck notifies an agent over agentbus when watched GitHub
// issues land on a repository's main branch. It never installs or builds
// anything; it only sends a message.
package installcheck

import (
	"errors"
	"fmt"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

// Config is the user's install-check config.
type Config struct {
	NotifyName     string       `toml:"notify_name"`
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

// ParseConfig parses and validates TOML.
func ParseConfig(data []byte) (Config, error) {
	var cfg Config
	if err := toml.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	if cfg.NotifyName == "" {
		return Config{}, errors.New("notify_name is required")
	}
	if len(cfg.Repos) == 0 {
		return Config{}, errors.New("at least one [[repo]] is required")
	}
	seen := map[string]bool{}
	for _, r := range cfg.Repos {
		if _, err := r.Slug(); err != nil {
			return Config{}, err
		}
		if len(r.Issues) == 0 && r.Query == "" {
			return Config{}, fmt.Errorf("repo %s: set issues, query, or both", r.URL)
		}
		if seen[r.URL] {
			return Config{}, fmt.Errorf("repo %s listed twice", r.URL)
		}
		seen[r.URL] = true
	}
	return cfg, nil
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
