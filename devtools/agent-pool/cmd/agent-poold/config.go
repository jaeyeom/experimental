package main

import (
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/pelletier/go-toml/v2"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/catsync"
)

// GitHubACLConfig configures optional broker-side GitHub repository access
// checks using a requester-supplied GitHub token.
type GitHubACLConfig struct {
	RequireRepoAccessCheck bool   `toml:"require_repo_access_check"`
	APIBaseURL             string `toml:"api_base_url"`
	CacheTTL               string `toml:"cache_ttl"`
}

// Config defines the broker.toml configuration schema for agent-poold.
type Config struct {
	ListenAddr                string               `toml:"listen_addr"`
	DBPath                    string               `toml:"db_path"`
	RepoCacheDir              string               `toml:"repo_cache_dir"`
	CatalogSyncInterval       string               `toml:"catalog_sync_interval"`
	LeaseTTL                  string               `toml:"lease_ttl"`
	WorkerStaleAfter          string               `toml:"worker_stale_after"`
	MaxAttempts               int                  `toml:"max_attempts"`
	MaxConcurrentPerRequester int                  `toml:"max_concurrent_per_requester"`
	Repos                     []catsync.RepoConfig `toml:"repos"`
	GitHubACL                 GitHubACLConfig      `toml:"github_acl"`
}

// DefaultConfig returns a Config populated with standard defaults.
func DefaultConfig() Config {
	return Config{
		ListenAddr:                "127.0.0.1:8080",
		DBPath:                    "./agent-poold.db",
		RepoCacheDir:              "./repo-cache",
		CatalogSyncInterval:       "60s",
		LeaseTTL:                  "60s",
		WorkerStaleAfter:          "90s",
		MaxAttempts:               2,
		MaxConcurrentPerRequester: 5,
		GitHubACL: GitHubACLConfig{
			RequireRepoAccessCheck: false,
			APIBaseURL:             "https://api.github.com",
			CacheTTL:               "5m",
		},
	}
}

// LoadConfig reads and parses a TOML configuration file from path, applying
// default values for omitted fields.
func LoadConfig(path string) (Config, error) {
	if path == "" {
		return Config{}, errors.New("config path must not be empty")
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		return Config{}, fmt.Errorf("read config %q: %w", path, err)
	}
	cfg := DefaultConfig()
	if err := toml.Unmarshal(raw, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse config %q: %w", path, err)
	}
	if _, err := cfg.ParsedDurations(); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// ParsedDurations holds validated duration fields from Config.
type ParsedDurations struct {
	CatalogSyncInterval time.Duration
	LeaseTTL            time.Duration
	WorkerStaleAfter    time.Duration
	GitHubACLCacheTTL   time.Duration
}

// ParsedDurations validates and parses all duration fields in Config.
func (c Config) ParsedDurations() (ParsedDurations, error) {
	syncInterval, err := parsePositiveDuration("catalog_sync_interval", c.CatalogSyncInterval)
	if err != nil {
		return ParsedDurations{}, err
	}
	leaseTTL, err := parsePositiveDuration("lease_ttl", c.LeaseTTL)
	if err != nil {
		return ParsedDurations{}, err
	}
	staleAfter, err := parsePositiveDuration("worker_stale_after", c.WorkerStaleAfter)
	if err != nil {
		return ParsedDurations{}, err
	}
	ghCacheTTL, err := parsePositiveDuration("github_acl.cache_ttl", c.GitHubACL.CacheTTL)
	if err != nil {
		return ParsedDurations{}, err
	}
	return ParsedDurations{
		CatalogSyncInterval: syncInterval,
		LeaseTTL:            leaseTTL,
		WorkerStaleAfter:    staleAfter,
		GitHubACLCacheTTL:   ghCacheTTL,
	}, nil
}

func parsePositiveDuration(name, raw string) (time.Duration, error) {
	d, err := time.ParseDuration(raw)
	if err != nil {
		return 0, fmt.Errorf("invalid %s %q: %w", name, raw, err)
	}
	if d <= 0 {
		return 0, fmt.Errorf("%s must be positive, got %s", name, raw)
	}
	return d, nil
}
