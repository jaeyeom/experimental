// Package memstore implements a compartmentalized personal memory store with
// default-deny profile policies and vault path guards.
package memstore

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

// DefaultRetentionDays is how long episodic events and access-log rows are
// kept before aging compacts or deletes them.
const DefaultRetentionDays = 365

// Supported vault residency modes.
const (
	ResidencyLocalOnly   = "local-only"
	ResidencyCloudInbox  = "cloud-inbox"
	ResidencyCloudHosted = "cloud-hosted"
)

// Config is the parsed memstore TOML configuration.
type Config struct {
	Vaults        map[string]VaultConfig   `toml:"vaults"`
	Profiles      map[string]ProfileConfig `toml:"profiles"`
	RetentionDays int                      `toml:"retention_days"`
}

// VaultConfig configures a single physical SQLite vault.
type VaultConfig struct {
	Name           string `toml:"name"`
	Path           string `toml:"path"`
	Residency      string `toml:"residency"`
	ContactsConfig string `toml:"contacts_config"`
}

// ProfileConfig configures a caller profile's allowed vaults, compartments,
// sensitivity ceiling, and capabilities.
type ProfileConfig struct {
	Name           string       `toml:"name"`
	Vaults         []string     `toml:"vaults"`
	Compartments   []string     `toml:"compartments"`
	MaxSensitivity Sensitivity  `toml:"max_sensitivity"`
	Capabilities   []Capability `toml:"capabilities"`
}

// ConfigPath resolves the memstore config file path in order:
// flagPath (--config), $MEMSTORE_CONFIG, $XDG_CONFIG_HOME/memstore/config.toml,
// or ~/.config/memstore/config.toml.
func ConfigPath(flagPath string, getenv func(string) string) (string, error) {
	if flagPath != "" {
		return flagPath, nil
	}
	if p := getenv("MEMSTORE_CONFIG"); p != "" {
		return p, nil
	}
	base := getenv("XDG_CONFIG_HOME")
	if base == "" {
		home, err := homeDir(getenv)
		if err != nil {
			return "", err
		}
		base = filepath.Join(home, ".config")
	}
	return filepath.Join(base, "memstore", "config.toml"), nil
}

// LoadConfig reads path, expands paths, fills defaults, and validates vaults
// and profiles. A missing file returns a Config with default RetentionDays.
func LoadConfig(path string, getenv func(string) string) (Config, error) {
	var cfg Config
	data, err := os.ReadFile(path)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return cfg, fmt.Errorf("read config: %w", err)
	default:
		if err := toml.Unmarshal(data, &cfg); err != nil {
			return cfg, fmt.Errorf("parse config %s: %w", path, err)
		}
	}

	if cfg.RetentionDays == 0 {
		cfg.RetentionDays = DefaultRetentionDays
	} else if cfg.RetentionDays < 0 {
		return cfg, fmt.Errorf("config %s: retention_days must be positive", path)
	}

	if err := normalizeVaults(&cfg, path, getenv); err != nil {
		return cfg, err
	}
	if err := validateProfiles(&cfg, path); err != nil {
		return cfg, err
	}
	return cfg, nil
}

func normalizeVaults(cfg *Config, cfgPath string, getenv func(string) string) error {
	for name, v := range cfg.Vaults {
		if v.Name == "" {
			v.Name = name
		}
		if v.Path == "" {
			return fmt.Errorf("config %s: vault %q has empty path", cfgPath, name)
		}
		expanded, err := expandTilde(v.Path, getenv)
		if err != nil {
			return fmt.Errorf("config %s: vault %q path: %w", cfgPath, name, err)
		}
		abs, err := filepath.Abs(expanded)
		if err != nil {
			return fmt.Errorf("config %s: vault %q path: %w", cfgPath, name, err)
		}
		v.Path = abs

		if v.ContactsConfig != "" {
			cc, err := expandTilde(v.ContactsConfig, getenv)
			if err != nil {
				return fmt.Errorf("config %s: vault %q contacts_config: %w", cfgPath, name, err)
			}
			v.ContactsConfig = cc
		}

		if v.Residency == "" {
			v.Residency = ResidencyLocalOnly
		} else if !isValidResidency(v.Residency) {
			return fmt.Errorf("config %s: vault %q has unknown residency %q", cfgPath, name, v.Residency)
		}
		cfg.Vaults[name] = v
	}
	return nil
}

func validateProfiles(cfg *Config, cfgPath string) error {
	for name, p := range cfg.Profiles {
		if p.Name == "" {
			p.Name = name
		}
		for _, v := range p.Vaults {
			if _, ok := cfg.Vaults[v]; !ok {
				return fmt.Errorf("config %s: profile %q references undefined vault %q", cfgPath, name, v)
			}
		}
		if _, ok := p.MaxSensitivity.rank(); !ok {
			return fmt.Errorf("config %s: profile %q has unknown max_sensitivity %q", cfgPath, name, p.MaxSensitivity)
		}
		for _, cap := range p.Capabilities {
			if !cap.valid() {
				return fmt.Errorf("config %s: profile %q has unknown capability %q", cfgPath, name, cap)
			}
		}
		cfg.Profiles[name] = p
	}
	return nil
}

func isValidResidency(r string) bool {
	switch r {
	case ResidencyLocalOnly, ResidencyCloudInbox, ResidencyCloudHosted:
		return true
	default:
		return false
	}
}

func expandTilde(p string, getenv func(string) string) (string, error) {
	rest, ok := strings.CutPrefix(p, "~/")
	if !ok {
		return p, nil
	}
	home, err := homeDir(getenv)
	if err != nil {
		return "", err
	}
	return filepath.Join(home, rest), nil
}

func homeDir(getenv func(string) string) (string, error) {
	if h := getenv("HOME"); h != "" {
		return h, nil
	}
	h, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("find home directory: %w", err)
	}
	return h, nil
}
