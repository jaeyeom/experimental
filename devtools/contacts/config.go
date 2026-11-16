// Package contacts is a personal contact and relationship store that coding
// agents can query. SQLite is the source of truth; each person may also have
// a free-form markdown notes file that only the user writes.
package contacts

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/pelletier/go-toml/v2"
)

// DefaultRetentionDays is how long events and access-log rows are kept in
// full before contacts age compacts or deletes them.
const DefaultRetentionDays = 365

// Config is the TOML file at ConfigPath.
type Config struct {
	// DataDir holds contacts.db and people/. It must not be inside a git
	// work tree.
	DataDir string `toml:"data_dir"`
	// RetentionDays is the age, in days, past which contacts age acts.
	RetentionDays int `toml:"retention_days"`
}

// ConfigPath returns flagPath when it is set, else
// $XDG_CONFIG_HOME/contacts/config.toml, else ~/.config/contacts/config.toml.
func ConfigPath(flagPath string, getenv func(string) string) (string, error) {
	if flagPath != "" {
		return flagPath, nil
	}
	if p := getenv("CONTACTS_CONFIG"); p != "" {
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
	return filepath.Join(base, "contacts", "config.toml"), nil
}

// LoadConfig reads path and fills defaults. A missing file means all
// defaults.
func LoadConfig(path string, getenv func(string) string) (Config, error) {
	var c Config
	data, err := os.ReadFile(path)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return c, fmt.Errorf("read config: %w", err)
	default:
		if err := toml.Unmarshal(data, &c); err != nil {
			return c, fmt.Errorf("parse config %s: %w", path, err)
		}
	}
	if c.RetentionDays == 0 {
		c.RetentionDays = DefaultRetentionDays
	}
	if c.RetentionDays < 0 {
		return c, fmt.Errorf("config %s: retention_days must be positive", path)
	}
	if c.DataDir == "" {
		base := getenv("XDG_DATA_HOME")
		if base == "" {
			home, err := homeDir(getenv)
			if err != nil {
				return c, err
			}
			base = filepath.Join(home, ".local", "share")
		}
		c.DataDir = filepath.Join(base, "contacts")
	} else if rest, ok := strings.CutPrefix(c.DataDir, "~/"); ok {
		home, err := homeDir(getenv)
		if err != nil {
			return c, err
		}
		c.DataDir = filepath.Join(home, rest)
	}
	abs, err := filepath.Abs(c.DataDir)
	if err != nil {
		return c, fmt.Errorf("data_dir: %w", err)
	}
	c.DataDir = abs
	return c, nil
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

// CheckDataDir refuses a data directory inside a git work tree, so contact
// data never ends up in a repository. dir must be absolute.
func CheckDataDir(dir string) error {
	for d := dir; ; d = filepath.Dir(d) {
		if _, err := os.Lstat(filepath.Join(d, ".git")); err == nil {
			return fmt.Errorf("data_dir %s is inside the git work tree %s; choose a directory outside any repository", dir, d)
		}
		if parent := filepath.Dir(d); parent == d {
			return nil
		}
	}
}
