package ghoutbox

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
)

// DefaultPollInterval is how often poll --watch checks decide.
const DefaultPollInterval = 30 * time.Second

// LintConfig is the deny rules.
type LintConfig struct {
	// Defaults turns on the decide id rule and DefaultPhrases.
	Defaults bool
	// AgentbusNames refuses every name agentbus list prints.
	AgentbusNames bool
	// AgentPattern is a regexp for agentbus names, such as role/name.
	AgentPattern string
	// Patterns are extra regexps.
	Patterns []string
	// Phrases are extra literal phrases, matched as whole words
	// without regard to case.
	Phrases []string
}

// Config is the gh-outbox config.
type Config struct {
	// DataDir holds one JSON file per draft.
	DataDir string
	// NotifyPane is passed to decide ask --notify-pane when set.
	NotifyPane      string
	DecideCommand   []string
	AgentbusCommand []string
	GHCommand       []string
	PollInterval    time.Duration
	Lint            LintConfig
}

type fileConfig struct {
	DataDir         string   `toml:"data_dir"`
	NotifyPane      string   `toml:"notify_pane"`
	DecideCommand   []string `toml:"decide_command"`
	AgentbusCommand []string `toml:"agentbus_command"`
	GHCommand       []string `toml:"gh_command"`
	PollInterval    string   `toml:"poll_interval"`
	Lint            struct {
		Defaults      *bool    `toml:"defaults"`
		AgentbusNames *bool    `toml:"agentbus_names"`
		AgentPattern  string   `toml:"agent_pattern"`
		Patterns      []string `toml:"patterns"`
		Phrases       []string `toml:"phrases"`
	} `toml:"lint"`
}

// DefaultConfig is the config used when there is no config file. Its
// DataDir is empty; LoadConfig fills it.
func DefaultConfig() Config {
	return Config{
		DecideCommand:   []string{"decide"},
		AgentbusCommand: []string{"agentbus"},
		GHCommand:       []string{"gh"},
		PollInterval:    DefaultPollInterval,
		Lint:            LintConfig{Defaults: true, AgentbusNames: true},
	}
}

// ParseConfig parses TOML config. Unknown keys are errors.
func ParseConfig(data []byte) (Config, error) {
	var raw fileConfig
	dec := toml.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&raw); err != nil {
		var strict *toml.StrictMissingError
		if errors.As(err, &strict) {
			return Config{}, fmt.Errorf("parse config: %s", strict.String())
		}
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	cfg := DefaultConfig()
	cfg.DataDir = raw.DataDir
	cfg.NotifyPane = raw.NotifyPane
	for _, c := range []struct {
		key string
		in  []string
		out *[]string
	}{
		{"decide_command", raw.DecideCommand, &cfg.DecideCommand},
		{"agentbus_command", raw.AgentbusCommand, &cfg.AgentbusCommand},
		{"gh_command", raw.GHCommand, &cfg.GHCommand},
	} {
		if raw := c.in; raw != nil {
			if len(raw) == 0 || raw[0] == "" {
				return Config{}, fmt.Errorf("%s: want a command and its arguments", c.key)
			}
			*c.out = raw
		}
	}
	if raw.PollInterval != "" {
		d, err := time.ParseDuration(raw.PollInterval)
		if err != nil || d <= 0 {
			return Config{}, fmt.Errorf("poll_interval %q: want a positive duration", raw.PollInterval)
		}
		cfg.PollInterval = d
	}
	if raw.Lint.Defaults != nil {
		cfg.Lint.Defaults = *raw.Lint.Defaults
	}
	if raw.Lint.AgentbusNames != nil {
		cfg.Lint.AgentbusNames = *raw.Lint.AgentbusNames
	}
	cfg.Lint.AgentPattern = raw.Lint.AgentPattern
	cfg.Lint.Patterns = raw.Lint.Patterns
	cfg.Lint.Phrases = raw.Lint.Phrases
	if _, err := BuildRules(cfg.Lint, nil); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

// LoadConfig reads path. A missing file is the default config. An empty
// data_dir is DefaultDataDir; a relative one is resolved against the
// config file's directory.
func LoadConfig(path string, env []string) (Config, error) {
	cfg := DefaultConfig()
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own config path
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return Config{}, fmt.Errorf("read config: %w", err)
	default:
		if cfg, err = ParseConfig(data); err != nil {
			return Config{}, fmt.Errorf("%s: %w", path, err)
		}
	}
	switch {
	case cfg.DataDir == "":
		if cfg.DataDir, err = DefaultDataDir(env); err != nil {
			return Config{}, err
		}
	case !filepath.IsAbs(cfg.DataDir):
		cfg.DataDir = filepath.Join(filepath.Dir(path), cfg.DataDir)
	}
	return cfg, nil
}

// DefaultConfigPath is $XDG_CONFIG_HOME/gh-outbox/config.toml, or
// $HOME/.config/gh-outbox/config.toml.
func DefaultConfigPath(env []string) (string, error) {
	return xdgPath(env, "XDG_CONFIG_HOME", ".config", "config.toml")
}

// DefaultDataDir is $XDG_STATE_HOME/gh-outbox, or
// $HOME/.local/state/gh-outbox.
func DefaultDataDir(env []string) (string, error) {
	return xdgPath(env, "XDG_STATE_HOME", filepath.Join(".local", "state"), "")
}

func xdgPath(env []string, key, homeDir, file string) (string, error) {
	root := lookupEnv(env, key)
	if root == "" {
		home := lookupEnv(env, "HOME")
		if home == "" {
			return "", errors.New("HOME is not set")
		}
		root = filepath.Join(home, homeDir)
	}
	return filepath.Join(root, "gh-outbox", file), nil
}

func lookupEnv(env []string, key string) string {
	value := ""
	for _, item := range env {
		if k, v, ok := strings.Cut(item, "="); ok && k == key {
			value = v
		}
	}
	return value
}
