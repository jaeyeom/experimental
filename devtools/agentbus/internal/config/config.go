// Package config reads the optional agentbus config file,
// $XDG_CONFIG_HOME/agentbus/config.toml. It holds a small TOML subset:
// `key = value` lines and # comments, where a value is a quoted string or
// a one-line list of quoted strings.
//
//	machines = ["linux2"]   # saved herdr machines to include
//	remote_timeout = "5s"   # bound on each call to another machine
//	hub = "me@hub"          # run every command on this ssh target
//	machine = "linux2"      # this machine's herdr label on the hub
package config

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Config is the parsed file. The zero value is the default.
type Config struct {
	// Machines are herdr saved-machine labels or ids. Search and
	// inventory include their agents.
	Machines []string
	// RemoteTimeout bounds each call to another machine. Zero is the
	// adapter's default.
	RemoteTimeout time.Duration
	// Hub is an ssh target. When set, commands run there instead of
	// against a local daemon. AGENTBUS_HUB overrides it.
	Hub string
	// Machine is this machine's herdr label as the hub knows it. Hub
	// mode builds the caller's herdr:<machine>/<session>/<pane> from it.
	Machine string
}

// Path is $XDG_CONFIG_HOME/agentbus/config.toml, or
// $HOME/.config/agentbus/config.toml when XDG_CONFIG_HOME is unset.
func Path(env []string) (string, error) {
	root := envGet(env, "XDG_CONFIG_HOME")
	if root == "" {
		home := envGet(env, "HOME")
		if home == "" {
			return "", errors.New("HOME is not set; the agentbus config belongs in ~/.config/agentbus")
		}
		root = filepath.Join(home, ".config")
	}
	return filepath.Join(root, "agentbus", "config.toml"), nil
}

// Load reads the config file. A missing file is the zero Config.
func Load(env []string) (Config, error) {
	path, err := Path(env)
	if err != nil {
		return Config{}, err
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read config: %w", err)
	}
	cfg, err := Parse(string(data))
	if err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	return cfg, nil
}

// Parse reads the config text.
func Parse(text string) (Config, error) {
	var cfg Config
	for i, line := range strings.Split(text, "\n") {
		if err := parseLine(&cfg, line); err != nil {
			return Config{}, fmt.Errorf("line %d: %w", i+1, err)
		}
	}
	return cfg, nil
}

func parseLine(cfg *Config, line string) error {
	line = strings.TrimSpace(line)
	if line == "" || strings.HasPrefix(line, "#") {
		return nil
	}
	key, value, ok := strings.Cut(line, "=")
	if !ok {
		return errors.New("want key = value")
	}
	key = strings.TrimSpace(key)
	value = strings.TrimSpace(value)
	switch key {
	case "machines":
		machines, ok := parseList(value)
		if !ok {
			return errors.New("machines must be a list of strings, such as [\"linux2\"]")
		}
		for _, m := range machines {
			if m == "local" {
				return errors.New("machines must not list local; it is always included")
			}
		}
		cfg.Machines = machines
	case "remote_timeout":
		raw, rest, ok := parseString(value)
		d, err := time.ParseDuration(raw)
		if !ok || !blankOrComment(rest) || err != nil || d <= 0 {
			return errors.New("remote_timeout must be a positive duration, such as \"5s\"")
		}
		cfg.RemoteTimeout = d
	case "hub", "machine":
		return parseHubKey(cfg, key, value)
	default:
		return fmt.Errorf("unknown key %s", key)
	}
	return nil
}

// parseHubKey reads hub or machine.
func parseHubKey(cfg *Config, key, value string) error {
	name, err := parseName(key, value)
	if err != nil {
		return err
	}
	switch {
	case key == "hub" && strings.HasPrefix(name, "-"):
		return errors.New("hub must not start with -")
	case key == "hub":
		cfg.Hub = name
	case name == "local":
		return errors.New("machine must name this machine, not local")
	case strings.Contains(name, "/"):
		return errors.New("machine must not contain /")
	default:
		cfg.Machine = name
	}
	return nil
}

// parseName reads one non-empty string followed by an optional comment.
func parseName(key, value string) (string, error) {
	raw, rest, ok := parseString(value)
	if !ok || raw == "" || !blankOrComment(rest) {
		return "", fmt.Errorf("%s must be a non-empty string, such as \"linux2\"", key)
	}
	return raw, nil
}

// parseList reads ["a", "b"] followed by an optional comment.
// Every item is a non-empty string.
func parseList(value string) ([]string, bool) {
	rest, ok := strings.CutPrefix(value, "[")
	if !ok {
		return nil, false
	}
	out := []string{}
	for {
		rest = strings.TrimSpace(rest)
		if after, done := strings.CutPrefix(rest, "]"); done {
			return out, blankOrComment(after)
		}
		item, after, ok := parseString(rest)
		if !ok || item == "" {
			return nil, false
		}
		out = append(out, item)
		rest = strings.TrimSpace(after)
		if after, more := strings.CutPrefix(rest, ","); more {
			rest = after
		} else if !strings.HasPrefix(rest, "]") {
			return nil, false
		}
	}
}

// parseString reads one "quoted" string without escapes and returns the
// text after it.
func parseString(value string) (string, string, bool) {
	rest, ok := strings.CutPrefix(value, `"`)
	if !ok {
		return "", "", false
	}
	end := strings.IndexByte(rest, '"')
	if end < 0 || strings.ContainsRune(rest[:end], '\\') {
		return "", "", false
	}
	return rest[:end], rest[end+1:], true
}

func blankOrComment(s string) bool {
	s = strings.TrimSpace(s)
	return s == "" || strings.HasPrefix(s, "#")
}

func envGet(env []string, key string) string {
	for _, entry := range env {
		name, value, ok := strings.Cut(entry, "=")
		if ok && name == key {
			return value
		}
	}
	return ""
}
