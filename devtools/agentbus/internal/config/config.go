// Package config reads the optional agentbus config file,
// $XDG_CONFIG_HOME/agentbus/config.toml. It holds a small TOML subset:
// `key = value` lines and # comments, where a value is a quoted string or
// a one-line list of quoted strings.
//
//	machines = ["linux2"]   # saved herdr machines to include
//	remote_timeout = "5s"   # bound on each call to another machine
//	hub = "me@hub"          # run every command on this ssh target
//	machine = "linux2"      # this machine's herdr label on the hub
//	hub_socket = "/run/user/1000/agentbus/hub.sock"  # hub.sock on the hub
//	lease_checkouts = ["/home/u/src/repo"]  # checkouts the lease guards
//	lease_check_interval = "2m"   # how often an overrun holder is checked
//	lease_checkin_repeat = "15m"  # least time between check-ins
//	lease_idle_grace = "5m"       # idle and silent this long: take over
//	lease_takeover = "log"        # "on" carries takeovers out; "log" only logs them
//	lease_ignore_procs = ["zsh", "claude"]  # adopt's process check ignores these
//	hibernate_exempt = ["ops/*"]  # names, handles, or globs hibernate never stops
//	wake_timeout = "60s"          # how long a woken agent has to get ready
package config

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
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
	// HubSocket is the hub daemon's hub.sock path on the hub. agentbus
	// install in hub mode forwards the local tunnel socket to it.
	HubSocket string
	// LeaseCheckouts are this machine's guarded checkouts: absolute
	// paths of git top-levels that need the lease before a change.
	LeaseCheckouts []string
	// LeaseCheckInterval is how often the daemon looks at an overrun
	// holder, and how far it extends one that is still working. Zero is
	// the default.
	LeaseCheckInterval time.Duration
	// LeaseCheckinRepeat is the least time between two check-ins to a
	// holder that keeps working. Zero is the default.
	LeaseCheckinRepeat time.Duration
	// LeaseIdleGrace is how long a checked-in holder may stay idle and
	// silent before it is taken over. Zero is the default.
	LeaseIdleGrace time.Duration
	// LeaseIgnoreProcs are the program names whose working directory in
	// a checkout does not block lease adopt. Nil means the default list;
	// a set list replaces it.
	LeaseIgnoreProcs []string
	// LeaseTakeover is lease_takeover as written when it is "log" or
	// "on", else empty. Read it through TakeoverMode.
	LeaseTakeover string
	// HibernateExempt are names, handles, or globs over them that
	// agentbus hibernate never stops.
	HibernateExempt []string
	// WakeTimeout is how long the daemon waits for a hibernated agent it
	// relaunched to get ready. Zero is the default, 60s.
	WakeTimeout time.Duration
	// Warnings are problems that did not fail the load, such as an
	// invalid lease_takeover.
	Warnings []string
}

// TakeoverMode is "on" when lease_takeover is exactly "on", else "log":
// a missing or invalid key never turns takeover on.
func (c Config) TakeoverMode() string {
	if c.LeaseTakeover == "on" {
		return "on"
	}
	return "log"
}

var errLeaseCheckouts = errors.New(`lease_checkouts must be a list of absolute paths, such as ["/home/u/src/repo"]`)

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
		return parseMachines(cfg, value)
	case "remote_timeout", "lease_check_interval", "lease_checkin_repeat", "lease_idle_grace", "wake_timeout":
		return parseDurationKey(cfg, key, value)
	case "lease_checkouts":
		return parseLeaseCheckouts(cfg, value)
	case "lease_takeover":
		parseLeaseTakeover(cfg, value)
		return nil
	case "lease_ignore_procs":
		return parseLeaseIgnoreProcs(cfg, value)
	case "hibernate_exempt":
		return parseHibernateExempt(cfg, value)
	case "hub", "machine", "hub_socket":
		return parseHubKey(cfg, key, value)
	default:
		return fmt.Errorf("unknown key %s", key)
	}
}

// parseHibernateExempt reads hibernate_exempt, a list of names,
// handles, or globs.
func parseHibernateExempt(cfg *Config, value string) error {
	exempt, ok := parseList(value)
	if !ok {
		return errors.New(`hibernate_exempt must be a list of names, handles, or globs, such as ["ops/*"]`)
	}
	cfg.HibernateExempt = exempt
	return nil
}

// parseLeaseIgnoreProcs reads lease_ignore_procs, a list of program
// names.
func parseLeaseIgnoreProcs(cfg *Config, value string) error {
	names, ok := parseList(value)
	if !ok || slices.ContainsFunc(names, func(n string) bool { return strings.Contains(n, "/") }) {
		return errors.New(`lease_ignore_procs must be a list of program names, such as ["zsh", "claude"]`)
	}
	cfg.LeaseIgnoreProcs = names
	return nil
}

// parseDurationKey reads one of the duration keys.
func parseDurationKey(cfg *Config, key, value string) error {
	switch key {
	case "remote_timeout":
		return parseDuration(&cfg.RemoteTimeout, key, value, "5s")
	case "lease_check_interval":
		return parseDuration(&cfg.LeaseCheckInterval, key, value, "2m")
	case "lease_checkin_repeat":
		return parseDuration(&cfg.LeaseCheckinRepeat, key, value, "15m")
	case "lease_idle_grace":
		return parseDuration(&cfg.LeaseIdleGrace, key, value, "5m")
	default:
		return parseDuration(&cfg.WakeTimeout, key, value, "60s")
	}
}

// parseDuration reads a positive duration string into dst.
func parseDuration(dst *time.Duration, key, value, example string) error {
	raw, rest, ok := parseString(value)
	d, err := time.ParseDuration(raw)
	if !ok || !blankOrComment(rest) || err != nil || d <= 0 {
		return fmt.Errorf("%s must be a positive duration, such as %q", key, example)
	}
	*dst = d
	return nil
}

// parseLeaseTakeover reads lease_takeover, "log" or "on". Any other
// value is a warning and means "log", so the daemon still starts with
// takeover off.
func parseLeaseTakeover(cfg *Config, value string) {
	raw, rest, ok := parseString(value)
	if ok && blankOrComment(rest) && (raw == "log" || raw == "on") {
		cfg.LeaseTakeover = raw
		return
	}
	cfg.LeaseTakeover = ""
	cfg.Warnings = append(cfg.Warnings, fmt.Sprintf(`lease_takeover = %s is not "log" or "on"; using "log"`, value))
}

// parseMachines reads machines, a list of saved-machine labels.
func parseMachines(cfg *Config, value string) error {
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
	return nil
}

// parseLeaseCheckouts reads lease_checkouts, a list of absolute paths.
func parseLeaseCheckouts(cfg *Config, value string) error {
	paths, ok := parseList(value)
	if !ok {
		return errLeaseCheckouts
	}
	for i, p := range paths {
		if !filepath.IsAbs(p) {
			return errLeaseCheckouts
		}
		paths[i] = filepath.Clean(p)
	}
	cfg.LeaseCheckouts = paths
	return nil
}

// parseHubSocket reads hub_socket, a path on the hub.
func parseHubSocket(cfg *Config, value string) error {
	path, err := parseName("hub_socket", value)
	switch {
	case err != nil:
		return err
	case !strings.HasPrefix(path, "/"):
		return errors.New("hub_socket must be an absolute path on the hub")
	case strings.Contains(path, ":"):
		// ssh -L separates the two socket paths with a colon.
		return errors.New("hub_socket must not contain a colon")
	}
	cfg.HubSocket = path
	return nil
}

// parseHubKey reads hub, machine, or hub_socket.
func parseHubKey(cfg *Config, key, value string) error {
	if key == "hub_socket" {
		return parseHubSocket(cfg, value)
	}
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
