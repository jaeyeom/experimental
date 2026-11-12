package config

import (
	"fmt"
	"path"
	"slices"
	"strings"
	"time"
)

// Defaults for the open_tabs_* keys.
const (
	defaultOpenTabsMaxPerRound  = 2
	defaultOpenTabsReadyTimeout = 90 * time.Second
)

// OpenTabWorkspace is the herdr workspace (label or id) for a new tab on
// machine, or empty for herdr's default. Empty machine is the local
// server.
func (c Config) OpenTabWorkspace(machine string) string {
	return c.OpenTabsWorkspace[machineKey(machine)]
}

// OpenTabCwd is the directory, on machine, for a new tab's agent:
// open_tabs_cwd, else the machine's machine_checkouts entry. Empty means
// none is configured.
func (c Config) OpenTabCwd(machine string) string {
	m := machineKey(machine)
	if dir := c.OpenTabsCwd[m]; dir != "" {
		return dir
	}
	return c.MachineCheckouts[m]
}

func machineKey(machine string) string {
	if machine == "" {
		return LocalMachine
	}
	return machine
}

func applyOpenTabsKey(cfg *Config, key, val string) error {
	switch key {
	case "open_missing_tabs":
		b, err := parseBool(key, val)
		cfg.OpenMissingTabs = b
		return err
	case "open_tabs_workspace":
		m, err := parseMachineValues(key, val, false)
		cfg.OpenTabsWorkspace = m
		return err
	case "open_tabs_cwd":
		m, err := parseMachineValues(key, val, true)
		cfg.OpenTabsCwd = m
		return err
	case "open_tabs_agent":
		cfg.OpenTabsAgent = strings.Fields(val)
		return nil
	case "open_tabs_max_per_round":
		n, err := parseNonNegativeInt(key, val)
		cfg.OpenTabsMaxPerRound = n
		return err
	case "open_tabs_ready_timeout":
		d, err := time.ParseDuration(val)
		if err != nil || d <= 0 {
			return &KeyError{Key: key, Reason: fmt.Sprintf("invalid duration %q; want a positive Go duration such as 90s", val)}
		}
		cfg.OpenTabsReadyTimeout = d
		return nil
	}
	return nil
}

// parseMachineValues reads comma-separated entries: machine=value for
// one machine, and at most one plain value, the local default. absPath
// requires every value to be an absolute path.
func parseMachineValues(key, val string, absPath bool) (map[string]string, error) {
	out := make(map[string]string)
	plain := false
	for _, entry := range strings.Split(val, ",") {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		machine, value, ok := strings.Cut(entry, "=")
		if !ok {
			if plain {
				return nil, &KeyError{Key: key, Reason: fmt.Sprintf("second plain value %q; use machine=value for other machines", entry)}
			}
			plain = true
			machine, value = LocalMachine, entry
		}
		machine, value = strings.TrimSpace(machine), strings.TrimSpace(value)
		switch {
		case machine == "" || value == "":
			return nil, &KeyError{Key: key, Reason: fmt.Sprintf("invalid entry %q; want machine=value", entry)}
		case absPath && !path.IsAbs(value):
			return nil, &KeyError{Key: key, Reason: fmt.Sprintf("%q for %s is not an absolute path", value, machine)}
		}
		out[machine] = value
	}
	return out, nil
}

func validateOpenTabs(c Config) error {
	for _, key := range []string{"open_tabs_workspace", "open_tabs_cwd"} {
		values := c.OpenTabsWorkspace
		if key == "open_tabs_cwd" {
			values = c.OpenTabsCwd
		}
		for m := range values {
			if m != LocalMachine && !slices.Contains(c.Machines, m) {
				return &KeyError{Key: key, Reason: fmt.Sprintf("unknown machine %q; list it in machines", m)}
			}
		}
	}
	if c.OpenMissingTabs && len(c.OpenTabsAgent) == 0 {
		return &KeyError{Key: "open_tabs_agent", Reason: "must name the agent to start when open_missing_tabs is true"}
	}
	return nil
}
