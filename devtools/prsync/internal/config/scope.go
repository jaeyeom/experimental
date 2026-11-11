package config

import (
	"fmt"
	"slices"
	"strings"
	"unicode"
)

// LocalMachine names the local herdr server in tab_machine.
const LocalMachine = "local"

// JobTeamCover is the skip_jobs name of the team-cover suggestions.
const JobTeamCover = "team-cover"

// Jobs are the machine-wide steps skip_jobs may name. They are not tied
// to one PR's tab, so a second watcher would run them again.
var Jobs = []string{JobTeamCover}

// IncludeUntabbed reports whether a PR with no tab on any machine belongs
// to this run. untabbed wins; otherwise only the local run (or an
// unscoped run) includes them.
func (c Config) IncludeUntabbed() bool {
	switch c.Untabbed {
	case "include":
		return true
	case "exclude":
		return false
	}
	return c.TabMachine == "" || c.TabMachine == LocalMachine
}

// Skips reports whether skip_jobs names job.
func (c Config) Skips(job string) bool {
	return slices.Contains(c.SkipJobs, job)
}

func validateScope(c Config) error {
	if c.TabMachine != "" && c.TabMachine != LocalMachine && !slices.Contains(c.Machines, c.TabMachine) {
		want := LocalMachine
		if len(c.Machines) > 0 {
			want += " or one of machines (" + strings.Join(c.Machines, ", ") + ")"
		}
		return &KeyError{Key: "tab_machine", Reason: fmt.Sprintf("unknown machine %q; want %s", c.TabMachine, want)}
	}
	switch c.Untabbed {
	case "", "include", "exclude":
	default:
		return &KeyError{Key: "untabbed", Reason: fmt.Sprintf("invalid value %q; want include or exclude", c.Untabbed)}
	}
	for _, job := range c.SkipJobs {
		if !slices.Contains(Jobs, job) {
			return &KeyError{Key: "skip_jobs", Reason: fmt.Sprintf("unknown job %q; want one of %s", job, strings.Join(Jobs, ", "))}
		}
	}
	return nil
}

// ParseJobs splits a comma- or space-separated skip_jobs list, dropping
// duplicates. Names are checked by Validate.
func ParseJobs(val string) []string {
	fields := strings.FieldsFunc(val, func(r rune) bool { return r == ',' || unicode.IsSpace(r) })
	var out []string
	for _, job := range fields {
		if !slices.Contains(out, job) {
			out = append(out, job)
		}
	}
	return out
}

func applyScopeKey(cfg *Config, key, val string) {
	switch key {
	case "tab_machine":
		cfg.TabMachine = val
	case "untabbed":
		cfg.Untabbed = val
	case "skip_jobs":
		cfg.SkipJobs = ParseJobs(val)
	}
}
