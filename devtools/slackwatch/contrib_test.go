package slackwatch

import (
	_ "embed"
	"strings"
	"testing"
)

//go:embed contrib/slackwatch.service
var exampleUnit string

// parseUnit parses a systemd unit file into section -> key -> values. It
// fails on anything that is not a comment, a blank line, a [Section]
// header, or Key=Value inside a section.
func parseUnit(t *testing.T, text string) map[string]map[string][]string {
	t.Helper()
	out := map[string]map[string][]string{}
	section := ""
	for i, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		switch {
		case line == "" || strings.HasPrefix(line, "#") || strings.HasPrefix(line, ";"):
		case strings.HasPrefix(line, "[") && strings.HasSuffix(line, "]"):
			section = line[1 : len(line)-1]
			if _, dup := out[section]; dup {
				t.Errorf("line %d: duplicate section %s", i+1, section)
			}
			out[section] = map[string][]string{}
		default:
			key, val, ok := strings.Cut(line, "=")
			if !ok || section == "" || strings.TrimSpace(key) == "" || strings.HasSuffix(line, `\`) {
				t.Errorf("line %d: not Key=Value in a section: %q", i+1, line)
				continue
			}
			key = strings.TrimSpace(key)
			out[section][key] = append(out[section][key], strings.TrimSpace(val))
		}
	}
	return out
}

func TestExampleUnitParses(t *testing.T) {
	unit := parseUnit(t, exampleUnit)
	want := map[string]map[string]string{
		"Unit":    {"Description": ""},
		"Service": {"Restart": "on-failure", "StandardOutput": "journal", "StandardError": "journal"},
		"Install": {"WantedBy": "default.target"},
	}
	for section, keys := range want {
		for key, val := range keys {
			got, ok := unit[section][key]
			if !ok || len(got) != 1 || (val != "" && got[0] != val) {
				t.Errorf("[%s] %s = %q, want %q", section, key, got, val)
			}
		}
	}
	exec := unit["Service"]["ExecStart"]
	if len(exec) != 1 || !strings.Contains(exec[0], "slackwatch") || !strings.HasSuffix(exec[0], " serve") ||
		!strings.Contains(exec[0], "--config ") {
		t.Errorf("ExecStart = %q, want slackwatch --config ... serve", exec)
	}
}

func TestExampleUnitHasNoToken(t *testing.T) {
	for _, line := range strings.Split(exampleUnit, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "#") {
			continue
		}
		lower := strings.ToLower(line)
		for _, bad := range []string{"xox", "xapp", "token"} {
			if strings.Contains(lower, bad) {
				t.Errorf("unit line %q mentions %q; tokens belong in tokenFile", line, bad)
			}
		}
	}
}
