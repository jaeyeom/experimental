package cli

import (
	"context"
	"encoding/json"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func statsFixture(t *testing.T) (string, string) {
	t.Helper()
	cfgPath, _ := writeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	day := time.Now().UTC().Add(-24 * time.Hour)
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{Source: "jira:acme", ID: "PRJ-1", Title: "Ship it", Type: "Task", State: "To Do", Parent: "PRJ-9", Created: day, Updated: day}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day, day); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.SetLastSync(ctx, "jira:acme", day); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

// useTerminal makes stats see a terminal of width columns (0: not a terminal).
func useTerminal(t *testing.T, width int) {
	t.Helper()
	prev := statsTerminal
	statsTerminal = func(*cobra.Command) (int, bool) { return width, width > 0 }
	t.Cleanup(func() { statsTerminal = prev })
}

func TestStatsColor(t *testing.T) {
	cfgPath, dbPath := statsFixture(t)
	tests := []struct {
		name    string
		width   int
		noColor string
		args    []string
		escapes bool
	}{
		{name: "auto on a terminal", width: 100, escapes: true},
		{name: "auto with NO_COLOR", width: 100, noColor: "1"},
		{name: "auto when piped", width: 0},
		{name: "always when piped", width: 0, args: []string{"--color", "always"}, escapes: true},
		{name: "never on a terminal", width: 100, args: []string{"--color", "never"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			useTerminal(t, tc.width)
			if tc.noColor != "" {
				t.Setenv("NO_COLOR", tc.noColor)
			}
			args := append([]string{"stats", "--config", cfgPath, "--db", dbPath}, tc.args...)
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitOK || stderr != "" {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			if got := strings.Contains(stdout, "\x1b["); got != tc.escapes {
				t.Fatalf("escapes = %v, want %v:\n%s", got, tc.escapes, stdout)
			}
			if !strings.Contains(stdout, "Burndown") {
				t.Fatalf("no burndown chart:\n%s", stdout)
			}
			// Color is only in charts and status cells, never in headings.
			for _, line := range strings.Split(stdout, "\n") {
				if (strings.HasPrefix(line, "stats ") || strings.HasPrefix(line, "Throughput") || strings.HasPrefix(line, "Backlog")) && strings.Contains(line, "\x1b[") {
					t.Fatalf("colored heading %q", line)
				}
			}
		})
	}
}

func TestStatsNarrowTerminalPrintsTablesOnly(t *testing.T) {
	cfgPath, dbPath := statsFixture(t)
	useTerminal(t, 30)
	stdout, _, code := runCmd(t, "stats", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || strings.Contains(stdout, "Burndown") || !strings.Contains(stdout, "Throughput") {
		t.Fatalf("exit = %d:\n%s", code, stdout)
	}
}

func TestStatsUTCAndJSONTimeZone(t *testing.T) {
	cfgPath, dbPath := statsFixture(t)
	useTerminal(t, 0)
	stdout, _, code := runCmd(t, "stats", "--utc", "--config", cfgPath, "--db", dbPath)
	first, _, _ := strings.Cut(stdout, "\n")
	if code != ExitOK || !strings.HasPrefix(first, "stats ") || !strings.Contains(first, " UTC to ") {
		t.Fatalf("exit = %d, header = %q", code, first)
	}
	stdout, _, code = runCmd(t, "stats", "--utc", "--json", "--config", cfgPath, "--db", dbPath)
	var rep struct {
		Window struct {
			TimeZone string `json:"time_zone"` //nolint:tagliatelle // documented factory JSON output field
			Start    string `json:"start"`
		} `json:"window"`
		Burndown json.RawMessage `json:"burndown"`
	}
	if code != ExitOK || json.Unmarshal([]byte(stdout), &rep) != nil {
		t.Fatalf("exit = %d:\n%s", code, stdout)
	}
	if rep.Window.TimeZone != "UTC" || !strings.HasSuffix(rep.Window.Start, "Z") || rep.Burndown != nil {
		t.Fatalf("window = %+v, burndown = %s", rep.Window, rep.Burndown)
	}
}

func TestStatsRejectsBadChartFlags(t *testing.T) {
	cfgPath, dbPath := statsFixture(t)
	for _, args := range [][]string{{"--color", "sometimes"}, {"--burndown", "pie"}} {
		_, stderr, code := runCmd(t, append([]string{"stats", "--config", cfgPath, "--db", dbPath}, args...)...)
		if code != ExitUsage {
			t.Fatalf("%v: exit = %d, stderr = %s", args, code, stderr)
		}
	}
}

// TestStatsColorKeepsTableAlignment strips the escapes from a colored
// report: every table, bar columns included, must line up exactly as it
// does uncolored. Bar cells are blocks with color and shades without, so
// both are folded to one rune before comparing.
func TestStatsColorKeepsTableAlignment(t *testing.T) {
	cfgPath, dbPath := statsFixture(t)
	useTerminal(t, 100)
	fold := strings.NewReplacer("▏", "#", "▎", "#", "▍", "#", "▌", "#", "▋", "#", "▊", "#", "▉", "#", "█", "#", "▓", "#", "▒", "#", "░", "#")
	norm := func(out string) string { return fold.Replace(ansi.ReplaceAllString(out, "")) }
	colored, _, _ := runCmd(t, "stats", "--color", "always", "--config", cfgPath, "--db", dbPath)
	plain, _, _ := runCmd(t, "stats", "--color", "never", "--config", cfgPath, "--db", dbPath)
	if got, want := norm(colored), norm(plain); got != want || !strings.Contains(want, "PROGRESS") {
		t.Fatalf("colored report:\n%s\nwant:\n%s", got, want)
	}
}

var ansi = regexp.MustCompile("\x1b\\[[0-9;]*m")
