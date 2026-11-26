package stats

import (
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
	"unicode/utf8"
)

var update = flag.Bool("update", false, "rewrite the golden files in testdata") //nolint:pkgvar // go test golden update flag

// goldenReport has a throughput day with more merged PRs than a 12-cell bar
// holds, a tiny ready count beside a large blocked one, and two epics, one
// with a long title.
func goldenReport() Report {
	day := func(d int) time.Time { return time.Date(2026, 11, d, 0, 0, 0, 0, time.UTC) }
	return Report{
		Window:    Window{Start: day(9), End: day(12), By: ByDay, TimeZone: "UTC"},
		Freshness: Freshness{LastSync: []SourceSync{{Source: "jira:acme"}}},
		Throughput: Throughput{
			By: ByDay,
			Buckets: []Bucket{
				{Start: day(9), Counts: Counts{Finished: 4, Started: 2, PRsOpened: 3, PRsMerged: 2}},
				{Start: day(10), Counts: Counts{Finished: 1, Started: 0, PRsOpened: 1, PRsMerged: 15}},
				{Start: day(11), Counts: Counts{}},
			},
			Total: Counts{Finished: 5, Started: 2, PRsOpened: 4, PRsMerged: 17},
		},
		Backlog: BacklogPart{
			Now:    BacklogCounts{Ready: 1, Blocked: 40, InFlight: 3, NeedsHuman: 0, ByStatus: map[string]int{"To Do": 44}},
			Change: &BacklogCounts{Ready: 1, Blocked: -2, ByStatus: map[string]int{"To Do": -1}},
		},
		Epics: Epics{
			Note: "items in the source queries",
			Parents: []EpicRow{
				{ID: "PRJ-1", Title: "A very long epic title that would push a trailing bar out of line", Priority: true, Done: 6, InProgress: 2, Remaining: 4, Percent: 50, Added: 1},
				{ID: "PRJ-20", Title: "Small", Done: 0, InProgress: 0, Remaining: 1, Percent: 0},
			},
			TopLevel: []EpicRow{
				{ID: "PRJ-1", Title: "A very long epic title that would push a trailing bar out of line", Done: 6, InProgress: 2, Remaining: 4, Percent: 50, Added: 1},
			},
		},
		loc: time.UTC,
	}
}

func TestTextGolden(t *testing.T) {
	tests := []struct {
		name string
		o    TextOptions
	}{
		{name: "default", o: TextOptions{Width: 100, BarWidth: DefaultBarWidth}},
		{name: "ascii", o: TextOptions{Width: 100, BarWidth: DefaultBarWidth, ASCII: true}},
		{name: "bar_width_0", o: TextOptions{Width: 100, BarWidth: 0}},
		{name: "narrow", o: TextOptions{Width: MinChartWidth - 1, BarWidth: DefaultBarWidth}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var out strings.Builder
			if err := goldenReport().WriteTextWith(&out, tc.o); err != nil {
				t.Fatal(err)
			}
			path := filepath.Join("testdata", "text_"+tc.name+".golden")
			if *update {
				if err := os.WriteFile(path, []byte(out.String()), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			want, err := os.ReadFile(path) //nolint:gosec // A fixed path under testdata.
			if err != nil {
				t.Fatal(err)
			}
			if out.String() != string(want) {
				t.Fatalf("text differs from %s (rerun with -update):\n%s", path, out.String())
			}
		})
	}
}

// TestTextBarsFitAndShow checks every bar column cell: at most BarWidth
// cells, and a row with a non-zero part never has an empty bar.
func TestTextBarsFitAndShow(t *testing.T) {
	var out strings.Builder
	if err := goldenReport().WriteTextWith(&out, TextOptions{Width: 100, BarWidth: DefaultBarWidth}); err != nil {
		t.Fatal(err)
	}
	text := out.String()
	if strings.Count(text, "Small") != 1 || strings.Count(text, "PRJ-20") != 1 {
		t.Fatalf("an epic is listed twice:\n%s", text)
	}
	for _, line := range strings.Split(text, "\n") {
		var run, longest int
		for _, r := range line {
			if strings.ContainsRune("▏▎▍▌▋▊▉█▓▒░", r) {
				run++
				longest = max(longest, run)
				continue
			}
			run = 0
		}
		if longest > DefaultBarWidth {
			t.Fatalf("bar wider than %d cells (%d): %q", DefaultBarWidth, longest, line)
		}
		for _, row := range []string{"2026-11-10 ", "now ", "PRJ-20 "} {
			if strings.HasPrefix(line, row) && longest == 0 {
				t.Fatalf("non-zero row has an empty bar: %q", line)
			}
		}
		if utf8.RuneCountInString(line) > 0 && strings.Contains(line, "◆◆◆◆◆◆◆◆◆◆◆◆◆") {
			t.Fatalf("merged marks overflow the bar width: %q", line)
		}
	}
	if !strings.Contains(text, "◆×15") {
		t.Fatalf("expected ◆×15 for 15 merged PRs:\n%s", text)
	}
}
