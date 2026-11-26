package stats

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/stats/chart"
)

// goldenBurndown is a growing backlog over eight days with a finish rate.
func goldenBurndown(unknown int, rate bool) *Burndown {
	day := func(d int) time.Time { return time.Date(2026, 11, d, 0, 0, 0, 0, time.UTC) }
	b := &Burndown{Start: day(1), UnknownCreated: unknown, RemainingNow: 465}
	for i, n := range []int{430, 438, 441, 450, 447, 455, 462, 465} {
		b.Points = append(b.Points, BurnPoint{At: day(2 + i), Remaining: n})
	}
	if rate {
		b.Rate = 74.0 / 7
		at := time.Date(2026, 12, 23, 9, 0, 0, 0, time.UTC)
		b.Projected = &at
	}
	return b
}

// burndownText renders only the burndown section.
func burndownText(t *testing.T, b *Burndown, o TextOptions) string {
	t.Helper()
	var out strings.Builder
	p := &printer{w: &out, o: o, style: chart.Style{Color: o.Color, ASCII: o.ASCII}, loc: time.UTC}
	Report{Window: Window{By: ByDay}, Burndown: b, loc: time.UTC}.writeBurndown(p)
	if p.err != nil {
		t.Fatal(p.err)
	}
	return out.String()
}

func TestBurndownGolden(t *testing.T) {
	tests := []struct {
		name    string
		unknown int
		rate    bool
		o       TextOptions
	}{
		{name: "braille", rate: true, o: TextOptions{Width: 80}},
		{name: "pace", rate: true, o: TextOptions{Width: 80, BurndownPace: true}},
		{name: "bars", rate: true, o: TextOptions{Width: 80, Burndown: BurndownBars, BurndownPace: true}},
		{name: "ascii", rate: true, o: TextOptions{Width: 80, ASCII: true, BurndownPace: true}},
		{name: "ascii_bars", rate: true, o: TextOptions{Width: 80, ASCII: true, Burndown: BurndownBars}},
		{name: "no_rate", o: TextOptions{Width: 80}},
		{name: "unknown_created", unknown: 14, rate: true, o: TextOptions{Width: 80}},
		{name: "unknown_created_one", unknown: 1, o: TextOptions{Width: 80, ASCII: true}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := burndownText(t, goldenBurndown(tc.unknown, tc.rate), tc.o)
			path := filepath.Join("testdata", "burndown_"+tc.name+".golden")
			if *update {
				if err := os.WriteFile(path, []byte(got), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			want, err := os.ReadFile(path) //nolint:gosec // A fixed path under testdata.
			if err != nil {
				t.Fatal(err)
			}
			if got != string(want) {
				t.Fatalf("text differs from %s (rerun with -update):\n%s", path, got)
			}
		})
	}
}

// TestBurndownPaceIsOptIn checks the default braille chart draws one series
// and the pace flag adds a second, with its legend line.
func TestBurndownPaceIsOptIn(t *testing.T) {
	b := goldenBurndown(0, true)
	plain := burndownText(t, b, TextOptions{Width: 80})
	paced := burndownText(t, b, TextOptions{Width: 80, BurndownPace: true})
	if strings.Contains(plain, "pace") || !strings.Contains(paced, "┄ pace to reach 0 by 2026-11-09 from 430 (reference only)\n") {
		t.Fatalf("plain:\n%s\npaced:\n%s", plain, paced)
	}
	if plain == paced {
		t.Fatal("pace drew nothing")
	}
	if !strings.Contains(plain, "─ remaining items (end of each day)\n") {
		t.Fatalf("legend missing:\n%s", plain)
	}
	want := "projection: 465 remaining ÷ 10.6 finished/day (last 7 days) ≈ 44 days → done around 2026-12-23\n" +
		"assumes the same finish rate and no new items; not drawn on the chart\n"
	if !strings.Contains(plain, want) {
		t.Fatalf("projection:\n%s", plain)
	}
	colored := burndownText(t, b, TextOptions{Width: 80, Color: true})
	if !strings.Contains(colored, "\x1b[") || stripANSI(colored) != plain {
		t.Fatalf("colored legend differs once escapes are stripped:\n%s", colored)
	}
}

var ansiCode = regexp.MustCompile("\x1b\\[[0-9;]*m")

func stripANSI(s string) string { return ansiCode.ReplaceAllString(s, "") }
