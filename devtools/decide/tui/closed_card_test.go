package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func closedCardTestItem(state, outcome, reason string, opts []decide.Option, closedAt time.Time) *decide.Item {
	it := &decide.Item{
		ID:       "D-101",
		Question: "ship release?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    state,
		Options:  opts,
	}
	if state == decide.StateWithdrawn {
		it.WithdrawnAt = closedAt
		it.Reason = reason
	} else {
		it.DoneAt = closedAt
		it.Outcome = outcome
	}
	return it
}

func TestClosedCardIconsAndHeaders(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "")

	now := time.Date(2026, 11, 20, 15, 40, 0, 0, time.UTC)
	closedAt := time.Date(2026, 11, 20, 15, 37, 0, 0, time.UTC)
	opts := []decide.Option{{Key: "yes", Label: "Yes"}, {Key: "no", Label: "No"}}

	cases := []struct {
		name      string
		state     string
		outcome   string
		reason    string
		wantIcon  string
		wantHead  string
		wantBody  string
		asciiTag  string
		cardClass string
	}{
		{
			name:     "done with outcome",
			state:    decide.StateDone,
			outcome:  "Not posted: cancelled.",
			wantIcon: "✅",
			wantHead: "✅ Closed by ada · 15:37 · 3m ago",
			wantBody: "Not posted: cancelled.",
			asciiTag: "[closed]",
		},
		{
			name:     "done-unread with outcome",
			state:    decide.StateDoneUnread,
			outcome:  "Handled out of band.",
			wantIcon: "✅",
			wantHead: "✅ Closed by ada · 15:37 · 3m ago",
			wantBody: "Handled out of band.",
			asciiTag: "[closed]",
		},
		{
			name:     "archived",
			state:    decide.StateDone,
			outcome:  "archived",
			wantIcon: "📦",
			wantHead: "📦 Closed by ada · 15:37 · 3m ago",
			wantBody: "archived",
			asciiTag: "[archived]",
		},
		{
			name:     "withdrawn",
			state:    decide.StateWithdrawn,
			reason:   "superseded by PR #42",
			wantIcon: "↩️",
			wantHead: "↩️ Withdrawn by ada · 15:37 · 3m ago",
			wantBody: "superseded by PR #42",
			asciiTag: "[withdrawn]",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("LC_ALL", "en_US.UTF-8")
			m := New(t.TempDir())
			m.Now = func() time.Time { return now }
			m.UTC = true

			it := closedCardTestItem(tc.state, tc.outcome, tc.reason, opts, closedAt)
			lines, ok := m.closedCardLines(it, 70)
			if !ok {
				t.Fatal("closedCardLines returned false, want true")
			}
			rendered := joinScreenLines(lines)
			plain := visibleLine(rendered)

			for _, want := range []string{"╭─", "╮", "│", "╰", "╯", tc.wantIcon, tc.wantHead, tc.wantBody} {
				if !strings.Contains(plain, want) {
					t.Fatalf("card missing %q:\n%s", want, plain)
				}
			}
			// Border uses muted color (ANSI 7 -> \x1b[37m), never error color (9 / 31 / 91).
			if got := sgrBefore(t, rendered, "╭─"); got != "\x1b[37m" {
				t.Fatalf("border SGR = %q, want muted \\x1b[37m", got)
			}
			if strings.Contains(rendered, "\x1b[91m") || strings.Contains(rendered, "\x1b[31m") || strings.Contains(rendered, "\x1b[38;5;9m") {
				t.Fatalf("card used error color:\n%q", rendered)
			}

			// Every line must have exact width 70.
			for i, l := range lines {
				if w := lipgloss.Width(l.text); w != 70 {
					t.Fatalf("line %d width = %d, want 70: %q", i, w, visibleLine(l.text))
				}
			}
		})
	}
}

func TestClosedCardMultilineWrappedAndLiteralEscapes(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "1")
	t.Setenv("LC_ALL", "en_US.UTF-8")

	now := time.Date(2026, 11, 20, 15, 40, 0, 0, time.UTC)
	closedAt := time.Date(2026, 11, 20, 15, 37, 0, 0, time.UTC)
	opts := []decide.Option{{Key: "yes", Label: "Yes"}}

	outcome := strings.Join([]string{
		`First line with "double" and 'single' quotes and > quote prefix`,
		"",
		`Second line has \x1b? No, raw escape ` + "\x1b[31mRED\x1b[0m and \x1b]8;;https://evil.example\x07click\x1b]8;;\x07 and \rCR and https://example.com/a/b",
		"Third line with\ttab and enough words to wrap across multiple card lines cleanly.",
	}, "\n")

	m := New(t.TempDir())
	m.Now = func() time.Time { return now }
	m.UTC = true

	it := closedCardTestItem(decide.StateDone, outcome, "", opts, closedAt)
	lines, ok := m.closedCardLines(it, 48)
	if !ok {
		t.Fatal("closedCardLines returned false")
	}
	rendered := joinScreenLines(lines)
	if strings.Contains(rendered, "\x1b") {
		t.Fatalf("NO_COLOR card with escaped input leaked raw ESC:\n%q", rendered)
	}
	if strings.Contains(rendered, "\r") || strings.Contains(rendered, "\x07") {
		t.Fatalf("card leaked raw control character:\n%q", rendered)
	}
	for _, want := range []string{
		`"double"`,
		`'single'`,
		`> quote`,
		`\x1b[31mRED\x1b[0m`,
		`\x1b]8;;https://evil.example\x07click`,
		`\x0dCR`,
		`https://example.com/a/b`,
		`multiple card lines`,
		`cleanly.`,
	} {
		if !strings.Contains(rendered, want) {
			t.Fatalf("card missing %q:\n%s", want, rendered)
		}
	}
	if len(lines) < 6 {
		t.Fatalf("expected multi-line wrapped card (>=6 lines), got %d:\n%s", len(lines), rendered)
	}
	for i, l := range lines {
		if w := lipgloss.Width(l.text); w != 48 {
			t.Fatalf("line %d width = %d, want 48: %q", i, w, l.text)
		}
	}
}

func TestClosedCardASCIIFallback(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "1")

	now := time.Date(2026, 11, 20, 15, 40, 0, 0, time.UTC)
	closedAt := time.Date(2026, 11, 20, 15, 37, 0, 0, time.UTC)
	opts := []decide.Option{{Key: "yes", Label: "Yes"}}

	cases := []struct {
		name    string
		state   string
		outcome string
		reason  string
		wantTag string
		wantBy  string
	}{
		{"closed", decide.StateDone, "cancelled", "", "[closed]", "Closed by ada"},
		{"archived", decide.StateDone, "archived", "", "[archived]", "Closed by ada"},
		{"withdrawn", decide.StateWithdrawn, "", "no longer needed", "[withdrawn]", "Withdrawn by ada"},
	}

	for _, mode := range []string{"asciiKeys", "localeC"} {
		t.Run(mode, func(t *testing.T) {
			if mode == "localeC" {
				t.Setenv("LC_ALL", "C")
			} else {
				t.Setenv("LC_ALL", "en_US.UTF-8")
			}
			for _, tc := range cases {
				t.Run(tc.name, func(t *testing.T) {
					m := New(t.TempDir())
					m.Now = func() time.Time { return now }
					m.UTC = true
					m.ASCIIKeys = mode == "asciiKeys"

					it := closedCardTestItem(tc.state, tc.outcome, tc.reason, opts, closedAt)
					lines, ok := m.closedCardLines(it, 60)
					if !ok {
						t.Fatal("closedCardLines returned false")
					}
					rendered := joinScreenLines(lines)
					for _, bad := range []string{"╭", "╮", "╰", "╯", "─", "│", "✅", "📦", "↩️"} {
						if strings.Contains(rendered, bad) {
							t.Fatalf("ASCII card contains non-ASCII %q:\n%s", bad, rendered)
						}
					}
					for _, want := range []string{"+-", "-+", "| ", " |", tc.wantTag, tc.wantBy} {
						if !strings.Contains(rendered, want) {
							t.Fatalf("ASCII card missing %q:\n%s", want, rendered)
						}
					}
					for i, l := range lines {
						if w := lipgloss.Width(l.text); w != 60 {
							t.Fatalf("line %d width = %d, want 60: %q", i, w, l.text)
						}
					}
				})
			}
		})
	}
}

func TestClosedCardNarrowWidthBorderAlignmentWithEmoji(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "1")
	t.Setenv("LC_ALL", "en_US.UTF-8")

	now := time.Date(2026, 11, 20, 15, 40, 0, 0, time.UTC)
	closedAt := time.Date(2026, 11, 20, 15, 37, 0, 0, time.UTC)
	opts := []decide.Option{{Key: "yes", Label: "Yes"}}

	items := []*decide.Item{
		closedCardTestItem(decide.StateDone, "shipped ✅ with details", "", opts, closedAt),
		closedCardTestItem(decide.StateDone, "archived", "", opts, closedAt),
		closedCardTestItem(decide.StateWithdrawn, "", "retracted ↩️ early", opts, closedAt),
	}

	for _, it := range items {
		for _, w := range []int{20, 26, 32, 38, 44} {
			m := New(t.TempDir())
			m.Now = func() time.Time { return now }
			m.UTC = true

			lines, ok := m.closedCardLines(it, w)
			if !ok {
				t.Fatalf("width %d: closedCardLines returned false", w)
			}
			for i, l := range lines {
				if got := lipgloss.Width(l.text); got != w {
					t.Fatalf("state=%s width=%d line=%d: got lipgloss width %d, want %d\n%s",
						it.State, w, i, got, w, joinScreenLines(lines))
				}
			}
			if !strings.HasSuffix(lines[0].text, "╮") {
				t.Fatalf("width=%d top border missing right corner:\n%s", w, lines[0].text)
			}
			if !strings.HasSuffix(lines[len(lines)-1].text, "╯") {
				t.Fatalf("width=%d bottom border missing right corner:\n%s", w, lines[len(lines)-1].text)
			}
		}
	}
}

func TestClosedCardRenderedInThreadView(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "1")
	t.Setenv("LC_ALL", "en_US.UTF-8")

	now := time.Date(2026, 11, 20, 15, 40, 0, 0, time.UTC)
	dir := t.TempDir()
	led := openLedger(t, dir)
	led.SetClock(func() time.Time { return now.Add(-3 * time.Minute) })

	opts := []decide.Option{{Key: "yes", Label: "Yes"}, {Key: "no", Label: "No"}}
	item := mustCreate(t, led, &decide.Item{
		Question: "withdraw me?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		Options:  opts,
	})
	if _, err := led.Withdraw(item.ID, "changed plan\nwill retry later"); err != nil {
		t.Fatal(err)
	}

	// Non-question note should NOT get a closed card.
	note := mustCreate(t, led, &decide.Item{
		Question: "fyi note",
		Owner:    "ada",
		Kind:     decide.KindRelay,
	})
	if _, err := led.Done(note.ID, "archived"); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.UTC = true
	m.width, m.height = 80, 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	for _, want := range []string{"↩️ Withdrawn by ada · 15:37 · 3m ago", "changed plan", "will retry later"} {
		if !strings.Contains(view, want) {
			t.Fatalf("withdrawn thread missing %q:\n%s", want, view)
		}
	}
	if strings.Contains(view, "1  Yes") {
		t.Fatalf("withdrawn thread still shows options:\n%s", view)
	}

	if err := m.openItemByID(note.ID); err != nil {
		t.Fatal(err)
	}
	noteView := m.View()
	if strings.Contains(noteView, "Closed by ada") || strings.Contains(noteView, "╭─") {
		t.Fatalf("note without options should not show closed card:\n%s", noteView)
	}

	// Narrow m.View() keeps card right borders aligned inside the thread pane.
	m.width, m.height = 42, 24
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	narrowView := m.View()
	var cardRows []string
	for _, row := range strings.Split(narrowView, "\n") {
		if strings.Contains(row, "╭─") || strings.Contains(row, "╰─") || strings.Contains(row, "changed plan") || strings.Contains(row, "will retry") {
			cardRows = append(cardRows, row)
		}
	}
	if len(cardRows) < 4 {
		t.Fatalf("expected >= 4 card rows in narrow view, got %d:\n%s", len(cardRows), narrowView)
	}
	for i, row := range cardRows {
		if w := lipgloss.Width(row); w != 42 {
			t.Fatalf("narrow view row %d width = %d, want 42: %q", i, w, row)
		}
	}
}

func joinScreenLines(lines []screenLine) string {
	out := make([]string, len(lines))
	for i, l := range lines {
		out[i] = l.text
	}
	return strings.Join(out, "\n")
}
