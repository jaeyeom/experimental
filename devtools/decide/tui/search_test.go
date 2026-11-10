package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func searchFixture(t *testing.T) (string, []decide.Item) {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	day := func(d int) time.Time { return time.Date(2026, 11, d, 0, 0, 0, 0, time.UTC) }
	items := []decide.Item{
		mustCreate(t, led, &decide.Item{Question: "rotate keys", Owner: "deploybot", Kind: decide.KindRelay, State: decide.StateOpen, CreatedAt: day(1)}),
		mustCreate(t, led, &decide.Item{Question: "Deploy now?", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, CreatedAt: day(2)}),
		mustCreate(t, led, &decide.Item{
			Question: "thread", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, CreatedAt: day(3),
			Messages: []decide.Message{{Author: "ada", Text: "hello"}, {Author: "bea", Text: "please DEPLOY it"}},
		}),
		mustCreate(t, led, &decide.Item{Question: "lunch", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, CreatedAt: day(4)}),
		mustCreate(t, led, &decide.Item{Question: "other", Owner: "bea", Kind: decide.KindRelay, State: decide.StateOpen, CreatedAt: day(5)}),
		mustCreate(t, led, &decide.Item{Question: "deploy done", Owner: "bea", Kind: decide.KindRelay, State: decide.StateAnswered, CreatedAt: day(6)}),
		mustCreate(t, led, &decide.Item{Question: "skip", Owner: "bea", Kind: decide.KindRelay, State: decide.StateAnswered, CreatedAt: day(7)}),
	}
	return dir, items
}

func itemIDs(items []decide.Item) []string {
	ids := make([]string, len(items))
	for i, it := range items {
		ids[i] = it.ID
	}
	return ids
}

func TestListSlashFiltersRows(t *testing.T) {
	dir, items := searchFixture(t)
	m := New(dir)
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if len(m.items) != 5 {
		t.Fatalf("inbox has %d items", len(m.items))
	}

	press(t, m, "/xyz\x1b")
	if len(m.items) != 5 || strings.Contains(m.View(), "/xyz") {
		t.Fatalf("esc without a filter changed the list: %v\n%s", itemIDs(m.items), m.View())
	}

	press(t, m, "/dep")
	if view := m.View(); !strings.Contains(view, "/dep") || len(m.items) != 5 {
		t.Fatalf("prompt did not show the draft\n%s", view)
	}
	press(t, m, "lo\r")
	got := strings.Join(itemIDs(m.items), " ")
	for _, want := range []string{items[0].ID, items[1].ID, items[2].ID} {
		if !strings.Contains(got, want) {
			t.Fatalf("filter dropped %s: %s", want, got)
		}
	}
	if len(m.items) != 3 {
		t.Fatalf("filter kept %s", got)
	}
	view := m.View()
	if !strings.Contains(view, "inbox  /deplo  (3 of 5)") {
		t.Fatalf("header missing the filter\n%s", view)
	}
	if strings.Contains(view, items[3].ID) || strings.Contains(view, items[4].ID) {
		t.Fatalf("hidden rows are drawn\n%s", view)
	}

	press(t, m, "/lunch\x1b")
	if len(m.items) != 3 || !strings.Contains(m.View(), "/deplo  (3 of 5)") {
		t.Fatalf("esc changed the filter: %v", itemIDs(m.items))
	}

	press(t, m, "ga")
	if len(m.items) != 1 || m.items[0].ID != items[5].ID || !strings.Contains(m.View(), "answered  /deplo  (1 of 2)") {
		t.Fatalf("answered lost the filter: %v\n%s", itemIDs(m.items), m.View())
	}
	press(t, m, "gi")
	if len(m.items) != 3 {
		t.Fatalf("inbox lost the filter: %v", itemIDs(m.items))
	}

	press(t, m, "G")
	want := m.items[m.cursor].ID
	press(t, m, "o")
	if m.detail == nil || m.detail.ID != want {
		t.Fatalf("o opened %v want %s", m.detail, want)
	}
	press(t, m, "u")

	press(t, m, "/zzz\r")
	if len(m.items) != 0 || m.cursor != 0 || !strings.Contains(m.View(), "no matches") {
		t.Fatalf("no-match view\n%s", m.View())
	}

	press(t, m, "/\r")
	if len(m.items) != 5 || strings.Contains(m.View(), "no matches") || strings.Contains(m.View(), "/zzz") {
		t.Fatalf("empty query did not clear: %v\n%s", itemIDs(m.items), m.View())
	}
}

func TestListFilterKeepsVisibleCursor(t *testing.T) {
	dir, items := searchFixture(t)
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "G")
	if m.items[m.cursor].ID != items[0].ID {
		t.Fatalf("cursor on %s", m.items[m.cursor].ID)
	}
	press(t, m, "/deploy\r")
	if m.cursor < 0 || m.cursor >= len(m.items) || m.items[m.cursor].ID != items[0].ID {
		t.Fatalf("cursor %d left the kept row: %v", m.cursor, itemIDs(m.items))
	}
	press(t, m, "/lunch\r")
	if m.cursor != 0 || len(m.items) != 1 || m.items[0].ID != items[3].ID {
		t.Fatalf("cursor %d items %v", m.cursor, itemIDs(m.items))
	}
}

func TestItemSlashHighlightsMatches(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	thread := mustCreate(t, led, &decide.Item{
		Question: "thread",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{
			{Author: "one", Text: "deploy first"},
			{Author: "two", Text: "nothing here"},
			{Author: "three", Text: "then Deploy again, deploy"},
		},
	})
	m := New(dir)
	m.width = 120
	m.height = 40
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	if m.detail == nil || m.detail.ID != thread.ID {
		t.Fatalf("open failed: %v", m.detail)
	}
	useStyles()
	lower := searchMatch.Render("deploy")
	upper := searchMatch.Render("Deploy")
	if strings.Contains(m.View(), lower) {
		t.Fatal("highlight before a search")
	}

	press(t, m, "/deploy\r")
	view := m.View()
	if strings.Count(view, lower) != 2 || strings.Count(view, upper) != 1 {
		t.Fatalf("highlight count lower %d upper %d\n%q", strings.Count(view, lower), strings.Count(view, upper), view)
	}
	if !strings.Contains(view, "/deploy (3)") {
		t.Fatalf("header missing the match count\n%s", view)
	}

	press(t, m, "n")
	if m.message != 1 || !strings.Contains(m.View(), upper) {
		t.Fatalf("n message %d lost the highlight", m.message)
	}
	press(t, m, "p")
	if m.message != 0 || !strings.Contains(m.View(), upper) {
		t.Fatalf("p message %d lost the highlight", m.message)
	}
	scroll := m.scroll
	press(t, m, "N")
	if m.message != 0 || m.scroll != scroll || m.detail == nil || !strings.Contains(m.View(), upper) {
		t.Fatalf("N moved message %d scroll %d", m.message, m.scroll)
	}

	press(t, m, "/nothing\x1b")
	if view := m.View(); !strings.Contains(view, upper) || strings.Contains(view, searchMatch.Render("nothing")) {
		t.Fatalf("esc changed the highlight\n%q", view)
	}

	press(t, m, "/\r")
	if view := m.View(); strings.Contains(view, upper) || strings.Contains(view, lower) || strings.Contains(view, "/deploy") {
		t.Fatalf("empty query kept the highlight\n%q", view)
	}
}

func TestSlashInFooterAndHelp(t *testing.T) {
	dir, _ := searchFixture(t)
	m := New(dir)
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	check := func(name string) {
		t.Helper()
		footer := m.View()
		if !strings.Contains(footer, "/find") || !strings.Contains(footer, "? help") {
			t.Fatalf("%s footer missing /find\n%s", name, footer)
		}
		press(t, m, "?")
		help := m.View()
		if !strings.Contains(help, "/ ") || !strings.Contains(help, "empty enter clears") {
			t.Fatalf("%s help missing /\n%s", name, help)
		}
		press(t, m, "?")
	}
	check("list")
	press(t, m, "/deplo\r")
	if !strings.Contains(m.View(), "? help") {
		t.Fatalf("filtered footer\n%s", m.View())
	}
	press(t, m, "o")
	check("item")
	press(t, m, "u")
	press(t, m, "gs")
	check("status")
}

func TestStatusSlashFiltersAgents(t *testing.T) {
	dir, _ := searchFixture(t)
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	for _, row := range [][3]string{{"alpha", "working", "deploy the inbox"}, {"beta", "working", "reading"}} {
		if _, err := board.SetAgent(row[0], row[1], row[2]); err != nil {
			t.Fatal(err)
		}
	}
	m := New(dir)
	m.width = 120
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "/deploy\r")
	press(t, m, "gs")
	view := m.View()
	if !strings.Contains(view, "alpha") || strings.Contains(view, "beta") || !strings.Contains(view, "status  /deploy  (1 of 2)") {
		t.Fatalf("status filter\n%s", view)
	}
	press(t, m, "/\r")
	if view := m.View(); !strings.Contains(view, "beta") || strings.Contains(view, "/deploy") {
		t.Fatalf("status clear\n%s", view)
	}
}
