package tui

import (
	"fmt"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
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
	if view := m.View(); !strings.Contains(view, "/dep") || len(m.items) != 3 {
		t.Fatalf("prompt did not filter as it typed: %v\n%s", itemIDs(m.items), view)
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
		if !strings.Contains(help, "as you type") || !strings.Contains(help, "esc restores") || !strings.Contains(help, "empty enter clears") {
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

func TestListSearchIsIncremental(t *testing.T) {
	dir, _ := searchFixture(t)
	m := New(dir)
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	counts := map[string]int{}
	for _, key := range []string{"/d", "e", "p"} {
		press(t, m, key)
		q := m.search
		counts[q] = len(m.items)
		if !m.searching || m.filter != q {
			t.Fatalf("after %q searching %v filter %q", key, m.searching, m.filter)
		}
		if header := fmt.Sprintf("inbox  /%s  (%d of 5)", q, len(m.items)); !strings.Contains(m.View(), header) {
			t.Fatalf("after %q want header %q\n%s", key, header, m.View())
		}
	}
	if counts["dep"] != 3 || counts["de"] < counts["dep"] || counts["d"] < counts["de"] {
		t.Fatalf("counts %v", counts)
	}
	useStyles()
	if view := m.View(); !strings.Contains(view, searchMatch.Render("dep")) || !strings.Contains(view, searchMatch.Render("Dep")) {
		t.Fatalf("rows do not highlight the match\n%q", view)
	}
	press(t, m, "\x7f")
	if len(m.items) != counts["de"] || m.filter != "de" {
		t.Fatalf("backspace kept %v filter %q", itemIDs(m.items), m.filter)
	}
	press(t, m, "p\r")
	if m.searching || m.filter != "dep" || len(m.items) != 3 {
		t.Fatalf("enter changed the result: searching %v filter %q items %v", m.searching, m.filter, itemIDs(m.items))
	}
}

func TestListSearchCursorFollowsMatches(t *testing.T) {
	dir, items := searchFixture(t)
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "G")
	press(t, m, "/deploy")
	if m.items[m.cursor].ID != items[0].ID {
		t.Fatalf("cursor left a matching row: %s", m.items[m.cursor].ID)
	}
	press(t, m, "\x7f\x7f\x7f\x7f\x7f\x7flunch")
	if m.cursor != 0 || len(m.items) != 1 || m.items[0].ID != items[3].ID {
		t.Fatalf("cursor %d items %v", m.cursor, itemIDs(m.items))
	}
}

func TestListSearchEscRestoresState(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	for i := range 20 {
		owner := "kim"
		if i%2 == 1 {
			owner = "lou"
		}
		mustCreate(t, led, &decide.Item{
			Question: fmt.Sprintf("row %02d", i), Owner: owner, Kind: decide.KindRelay, State: decide.StateOpen,
			CreatedAt: time.Date(2026, 11, 1, i, 0, 0, 0, time.UTC),
		})
	}
	m := New(dir)
	m.width = 120
	m.height = 10
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "/kim\r")
	press(t, m, "G")
	if len(m.items) != 10 || m.scroll == 0 {
		t.Fatalf("setup items %d scroll %d", len(m.items), m.scroll)
	}
	id, cursor, scroll := m.items[m.cursor].ID, m.cursor, m.scroll

	press(t, m, "/lou")
	if m.filter != "lou" || m.scroll == scroll && m.items[m.cursor].ID == id {
		t.Fatalf("typing did not change the list: filter %q", m.filter)
	}
	press(t, m, "\x1b")
	if m.searching || m.filter != "kim" || len(m.items) != 10 || m.cursor != cursor || m.items[m.cursor].ID != id || m.scroll != scroll {
		t.Fatalf("esc filter %q items %d cursor %d/%d scroll %d/%d", m.filter, len(m.items), m.cursor, cursor, m.scroll, scroll)
	}
	if !strings.Contains(m.View(), "inbox  /kim  (10 of 20)") {
		t.Fatalf("header after esc\n%s", m.View())
	}

	press(t, m, "/\x1b")
	if m.filter != "kim" || len(m.items) != 10 {
		t.Fatalf("esc on an empty prompt changed the filter %q", m.filter)
	}
}

func searchThread(t *testing.T) *Model {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	var msgs []decide.Message
	for i := range 10 {
		text := fmt.Sprintf("body %d\nfiller\nfiller", i)
		switch i {
		case 1:
			text += "\nearly bird"
		case 8:
			text += "\nneedle here"
		}
		msgs = append(msgs, decide.Message{Author: "ada", Time: time.Date(2026, 11, 1, i, 0, 0, 0, time.UTC), Text: text})
	}
	mustCreate(t, led, &decide.Item{Question: "thread", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, Messages: msgs})
	m := New(dir)
	m.width = 80
	m.height = 14
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	return m
}

// matchLine is the first detail line at or after from that paints a match.
func matchLine(m *Model, from int) int {
	for i, line := range m.detailLines(m.detailInnerWidth(m.contentWidth())) {
		if i >= from && line.match {
			return i
		}
	}
	return -1
}

func TestItemSearchScrollsFromStart(t *testing.T) {
	m := searchThread(t)
	press(t, m, "/body\r")
	press(t, m, "nnnnn")
	start, message := m.scroll, m.message
	if start == 0 || message != 5 {
		t.Fatalf("setup scroll %d message %d", start, message)
	}
	useStyles()

	press(t, m, "/need")
	if m.highlight != "need" || m.scroll <= start || m.scroll != matchLine(m, start) {
		t.Fatalf("highlight %q scroll %d start %d", m.highlight, m.scroll, start)
	}
	if !strings.Contains(m.View(), searchMatch.Render("need")) {
		t.Fatalf("match not in view\n%q", m.View())
	}
	press(t, m, "\x1b")
	if m.highlight != "body" || m.scroll != start || m.message != message {
		t.Fatalf("esc highlight %q scroll %d message %d", m.highlight, m.scroll, m.message)
	}

	press(t, m, "/early")
	if m.scroll >= start || m.scroll != matchLine(m, 0) || !strings.Contains(m.View(), searchMatch.Render("early")) {
		t.Fatalf("no wrap: scroll %d start %d", m.scroll, start)
	}
	press(t, m, "\x7f\x7f\x7f\x7f\x7f")
	if m.highlight != "" || m.scroll != start {
		t.Fatalf("empty query highlight %q scroll %d", m.highlight, m.scroll)
	}
	press(t, m, "early\r")
	if m.searching || m.highlight != "early" || m.scroll >= start {
		t.Fatalf("enter searching %v highlight %q scroll %d", m.searching, m.highlight, m.scroll)
	}
	press(t, m, "n")
	if m.message != message+1 || m.highlight != "early" {
		t.Fatalf("n after enter: message %d highlight %q", m.message, m.highlight)
	}
}

func TestSearchPromptAndPickerFilterStaySeparate(t *testing.T) {
	dir, _ := searchFixture(t)
	m := New(dir)
	m.width = 120
	m.height = 30
	m.Targets = configuredTargets()
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "/c")
	if m.composing || m.search != "c" || m.pickFilter != "" {
		t.Fatalf("c in the prompt: composing %v search %q", m.composing, m.search)
	}
	press(t, m, "\x1bc/in")
	if m.searching || m.pickFilter != "/in" || m.filter != "" {
		t.Fatalf("/ in the picker: searching %v pick %q filter %q", m.searching, m.pickFilter, m.filter)
	}
}

func TestListSearchDebouncesLargeInbox(t *testing.T) {
	items := make([]decide.Item, 5000)
	for i := range items {
		items[i] = decide.Item{ID: fmt.Sprintf("D-%04d", i), Owner: "ada", Question: fmt.Sprintf("question %d", i), State: decide.StateOpen}
		if i%10 == 0 {
			items[i].Messages = []decide.Message{{Author: "ada", Text: "a Needle here"}}
		}
	}
	m := New(t.TempDir())
	clock := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	m.Now = func() time.Time { return clock }
	m.setFolder(items, "")
	key := func(r rune) tea.Cmd {
		t.Helper()
		_, cmd := m.Update(runeKey(r))
		if m.runErr != nil {
			t.Fatal(m.runErr)
		}
		return cmd
	}
	key('/')
	for _, r := range "nee" {
		if cmd := key(r); cmd == nil {
			t.Fatalf("key %q scheduled no filter pass", r)
		}
		clock = clock.Add(10 * time.Millisecond)
	}
	if m.filter != "" || len(m.items) != 5000 {
		t.Fatalf("filtered inside the debounce window: %q %d", m.filter, len(m.items))
	}
	sendKeyMsg(t, m, searchTickMsg{})
	if m.filter != "" {
		t.Fatalf("tick inside the window filtered %q", m.filter)
	}
	clock = clock.Add(searchDebounce)
	sendKeyMsg(t, m, searchTickMsg{})
	if m.filter != "nee" || len(m.items) != 500 {
		t.Fatalf("one pass after the window: %q %d", m.filter, len(m.items))
	}
	key('x')
	key('\r')
	if m.searching || m.filter != "neex" || len(m.items) != 0 {
		t.Fatalf("enter did not force the final pass: %q %d", m.filter, len(m.items))
	}
}
