package tui

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// titleModel opens one thread whose body is long enough to scroll.
func titleModel(t *testing.T, it *decide.Item, width, height int) (*Model, string) {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	it.Owner = "ada"
	it.Kind = decide.KindRelay
	created := mustCreate(t, led, it)
	m := New(dir)
	m.width, m.height = width, height
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	return m, created.ID
}

func longBody() string {
	return "first line\n" + strings.Repeat("- body line\n", 60) + "- bottom"
}

// pinnedTitle is the row right below the box top of the open item, inside
// the outer frame.
func pinnedTitle(view string) string {
	line := visibleLine(strings.Split(view, "\n")[2])
	return strings.TrimSuffix(strings.TrimPrefix(line, "││"), "││")
}

func TestTitleLineShowsWhenOpeningAtTheLastMessage(t *testing.T) {
	t.Parallel()
	m, id := titleModel(t, &decide.Item{Question: longBody()}, 120, 20)
	m.scrollEnd()
	view := m.View()
	if !strings.Contains(view, "- bottom") {
		t.Fatalf("not at the bottom\n%s", view)
	}
	if got := pinnedTitle(view); !strings.Contains(got, id+" first line") || !strings.HasSuffix(got, "open") {
		t.Fatalf("title = %q\n%s", got, view)
	}
}

func TestTitleLineCutsTheQuestionNotTheID(t *testing.T) {
	t.Parallel()
	long := strings.Repeat("word ", 18)
	// Width 48 (inner 44): collapsed toolbar (23) + space (1) leaves 20 cols for the title.
	m, id := titleModel(t, &decide.Item{Question: long}, 48, 20)
	got := pinnedTitle(m.View())
	if !strings.Contains(got, id+" word") || !strings.Contains(got, "…") {
		t.Fatalf("title = %q", got)
	}
	if strings.HasSuffix(got, "open") {
		t.Fatalf("state kept while the question is cut below the minimum: %q", got)
	}
}

func TestTitleLineDropsStateBeforeCuttingTheQuestion(t *testing.T) {
	t.Parallel()
	m, id := titleModel(t, &decide.Item{Question: "short question"}, 120, 20)
	if got := pinnedTitle(m.View()); !strings.Contains(got, id+" short question") || !strings.HasSuffix(got, "open") {
		t.Fatalf("wide title = %q", got)
	}
	// Width 48 (inner 44): collapsed toolbar (23) + space (1) leaves 20 cols for the title;
	// the question fits whole, the state would squeeze it below the minimum, so the state goes.
	m.width = 48
	if got := pinnedTitle(m.View()); !strings.Contains(got, "short question") || strings.Contains(got, "open") {
		t.Fatalf("title = %q", got)
	}
}

func TestTitleLineUsesVerbatimWhenQuestionIsEmpty(t *testing.T) {
	t.Parallel()
	m, id := titleModel(t, &decide.Item{Question: "", Verbatim: "exact words\nsecond line"}, 120, 20)
	got := pinnedTitle(m.View())
	if !strings.Contains(got, id+" exact words") || strings.Contains(got, "second line") {
		t.Fatalf("title = %q", got)
	}
}

func TestTitleLineStaysFixedWhileScrolling(t *testing.T) {
	t.Parallel()
	m, _ := titleModel(t, &decide.Item{Question: longBody()}, 100, 20)
	want := pinnedTitle(m.View())
	for _, scroll := range []func(){func() { m.Wheel(true) }, m.scrollEnd, func() { m.scrollPage(false) }} {
		scroll()
		view := m.View()
		if got := pinnedTitle(view); got != want {
			t.Fatalf("title moved: %q want %q\n%s", got, want, view)
		}
	}
	if m.scroll == 0 {
		t.Fatal("never scrolled")
	}
}

func TestClickingTheTitleGoesToTheTop(t *testing.T) {
	t.Parallel()
	m, id := titleModel(t, &decide.Item{Question: longBody()}, 120, 20)
	m.scrollEnd()
	m.View()
	if m.scroll == 0 {
		t.Fatal("did not scroll")
	}
	clickOn(t, m, id+" first line", 1)
	if m.scroll != 0 || m.message != 0 {
		t.Fatalf("scroll %d message %d after the title click", m.scroll, m.message)
	}
}

func TestTitleLineIsOneRowInBorderlessAndNarrowLayouts(t *testing.T) {
	t.Parallel()
	long := strings.Repeat("word ", 18)
	for _, tc := range []struct {
		name  string
		width int
		plain bool
	}{{"narrow", 48, false}, {"borderless", 60, true}, {"borderless narrow", 30, true}} {
		t.Run(tc.name, func(t *testing.T) {
			m, id := titleModel(t, &decide.Item{Question: long}, tc.width, 20)
			if tc.plain {
				press(t, m, "V")
				if !m.plain {
					t.Fatal("not borderless")
				}
			}
			view := m.View()
			lines := strings.Split(view, "\n")
			first := visibleLine(lines[0])
			if !tc.plain {
				first = pinnedTitle(view)
			}
			first = strings.TrimRight(strings.Trim(first, "│"), " ")
			if !strings.Contains(first, id+" word") || !strings.Contains(first, "…") {
				t.Fatalf("title = %q\n%s", first, view)
			}
			if n := strings.Count(view, id); n != 2 && n != 1 {
				t.Fatalf("id appears %d times\n%s", n, view)
			}
		})
	}
}
