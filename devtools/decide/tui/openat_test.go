package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func openAtBase() time.Time { return time.Date(2026, 11, 5, 14, 0, 0, 0, time.UTC) }

// seedOpenAt stores a four message thread and returns its directory. The
// third message is the first one newer than readAt (14:05).
func seedOpenAt(t *testing.T, readAt *time.Time) string {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	at := func(min int) time.Time { return openAtBase().Add(time.Duration(min) * time.Minute) }
	mustCreate(t, led, &decide.Item{
		Question:  "root ask",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		FreeText:  true,
		CreatedAt: at(0),
		ReadAt:    readAt,
		Messages: []decide.Message{
			{Author: "ada", Text: "m1", Time: at(0)},
			{Author: "human", Text: "m2", Time: at(3)},
			{Author: "ada", Text: "m3", Time: at(10)},
			{Author: "ada", Text: "m4", Time: at(20)},
		},
	})
	return dir
}

func openAtModel(t *testing.T, dir, openAt string) *Model {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m := New(dir)
	m.Now = func() time.Time { return openAtBase().Add(time.Hour) }
	m.UTC = true
	m.openAt = openAt
	m.width, m.height = 80, 20
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	return m
}

func scrolledLine(m *Model) string {
	lines := m.detailLines(m.detailInnerWidth(m.contentWidth()))
	return visibleLine(lines[m.scroll].text)
}

func TestOpenAtNewStartsAtFirstUnreadWithDivider(t *testing.T) {
	readAt := openAtBase().Add(5 * time.Minute)
	m := openAtModel(t, seedOpenAt(t, &readAt), "")
	if m.message != 2 {
		t.Fatalf("message = %d, want 2", m.message)
	}
	if got := scrolledLine(m); !strings.HasPrefix(got, "── new since 14:05") || !strings.HasSuffix(got, " ──") {
		t.Fatalf("top line = %q, want the new-since divider", got)
	}
}

func TestOpenAtNewNothingNewOpensAtLast(t *testing.T) {
	readAt := openAtBase().Add(time.Hour)
	m := openAtModel(t, seedOpenAt(t, &readAt), "new")
	if m.message != 3 {
		t.Fatalf("message = %d, want 3", m.message)
	}
	if strings.Contains(strings.Join(threadText(m), "\n"), "new since") {
		t.Fatal("no divider expected when nothing is new")
	}
}

func TestOpenAtNewNeverReadOpensAtTop(t *testing.T) {
	m := openAtModel(t, seedOpenAt(t, nil), "new")
	if m.message != 0 || m.scroll != 0 {
		t.Fatalf("message = %d scroll = %d, want 0 0", m.message, m.scroll)
	}
}

func threadText(m *Model) []string {
	lines := m.detailLines(m.detailInnerWidth(m.contentWidth()))
	out := make([]string, len(lines))
	for i, l := range lines {
		out[i] = visibleLine(l.text)
	}
	return out
}

func TestOpenAtLastAndTop(t *testing.T) {
	readAt := openAtBase().Add(5 * time.Minute)
	dir := seedOpenAt(t, &readAt)
	if m := openAtModel(t, dir, "last"); m.message != 3 {
		t.Fatalf("last: message = %d, want 3", m.message)
	}
	m := openAtModel(t, dir, "top")
	if m.message != 0 || m.scroll != 0 {
		t.Fatalf("top: message = %d scroll = %d, want 0 0", m.message, m.scroll)
	}
}

func TestOpenAtReadAtFromBeforeMarkRead(t *testing.T) {
	// The first open marks the item read; the first-new index must come
	// from the ReadAt that was stored before that mark.
	readAt := openAtBase().Add(5 * time.Minute)
	dir := seedOpenAt(t, &readAt)
	hub := &fakeHub{hub: &LocalBackend{Dir: dir}}
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m := NewWithBackend(hub)
	m.Now = func() time.Time { return openAtBase().Add(time.Hour) }
	m.width, m.height = 80, 20
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	if m.message != 2 {
		t.Fatalf("message = %d, want 2", m.message)
	}
	if m.detail.ReadAt == nil || !m.detail.ReadAt.After(readAt) {
		t.Fatalf("detail ReadAt = %v, want the new mark", m.detail.ReadAt)
	}
}

func TestOpenAtMidThreadDoesNotCountUnrenderedAsSeen(t *testing.T) {
	// ui.openAt=last leaves m3 unrendered above the view; archive must hold.
	readAt := openAtBase().Add(5 * time.Minute)
	m := openAtModel(t, seedOpenAt(t, &readAt), "last")
	m.Now = func() time.Time { return openAtBase().Add(24 * time.Hour) }
	press(t, m, "e")
	if m.status != statusArchiveHeld {
		t.Fatalf("status = %q, want archive held", m.status)
	}
}
