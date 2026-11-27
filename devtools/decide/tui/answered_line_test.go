package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func answeredLineModel(t *testing.T, opts []decide.Option, freeText bool) (*Model, *decide.Ledger, decide.Item) {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  opts,
		FreeText: freeText,
	})
	m := New(dir)
	m.width, m.height = 100, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	return m, led, item
}

var yesNo = []decide.Option{{Key: "y", Label: "Yes"}, {Key: "n", Label: "No"}}

func TestAnsweredHidesOptions(t *testing.T) {
	t.Parallel()
	m, led, item := answeredLineModel(t, yesNo, false)
	press(t, m, "\r1")
	view := m.View()
	if !strings.Contains(view, "Answered: y · Yes · ") {
		t.Fatalf("missing answered line:\n%s", view)
	}
	if strings.Contains(view, "2  No") || strings.Contains(view, "1  Yes") {
		t.Fatalf("options still shown:\n%s", view)
	}
	press(t, m, "2")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Value != "y" {
		t.Fatalf("digit re-answered: %+v", got.Answer)
	}
}

func TestAnsweredFreeTextLine(t *testing.T) {
	t.Parallel()
	m, led, item := answeredLineModel(t, nil, true)
	if _, err := led.Answer(item.ID, "later please\nthanks", decide.ViaUI); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "Answered: later please · ") {
		t.Fatalf("free-text line wrong:\n%s", view)
	}
}

func TestAnsweredOpenedFreshShowsLine(t *testing.T) {
	t.Parallel()
	m, led, item := answeredLineModel(t, yesNo, false)
	if _, err := led.Answer(item.ID, "n", decide.ViaUI); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "Answered: n · No · ") || strings.Contains(view, "1  Yes") {
		t.Fatalf("fresh open wrong:\n%s", view)
	}
}

func TestFollowUpOptionsShownAgain(t *testing.T) {
	t.Parallel()
	m, led, item := answeredLineModel(t, yesNo, false)
	press(t, m, "\r1")
	if _, err := led.Reply(item.ID, "ada", "which one?", []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}}, false); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "1  Alpha") || strings.Contains(view, "Answered: y") {
		t.Fatalf("follow-up options missing:\n%s", view)
	}
	press(t, m, "2")
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Value != "b" {
		t.Fatalf("follow-up not answered: %+v", got.Answer)
	}
}

func TestOutboxPendingAnswerLine(t *testing.T) {
	t.Parallel()
	m, _, item := answeredLineModel(t, yesNo, false)
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: m.Dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{{
			ID: "OB-001", Op: "answer", ItemID: item.ID, Text: "y", WrittenAt: now, Status: OutboxPending,
		}},
	}
	m.backend = ob
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(item.ID); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "Answering: y · Yes (in outbox)") || strings.Contains(view, "2  No") {
		t.Fatalf("pending line wrong:\n%s", view)
	}
	press(t, m, "2")
	if len(ob.entries) != 1 {
		t.Fatalf("digit queued another answer: %+v", ob.entries)
	}
	ob.entries[0].Status = OutboxFailed
	ob.entries[0].Error = "boom"
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view = m.View()
	if !strings.Contains(view, "2  No") || strings.Contains(view, "Answering:") {
		t.Fatalf("failed send did not restore options:\n%s", view)
	}
}
