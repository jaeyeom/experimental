package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func TestInboxShowsCloseWhenTheOwnerIsGone(t *testing.T) {
	herdrtest.Install(t)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	start := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	now := start
	led := openLedger(t, dir)
	led.SetClock(func() time.Time { return now })
	item := mustCreate(t, led, &decide.Item{
		Question:   "ship",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		NotifyPane: "w3:p9",
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}},
		CreatedAt:  start,
	})
	if _, err := led.Answer(item.ID, "a", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}
	now = start.Add(30 * time.Minute)
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	marked, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if marked.FollowUp != decide.FollowUpOwnerGone {
		t.Fatalf("followUp = %q", marked.FollowUp)
	}
	for _, d := range marked.Delivery {
		if d.Outcome == decide.OutcomeResend {
			t.Fatalf("re-sent a gone owner: %+v", marked.Delivery)
		}
	}
	press(t, m, "ga\r")
	view := m.View()
	if !strings.Contains(view, "Close") || !strings.Contains(view, "owner gone") {
		t.Fatalf("item view\n%s", view)
	}
	if err := m.Click(1, screenRow(view, "Close")); err != nil {
		t.Fatal(err)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateDone || got.Outcome != "closed" {
		t.Fatalf("after close: state %s outcome %q", got.State, got.Outcome)
	}
}
