package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func inboxTestItem(t *testing.T, led *decide.Ledger, it *decide.Item) decide.Item {
	t.Helper()
	if it.Owner == "" {
		it.Owner = "ada"
	}
	if it.Kind == "" {
		it.Kind = decide.KindRelay
	}
	if it.Class == decide.ClassApproval && it.Verbatim == "" {
		it.Verbatim = "exact text"
	}
	if it.Class == decide.ClassApproval && len(it.Options) == 0 {
		it.Options = []decide.Option{
			{Key: "yes", Label: "Approve", Recommended: true},
			{Key: "no", Label: "Decline"},
			{Key: "edit", Label: "Edit"},
		}
	}
	if (it.Class == decide.ClassDecision || it.Class == decide.ClassTask) && len(it.Options) == 0 {
		it.Options = []decide.Option{
			{Key: "yes", Label: "Yes", Recommended: true},
			{Key: "no", Label: "No"},
		}
	}
	got, err := led.Create(it)
	if err != nil {
		t.Fatalf("Create(%s): %v", it.Class, err)
	}
	return got
}

func TestInboxTUI_GroupOrderAndTasksByDueDate(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)
	late := time.Date(2026, 12, 5, 0, 0, 0, 0, time.UTC)
	soon := time.Date(2026, 11, 25, 0, 0, 0, 0, time.UTC)

	note := inboxTestItem(t, led, &decide.Item{Class: decide.ClassNote, Question: "note row", CreatedAt: base})
	req := inboxTestItem(t, led, &decide.Item{Class: decide.ClassRequest, Question: "request row", CreatedAt: base.Add(time.Minute)})
	taskLate := inboxTestItem(t, led, &decide.Item{Class: decide.ClassTask, Question: "task late", Deadline: &late, CreatedAt: base.Add(2 * time.Minute)})
	taskSoon := inboxTestItem(t, led, &decide.Item{Class: decide.ClassTask, Question: "task soon", Deadline: &soon, CreatedAt: base.Add(3 * time.Minute)})
	dec := inboxTestItem(t, led, &decide.Item{Class: decide.ClassDecision, Question: "decision row", CreatedAt: base.Add(4 * time.Minute)})
	app := inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Question: "approval row", CreatedAt: base.Add(5 * time.Minute)})

	m := New(dir)
	m.Now = func() time.Time { return base.Add(time.Hour) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24
	view := m.View()

	order := []string{app.ID, taskSoon.ID, taskLate.ID, dec.ID, req.ID, note.ID}
	pos := -1
	for _, id := range order {
		at := strings.Index(view, id)
		if at < 0 {
			t.Fatalf("missing %s in view:\n%s", id, view)
		}
		if at <= pos {
			t.Fatalf("order wrong at %s (order=%v):\n%s", id, order, view)
		}
		pos = at
	}
}

func TestInboxTUI_NestedApprovalAndParentCard(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)

	parent := inboxTestItem(t, led, &decide.Item{Class: decide.ClassRequest, Question: "parent thread", CreatedAt: base})
	note := inboxTestItem(t, led, &decide.Item{Class: decide.ClassNote, Question: "note row", CreatedAt: base.Add(time.Minute)})
	app := inboxTestItem(t, led, &decide.Item{
		Class:     decide.ClassApproval,
		Parent:    parent.ID,
		Question:  "post to slack?",
		Verbatim:  "Hello team, release is live!",
		CreatedAt: base.Add(2 * time.Minute),
	})
	orphan := inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Question: "orphan approval", CreatedAt: base.Add(3 * time.Minute)})

	m := New(dir)
	m.Now = func() time.Time { return base.Add(time.Hour) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24
	view := m.View()

	if strings.Contains(view, app.ID) {
		t.Fatalf("nested approval %s has its own row:\n%s", app.ID, view)
	}
	parentRow := visibleRow(t, view, parent.ID)
	if !strings.Contains(parentRow, "approval waiting") {
		t.Fatalf("parent row missing 'approval waiting': %q", parentRow)
	}
	// Orphan and parent are both before note (approvals group).
	if strings.Index(view, orphan.ID) > strings.Index(view, note.ID) || strings.Index(view, parent.ID) > strings.Index(view, note.ID) {
		t.Fatalf("approvals not before note:\n%s", view)
	}

	// Select parent row and open it.
	for m.cursorID() != parent.ID {
		press(t, m, "j")
	}
	press(t, m, "\r")
	detail := m.View()
	if !strings.Contains(detail, "Hello team, release is live!") {
		t.Fatalf("parent detail missing approval card verbatim:\n%s", detail)
	}
	if !strings.Contains(detail, "Approve") || !strings.Contains(detail, "Decline") || !strings.Contains(detail, "Edit") {
		t.Fatalf("parent detail missing approval card options:\n%s", detail)
	}

	// Answer option 1 (yes) from the parent thread.
	press(t, m, "1")
	gotApp, err := led.Get(app.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotApp.State != decide.StateAnswered || gotApp.Answer == nil || gotApp.Answer.Value != "yes" {
		t.Fatalf("approval after answering card = %+v", gotApp)
	}
	gotParent, err := led.Get(parent.ID)
	if err != nil {
		t.Fatal(err)
	}
	last := gotParent.Messages[len(gotParent.Messages)-1]
	if !strings.HasPrefix(last.Text, app.ID+" approved, posted ") {
		t.Fatalf("parent last message = %q, want prefix %q", last.Text, app.ID+" approved, posted ")
	}
	if detailAfter := m.View(); !strings.Contains(detailAfter, app.ID+" approved, posted ") {
		t.Fatalf("parent detail view missing result line:\n%s", detailAfter)
	}
}

func TestInboxTUI_ClassBadgesAndFilter(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)

	dec := inboxTestItem(t, led, &decide.Item{Class: decide.ClassDecision, Question: "pick db", CreatedAt: base})
	note := inboxTestItem(t, led, &decide.Item{Class: decide.ClassNote, Question: "fyi note", CreatedAt: base.Add(time.Minute)})
	req := inboxTestItem(t, led, &decide.Item{Class: decide.ClassRequest, Question: "plain req", CreatedAt: base.Add(2 * time.Minute)})
	parent := inboxTestItem(t, led, &decide.Item{Class: decide.ClassRequest, Question: "parent req", CreatedAt: base.Add(3 * time.Minute)})
	inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Parent: parent.ID, Question: "approve?", CreatedAt: base.Add(4 * time.Minute)})

	m := New(dir)
	m.Now = func() time.Time { return base.Add(time.Hour) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24
	view := m.View()
	if !strings.Contains(visibleRow(t, view, dec.ID), "[D]") || !strings.Contains(visibleRow(t, view, note.ID), "[N]") {
		t.Fatalf("missing class badges in view:\n%s", view)
	}

	// Filter to decisions and approvals via f -> d, a, enter.
	press(t, m, "fda\r")
	filtered := m.View()
	if !strings.Contains(filtered, dec.ID) || !strings.Contains(filtered, parent.ID) {
		t.Fatalf("filtered view missing decision or parent-with-approval:\n%s", filtered)
	}
	if strings.Contains(filtered, note.ID) || strings.Contains(filtered, req.ID) {
		t.Fatalf("filtered view still shows note or plain request:\n%s", filtered)
	}

	// Esc clears the class filter.
	press(t, m, "\x1b")
	cleared := m.View()
	if !strings.Contains(cleared, note.ID) || !strings.Contains(cleared, req.ID) {
		t.Fatalf("esc did not clear class filter:\n%s", cleared)
	}
}

func TestInboxTUI_QuickKeysFromList(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)

	app1 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Question: "first?", Verbatim: "v1", CreatedAt: base.Add(3 * time.Minute)})
	parent := inboxTestItem(t, led, &decide.Item{Class: decide.ClassRequest, Question: "parent", CreatedAt: base.Add(2 * time.Minute)})
	app2 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Parent: parent.ID, Question: "second?", Verbatim: "v2", CreatedAt: base.Add(time.Minute)})
	app3 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassApproval, Question: "third?", Verbatim: "edit me please", CreatedAt: base})

	m := New(dir)
	m.Now = func() time.Time { return base.Add(time.Hour) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24

	// Cursor starts on app1 -> press y to approve.
	if m.cursorID() != app1.ID {
		t.Fatalf("cursor = %s, want %s", m.cursorID(), app1.ID)
	}
	press(t, m, "y")
	if got, _ := led.Get(app1.ID); got.State != decide.StateAnswered || got.Answer.Value != "yes" {
		t.Fatalf("app1 after y = %+v", got)
	}

	// Next row is parent of app2 -> press n to decline app2.
	for m.cursorID() != parent.ID {
		press(t, m, "j")
	}
	press(t, m, "n")
	if got, _ := led.Get(app2.ID); got.State != decide.StateAnswered || got.Answer.Value != "no" {
		t.Fatalf("app2 after n = %+v", got)
	}
	if p, _ := led.Get(parent.ID); p.Messages[len(p.Messages)-1].Text != app2.ID+" declined" {
		t.Fatalf("parent after n = %+v", p.Messages)
	}

	// Select app3 (now at the top of approvals group) -> press e to open edit with verbatim prefilled.
	press(t, m, "gg")
	if m.cursorID() != app3.ID {
		t.Fatalf("cursor = %s, want %s", m.cursorID(), app3.ID)
	}
	press(t, m, "e")
	if m.mode != modeDetail || !m.replyFocus || m.reply != "edit me please" {
		t.Fatalf("after e: mode=%d replyFocus=%v reply=%q", m.mode, m.replyFocus, m.reply)
	}
}

func TestInboxTUI_BulkArchiveNotesAndUnreadCount(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)

	n1 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassNote, Question: "note 1", Unread: true, CreatedAt: base})
	n2 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassNote, Question: "note 2", Unread: true, CreatedAt: base.Add(time.Minute)})
	dec := inboxTestItem(t, led, &decide.Item{Class: decide.ClassDecision, Question: "dec 1", Unread: true, CreatedAt: base.Add(2 * time.Minute)})
	dec2 := inboxTestItem(t, led, &decide.Item{Class: decide.ClassDecision, Question: "dec 2", Unread: false, CreatedAt: base.Add(3 * time.Minute)})
	if _, err := led.MarkRead(dec2.ID); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return base.Add(time.Hour) }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if got := m.UnreadCount(); got != 1 {
		t.Fatalf("UnreadCount() = %d, want 1 (notes excluded)", got)
	}
	m.width = 120
	m.height = 24
	if view := m.View(); !strings.Contains(view, "unread: 1") {
		t.Fatalf("view missing 'unread: 1':\n%s", view)
	}

	// Bulk archive notes with E.
	press(t, m, "E")
	for _, id := range []string{n1.ID, n2.ID} {
		if got, _ := led.Get(id); got.State != decide.StateDone {
			t.Fatalf("note %s state = %s, want done", id, got.State)
		}
	}
	if got, _ := led.Get(dec.ID); got.State != decide.StateOpen {
		t.Fatalf("decision %s state = %s, want open", dec.ID, got.State)
	}
}
