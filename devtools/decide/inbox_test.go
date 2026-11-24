package decide

import (
	"strings"
	"testing"
	"time"
)

func inboxLedger(t *testing.T) *Ledger {
	t.Helper()
	led, err := Open(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	base := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)
	n := 0
	led.SetClock(func() time.Time {
		n++
		return base.Add(time.Duration(n) * time.Minute)
	})
	return led
}

var yesNo = []Option{{Key: "yes", Label: "Yes", Recommended: true}, {Key: "no", Label: "No"}}

func mk(t *testing.T, led *Ledger, it Item) Item {
	t.Helper()
	if it.Owner == "" {
		it.Owner = "ada"
	}
	if it.Kind == "" {
		it.Kind = KindRelay
	}
	if it.Question == "" {
		it.Question = "q " + it.Class
	}
	if (it.Class == ClassDecision || it.Class == ClassTask) && it.Options == nil {
		it.Options = yesNo
	}
	if it.Class == ClassApproval {
		it.Verbatim = "exact text"
		it.Options = []Option{{Key: "yes", Label: "Post", Recommended: true}, {Key: "no", Label: "No"}, {Key: "edit", Label: "Edit"}}
	}
	got, err := led.Create(&it)
	if err != nil {
		t.Fatalf("Create %s: %v", it.Class, err)
	}
	return got
}

func entryIDs(entries []InboxEntry) []string {
	var out []string
	for _, e := range entries {
		out = append(out, e.Group+":"+e.Item.ID)
	}
	return out
}

func TestInboxGroupOrderAndTaskDueDate(t *testing.T) {
	led := inboxLedger(t)
	late := time.Date(2026, 12, 5, 0, 0, 0, 0, time.UTC)
	soon := time.Date(2026, 11, 25, 0, 0, 0, 0, time.UTC)
	note := mk(t, led, Item{Class: ClassNote})
	req := mk(t, led, Item{Class: ClassRequest})
	task1 := mk(t, led, Item{Class: ClassTask, Deadline: &late})
	task2 := mk(t, led, Item{Class: ClassTask, Deadline: &soon})
	dec := mk(t, led, Item{Class: ClassDecision})
	app := mk(t, led, Item{Class: ClassApproval})
	items, _ := led.List()
	got := entryIDs(InboxEntries(items, nil))
	want := []string{
		GroupApprovals + ":" + app.ID,
		GroupNeedsYou + ":" + task2.ID,
		GroupNeedsYou + ":" + task1.ID,
		GroupNeedsYou + ":" + dec.ID,
		GroupRequests + ":" + req.ID,
		GroupNotes + ":" + note.ID,
	}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("entries = %v, want %v", got, want)
	}
}

func TestInboxNestsApprovalUnderParent(t *testing.T) {
	led := inboxLedger(t)
	parent := mk(t, led, Item{Class: ClassRequest})
	note := mk(t, led, Item{Class: ClassNote})
	app := mk(t, led, Item{Class: ClassApproval, Parent: parent.ID})
	orphan := mk(t, led, Item{Class: ClassApproval})
	items, _ := led.List()
	entries := InboxEntries(items, nil)
	got := entryIDs(entries)
	// Parent moves to approvals; the nested approval has no row of its own.
	want := []string{GroupApprovals + ":" + orphan.ID, GroupApprovals + ":" + parent.ID, GroupNotes + ":" + note.ID}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("entries = %v, want %v", got, want)
	}
	nested := entries[1]
	if !nested.ApprovalWaiting || len(nested.Approvals) != 1 || nested.Approvals[0].ID != app.ID {
		t.Fatalf("nested = %+v", nested)
	}
	if entries[0].ApprovalWaiting || len(entries[0].Approvals) != 0 {
		t.Fatalf("parentless approval = %+v", entries[0])
	}
	card, ok := ApprovalCard(items, parent.ID)
	if !ok || card.ID != app.ID {
		t.Fatalf("ApprovalCard = %v, %v", card.ID, ok)
	}
	if _, ok := ApprovalCard(items, note.ID); ok {
		t.Fatal("ApprovalCard found a card on a thread without one")
	}
}

func TestAnswerCardPostsLineToParent(t *testing.T) {
	led := inboxLedger(t)
	parent := mk(t, led, Item{Class: ClassRequest})
	app := mk(t, led, Item{Class: ClassApproval, Parent: parent.ID})
	got, err := led.AnswerCard(parent.ID, "yes", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if got.ID != app.ID || got.State != StateAnswered || got.Answer.Value != "yes" {
		t.Fatalf("card = %+v", got)
	}
	p, _ := led.Get(parent.ID)
	last := p.Messages[len(p.Messages)-1]
	if last.Author != "system" || !strings.HasPrefix(last.Text, app.ID+" approved, posted ") {
		t.Fatalf("parent line = %+v", last)
	}
	// A later answer on a second card posts "declined".
	app2 := mk(t, led, Item{Class: ClassApproval, Parent: parent.ID})
	if _, err := led.Answer(app2.ID, "no", ViaCLI); err != nil {
		t.Fatal(err)
	}
	p, _ = led.Get(parent.ID)
	if last := p.Messages[len(p.Messages)-1]; last.Text != app2.ID+" declined" {
		t.Fatalf("declined line = %q", last.Text)
	}
	if _, err := led.AnswerCard(parent.ID, "yes", ViaUI); err == nil {
		t.Fatal("AnswerCard with no open card succeeded")
	}
}

func TestInboxClassFilterAndUnreadCount(t *testing.T) {
	led := inboxLedger(t)
	dec := mk(t, led, Item{Class: ClassDecision})
	mk(t, led, Item{Class: ClassNote})
	mk(t, led, Item{Class: ClassRequest})
	parent := mk(t, led, Item{Class: ClassRequest})
	app := mk(t, led, Item{Class: ClassApproval, Parent: parent.ID})
	items, _ := led.List()
	got := entryIDs(InboxEntries(items, []string{ClassDecision, ClassApproval}))
	want := []string{GroupApprovals + ":" + parent.ID, GroupNeedsYou + ":" + dec.ID}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("filtered = %v, want %v (app %s)", got, want, app.ID)
	}
	if BadgeOf(&dec) != "D" {
		t.Fatalf("badge = %q", BadgeOf(&dec))
	}
	for i := range items {
		items[i].Unread = true
	}
	// 5 items, one is a note.
	if n := UnreadCount(items); n != 4 {
		t.Fatalf("UnreadCount = %d, want 4", n)
	}
}

func TestApprovalKeyAndArchiveNotes(t *testing.T) {
	for r, want := range map[rune]string{'y': "yes", 'n': "no", 'e': "edit"} {
		if got, ok := ApprovalKeyOption(r); !ok || got != want {
			t.Errorf("ApprovalKeyOption(%q) = %q, %v", r, got, ok)
		}
	}
	if _, ok := ApprovalKeyOption('x'); ok {
		t.Error("ApprovalKeyOption(x) ok")
	}
	led := inboxLedger(t)
	n1 := mk(t, led, Item{Class: ClassNote})
	n2 := mk(t, led, Item{Class: ClassNote})
	dec := mk(t, led, Item{Class: ClassDecision})
	count, err := led.ArchiveNotes(time.Date(2026, 11, 22, 23, 0, 0, 0, time.UTC))
	if err != nil || count != 2 {
		t.Fatalf("ArchiveNotes = %d, %v", count, err)
	}
	for _, id := range []string{n1.ID, n2.ID} {
		if it, _ := led.Get(id); it.State != StateDone {
			t.Errorf("%s state = %s", id, it.State)
		}
	}
	if it, _ := led.Get(dec.ID); it.State != StateOpen {
		t.Errorf("decision state = %s", it.State)
	}
}

func TestBackfillProposesWithoutWritingThenApplies(t *testing.T) {
	led := inboxLedger(t)
	post := mk(t, led, Item{Class: ClassDecision, Question: "Post as written?", Verbatim: "hello"})
	report := mk(t, led, Item{Class: ClassDecision, Question: "Installed foo 1.2", Options: []Option{}})
	report, _ = led.Get(report.ID)
	started := mk(t, led, Item{
		Class:    ClassDecision,
		Question: "please look at X",
		Options:  []Option{},
		Messages: []Message{{Author: "human", Text: "please look at X"}},
	})
	keep := mk(t, led, Item{Class: ClassDecision, Question: "Pick a db?"})
	items, _ := led.List()
	props := ProposeBackfill(items)
	want := map[string]string{post.ID: ClassApproval, report.ID: ClassNote, started.ID: ClassRequest}
	if len(props) != len(want) {
		t.Fatalf("proposals = %+v", props)
	}
	for _, p := range props {
		if want[p.ID] != p.To || p.From != ClassDecision || p.Reason == "" {
			t.Errorf("proposal %+v", p)
		}
		if p.ID == keep.ID {
			t.Errorf("uncertain item proposed: %+v", p)
		}
	}
	// Proposing wrote nothing.
	after, _ := led.List()
	for i := range after {
		if after[i].EffectiveClass() != ClassDecision || len(after[i].Messages) != len(items[i].Messages) {
			t.Fatalf("proposal changed %s", after[i].ID)
		}
	}
	n, err := led.ApplyBackfill(props, "me")
	if err != nil || n != 3 {
		t.Fatalf("ApplyBackfill = %d, %v", n, err)
	}
	got, err := led.Get("A-" + post.ID[2:])
	if err != nil || got.EffectiveClass() != ClassApproval {
		t.Fatalf("applied = %+v, %v", got, err)
	}
	if last := got.Messages[len(got.Messages)-1]; !strings.Contains(last.Text, "retagged: decision → approval") {
		t.Fatalf("retag event = %q", last.Text)
	}
}
