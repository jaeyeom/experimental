package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func (f *fakeOutboxBackend) SendOutboxAsMessage(id string) error {
	f.asMessage = append(f.asMessage, id)
	for i := range f.entries {
		if f.entries[i].ID == id {
			f.entries[i].Op = "reply"
			f.entries[i].Status = OutboxPending
			f.entries[i].Error = ""
		}
	}
	return nil
}

func outboxFolderModel(t *testing.T, entries ...OutboxEntry) (*Model, *fakeOutboxBackend) {
	t.Helper()
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	it, err := led.Create(&decide.Item{Question: "pick?", Owner: "ada", Kind: decide.KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	for i := range entries {
		entries[i].ItemID = it.ID
		entries[i].WrittenAt = now
	}
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries:      entries,
	}
	m := New(dir)
	m.backend = ob
	m.width, m.height = 100, 30
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.showOutbox(); err != nil {
		t.Fatal(err)
	}
	return m, ob
}

func pressAll(t *testing.T, m *Model, keys string) {
	t.Helper()
	for _, r := range keys {
		if _, err := m.Key(r); err != nil {
			t.Fatal(err)
		}
	}
}

func TestOutboxDiscardKeyRemovesFailedAndPendingEntries(t *testing.T) {
	m, ob := outboxFolderModel(t,
		OutboxEntry{ID: "OB-001", Op: "answer", Text: "y", Status: OutboxFailed, Error: "decide item D-001 is done"},
		OutboxEntry{ID: "OB-002", Op: "reply", Text: "hello", Status: OutboxPending},
	)
	pressAll(t, m, "x")
	if len(ob.entries) != 1 || ob.entries[0].ID != "OB-002" {
		t.Fatalf("after x on a failed entry: %+v", ob.entries)
	}
	pressAll(t, m, "x")
	if len(ob.entries) != 0 {
		t.Fatalf("after x on a pending entry: %+v", ob.entries)
	}
}

func TestOutboxDiscardConfirmsOnlyLongEntries(t *testing.T) {
	short := "one\ntwo\nthree"
	long := "one\ntwo\nthree\nfour"
	m, ob := outboxFolderModel(t,
		OutboxEntry{ID: "OB-001", Op: "reply", Text: short, Status: OutboxPending},
		OutboxEntry{ID: "OB-002", Op: "reply", Text: long, Status: OutboxPending},
	)
	pressAll(t, m, "x")
	if len(ob.entries) != 1 || m.confirmKind != confirmNone {
		t.Fatalf("a 3-line entry asked to confirm or stayed: %+v", ob.entries)
	}
	pressAll(t, m, "x")
	if len(ob.entries) != 1 || m.confirmKind == confirmNone {
		t.Fatalf("a 4-line entry was discarded without a confirm: %+v", ob.entries)
	}
	if !strings.Contains(m.status, "discard OB-002?") {
		t.Fatalf("status = %q", m.status)
	}
	pressAll(t, m, "n")
	if len(ob.entries) != 1 {
		t.Fatalf("n discarded the entry: %+v", ob.entries)
	}
	pressAll(t, m, "xy")
	if len(ob.entries) != 0 {
		t.Fatalf("y did not discard: %+v", ob.entries)
	}
}

func TestOutboxDiscardFollowsRemappedKeyAndHint(t *testing.T) {
	km, err := decide.ResolveKeyMap(map[string][]string{"outbox.discard": {"D"}})
	if err != nil {
		t.Fatal(err)
	}
	m, ob := outboxFolderModel(t,
		OutboxEntry{ID: "OB-001", Op: "answer", Text: "y", Status: OutboxFailed, Error: "decide item D-001 is done"},
	)
	m.SetKeys(km)
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	if view := m.View(); !strings.Contains(view, "[Discard (D)]") || strings.Contains(view, "[Discard (x)]") {
		t.Fatalf("hint does not follow the binding:\n%s", view)
	}
	pressAll(t, m, "x")
	if len(ob.entries) != 1 {
		t.Fatalf("old key x still discards: %+v", ob.entries)
	}
	pressAll(t, m, "D")
	if len(ob.entries) != 0 {
		t.Fatalf("remapped key D did not discard: %+v", ob.entries)
	}
}

func TestOutboxFailedEntryThreadShowsNotSentWithActions(t *testing.T) {
	m, ob := outboxFolderModel(t,
		OutboxEntry{ID: "OB-001", Op: "answer", Text: "y", Status: OutboxFailed, Error: "decide item D-001 is done"},
	)
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	for _, want := range []string{"not sent: decide item D-001 is done", "[Send as message (s)]", "[Discard (x)]"} {
		if !strings.Contains(view, want) {
			t.Fatalf("thread missing %q:\n%s", want, view)
		}
	}
	pressAll(t, m, "s")
	if len(ob.asMessage) != 1 || ob.asMessage[0] != "OB-001" {
		t.Fatalf("asMessage = %v", ob.asMessage)
	}
}

func TestOutboxHelpListsDiscard(t *testing.T) {
	m, _ := outboxFolderModel(t,
		OutboxEntry{ID: "OB-001", Op: "reply", Text: "a", Status: OutboxPending},
	)
	pressAll(t, m, "?")
	if view := m.View(); !strings.Contains(view, "x discards the outbox entry") {
		t.Fatalf("help missing the discard key:\n%s", view)
	}
}

func TestDoneThreadOffersNoAnswerKeysOrDoneButAcceptsReply(t *testing.T) {
	dir := t.TempDir()
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	it, err := led.Create(&decide.Item{
		Question: "pick?", Owner: "ada", Kind: decide.KindRelay,
		Options: []decide.Option{{Key: "y", Label: "Yes please"}, {Key: "n", Label: "No thanks"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.DoneVia(it.ID, "finished", decide.ViaUI); err != nil {
		t.Fatal(err)
	}
	m := New(dir)
	m.width, m.height = 100, 30
	m.folder = 3
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(it.ID); err != nil {
		t.Fatal(err)
	}
	if view := m.View(); strings.Contains(view, "Yes please") || strings.Contains(view, "No thanks") {
		t.Fatalf("done thread offers answer choices:\n%s", view)
	}
	pressAll(t, m, "1")
	if got, _ := led.Get(it.ID); got.Answer != nil {
		t.Fatalf("1 answered a done item: %+v", got.Answer)
	}
	pressAll(t, m, "d")
	if m.mode == modeOutcome {
		t.Fatal("d opened the outcome prompt on a done thread")
	}
	pressAll(t, m, "0")
	if !m.replyFocus {
		t.Fatal("a done thread does not accept a reply")
	}
}

func TestFailedAnswerOnDoneItemRestoresTextToReplyBox(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	it, err := led.Create(&decide.Item{Question: "pick?", Owner: "ada", Kind: decide.KindRelay, FreeText: true})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.DoneVia(it.ID, "finished", decide.ViaUI); err != nil {
		t.Fatal(err)
	}
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "answer", ItemID: it.ID, Text: "my answer", WrittenAt: now, Status: OutboxFailed, Error: "decide item " + it.ID + " is done"},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width, m.height = 100, 30
	m.folder = 3
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openItemByID(it.ID); err != nil {
		t.Fatal(err)
	}
	if m.reply != "my answer" || m.draft != "" {
		t.Fatalf("reply = %q, draft = %q; want the text in the reply box", m.reply, m.draft)
	}
}
