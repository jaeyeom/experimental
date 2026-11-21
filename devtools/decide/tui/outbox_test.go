package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

type fakeOutboxBackend struct {
	*LocalBackend
	entries   []OutboxEntry
	sentIDs   []string
	editedIDs map[string]string
	cancelled []string
}

func (f *fakeOutboxBackend) OutboxEnabled() bool { return true }
func (f *fakeOutboxBackend) Outbox() []OutboxEntry {
	out := make([]OutboxEntry, len(f.entries))
	copy(out, f.entries)
	return out
}

func (f *fakeOutboxBackend) EditOutbox(id, text string) error {
	if f.editedIDs == nil {
		f.editedIDs = make(map[string]string)
	}
	f.editedIDs[id] = text
	for i := range f.entries {
		if f.entries[i].ID == id {
			f.entries[i].Text = text
			f.entries[i].Status = OutboxPending
			f.entries[i].Error = ""
			f.entries[i].HoldReason = ""
		}
	}
	return nil
}

func (f *fakeOutboxBackend) CancelOutbox(id string) error {
	f.cancelled = append(f.cancelled, id)
	for i := range f.entries {
		if f.entries[i].ID == id {
			f.entries = append(f.entries[:i], f.entries[i+1:]...)
			break
		}
	}
	return nil
}

func (f *fakeOutboxBackend) SendOutbox(id string) error {
	f.sentIDs = append(f.sentIDs, id)
	for i := range f.entries {
		if f.entries[i].ID == id {
			f.entries[i].Status = OutboxPending
			f.entries[i].HoldReason = ""
		}
	}
	return nil
}

func (f *fakeOutboxBackend) DismissFailedOutbox(id string) error {
	return f.CancelOutbox(id)
}

func TestOutboxFolderShownWithCountAndGoToChord(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: "D-001", Text: "hello", WrittenAt: now, Status: OutboxPending},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "Outbox") {
		t.Fatalf("expected Outbox in sidebar:\n%s", view)
	}
	if _, err := m.Key('g'); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('o'); err != nil {
		t.Fatal(err)
	}
	if m.mode != modeOutbox {
		t.Fatalf("mode = %d, want modeOutbox (%d)", m.mode, modeOutbox)
	}
	if !strings.Contains(m.View(), "OB-001") || !strings.Contains(m.View(), "in outbox") {
		t.Fatalf("expected OB-001 and 'in outbox' in outbox view:\n%s", m.View())
	}
}

func TestOutboxInThreadShowsInOutboxAndTurnsNormalWhenConfirmed(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	it, err := led.Create(&decide.Item{Question: "status?", Owner: "ada", Kind: decide.KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: it.ID, Text: "pending reply", WrittenAt: now, Status: OutboxPending},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "in outbox") || !strings.Contains(view, "pending reply") {
		t.Fatalf("expected 'in outbox' and pending reply in thread:\n%s", view)
	}

	// Simulate hub confirmation: remove from outbox and add to ledger.
	ob.entries = nil
	if _, err := led.Compose(it.ID, "pending reply", decide.ViaUI, ""); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view = m.View()
	if strings.Contains(view, "in outbox") {
		t.Fatalf("did not expect 'in outbox' after confirmation:\n%s", view)
	}
	if !strings.Contains(view, "pending reply") {
		t.Fatalf("expected confirmed reply in thread:\n%s", view)
	}
}

func TestOutboxPermanentErrorShowsFailedAndRestoresComposerText(t *testing.T) {
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
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: it.ID, Text: "restored draft", WrittenAt: now, Status: OutboxFailed, Error: "item is withdrawn"},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "failed: item is withdrawn") {
		t.Fatalf("expected failed reason in thread:\n%s", view)
	}
	if m.reply != "restored draft" {
		t.Fatalf("m.reply = %q, want %q", m.reply, "restored draft")
	}
}

func TestOutboxStaleWarningAppearsAfterThreshold(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: "D-001", Text: "old", WrittenAt: now.Add(-6 * time.Minute), Status: OutboxPending},
		},
	}
	m := New(dir)
	m.backend = ob
	m.OutboxStaleAfter = 5 * time.Minute
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if got := m.footerStatus(); !strings.Contains(got, "outbox: 1 pending (>5m)") {
		t.Fatalf("footerStatus = %q, want stale warning", got)
	}

	// Custom threshold of 10m should not warn at 6m.
	m.OutboxStaleAfter = 10 * time.Minute
	if got := m.footerStatus(); strings.Contains(got, "pending") {
		t.Fatalf("footerStatus = %q, did not expect warning below 10m", got)
	}
}

func TestOutboxQuitConfirmationShowsPendingCount(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: "D-001", Text: "a", WrittenAt: now, Status: OutboxPending},
			{ID: "OB-002", Op: "reply", ItemID: "D-002", Text: "b", WrittenAt: now, Status: OutboxPending},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	quit, err := m.Key('q')
	if err != nil || quit {
		t.Fatalf("expected quit to ask confirmation, got quit=%v err=%v", quit, err)
	}
	if !strings.Contains(m.status, "2 pending entries") {
		t.Fatalf("status = %q, want 2 pending entries", m.status)
	}
	quit, err = m.Key('y')
	if err != nil || !quit {
		t.Fatalf("expected 'y' to confirm quit, got quit=%v err=%v", quit, err)
	}
}

func TestOutboxConflictHoldOffersSendEditCancelInThread(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	it, err := led.Create(&decide.Item{Question: "hold?", Owner: "ada", Kind: decide.KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
		entries: []OutboxEntry{
			{ID: "OB-001", Op: "reply", ItemID: it.ID, Text: "my reply", WrittenAt: now, Status: OutboxHeld, HoldReason: "new message arrived"},
		},
	}
	m := New(dir)
	m.backend = ob
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Key('\r'); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "held: new message arrived") || !strings.Contains(view, "[s] send  [e] edit  [x] cancel") {
		t.Fatalf("expected held message and actions in thread:\n%s", view)
	}
	if _, err := m.Key('s'); err != nil {
		t.Fatal(err)
	}
	if len(ob.sentIDs) != 1 || ob.sentIDs[0] != "OB-001" {
		t.Fatalf("sentIDs = %v, want [OB-001]", ob.sentIDs)
	}
}
