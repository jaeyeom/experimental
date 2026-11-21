package inboxrpc

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func pressTUI(t *testing.T, m *tui.Model, keys string) {
	t.Helper()
	for _, r := range keys {
		if _, err := m.Key(r); err != nil {
			t.Fatalf("key %q: %v", r, err)
		}
	}
}

func newTwoTUIClients(t *testing.T) (*hub, *Client, *Client, *tui.Model, *tui.Model) {
	t.Helper()
	h := newHub(t)
	cA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	cB := &Client{Dial: h.dial, Label: "me@hub", Machine: "beta", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	if err := cA.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(cA.Close)
	if err := cB.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(cB.Close)

	mA := tui.NewWithBackend(cA)
	mB := tui.NewWithBackend(cB)
	mA.Update(tea.WindowSizeMsg{Width: 100, Height: 24})
	mB.Update(tea.WindowSizeMsg{Width: 100, Height: 24})
	return h, cA, cB, mA, mB
}

func TestTwoClientTUI_ReplyDraftSyncsAndShowsMark(t *testing.T) {
	h, cA, cB, mA, mB := newTwoTUIClients(t)
	h.create(t, &decide.Item{
		Question: "sync draft across clients?",
		Owner:    "ada",
		FreeText: true,
		Messages: []decide.Message{{Author: "ada", Text: "sync draft across clients?"}},
	})

	eventually(t, func() bool {
		return mA.Reload() == nil && mB.Reload() == nil &&
			strings.Contains(mA.View(), "D-001") &&
			strings.Contains(mB.View(), "D-001")
	}, "clients did not sync initial item")

	// Client A opens the item, types a reply draft, and leaves to flush it.
	pressTUI(t, mA, "o0hello from A\x1bu")

	// Client B sees the draft mark in the list view via watch sync.
	eventually(t, func() bool {
		if err := mB.Reload(); err != nil {
			return false
		}
		v := mB.View()
		return strings.Contains(v, "draft") && !strings.Contains(v, "draft!")
	}, "client B list did not show draft mark")

	// Opening the thread on Client B loads the draft into the reply box.
	pressTUI(t, mB, "o")
	if !strings.Contains(mB.View(), "hello from A") {
		t.Fatalf("client B detail view did not load draft:\n%s", mB.View())
	}

	// Verify both clients see the same draft in their replicas.
	if ds, err := cA.ListDrafts(); err != nil || len(ds) != 1 || ds[0].Text != "hello from A" {
		t.Fatalf("client A drafts = %+v, err=%v", ds, err)
	}
	if ds, err := cB.ListDrafts(); err != nil || len(ds) != 1 || ds[0].Text != "hello from A" {
		t.Fatalf("client B drafts = %+v, err=%v", ds, err)
	}
}

func TestTwoClientTUI_ConcurrentEditsPreserveBothAndShowConflictBanner(t *testing.T) {
	h, cA, cB, mA, mB := newTwoTUIClients(t)
	h.create(t, &decide.Item{
		Question: "concurrent edit?",
		Owner:    "ada",
		FreeText: true,
		Messages: []decide.Message{{Author: "ada", Text: "concurrent edit?"}},
	})

	eventually(t, func() bool {
		return mA.Reload() == nil && mB.Reload() == nil &&
			strings.Contains(mA.View(), "D-001") &&
			strings.Contains(mB.View(), "D-001")
	}, "clients did not sync initial item")

	// Seed an initial shared draft from Client A.
	pressTUI(t, mA, "o0shared\x1bu")
	eventually(t, func() bool {
		if err := mB.Reload(); err != nil {
			return false
		}
		return strings.Contains(mB.View(), "draft")
	}, "client B did not receive initial draft")

	// Both clients open the item at baseRev=1.
	pressTUI(t, mA, "o")
	pressTUI(t, mB, "o")

	// Client A edits and flushes first (rev 1 -> rev 2).
	pressTUI(t, mA, "0 from A\x1bu")

	// Client B still has baseRev=1, edits and flushes -> triggers conflict!
	pressTUI(t, mB, "0 from B\x1bu")

	// Both clients see draft! in the list and both versions preserved.
	eventually(t, func() bool {
		if err := mA.Reload(); err != nil {
			return false
		}
		if err := mB.Reload(); err != nil {
			return false
		}
		return strings.Contains(mA.View(), "draft!") && strings.Contains(mB.View(), "draft!")
	}, "clients did not show draft! conflict mark")

	dsA, err := cA.ListDrafts()
	if err != nil || len(dsA) != 1 || dsA[0].Text != "shared from A" || len(dsA[0].Conflicts) != 1 || dsA[0].Conflicts[0].Text != "shared from B" {
		t.Fatalf("client A conflict drafts = %+v, err=%v", dsA, err)
	}
	dsB, err := cB.ListDrafts()
	if err != nil || len(dsB) != 1 || dsB[0].Text != "shared from A" || len(dsB[0].Conflicts) != 1 || dsB[0].Conflicts[0].Text != "shared from B" {
		t.Fatalf("client B conflict drafts = %+v, err=%v", dsB, err)
	}

	// Opening the thread on Client A shows the conflict banner with Keep/Use/Merge.
	pressTUI(t, mA, "o")
	view := mA.View()
	if !strings.Contains(view, "Conflict with beta") ||
		!strings.Contains(view, "[k] Keep current") ||
		!strings.Contains(view, "[u] Use other") ||
		!strings.Contains(view, "[m] Merge both") {
		t.Fatalf("conflict banner missing:\n%s", view)
	}
}

func TestTwoClientTUI_SendAndArchiveRemoveDraftOnBothClients(t *testing.T) {
	h, cA, cB, mA, mB := newTwoTUIClients(t)
	mA.Now = func() time.Time { return time.Now().UTC().Add(10 * time.Second) }
	h.create(t, &decide.Item{
		Question: "send clears item?",
		Owner:    "ada",
		FreeText: true,
		Messages: []decide.Message{{Author: "ada", Text: "send clears item?"}},
	})
	h.create(t, &decide.Item{
		Question: "archive clears item?",
		Owner:    "ada",
		FreeText: true,
		Messages: []decide.Message{{Author: "ada", Text: "archive clears item?"}},
	})

	eventually(t, func() bool {
		return mA.Reload() == nil && mB.Reload() == nil &&
			strings.Contains(mA.View(), "D-001") &&
			strings.Contains(mA.View(), "D-002") &&
			strings.Contains(mB.View(), "D-002")
	}, "clients did not sync initial items")

	// 1. Send removes the draft on both clients.
	pressTUI(t, mA, "o0reply to send\x1bu")
	eventually(t, func() bool {
		return mB.Reload() == nil && strings.Contains(mB.View(), "draft")
	}, "client B did not see D-001 draft")

	pressTUI(t, mA, "o0")
	mA.Update(tea.KeyMsg{Type: tea.KeyCtrlS})

	eventually(t, func() bool {
		dsA, errA := cA.ListDrafts()
		dsB, errB := cB.ListDrafts()
		return errA == nil && errB == nil && len(dsA) == 0 && len(dsB) == 0
	}, "draft was not removed on both clients after send")

	if err := mB.Reload(); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(mB.View(), "draft") {
		t.Fatalf("client B still shows draft mark after send:\n%s", mB.View())
	}

	// 2. Archive removes the draft on both clients.
	pressTUI(t, mA, "uj") // back to list, move to D-002
	pressTUI(t, mA, "o0draft to archive\x1bu")
	eventually(t, func() bool {
		return mB.Reload() == nil && strings.Contains(mB.View(), "draft")
	}, "client B did not see D-002 draft")

	pressTUI(t, mA, "e") // archive D-002 from list
	eventually(t, func() bool {
		dsA, errA := cA.ListDrafts()
		dsB, errB := cB.ListDrafts()
		return errA == nil && errB == nil && len(dsA) == 0 && len(dsB) == 0
	}, "draft was not removed on both clients after archive")

	if err := mB.Reload(); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(mB.View(), "draft") {
		t.Fatalf("client B still shows draft mark after archive:\n%s", mB.View())
	}
}
