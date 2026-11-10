package registry

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

func TestListShowsLivenessAndDoesNotGuessNames(t *testing.T) {
	holder := "herdr:local/default/w1:p1"
	raw := "herdr:local/default/w2:p3"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holder: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": holder},
		InventoryAgents: []adapter.Agent{
			{Handle: holder, PaneID: "p1", SessionID: "default", TabLabel: "inbox", Title: "Holder", Status: "idle"},
			{Handle: raw, PaneID: "p3", SessionID: "default", TabLabel: "notes", Title: "ops/inbox", Status: "idle"},
		},
	}
	reg := openReg(t, ad)
	ctx := context.Background()
	if _, err := reg.Register(ctx, envPane("p1"), "ops/inbox", "reviews pull requests"); err != nil {
		t.Fatal(err)
	}

	listed, err := reg.List(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(listed) != 1 || listed[0].Name != "ops/inbox" || listed[0].When != "reviews pull requests" || listed[0].Liveness != "alive" || listed[0].Transport != "herdr" {
		t.Fatalf("list = %+v", listed)
	}

	byWhen, err := reg.Search(ctx, "pull requests")
	if err != nil {
		t.Fatal(err)
	}
	if len(byWhen) != 1 || byWhen[0].Name != "ops/inbox" || byWhen[0].Handle != holder {
		t.Fatalf("search when = %+v", byWhen)
	}
	byLabel, err := reg.Search(ctx, "notes")
	if err != nil {
		t.Fatal(err)
	}
	if len(byLabel) != 1 || byLabel[0].Name != "" || byLabel[0].Handle != raw {
		t.Fatalf("search label guessed a name: %+v", byLabel)
	}
	byTitle, err := reg.Search(ctx, "ops/inbox")
	if err != nil {
		t.Fatal(err)
	}
	if len(byTitle) != 2 {
		t.Fatalf("search title = %+v", byTitle)
	}
	var unregistered Row
	for _, row := range byTitle {
		if row.Handle == raw {
			unregistered = row
		}
	}
	if unregistered.Name != "" || unregistered.Handle != raw || unregistered.Title != "ops/inbox" {
		t.Fatalf("unregistered row = %+v", unregistered)
	}

	ad.Reach[holder] = adapter.StateBlocked
	listed, err = reg.List(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if listed[0].Liveness != "blocked" {
		t.Fatalf("blocked list = %+v", listed)
	}
	ad.Reach[holder] = adapter.StateGone
	listed, err = reg.List(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if listed[0].Liveness != "gone" {
		t.Fatalf("gone list = %+v", listed)
	}
}

func TestSearchRefreshesWhenTheQueryMisses(t *testing.T) {
	raw := "herdr:local/default/w2:p3"
	ad := &fake.Adapter{
		AdapterName:     "herdr",
		Reach:           map[string]adapter.State{},
		InventoryAgents: []adapter.Agent{},
	}
	reg := openReg(t, ad)
	ctx := context.Background()
	miss, err := reg.Search(ctx, "Release notes")
	if err != nil {
		t.Fatal(err)
	}
	if len(miss) != 0 {
		t.Fatalf("first search = %+v", miss)
	}
	ad.InventoryAgents = []adapter.Agent{{
		Handle: raw, PaneID: "p3", SessionID: "default", Title: "Release notes", Status: "idle",
	}}
	found, err := reg.Search(ctx, "Release notes")
	if err != nil {
		t.Fatal(err)
	}
	if len(found) != 1 || found[0].Handle != raw || found[0].Name != "" {
		t.Fatalf("refresh search = %+v", found)
	}
}

func TestPollRefreshesInventory(t *testing.T) {
	got := make(chan struct{})
	ad := &notifyAdapter{got: got}
	reg, err := Open(t.TempDir(), []adapter.Adapter{ad}, nil)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		reg.Poll(ctx, time.Hour)
		close(done)
	}()
	select {
	case <-got:
	case <-time.After(2 * time.Second):
		cancel()
		t.Fatal("poll did not refresh")
	}
	cancel()
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("poll did not stop")
	}
}

type notifyAdapter struct {
	got  chan struct{}
	once bool
}

func (a *notifyAdapter) Name() string { return "herdr" }

func (a *notifyAdapter) Inventory(context.Context) ([]adapter.Agent, error) {
	if !a.once {
		a.once = true
		close(a.got)
	}
	return nil, nil
}

func (a *notifyAdapter) Identify(context.Context, []string) (string, error) {
	return "", errors.New("no caller")
}

func (a *notifyAdapter) State(context.Context, string) (adapter.State, error) {
	return adapter.StateGone, nil
}

func (a *notifyAdapter) ComposerEmpty(context.Context, string) (bool, string, error) {
	return true, "", nil
}

func (a *notifyAdapter) Deliver(context.Context, string, string) (adapter.Result, error) {
	return adapter.ResultFailed, nil
}

func (a *notifyAdapter) SelfTest(context.Context) error { return nil }

func TestSessionReplacementListsTheNameAsGone(t *testing.T) {
	oldHandle := "herdr:local/old/p9"
	newHandle := "herdr:local/new/p9"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{oldHandle: adapter.StateIdle},
		PaneHandles: map[string]string{"old": oldHandle},
		InventoryAgents: []adapter.Agent{{
			Handle: oldHandle, PaneID: "p9", SessionID: "old", Status: "idle",
		}},
	}
	reg := openReg(t, ad)
	ctx := context.Background()
	if _, err := reg.Register(ctx, envPane("old"), "ops/inbox", "reviews"); err != nil {
		t.Fatal(err)
	}
	ad.Reach[oldHandle] = adapter.StateIdle
	ad.InventoryAgents = []adapter.Agent{{
		Handle: newHandle, PaneID: "p9", SessionID: "new", Status: "idle",
	}}
	// A missed lookup refreshes the cache. State stays idle because herdr
	// matches the pane and ignores the session.
	if _, err := reg.Search(ctx, "no such agent"); err != nil {
		t.Fatal(err)
	}
	listed, err := reg.List(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(listed) != 1 || listed[0].Liveness != "gone" || listed[0].Name != "ops/inbox" {
		t.Fatalf("list = %+v", listed)
	}
}
