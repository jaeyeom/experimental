package registry

import (
	"context"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

func TestLivenessClaimRules(t *testing.T) {
	holder := "herdr:local/default/w1:p1"
	other := "herdr:local/default/w1:p2"
	tests := []struct {
		name    string
		state   adapter.State
		wantErr bool
	}{
		{name: "idle", state: adapter.StateIdle, wantErr: true},
		{name: "working", state: adapter.StateWorking, wantErr: true},
		{name: "blocked", state: adapter.StateBlocked, wantErr: true},
		{name: "done", state: adapter.StateDone, wantErr: true},
		{name: "unknown", state: adapter.StateUnknown, wantErr: true},
		{name: "gone", state: adapter.StateGone},
		{name: "no agent", state: adapter.StateNoAgent},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ad := &fake.Adapter{
				AdapterName: "herdr",
				Reach: map[string]adapter.State{
					holder: tc.state,
					other:  adapter.StateIdle,
				},
				PaneHandles: map[string]string{"p1": holder, "p2": other},
			}
			reg := openReg(t, ad)
			if _, err := reg.Register(context.Background(), envPane("p1"), "ops/inbox", "reviews"); err != nil {
				t.Fatal(err)
			}
			_, err := reg.Register(context.Background(), envPane("p2"), "ops/inbox", "steal")
			if tc.wantErr {
				if err == nil {
					t.Fatal("claim succeeded")
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			_, got, err := reg.WhoAmI(context.Background(), envPane("p2"))
			if err != nil {
				t.Fatal(err)
			}
			if len(got) != 1 || got[0].Name != "ops/inbox" {
				t.Fatalf("whoami = %+v", got)
			}
		})
	}
}

func TestSessionReplacementLetsTheNewSessionClaim(t *testing.T) {
	oldHandle := "herdr:local/old/p9"
	newHandle := "herdr:local/new/p9"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{oldHandle: adapter.StateIdle, newHandle: adapter.StateIdle},
		PaneHandles: map[string]string{"old": oldHandle, "new": newHandle},
		InventoryAgents: []adapter.Agent{{
			Handle: oldHandle, PaneID: "p9", SessionID: "old", Status: "idle",
		}},
	}
	reg := openReg(t, ad)
	if _, err := reg.Register(context.Background(), envPane("old"), "ops/inbox", "reviews"); err != nil {
		t.Fatal(err)
	}
	// herdr State matches the pane and ignores the session, so the old handle
	// still looks idle. Inventory shows a different session on that pane.
	ad.Reach[oldHandle] = adapter.StateIdle
	ad.InventoryAgents = []adapter.Agent{{
		Handle: newHandle, PaneID: "p9", SessionID: "new", Status: "idle",
	}}
	if _, err := reg.Register(context.Background(), envPane("new"), "ops/inbox", "again"); err != nil {
		t.Fatal(err)
	}
	_, got, err := reg.WhoAmI(context.Background(), envPane("new"))
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Handle != newHandle {
		t.Fatalf("whoami = %+v", got)
	}
}

func TestHandoffLeavesNoGapForAThirdAgent(t *testing.T) {
	holder := "herdr:local/default/w1:p1"
	claimer := "herdr:local/default/w1:p2"
	third := "herdr:local/default/w1:p3"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holder:  adapter.StateIdle,
			claimer: adapter.StateIdle,
			third:   adapter.StateIdle,
		},
		PaneHandles: map[string]string{"p1": holder, "p2": claimer, "p3": third},
	}
	reg := openReg(t, ad)
	ctx := context.Background()
	if _, err := reg.Register(ctx, envPane("p1"), "ops/inbox", "reviews"); err != nil {
		t.Fatal(err)
	}
	var wg sync.WaitGroup
	wg.Add(2)
	var releaseErr error
	thirdWon := false
	var mu sync.Mutex
	go func() {
		defer wg.Done()
		_, releaseErr = reg.Release(ctx, envPane("p1"), "ops/inbox", claimer)
	}()
	go func() {
		defer wg.Done()
		_, err := reg.Register(ctx, envPane("p3"), "ops/inbox", "steal")
		if err == nil {
			mu.Lock()
			thirdWon = true
			mu.Unlock()
		}
	}()
	wg.Wait()
	if releaseErr != nil {
		t.Fatal(releaseErr)
	}
	if thirdWon {
		t.Fatal("third agent took the name")
	}
	_, got, err := reg.WhoAmI(ctx, envPane("p2"))
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Handle != claimer {
		t.Fatalf("whoami = %+v", got)
	}
}

func TestHumanAndNonHerdrCannotRegister(t *testing.T) {
	holder := "herdr:local/default/w1:p1"
	anon := "anon:host-1"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holder: adapter.StateIdle, anon: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": holder, "anon": anon},
	}
	reg := openReg(t, ad)
	if _, err := reg.Register(context.Background(), envPane("p1"), "human/ops", "x"); err == nil || !strings.Contains(err.Error(), "human:* is reserved") {
		t.Fatalf("human name err = %v", err)
	}
	if _, err := reg.Register(context.Background(), envPane("anon"), "ops/inbox", "x"); err == nil || !strings.Contains(err.Error(), "outside herdr") {
		t.Fatalf("anon err = %v", err)
	}
	if _, err := reg.Register(context.Background(), nil, "ops/inbox", "x"); err == nil || !strings.Contains(err.Error(), "cannot register") {
		t.Fatalf("missing env err = %v", err)
	}
}

func openReg(t *testing.T, ad *fake.Adapter) *Registry {
	t.Helper()
	reg, err := Open(t.TempDir(), []adapter.Adapter{ad}, nil)
	if err != nil {
		t.Fatal(err)
	}
	return reg
}

func envPane(pane string) []string {
	return []string{"HERDR_PANE_ID=" + pane}
}
