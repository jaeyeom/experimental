package registry

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
)

// hibernateWorld has alice holding ops/x in w1:p1 and bob in w1:p2.
func hibernateWorld(t *testing.T) (*Registry, *fake.Adapter, string, string) {
	t.Helper()
	alice := "herdr:local/s1/w1:p1"
	bob := "herdr:local/s2/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{alice: adapter.StateIdle, bob: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": alice, "p2": bob},
	}
	reg := openReg(t, ad)
	if _, err := reg.Register(context.Background(), envPane("p1"), "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}
	return reg, ad, alice, bob
}

// hibernateAlice records alice hibernated and takes her agent out of
// the pane, which herdr then reports as running no agent.
func hibernateAlice(t *testing.T, reg *Registry, ad *fake.Adapter, alice string) {
	t.Helper()
	rec, err := reg.BeginHibernation(hibernate.Record{Handle: alice, Pane: "w1:p1", Kind: "claude", Session: "s1"})
	if err != nil {
		t.Fatal(err)
	}
	if rec.Status != hibernate.StatusStopping || len(rec.Names) != 1 || rec.Names[0] != "ops/x" {
		t.Fatalf("begun record = %+v", rec)
	}
	if _, err := reg.EndHibernation(alice, hibernate.StatusHibernated, ""); err != nil {
		t.Fatal(err)
	}
	ad.Reach[alice] = adapter.StateNoAgent
}

func TestAHibernatedNameIsKeptAndCannotBeClaimed(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	hibernateAlice(t, reg, ad, alice)
	_, err := reg.Register(context.Background(), envPane("p2"), "ops/x", "steal")
	if err == nil || !strings.Contains(err.Error(), "hibernated") {
		t.Fatalf("claim over a hibernated name = %v, want a refusal naming hibernated", err)
	}
	if got := reg.NamesOf(alice); len(got) != 1 || got[0] != "ops/x" {
		t.Fatalf("names of alice = %q", got)
	}
	rows, err := reg.List(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 || rows[0].Liveness != "hibernated" {
		t.Fatalf("list = %+v", rows)
	}
	if !reg.Hibernated(alice) {
		t.Fatal("Hibernated(alice) = false")
	}
}

func TestAStoppingRecordAlsoKeepsTheName(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	if _, err := reg.BeginHibernation(hibernate.Record{Handle: alice}); err != nil {
		t.Fatal(err)
	}
	ad.Reach[alice] = adapter.StateNoAgent
	if _, err := reg.Register(context.Background(), envPane("p2"), "ops/x", "steal"); err == nil {
		t.Fatal("claim over a stopping agent's name succeeded")
	}
	if reg.Hibernated(alice) {
		t.Fatal("a stopping agent counts as hibernated")
	}
}

func TestSearchShowsAHibernatedAgentOutOfTheInventory(t *testing.T) {
	reg, ad, alice, bob := hibernateWorld(t)
	hibernateAlice(t, reg, ad, alice)
	ad.InventoryAgents = []adapter.Agent{{Handle: bob, PaneID: "w1:p2", Status: "idle"}}
	rows, err := reg.Search(context.Background(), "ops")
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 || rows[0].Handle != alice || rows[0].Name != "ops/x" || rows[0].Liveness != "hibernated" {
		t.Fatalf("search = %+v", rows)
	}
}

func TestHibernationRecordsSurviveARestart(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	hibernateAlice(t, reg, ad, alice)
	again, err := Open(reg.dir, []adapter.Adapter{ad}, nil)
	if err != nil {
		t.Fatal(err)
	}
	recs := again.Hibernations()
	if len(recs) != 1 || recs[0].Handle != alice || recs[0].Status != hibernate.StatusHibernated || recs[0].Session != "s1" {
		t.Fatalf("records after restart = %+v", recs)
	}
}

func TestAFailedWakeKeepsTheRecordAndTheName(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	hibernateAlice(t, reg, ad, alice)
	retry := time.Date(2026, 11, 11, 9, 1, 0, 0, time.UTC)
	rec, err := reg.FailWake(alice, "relaunched agent not ready within 1m0s", retry)
	if err != nil {
		t.Fatal(err)
	}
	if rec.Status != hibernate.StatusWakeFailed || rec.Failures != 1 || !rec.RetryAt.Equal(retry) || !strings.Contains(rec.Reason, "not ready") {
		t.Fatalf("record = %+v", rec)
	}
	if rec, _ = reg.FailWake(alice, "again", retry); rec.Failures != 2 {
		t.Fatalf("second failure = %+v, want 2 failures", rec)
	}
	if !reg.Hibernated(alice) {
		t.Fatal("a wake-failed agent does not count as hibernated")
	}
	if got, ok := reg.Hibernation(alice); !ok || got.Status != hibernate.StatusWakeFailed {
		t.Fatalf("Hibernation(alice) = %+v, %v", got, ok)
	}
	if _, err := reg.Register(context.Background(), envPane("p2"), "ops/x", "steal"); err == nil {
		t.Fatal("claim over a wake-failed agent's name succeeded")
	}
}

func TestAWakeClearsTheRecordAndTheNameIsActiveAgain(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	hibernateAlice(t, reg, ad, alice)
	if err := reg.ClearHibernation(alice); err != nil {
		t.Fatal(err)
	}
	ad.Reach[alice] = adapter.StateIdle
	if reg.Hibernated(alice) || len(reg.Hibernations()) != 0 {
		t.Fatalf("records after the wake = %+v", reg.Hibernations())
	}
	rows, err := reg.List(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 || rows[0].Name != "ops/x" || rows[0].Liveness != "alive" {
		t.Fatalf("list = %+v", rows)
	}
	if _, err := reg.FailWake(alice, "x", time.Time{}); err == nil {
		t.Fatal("FailWake without a record succeeded")
	}
}

func TestEndWithoutARecordFails(t *testing.T) {
	reg, _, alice, _ := hibernateWorld(t)
	if _, err := reg.EndHibernation(alice, hibernate.StatusHibernated, ""); err == nil {
		t.Fatal("EndHibernation without a record succeeded")
	}
}

func TestSeenTracksHowLongAnAgentHasBeenInItsState(t *testing.T) {
	reg, ad, alice, _ := hibernateWorld(t)
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	reg.Now = func() time.Time { return now }
	poll := func(status string, seq int64, after time.Duration) hibernate.Seen {
		t.Helper()
		now = now.Add(after)
		ad.InventoryAgents = []adapter.Agent{{Handle: alice, PaneID: "w1:p1", Status: status, StateSeq: seq}}
		reg.pollOnce(context.Background())
		return reg.Seen()[alice]
	}
	if got := poll("idle", 1, 0); got.Status != "idle" || got.For != 0 {
		t.Fatalf("first sight = %+v", got)
	}
	if got := poll("idle", 1, time.Hour); got.For != time.Hour {
		t.Fatalf("after an hour = %+v", got)
	}
	if got := poll("idle", 3, time.Minute); got.For != 0 {
		t.Fatalf("a turn between polls = %+v, want reset", got)
	}
	if got := poll("done", 4, time.Hour); got.For != 0 {
		t.Fatalf("a finished turn = %+v, want reset", got)
	}
	if got := poll("idle", 5, time.Hour); got.Status != "idle" || got.For != time.Hour {
		t.Fatalf("done to idle = %+v, want the idle time kept", got)
	}
	if got := poll("working", 6, time.Minute); got.Status != "working" || got.For != 0 {
		t.Fatalf("working = %+v", got)
	}
	ad.InventoryAgents = []adapter.Agent{}
	reg.pollOnce(context.Background())
	if _, ok := reg.Seen()[alice]; ok {
		t.Fatal("an agent gone from the inventory is still seen")
	}
}
