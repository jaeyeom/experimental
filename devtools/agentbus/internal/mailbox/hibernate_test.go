package mailbox

import (
	"context"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

func TestSendToAHibernatedNameQueuesIt(t *testing.T) {
	const alice = "herdr:local/s1/w1:p1"
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		// The pane's shell is back; herdr would take a paste.
		Reach:       map[string]adapter.State{alice: adapter.StateIdle},
		Result:      map[string]adapter.Result{alice: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": alice},
	}
	ads := []adapter.Adapter{ad}
	reg, err := registry.Open(dir, ads, nil)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=p1"}, "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.BeginHibernation(hibernate.Record{Handle: alice}); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.EndHibernation(alice, hibernate.StatusHibernated, ""); err != nil {
		t.Fatal(err)
	}
	box, err := Open(dir, reg, ads, nil, 0)
	if err != nil {
		t.Fatal(err)
	}
	res, err := box.Send(ctx, nil, 0, "ops/x", "wake up", Options{})
	if err != nil {
		t.Fatal(err)
	}
	if res.State != "held: "+HoldHibernated {
		t.Fatalf("state = %q, want held: %s", res.State, HoldHibernated)
	}
	if ad.Calls != 0 {
		t.Fatalf("pasted %d times into a hibernated pane", ad.Calls)
	}
}
