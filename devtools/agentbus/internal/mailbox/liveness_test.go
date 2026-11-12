package mailbox

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

func TestAcceptedDeliveryRefreshesListLiveness(t *testing.T) {
	const alice = "herdr:local/s1/w1:p1"
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{alice: adapter.StateIdle},
		Result:      map[string]adapter.Result{alice: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": alice},
	}
	ads := []adapter.Adapter{ad}
	reg, err := registry.Open(dir, ads, nil)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 12, 9, 0, 0, 0, time.UTC)
	reg.Now = func() time.Time { return now }
	ctx := context.Background()
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=p1"}, "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}
	box, err := Open(dir, reg, ads, nil, 0)
	if err != nil {
		t.Fatal(err)
	}
	now = now.Add(5 * time.Second)
	if _, err := box.Send(ctx, nil, 0, "ops/x", "hello", Options{}); err != nil {
		t.Fatal(err)
	}
	if ad.Calls != 1 {
		t.Fatalf("deliveries = %d", ad.Calls)
	}
	rows := reg.ListCached(ctx)
	if len(rows) != 1 || rows[0].Liveness != "alive" || !rows[0].Observed.Equal(now) {
		t.Fatalf("rows = %+v, want alive observed at %s", rows, now)
	}
}
