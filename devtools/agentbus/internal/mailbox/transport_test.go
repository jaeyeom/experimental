package mailbox

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

// TestHeldWhileHerdrIsDownDrainsWhenItComesUp is a daemon that started
// before the herdr server. The message sent meanwhile is held, and is
// delivered once herdr answers, with the same mailbox.
func TestHeldWhileHerdrIsDownDrainsWhenItComesUp(t *testing.T) {
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holdTo: adapter.StateIdle, holdFrom: adapter.StateIdle},
		Result:      map[string]adapter.Result{holdTo: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": holdTo, "p2": holdFrom},
	}
	ads := []adapter.Adapter{ad}
	first, err := registry.Open(dir, ads, nil)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := first.Register(ctx, []string{"HERDR_PANE_ID=p1"}, "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}

	ad.SelfTestErr = errors.New("herdr agent list: no herdr server is running at /run/user/1000/herdr.sock")
	lg := eventlog.Open(dir)
	reg, err := registry.Open(dir, ads, lg)
	if err != nil {
		t.Fatal(err)
	}
	clock := &fakeNow{t: time.Date(2026, 11, 14, 8, 0, 0, 0, time.UTC)}
	reg.Now = clock.now
	box, err := Open(dir, reg, ads, lg, 0)
	if err != nil {
		t.Fatal(err)
	}
	box.now = clock.now

	res, err := box.Send(ctx, []string{"HERDR_PANE_ID=p2"}, 0, "ops/x", "hello", Options{})
	if err != nil {
		t.Fatal(err)
	}
	if res.State == stateDelivered || len(ad.Texts) != 0 {
		t.Fatalf("sent while herdr is down: state %q texts %q", res.State, ad.Texts)
	}

	ad.SelfTestErr = nil
	clock.add(30 * time.Second)
	if err := box.flushHandle(ctx, holdTo); err != nil {
		t.Fatal(err)
	}
	if len(ad.Texts) != 1 {
		t.Fatalf("texts after herdr came up = %q", ad.Texts)
	}
	msg, err := box.get(res.ID)
	if err != nil {
		t.Fatal(err)
	}
	if msg.State != stateDelivered {
		t.Fatalf("state = %q", msg.State)
	}
}
