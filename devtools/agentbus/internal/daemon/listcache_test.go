package daemon

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func TestListAnswersFromTheCacheUnlessFresh(t *testing.T) {
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{hibAlice: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": hibAlice},
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	c := &clock{now: time.Date(2026, 11, 12, 9, 0, 0, 0, time.UTC)}
	srv, err := newServer(ctx, dir, Config{
		Adapters: []adapter.Adapter{ad}, Now: c.Now, ListLivenessMaxAge: time.Minute,
	}, eventlog.Open(dir))
	if err != nil {
		t.Fatal(err)
	}
	if resp := srv.dispatch(Request{Op: "register", Name: "ops/x", When: "reviews", Env: []string{"HERDR_PANE_ID=p1"}}, 0); !resp.OK {
		t.Fatalf("register = %+v", resp)
	}
	listOne := func(fresh bool) AgentRow {
		t.Helper()
		resp := srv.dispatch(Request{Op: "list", Fresh: fresh}, 0)
		if !resp.OK || len(resp.Rows) != 1 || resp.Rows[0].LivenessAge == nil {
			t.Fatalf("list fresh=%t = %+v", fresh, resp)
		}
		return resp.Rows[0]
	}

	if row := listOne(true); row.Liveness != "alive" || *row.LivenessAge != 0 {
		t.Fatalf("fresh list = %+v", row)
	}
	ad.Reach[hibAlice] = adapter.StateGone
	c.Set(c.Now().Add(5 * time.Second))
	if row := listOne(false); row.Liveness != "alive" || *row.LivenessAge != 5 {
		t.Fatalf("cached list = %+v age %d, want alive 5s old", row, *row.LivenessAge)
	}
	if row := listOne(true); row.Liveness != "gone" || *row.LivenessAge != 0 {
		t.Fatalf("fresh list after gone = %+v", row)
	}
}

func TestListRowLivenessAgeOnTheWire(t *testing.T) {
	age := int64(0)
	data, err := json.Marshal(AgentRow{Name: "ops/x", LivenessAge: &age})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), `"liveness_age":0`) {
		t.Fatalf("row = %s", data)
	}
	data, err = json.Marshal(Request{Op: "list"})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(data), "fresh") {
		t.Fatalf("a plain list carries fresh: %s", data)
	}
}
