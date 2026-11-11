package daemon

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

const hibAlice = "herdr:local/s1/w1:p1"

func newHibernateServer(t *testing.T) (*server, string) {
	t.Helper()
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{hibAlice: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": hibAlice},
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	c := &clock{now: time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)}
	srv, err := newServer(ctx, dir, Config{Adapters: []adapter.Adapter{ad}, Now: c.Now}, eventlog.Open(dir))
	if err != nil {
		t.Fatal(err)
	}
	if resp := srv.dispatch(Request{Op: "register", Name: "ops/x", When: "reviews", Env: []string{"HERDR_PANE_ID=p1"}}, 0); !resp.OK {
		t.Fatalf("register = %+v", resp)
	}
	return srv, dir
}

func hibernateEvents(t *testing.T, dir, name string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(dir).Read(eventlog.Filter{Event: name})
	if err != nil {
		t.Fatal(err)
	}
	var out []eventlog.Event
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
	return out
}

func TestHibernateOpsWriteTheRecordAndLog(t *testing.T) {
	srv, dir := newHibernateServer(t)
	state := srv.dispatch(Request{Op: "hibernate-state"}, 0)
	if !state.OK || state.Hibernate == nil || len(state.Hibernate.Names[hibAlice]) != 1 {
		t.Fatalf("state = %+v", state)
	}
	rec := hibernate.Record{Handle: hibAlice, Pane: "w1:p1", Kind: "claude", Session: "s1", Cwd: "/src", Argv: []string{"claude"}, RSSBytes: 300 << 20}
	begun := srv.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &rec}}, 0)
	if !begun.OK || begun.Hibernate.Record.Status != hibernate.StatusStopping || begun.Hibernate.Record.Names[0] != "ops/x" {
		t.Fatalf("begin = %+v", begun)
	}
	ended := srv.dispatch(Request{Op: "hibernate-end", Hibernate: &hibernate.Args{Handle: hibAlice, Status: hibernate.StatusHibernated}}, 0)
	if !ended.OK || ended.Hibernate.Record.Status != hibernate.StatusHibernated {
		t.Fatalf("end = %+v", ended)
	}
	state = srv.dispatch(Request{Op: "hibernate-state"}, 0)
	if len(state.Hibernate.Records) != 1 || state.Hibernate.Records[0].Session != "s1" {
		t.Fatalf("records = %+v", state.Hibernate.Records)
	}
	evs := hibernateEvents(t, dir, eventlog.EventHibernated)
	if len(evs) != 1 || evs[0].Name != "ops/x" || !strings.Contains(evs[0].Detail, "300.0 MiB") {
		t.Fatalf("agent_hibernated events = %+v", evs)
	}
	refused := srv.dispatch(Request{Op: "hibernate-refused", Hibernate: &hibernate.Args{Handle: hibAlice, Reasons: []string{"agent is working"}}}, 0)
	if !refused.OK {
		t.Fatalf("refused = %+v", refused)
	}
	evs = hibernateEvents(t, dir, eventlog.EventHibernateRefused)
	if len(evs) != 1 || evs[0].Name != "ops/x" || evs[0].Reason != "agent is working" {
		t.Fatalf("agent_hibernate_refused events = %+v", evs)
	}
}

func TestHibernateStopFailedIsLoggedAsARefusal(t *testing.T) {
	srv, dir := newHibernateServer(t)
	srv.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &hibernate.Record{Handle: hibAlice}}}, 0)
	resp := srv.dispatch(Request{Op: "hibernate-end", Hibernate: &hibernate.Args{Handle: hibAlice, Status: hibernate.StatusStopFailed, Reasons: []string{"did not exit within 20s"}}}, 0)
	if !resp.OK || resp.Hibernate.Record.Status != hibernate.StatusStopFailed || resp.Hibernate.Record.Reason != "did not exit within 20s" {
		t.Fatalf("end = %+v", resp)
	}
	evs := hibernateEvents(t, dir, eventlog.EventHibernateRefused)
	if len(evs) != 1 || evs[0].Detail != hibernate.StatusStopFailed {
		t.Fatalf("events = %+v", evs)
	}
}

func TestHibernateBeginRefusesALeaseHolder(t *testing.T) {
	srv, _ := newHibernateServer(t)
	if _, err := srv.leases.Claim(hibAlice, lease.Args{Checkout: "local:/src", For: "40m"}); err != nil {
		t.Fatal(err)
	}
	resp := srv.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &hibernate.Record{Handle: hibAlice}}}, 0)
	if resp.OK || !strings.Contains(resp.Error, "holds the lease on local:/src") {
		t.Fatalf("begin = %+v", resp)
	}
	if recs := srv.reg.Hibernations(); len(recs) != 0 {
		t.Fatalf("records = %+v", recs)
	}
}

func TestHibernateFromAnotherMachineOnlyForItsOwnAgents(t *testing.T) {
	srv, _ := newHibernateServer(t)
	srv.hubCaller = herdr.RemoteCaller
	scoped, err := srv.forwarded(Request{Protocol: ProtocolVersion, Handle: "herdr:mac/s9/w2:p1"})
	if err != nil {
		t.Fatal(err)
	}
	for _, handle := range []string{hibAlice, "herdr:linux2/s1/w1:p1"} {
		resp := scoped.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &hibernate.Record{Handle: handle}}}, 0)
		if resp.OK || !strings.Contains(resp.Error, "machine mac") {
			t.Errorf("begin of %s from mac = %+v, want a machine refusal", handle, resp)
		}
	}
	own := "herdr:mac/s1/w1:p1"
	if resp := scoped.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &hibernate.Record{Handle: own}}}, 0); !resp.OK {
		t.Fatalf("begin of its own agent = %+v", resp)
	}
	if resp := scoped.dispatch(Request{Op: "hibernate-state"}, 0); !resp.OK {
		t.Fatalf("state from mac = %+v", resp)
	}
}

// recordingWaker wakes every agent and records the records it got.
type recordingWaker struct{ recs []hibernate.Record }

func (w *recordingWaker) Wake(_ context.Context, rec hibernate.Record) error {
	w.recs = append(w.recs, rec)
	return nil
}

func TestWakeOpRelaunchesAndClearsTheRecord(t *testing.T) {
	srv, dir := newHibernateServer(t)
	waker := &recordingWaker{}
	srv.box.SetWaker(waker)
	srv.dispatch(Request{Op: "hibernate-begin", Hibernate: &hibernate.Args{Record: &hibernate.Record{Handle: hibAlice, Kind: "claude", Session: "s1"}}}, 0)
	srv.dispatch(Request{Op: "hibernate-end", Hibernate: &hibernate.Args{Handle: hibAlice, Status: hibernate.StatusHibernated}}, 0)
	resp := srv.dispatch(Request{Op: "wake", Name: "ops/x"}, 0)
	if !resp.OK || resp.Handle != hibAlice || resp.Name != "ops/x" {
		t.Fatalf("wake = %+v", resp)
	}
	if len(waker.recs) != 1 || waker.recs[0].Session != "s1" {
		t.Fatalf("woke %+v", waker.recs)
	}
	if recs := srv.reg.Hibernations(); len(recs) != 0 {
		t.Fatalf("records after the wake = %+v", recs)
	}
	if evs := hibernateEvents(t, dir, eventlog.EventWake); len(evs) != 1 || evs[0].Name != "ops/x" {
		t.Fatalf("agent_wake events = %+v", evs)
	}
	if resp := srv.dispatch(Request{Op: "wake", Name: "ops/x"}, 0); resp.OK || !strings.Contains(resp.Error, "not hibernated") {
		t.Fatalf("second wake = %+v", resp)
	}
}
