package hibernate

import (
	"context"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

const gib = 1024 * mib

// fakeMemory is the available memory. The fake hibernator frees an
// agent's RSS into it.
type fakeMemory struct {
	avail int64
	reads int
}

func (m *fakeMemory) Available(context.Context) (int64, error) {
	m.reads++
	return m.avail, nil
}

// fakeHibernator stops the targets it is asked for. Agents in refuse
// fail a check.
type fakeHibernator struct {
	mem    *fakeMemory
	rss    map[string]int64
	names  map[string]string
	refuse map[string]string
	calls  []Request
}

func (f *fakeHibernator) Run(_ context.Context, req Request) ([]Result, error) {
	f.calls = append(f.calls, req)
	var out []Result
	for _, handle := range req.Targets {
		r := Result{Name: f.names[handle], Handle: handle, RSSBytes: f.rss[handle]}
		switch {
		case f.refuse[handle] != "":
			r.Outcome, r.Reasons = OutcomeRefused, []string{f.refuse[handle]}
		case req.DryRun:
			r.Outcome = OutcomeWould
		default:
			r.Outcome = OutcomeHibernated
			f.mem.avail += r.RSSBytes
		}
		out = append(out, r)
	}
	return out, nil
}

// targets are the handles Run was asked for, in order.
func (f *fakeHibernator) targets() []string {
	var out []string
	for _, req := range f.calls {
		out = append(out, req.Targets...)
	}
	return out
}

type stateBus struct{ state Reply }

func (b *stateBus) State(context.Context) (Reply, error) { return b.state, nil }

const (
	carol = "herdr:local/s3/w1:p3"
	dave  = "herdr:local/s4/w1:p4"
	remo  = "herdr:linux2/s5/w1:p1"
)

// policyWorld has three idle local agents, alice idle longest, then
// carol, then bob, all past the minimum idle time, each holding 1 GiB.
func policyWorld(mode string, avail int64) (*Policy, *fakeHibernator, *stateBus, *[]eventlog.Event) {
	mem := &fakeMemory{avail: avail}
	h := &fakeHibernator{
		mem:   mem,
		rss:   map[string]int64{alice: gib, bob: gib, carol: gib, dave: gib, remo: gib},
		names: map[string]string{alice: "ops/a", bob: "ops/b", carol: "ops/c", dave: "ops/d", remo: "ops/r"},
	}
	bus := &stateBus{state: Reply{Seen: map[string]Seen{
		alice: {Status: "idle", For: 3 * time.Hour},
		bob:   {Status: "done", For: time.Hour},
		carol: {Status: "idle", For: 2 * time.Hour},
	}}}
	var events []eventlog.Event
	p := &Policy{
		Mode: mode, Below: 3 * gib / 2, Target: 3 * gib, MinIdle: 30 * time.Minute,
		Memory: mem, Bus: bus, Hibernator: h,
		Log: func(ev eventlog.Event) error {
			events = append(events, ev)
			return nil
		},
	}
	return p, h, bus, &events
}

func eventNames(events []eventlog.Event) []string {
	out := make([]string, len(events))
	for i, ev := range events {
		out[i] = ev.Event + " " + ev.Name
	}
	return out
}

func TestPolicyHibernatesLongestIdleFirstUntilTheTarget(t *testing.T) {
	p, h, _, events := policyWorld(PolicyOn, gib)
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got, want := h.targets(), []string{alice, carol}; !slices.Equal(got, want) {
		t.Fatalf("hibernated %v, want %v: one at a time, longest idle first, until 3 GiB", got, want)
	}
	for _, req := range h.calls {
		if req.DryRun || len(req.Targets) != 1 {
			t.Fatalf("request = %+v, want one target, not a dry run", req)
		}
	}
	want := []string{eventlog.EventPolicyHibernate + " ops/a", eventlog.EventPolicyHibernate + " ops/c"}
	if got := eventNames(*events); !slices.Equal(got, want) {
		t.Fatalf("events = %v, want %v", got, want)
	}
	ev := (*events)[0]
	for _, part := range []string{"idle 3h0m0s", "1.0 GiB", "1.0 GiB available before"} {
		if !strings.Contains(ev.Detail, part) {
			t.Errorf("detail %q lacks %q", ev.Detail, part)
		}
	}
	if (*events)[1].Detail == ev.Detail || !strings.Contains((*events)[1].Detail, "2.0 GiB available before") {
		t.Errorf("second detail %q, want memory re-read after the first", (*events)[1].Detail)
	}
	if ev.Actor == "" {
		t.Error("event has no actor")
	}
}

func TestPolicyDoesNothingWithEnoughMemory(t *testing.T) {
	p, h, _, events := policyWorld(PolicyOn, 2*gib)
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if len(h.calls) != 0 || len(*events) != 0 {
		t.Fatalf("calls %v, events %v; want none above hibernate_below", h.calls, *events)
	}
}

func TestPolicySkipsExemptIneligibleAndOtherAgents(t *testing.T) {
	p, h, bus, events := policyWorld(PolicyOn, gib)
	h.refuse = map[string]string{alice: `exempt by hibernate_exempt "ops/*"`, carol: "unsent text in the composer"}
	bus.state.Seen[dave] = Seen{Status: "idle", For: 10 * time.Minute}
	bus.state.Seen[remo] = Seen{Status: "idle", For: 5 * time.Hour}
	bus.state.Seen["herdr:local/s6/w1:p6"] = Seen{Status: "working", For: 5 * time.Hour}
	bus.state.Seen["herdr:local/s7/w1:p7"] = Seen{Status: "idle", For: 4 * time.Hour}
	bus.state.Records = []Record{{Handle: "herdr:local/s7/w1:p7", Status: StatusStopFailed}}
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got, want := h.targets(), []string{alice, carol, bob}; !slices.Equal(got, want) {
		t.Fatalf("asked %v, want %v: not too young, not another machine's, not working, not with a record", got, want)
	}
	if got, want := eventNames(*events), []string{eventlog.EventPolicyHibernate + " ops/b"}; !slices.Equal(got, want) {
		t.Fatalf("events = %v, want %v", got, want)
	}
}

func TestPolicyDryRunOnlyLogs(t *testing.T) {
	p, h, _, events := policyWorld(PolicyDryRun, gib)
	for range 2 {
		if err := p.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
	}
	for _, req := range h.calls {
		if !req.DryRun {
			t.Fatalf("request %+v is not a dry run", req)
		}
	}
	if h.mem.avail != gib {
		t.Fatalf("available = %d; a dry run freed memory", h.mem.avail)
	}
	want := []string{eventlog.EventPolicyWouldHibernate + " ops/a", eventlog.EventPolicyWouldHibernate + " ops/c"}
	if got := eventNames(*events); !slices.Equal(got, want) {
		t.Fatalf("events = %v, want %v: counted as freed, once per low episode", got, want)
	}
}

func TestPolicyOffDoesNothing(t *testing.T) {
	for _, mode := range []string{PolicyOff, ""} {
		p, h, _, events := policyWorld(mode, 0)
		if err := p.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
		if len(h.calls) != 0 || len(*events) != 0 || h.mem.reads != 0 {
			t.Fatalf("mode %q: calls %v, events %v, reads %d", mode, h.calls, *events, h.mem.reads)
		}
	}
}

func TestPolicyLogsNoCandidateOncePerLowEpisode(t *testing.T) {
	p, h, bus, events := policyWorld(PolicyOn, gib)
	bus.state.Seen = map[string]Seen{alice: {Status: "working", For: 5 * time.Hour}}
	tick := func() {
		t.Helper()
		if err := p.Tick(context.Background()); err != nil {
			t.Fatal(err)
		}
	}
	tick()
	tick()
	h.mem.avail = 2 * gib
	tick()
	h.mem.avail = gib
	tick()
	tick()
	want := []string{eventlog.EventPolicyNoCandidate + " ", eventlog.EventPolicyNoCandidate + " "}
	if got := eventNames(*events); !slices.Equal(got, want) {
		t.Fatalf("events = %v, want one per low episode", got)
	}
	if d := (*events)[0].Detail; !strings.Contains(d, "1.0 GiB available") {
		t.Fatalf("detail = %q", d)
	}
}

func TestPolicyDoesNotRehibernateAWokenAgentUntilIdleAgain(t *testing.T) {
	p, h, bus, _ := policyWorld(PolicyOn, gib)
	bus.state.Seen = map[string]Seen{alice: {Status: "idle", For: 3 * time.Hour}}
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	// alice woke on a message, answered it, and is idle again: the
	// daemon's idle clock started over.
	h.mem.avail = gib
	bus.state.Seen[alice] = Seen{Status: "idle", For: 10 * time.Minute}
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := h.targets(); !slices.Equal(got, []string{alice}) {
		t.Fatalf("asked %v, want alice only once before she is idle 30m again", got)
	}
	bus.state.Seen[alice] = Seen{Status: "idle", For: 30 * time.Minute}
	if err := p.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := h.targets(); !slices.Equal(got, []string{alice, alice}) {
		t.Fatalf("asked %v, want alice again once idle 30m", got)
	}
}

func TestPolicyWatchTicks(t *testing.T) {
	p, h, _, _ := policyWorld(PolicyOn, gib)
	ticks := make(chan time.Time)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		p.Watch(ctx, ticks)
	}()
	ticks <- time.Time{}
	ticks <- time.Time{}
	cancel()
	<-done
	if len(h.calls) == 0 {
		t.Fatal("Watch never ran the policy")
	}
}
