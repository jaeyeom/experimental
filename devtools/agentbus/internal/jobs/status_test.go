package jobs

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
)

func TestStatusPerMachine(t *testing.T) {
	s, c, _ := newStore(t, Config{})
	s.SetPolicy(Policy{Default: GateAuto, Kinds: map[string]Gate{"merge": GateAsk}})
	t0 := c.t
	at := t0.Add

	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Machine: "tiger"}})
	if _, err := s.Beat(Args{Runner: "tiger-runner", Machine: "tiger"}); err != nil {
		t.Fatal(err)
	}
	if r, err := s.Claim(Args{Runner: "tiger-runner", Machine: "tiger"}); err != nil || r.Job == nil {
		t.Fatalf("claim on tiger = %+v, %v", r, err)
	}
	c.t = at(time.Minute)
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#2", Needs: Needs{Machine: "tiger"}})
	c.t = at(2 * time.Minute)
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#3", Needs: Needs{Machine: "tiger"}})
	mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#4"})
	mustEnqueue(t, s, Args{Kind: "push", Subject: "o/r#5", Needs: Needs{Machine: "lion"}})
	if r, err := s.Claim(Args{Runner: "lion-runner", Machine: "lion", Kinds: []string{"push"}}); err != nil || r.Job == nil {
		t.Fatalf("claim on lion = %+v, %v", r, err)
	}
	c.t = at(10 * time.Minute)
	if _, err := s.Heartbeat(Args{ID: "j-1", Runner: "tiger-runner"}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.Expire(); err != nil {
		t.Fatal(err)
	}
	mustEnqueue(t, s, Args{Kind: "review", Subject: "o/r#6", Needs: Needs{Machine: "lion"}})
	if r, err := s.Claim(Args{Runner: "lion-runner", Machine: "lion", Kinds: []string{"review"}}); err != nil || r.Job == nil {
		t.Fatalf("claim review = %+v, %v", r, err)
	}
	c.t = at(11 * time.Minute)
	if _, err := s.Park(Args{ID: "j-6", Runner: "lion-runner", Decide: "d-1", Saved: "park/pr-6", Note: "waits on c1"}); err != nil {
		t.Fatal(err)
	}
	c.t = at(15 * time.Minute)

	got := s.Status([]LeaseView{
		{Checkout: "tiger:/w/a", State: "held", Holder: "job:j-1"},
		{Checkout: "lion:/w/b", State: "free"},
		{Checkout: "local:/w/c", State: "paused", Holder: "user"},
	})
	zero := StateCount{}
	want := StatusView{
		At: at(15 * time.Minute),
		Machines: []MachineStatus{
			{Machine: "any", Leases: []LeaseView{}, Jobs: map[State]StateCount{
				StateQueued: zero, StateRunning: zero, StateGated: {Count: 1, Oldest: at(2 * time.Minute)}, StateParked: zero, StateUnknown: zero,
			}},
			{Machine: "lion", Leases: []LeaseView{}, Jobs: map[State]StateCount{
				StateQueued: zero, StateRunning: zero, StateGated: zero,
				StateParked:  {Count: 1, Oldest: at(11 * time.Minute)},
				StateUnknown: {Count: 1, Oldest: at(10 * time.Minute)},
			}},
			{Machine: "local", Leases: []LeaseView{{Checkout: "/w/c", State: "paused", Holder: "user"}}, Jobs: map[State]StateCount{
				StateQueued: zero, StateRunning: zero, StateGated: zero, StateParked: zero, StateUnknown: zero,
			}},
			{Machine: "tiger", Leases: []LeaseView{{Checkout: "/w/a", State: "held", Holder: "job:j-1"}}, Jobs: map[State]StateCount{
				StateQueued:  {Count: 2, Oldest: at(time.Minute)},
				StateRunning: {Count: 1, Oldest: t0},
				StateGated:   zero, StateParked: zero, StateUnknown: zero,
			}},
		},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Status =\n%+v\nwant\n%+v", got, want)
	}
}

func TestStatusRowText(t *testing.T) {
	t0 := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	m := MachineStatus{
		Machine: "tiger",
		Leases:  []LeaseView{{Checkout: "/w/a", State: "held", Holder: "job:j-1"}},
		Jobs: map[State]StateCount{
			StateQueued:  {Count: 2, Oldest: t0},
			StateRunning: {Count: 1, Oldest: t0.Add(time.Minute)},
		},
	}
	row := m.Row()
	if row.Name != "jobs@tiger" || row.State != "working" {
		t.Fatalf("row = %+v", row)
	}
	if want := "queued 2 since 2026-11-16T12:00:00Z, running 1 since 2026-11-16T12:01:00Z; lease /w/a held by job:j-1"; row.Text != want {
		t.Fatalf("text = %q, want %q", row.Text, want)
	}
	m.Jobs[StateGated] = StateCount{Count: 1, Oldest: t0}
	if row := m.Row(); row.State != "blocked" {
		t.Fatalf("with a gated job, state = %q, want blocked", row.State)
	}
	idle := MachineStatus{Machine: "local", Leases: []LeaseView{{Checkout: "/w/c", State: "paused", Holder: "user"}}}
	if row := idle.Row(); row.State != "idle" || row.Text != "lease /w/c paused by user" {
		t.Fatalf("idle row = %+v", row)
	}
}

type sinkCall struct{ op, name, state, text string }

type fakeSink struct {
	calls []sinkCall
	fail  bool
}

func (f *fakeSink) Set(_ context.Context, name, state, text string) error {
	f.calls = append(f.calls, sinkCall{"set", name, state, text})
	if f.fail {
		return errors.New("decide failed")
	}
	return nil
}

func (f *fakeSink) Clear(_ context.Context, name string) error {
	f.calls = append(f.calls, sinkCall{op: "clear", name: name})
	if f.fail {
		return errors.New("decide failed")
	}
	return nil
}

func (f *fakeSink) take() []sinkCall {
	c := f.calls
	f.calls = nil
	return c
}

func TestStatusPublisherOnChangeAtMostOncePerInterval(t *testing.T) {
	t0 := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	ready := func(n int) map[State]StateCount {
		return map[State]StateCount{StateQueued: {Count: n, Oldest: t0}}
	}
	view := func(at time.Duration, ms ...MachineStatus) StatusView {
		return StatusView{At: t0.Add(at), Machines: ms}
	}
	sink := &fakeSink{}
	p := NewStatusPublisher(sink, time.Minute)
	ctx := context.Background()

	tiger1 := MachineStatus{Machine: "tiger", Jobs: ready(1)}
	lion1 := MachineStatus{Machine: "lion", Jobs: ready(1)}
	p.Publish(ctx, view(0, lion1, tiger1))
	if got, want := sink.take(), []sinkCall{
		{"set", "jobs@lion", "working", "queued 1 since 2026-11-16T12:00:00Z"},
		{"set", "jobs@tiger", "working", "queued 1 since 2026-11-16T12:00:00Z"},
	}; !reflect.DeepEqual(got, want) {
		t.Fatalf("first publish = %+v, want %+v", got, want)
	}

	p.Publish(ctx, view(20*time.Second, lion1, tiger1))
	if got := sink.take(); len(got) != 0 {
		t.Fatalf("unchanged view published %+v", got)
	}

	tiger2 := MachineStatus{Machine: "tiger", Jobs: ready(2)}
	p.Publish(ctx, view(30*time.Second, lion1, tiger2))
	if got := sink.take(); len(got) != 0 {
		t.Fatalf("a change before the interval published %+v", got)
	}
	p.Publish(ctx, view(50*time.Second, lion1, tiger2))
	if got := sink.take(); len(got) != 0 {
		t.Fatalf("a change before the interval published %+v", got)
	}
	p.Publish(ctx, view(time.Minute, lion1, tiger2))
	if got, want := sink.take(), []sinkCall{{"set", "jobs@tiger", "working", "queued 2 since 2026-11-16T12:00:00Z"}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("publish after the interval = %+v, want only the changed row %+v", got, want)
	}

	p.Publish(ctx, view(2*time.Minute, tiger2))
	if got, want := sink.take(), []sinkCall{{op: "clear", name: "jobs@lion"}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("machine gone = %+v, want %+v", got, want)
	}

	sink.fail = true
	tiger3 := MachineStatus{Machine: "tiger", Jobs: ready(3)}
	p.Publish(ctx, view(3*time.Minute, tiger3))
	if got := sink.take(); len(got) != 1 {
		t.Fatalf("refused set = %+v", got)
	}
	sink.fail = false
	p.Publish(ctx, view(3*time.Minute+30*time.Second, tiger3))
	if got := sink.take(); len(got) != 0 {
		t.Fatalf("a retry before the interval published %+v", got)
	}
	p.Publish(ctx, view(4*time.Minute, tiger3))
	if got, want := sink.take(), []sinkCall{{"set", "jobs@tiger", "working", "queued 3 since 2026-11-16T12:00:00Z"}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("retry = %+v, want %+v", got, want)
	}
}

func TestParseConfigStatusInterval(t *testing.T) {
	if got := (Config{}).StatusInterval(); got != DefaultStatusInterval {
		t.Fatalf("default status interval = %s", got)
	}
	cfg, err := ParseConfig([]byte(`{"status": {"interval": "30s"}}`))
	if err != nil {
		t.Fatal(err)
	}
	if got := cfg.StatusInterval(); got != 30*time.Second {
		t.Fatalf("status interval = %s, want 30s", got)
	}
	for _, bad := range []string{`{"status": {"interval": "soon"}}`, `{"status": {"interval": "0s"}}`, `{"status": {"every": "1m"}}`} {
		if _, err := ParseConfig([]byte(bad)); err == nil {
			t.Errorf("ParseConfig(%s) succeeded", bad)
		}
	}
}
