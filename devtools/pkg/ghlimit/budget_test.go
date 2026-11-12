package ghlimit

import (
	"context"
	"strings"
	"testing"
	"time"
)

func TestPointsBudgetWaitsForWindow(t *testing.T) {
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	l.Config.PointsPerMin = 10
	var starts []time.Time
	run := func(context.Context) (Result, error) {
		starts = append(starts, clock.Now())
		return Result{}, nil
	}
	for range 11 {
		if _, err := l.Run(context.Background(), strings.Fields("api repos/o/r"), run); err != nil {
			t.Fatal(err)
		}
	}
	if gap := starts[9].Sub(starts[0]); gap >= time.Second {
		t.Fatalf("the 10th call waited %v; it fits the budget", gap)
	}
	if gap := starts[10].Sub(starts[0]); gap < time.Minute || gap > time.Minute+time.Second {
		t.Fatalf("the 11th call started %v after the first, want about 1m", gap)
	}
	entries := sink.Entries()
	if len(entries) != 1 || entries[0].Reason != ReasonPointsBudget || entries[0].WaitMS < 59000 {
		t.Fatalf("log = %+v, want one points_budget wait", entries)
	}
}

func TestPointsBudgetIsSharedAcrossLimiters(t *testing.T) {
	clock := newFakeClock()
	a, _ := newLimiter(t, clock)
	a.Config.PointsPerMin = 5
	b := &Limiter{Dir: a.Dir, Config: a.Config, Now: clock.Now, Sleep: clock.Sleep}
	run := func(context.Context) (Result, error) { return Result{}, nil }
	if _, err := a.Run(context.Background(), strings.Fields("pr merge 1"), run); err != nil {
		t.Fatal(err)
	}
	before := clock.Now()
	if _, err := b.Run(context.Background(), strings.Fields("pr view 1"), run); err != nil {
		t.Fatal(err)
	}
	if waited := clock.Now().Sub(before); waited < 59*time.Second {
		t.Fatalf("second limiter waited %v after a 5-point mutation, want about 1m", waited)
	}
}

func TestGraphQLTimeBudget(t *testing.T) {
	clock := newFakeClock()
	l, sink := newLimiter(t, clock)
	l.Config.GraphQLTimePerMin = 2 * time.Second
	var starts []time.Time
	run := func(context.Context) (Result, error) {
		starts = append(starts, clock.Now())
		clock.Sleep(1500 * time.Millisecond)
		return Result{}, nil
	}
	args := []string{"api", "graphql", "-f", "query={ viewer { login } }"}
	for range 2 {
		if _, err := l.Run(context.Background(), args, run); err != nil {
			t.Fatal(err)
		}
	}
	if gap := starts[1].Sub(starts[0]); gap < time.Minute {
		t.Fatalf("second GraphQL call started %v after the first, want it to wait for the window", gap)
	}
	entries := sink.Entries()
	if len(entries) != 1 || entries[0].Reason != ReasonGraphQLTimeBudget {
		t.Fatalf("log = %+v, want one graphql_time_budget wait", entries)
	}

	// REST calls do not count against the GraphQL time budget.
	before := clock.Now()
	for range 3 {
		if _, err := l.Run(context.Background(), strings.Fields("api repos/o/r"), run); err != nil {
			t.Fatal(err)
		}
	}
	if took := clock.Now().Sub(before); took != 3*1500*time.Millisecond {
		t.Fatalf("REST calls took %v, want no budget wait", took)
	}
}

func TestZeroDisablesBudgets(t *testing.T) {
	clock := newFakeClock()
	l, _ := newLimiter(t, clock)
	l.Config.PointsPerMin = 0
	l.Config.GraphQLTimePerMin = 0
	run := func(context.Context) (Result, error) {
		clock.Sleep(10 * time.Second)
		return Result{}, nil
	}
	for range 3000 / 100 {
		if _, err := l.Run(context.Background(), strings.Fields("pr view 1"), run); err != nil {
			t.Fatal(err)
		}
	}
	for _, d := range clock.slept {
		if d != 10*time.Second {
			t.Fatalf("slept %v outside the calls; budgets are off", d)
		}
	}
}

func TestBudgetConfigFromEnv(t *testing.T) {
	cfg, err := ConfigFromEnv(func(string) string { return "" })
	if err != nil || cfg.PointsPerMin != 1500 || cfg.GraphQLTimePerMin != 40*time.Second {
		t.Fatalf("defaults = %+v, %v", cfg, err)
	}
	env := map[string]string{EnvPointsPerMin: "0", EnvGraphQLSecsPerMin: "2.5"}
	cfg, err = ConfigFromEnv(func(k string) string { return env[k] })
	if err != nil || cfg.PointsPerMin != 0 || cfg.GraphQLTimePerMin != 2500*time.Millisecond {
		t.Fatalf("cfg = %+v, %v", cfg, err)
	}
	for _, bad := range []map[string]string{{EnvPointsPerMin: "-1"}, {EnvPointsPerMin: "x"}, {EnvGraphQLSecsPerMin: "-3"}} {
		if _, err := ConfigFromEnv(func(k string) string { return bad[k] }); err == nil {
			t.Errorf("ConfigFromEnv(%v) accepted a bad value", bad)
		}
	}
}
