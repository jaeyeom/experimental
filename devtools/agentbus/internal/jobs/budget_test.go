package jobs

import (
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

func budgetConfig() Config {
	return Config{
		Budget: BudgetConfig{Limit: 100, Window: time.Hour, Floor: 20},
		Kinds: map[string]Kind{
			"rebase": {Cost: 10},
			"merge":  {Cost: 10, BelowFloor: true},
			"label":  {Cost: 1},
		},
	}
}

func mustReserve(t *testing.T, s *Store, a Args) Reply {
	t.Helper()
	r, err := s.Reserve(a)
	if err != nil {
		t.Fatalf("Reserve: %v", err)
	}
	return r
}

func TestConcurrentReservationsNeverOverspend(t *testing.T) {
	s, _, _ := newStore(t, Config{Budget: BudgetConfig{Limit: 50, Window: time.Hour}})
	var (
		wg      sync.WaitGroup
		mu      sync.Mutex
		granted int
	)
	for range 200 {
		wg.Go(func() {
			r, err := s.Reserve(Args{Calls: 1})
			if err != nil {
				t.Error(err)
				return
			}
			if r.Reserved {
				mu.Lock()
				granted++
				mu.Unlock()
			}
		})
	}
	wg.Wait()
	if granted != 50 {
		t.Fatalf("granted %d reservations of 1 call, want the limit 50", granted)
	}
	st, _ := s.Stats()
	if b := st.Stats.Budget; b.Used != 50 || b.Remaining != 0 {
		t.Fatalf("budget = %+v, want 50 used and 0 left", b)
	}
}

func TestConcurrentClaimsNeverOverspend(t *testing.T) {
	s, _, _ := newStore(t, Config{
		Budget: BudgetConfig{Limit: 95, Window: time.Hour},
		Kinds:  map[string]Kind{"rebase": {Cost: 10}},
	})
	for i := range 20 {
		mustEnqueue(t, s, Args{Kind: "rebase", Subject: fmt.Sprintf("o/r#%d", i)})
	}
	var (
		wg      sync.WaitGroup
		mu      sync.Mutex
		claimed int
	)
	for i := range 20 {
		wg.Go(func() {
			r, err := s.Claim(Args{Runner: fmt.Sprintf("r%d", i)})
			if err != nil {
				t.Error(err)
				return
			}
			if r.Job != nil {
				mu.Lock()
				claimed++
				mu.Unlock()
			}
		})
	}
	wg.Wait()
	if claimed != 9 {
		t.Fatalf("claimed %d jobs of cost 10 from 95 calls, want 9", claimed)
	}
	st, _ := s.Stats()
	if b := st.Stats.Budget; b.Used != 90 {
		t.Fatalf("budget = %+v, want 90 used", b)
	}
}

func TestReserveRefusesBelowFloorAndRollsAtReset(t *testing.T) {
	s, c, _ := newStore(t, budgetConfig())
	start := c.t
	r := mustReserve(t, s, Args{Calls: 75})
	if !r.Reserved || r.Budget.Remaining != 25 || !r.Budget.Reset.Equal(start.Add(time.Hour)) {
		t.Fatalf("reserve 75 = %+v %+v", r.Reserved, r.Budget)
	}
	// 25 - 10 is below the floor of 20.
	if r := mustReserve(t, s, Args{Calls: 10}); r.Reserved || r.Budget.Remaining != 25 {
		t.Fatalf("reserve 10 below the floor = %v %+v, want refused with 25 left", r.Reserved, r.Budget)
	}
	// A kind allowed below the floor may spend down to zero, not past.
	if r := mustReserve(t, s, Args{Calls: 10, Kind: "merge"}); !r.Reserved || r.Budget.Remaining != 15 {
		t.Fatalf("merge reserve = %v %+v, want granted with 15 left", r.Reserved, r.Budget)
	}
	if r := mustReserve(t, s, Args{Calls: 16, Kind: "merge"}); r.Reserved {
		t.Fatalf("merge reserve past zero granted: %+v", r.Budget)
	}
	c.t = start.Add(time.Hour)
	r = mustReserve(t, s, Args{Calls: 10})
	if !r.Reserved || r.Budget.Used != 10 || !r.Budget.Reset.Equal(c.t.Add(time.Hour)) {
		t.Fatalf("reserve after reset = %v %+v, want a new window with 10 used", r.Reserved, r.Budget)
	}
}

func TestReserveWithoutBudgetIsGranted(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	r := mustReserve(t, s, Args{Calls: 1000})
	if !r.Reserved || r.Budget != nil {
		t.Fatalf("reserve without a budget = %v %+v, want granted and no budget", r.Reserved, r.Budget)
	}
	if _, err := s.Reserve(Args{Calls: 0}); err == nil {
		t.Fatal("reserve of 0 calls succeeded, want an error")
	}
}

func TestLowBudgetDefersExpensiveJobsToReset(t *testing.T) {
	s, c, lg := newStore(t, budgetConfig())
	start := c.t
	mustReserve(t, s, Args{Calls: 75})
	exp := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Priority: 5})
	cheap := mustEnqueue(t, s, Args{Kind: "label", Subject: "o/r#2"})

	// The rebase does not fit above the floor: it is deferred to the
	// reset and the cheap job runs instead.
	got := mustClaim(t, s, "tiger")
	if got.ID != cheap.ID {
		t.Fatalf("claimed %s, want the cheap %s", got.ID, cheap.ID)
	}
	r, _ := s.Show(exp.ID)
	if r.Job.State != StateReady || !r.Job.NotBefore.Equal(start.Add(time.Hour)) {
		t.Fatalf("expensive job = %s not before %s, want ready until the reset %s", r.Job.State, r.Job.NotBefore, start.Add(time.Hour))
	}
	if !hasEvent(lg, exp.ID+" rebase o/r#1: deferred") {
		t.Fatalf("no deferral event: %+v", lg.events)
	}
	if r, _ := s.Claim(Args{Runner: "tiger"}); r.Job != nil {
		t.Fatalf("claimed %s before the reset", r.Job.ID)
	}

	c.t = start.Add(time.Hour)
	got = mustClaim(t, s, "tiger")
	if got.ID != exp.ID {
		t.Fatalf("claimed %s after the reset, want %s", got.ID, exp.ID)
	}
	st, _ := s.Stats()
	if b := st.Stats.Budget; b.Used != 10 || b.Remaining != 90 {
		t.Fatalf("budget after the reset = %+v, want 10 used", b)
	}
}

func TestFloorExemptKindRunsBelowFloor(t *testing.T) {
	s, _, _ := newStore(t, budgetConfig())
	mustReserve(t, s, Args{Calls: 75})
	rebase := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	merge := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#2"})
	if got := mustClaim(t, s, "tiger"); got.ID != merge.ID {
		t.Fatalf("claimed %s, want the floor-exempt merge %s", got.ID, merge.ID)
	}
	r, _ := s.Show(rebase.ID)
	if r.Job.NotBefore.IsZero() {
		t.Fatal("rebase was not deferred")
	}
	st, _ := s.Stats()
	if b := st.Stats.Budget; b.Remaining != 15 {
		t.Fatalf("budget = %+v, want 15 left", b)
	}
}

func TestBudgetSurvivesRestart(t *testing.T) {
	dir := t.TempDir()
	c := &clock{t: time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)}
	s, err := Open(dir, &memLog{}, c.now, budgetConfig())
	if err != nil {
		t.Fatal(err)
	}
	mustReserve(t, s, Args{Calls: 60})
	s2, err := Open(dir, &memLog{}, c.now, budgetConfig())
	if err != nil {
		t.Fatal(err)
	}
	st, _ := s2.Stats()
	if b := st.Stats.Budget; b.Used != 60 {
		t.Fatalf("budget after restart = %+v, want 60 used (file %s)", b, filepath.Join(dir, stateFile))
	}
}

func TestStatsBudget(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	if st, _ := s.Stats(); st.Stats.Budget != nil {
		t.Fatalf("stats without a budget = %+v", st.Stats.Budget)
	}
	s, c, _ := newStore(t, budgetConfig())
	st, _ := s.Stats()
	if b := st.Stats.Budget; b == nil || b.Limit != 100 || b.Remaining != 100 || b.Floor != 20 || !b.Reset.IsZero() {
		t.Fatalf("fresh budget = %+v", b)
	}
	mustReserve(t, s, Args{Calls: 30})
	c.t = c.t.Add(2 * time.Hour)
	// A window past its reset shows the full budget.
	if st, _ := s.Stats(); st.Stats.Budget.Remaining != 100 || st.Stats.Budget.Used != 0 {
		t.Fatalf("budget past the reset = %+v", st.Stats.Budget)
	}
}

func hasEvent(lg *memLog, prefix string) bool {
	lg.mu.Lock()
	defer lg.mu.Unlock()
	for _, e := range lg.events {
		if strings.HasPrefix(e.Detail, prefix) {
			return true
		}
	}
	return false
}
