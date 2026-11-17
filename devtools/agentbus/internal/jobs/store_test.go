package jobs

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

type memLog struct {
	mu     sync.Mutex
	events []eventlog.Event
}

func (l *memLog) Append(e eventlog.Event) error {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.events = append(l.events, e)
	return nil
}

type clock struct{ t time.Time }

func (c *clock) now() time.Time { return c.t }

func newStore(t *testing.T, cfg Config) (*Store, *clock, *memLog) {
	t.Helper()
	c := &clock{t: time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)}
	lg := &memLog{}
	s, err := Open(t.TempDir(), lg, c.now, cfg)
	if err != nil {
		t.Fatal(err)
	}
	// Most tests are about what happens past the gate.
	s.SetPolicy(Policy{Default: GateAuto})
	return s, c, lg
}

func mustEnqueue(t *testing.T, s *Store, a Args) Job {
	t.Helper()
	r, err := s.Enqueue(a)
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	return *r.Job
}

func mustClaim(t *testing.T, s *Store, runner string) Job {
	t.Helper()
	r, err := s.Claim(Args{Runner: runner})
	if err != nil {
		t.Fatalf("Claim: %v", err)
	}
	if r.Job == nil {
		t.Fatalf("Claim: no job (held=%v)", r.Held)
	}
	return *r.Job
}

func intp(n int) *int { return &n }

func TestEnqueueIsReadyAndLogsEachStep(t *testing.T) {
	s, _, lg := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	if j.ID != "j-1" || j.State != StateReady {
		t.Fatalf("job = %s %s, want j-1 ready", j.ID, j.State)
	}
	if j.Needs.Machine != "any" || j.Needs.Checkout != "none" || j.Needs.Executor != "cli" {
		t.Fatalf("needs defaults = %+v", j.Needs)
	}
	if len(j.History) != 2 || j.History[0].To != StateQueued || j.History[1].To != StateReady {
		t.Fatalf("history = %+v, want queued then ready", j.History)
	}
	if len(lg.events) != 2 || lg.events[0].Event != eventlog.EventJob {
		t.Fatalf("events = %+v", lg.events)
	}
}

func TestEnqueueDedupesOnKindSubjectHead(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	first := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	r, err := s.Enqueue(Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	if err != nil {
		t.Fatal(err)
	}
	if !r.Duplicate || r.Job.ID != first.ID {
		t.Fatalf("second enqueue = dup %v id %s, want duplicate of %s", r.Duplicate, r.Job.ID, first.ID)
	}
	for _, a := range []Args{
		{Kind: "rebase", Subject: "o/r#1", Head: "def"},
		{Kind: "merge", Subject: "o/r#1", Head: "abc"},
		{Kind: "rebase", Subject: "o/r#2", Head: "abc"},
	} {
		if r, _ := s.Enqueue(a); r.Duplicate {
			t.Errorf("Enqueue(%+v) is a duplicate; only kind+subject+head match", a)
		}
	}
	// A finished job is not live, so the same key enqueues again.
	j := mustClaim(t, s, "r1")
	if _, err := s.Done(Args{ID: j.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
	if r, _ := s.Enqueue(Args{Kind: j.Kind, Subject: j.Subject, Head: j.Head}); r.Duplicate {
		t.Fatalf("enqueue after done is a duplicate of %s", r.Job.ID)
	}
}

func TestEnqueueRefusesBadArgs(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	for _, a := range []Args{
		{Subject: "o/r#1"},
		{Kind: "rebase"},
		{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Affinity: "nearby"}},
		{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Executor: "shell"}},
		{Kind: "re base", Subject: "o/r#1"},
	} {
		if _, err := s.Enqueue(a); err == nil {
			t.Errorf("Enqueue(%+v) succeeded, want an error", a)
		}
	}
}

func TestTransitions(t *testing.T) {
	allowed := map[State][]State{
		StateQueued:  {StateGated, StateReady, StateDead},
		StateGated:   {StateReady, StateFailed, StateParked, StateDead},
		StateReady:   {StateRunning, StateParked, StateDead},
		StateRunning: {StateDone, StateFailed, StateDead, StateUnknown, StateReady, StateGated, StateParked},
		StateUnknown: {StateDone, StateFailed, StateDead, StateParked},
		StateParked:  {StateReady, StateDone, StateFailed, StateDead},
	}
	for _, from := range AllStates {
		for _, to := range AllStates {
			want := false
			for _, s := range allowed[from] {
				if s == to {
					want = true
				}
			}
			if got := CanTransition(from, to); got != want {
				t.Errorf("CanTransition(%s, %s) = %v, want %v", from, to, got, want)
			}
		}
	}
}

func TestRefusedTransitionsThroughTheStore(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	// ready: done, fail, and heartbeat are refused; only claim runs it.
	if _, err := s.Done(Args{ID: j.ID, Runner: "r1"}); err == nil {
		t.Error("done on a ready job succeeded")
	}
	if _, err := s.Fail(Args{ID: j.ID, Runner: "r1"}); err == nil {
		t.Error("fail on a ready job succeeded")
	}
	if _, err := s.Heartbeat(Args{ID: j.ID, Runner: "r1"}); err == nil {
		t.Error("heartbeat on a ready job succeeded")
	}
	mustClaim(t, s, "r1")
	// Only the holder may finish a running job.
	if _, err := s.Done(Args{ID: j.ID, Runner: "r2"}); err == nil {
		t.Error("done by a non-holder succeeded")
	}
	if _, err := s.Heartbeat(Args{ID: j.ID, Runner: "r2"}); err == nil {
		t.Error("heartbeat by a non-holder succeeded")
	}
	if _, err := s.Done(Args{ID: j.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
	// done is final.
	for name, op := range map[string]func(Args) (Reply, error){"done": s.Done, "fail": s.Fail, "heartbeat": s.Heartbeat} {
		if _, err := op(Args{ID: j.ID, Runner: "r1"}); err == nil {
			t.Errorf("%s on a done job succeeded", name)
		}
	}
	if _, err := s.Done(Args{ID: "j-99", Runner: "r1"}); err == nil {
		t.Error("done on a missing job succeeded")
	}
}

func TestClaimRaceHasOneWinner(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	var wg sync.WaitGroup
	got := make([]*Job, 8)
	for i := range got {
		wg.Add(1)
		go func() {
			defer wg.Done()
			r, err := s.Claim(Args{Runner: fmt.Sprintf("r%d", i)})
			if err != nil {
				t.Error(err)
				return
			}
			got[i] = r.Job
		}()
	}
	wg.Wait()
	winners := 0
	for _, j := range got {
		if j != nil {
			winners++
		}
	}
	if winners != 1 {
		t.Fatalf("%d claimers got the job, want 1", winners)
	}
}

func TestClaimOrderAndFilters(t *testing.T) {
	s, c, _ := newStore(t, Config{})
	low := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	high := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#2", Priority: 5})
	other := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#3", Needs: Needs{Machine: "linux2"}})
	if r, _ := s.Claim(Args{Runner: "r1", Kinds: []string{"merge"}, Machine: "mac"}); r.Job != nil {
		t.Fatalf("claim on mac got %s, which needs linux2", r.Job.ID)
	}
	if r, _ := s.Claim(Args{Runner: "r1", Kinds: []string{"merge"}, Machine: "linux2"}); r.Job == nil || r.Job.ID != other.ID {
		t.Fatalf("claim of merge on linux2 = %+v, want %s", r.Job, other.ID)
	}
	if j := mustClaim(t, s, "r1"); j.ID != high.ID {
		t.Fatalf("first claim = %s, want the higher priority %s", j.ID, high.ID)
	}
	if j := mustClaim(t, s, "r1"); j.ID != low.ID || j.Attempts != 1 || j.Holder != "r1" || !j.Heartbeat.Equal(c.t) {
		t.Fatalf("second claim = %+v", j)
	}
	if r, _ := s.Claim(Args{Runner: "r1"}); r.Job != nil {
		t.Fatalf("claim on an empty queue got %s", r.Job.ID)
	}
	if _, err := s.Claim(Args{}); err == nil {
		t.Fatal("claim without a runner succeeded")
	}
}

func TestRetryOnlyOnRetryableExits(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{"ci": {RetryExits: []int{75}, MaxAttempts: 2, Backoff: time.Minute}}}
	s, c, _ := newStore(t, cfg)
	j := mustEnqueue(t, s, Args{Kind: "ci", Subject: "o/r#1"})
	mustClaim(t, s, "r1")
	r, err := s.Fail(Args{ID: j.ID, Runner: "r1", Exit: intp(75)})
	if err != nil {
		t.Fatal(err)
	}
	if r.Job.State != StateReady || r.Job.Holder != "" || !r.Job.NotBefore.Equal(c.t.Add(time.Minute)) {
		t.Fatalf("after retryable fail: %+v, want ready not before +1m", r.Job)
	}
	if r, _ := s.Claim(Args{Runner: "r1"}); r.Job != nil {
		t.Fatal("claimed before notBefore")
	}
	c.t = c.t.Add(time.Minute)
	mustClaim(t, s, "r1")
	// Attempts are used up: a retryable exit is dead now.
	if r, _ := s.Fail(Args{ID: j.ID, Runner: "r1", Exit: intp(75)}); r.Job.State != StateDead {
		t.Fatalf("fail at max attempts = %s, want dead", r.Job.State)
	}

	other := mustEnqueue(t, s, Args{Kind: "ci", Subject: "o/r#2"})
	mustClaim(t, s, "r1")
	if r, _ := s.Fail(Args{ID: other.ID, Runner: "r1", Exit: intp(1)}); r.Job.State != StateFailed {
		t.Fatalf("fail with a non-retryable exit = %s, want failed", r.Job.State)
	}
	third := mustEnqueue(t, s, Args{Kind: "ci", Subject: "o/r#3"})
	mustClaim(t, s, "r1")
	if r, _ := s.Fail(Args{ID: third.ID, Runner: "r1"}); r.Job.State != StateFailed {
		t.Fatalf("fail with no exit code = %s, want failed", r.Job.State)
	}
}

func TestBackoffDoubles(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{"ci": {RetryExits: []int{75}, MaxAttempts: 5, Backoff: time.Minute}}}
	s, c, _ := newStore(t, cfg)
	j := mustEnqueue(t, s, Args{Kind: "ci", Subject: "o/r#1"})
	for _, want := range []time.Duration{time.Minute, 2 * time.Minute, 4 * time.Minute} {
		mustClaim(t, s, "r1")
		r, _ := s.Fail(Args{ID: j.ID, Runner: "r1", Exit: intp(75)})
		if got := r.Job.NotBefore.Sub(c.t); got != want {
			t.Fatalf("backoff = %s, want %s", got, want)
		}
		c.t = r.Job.NotBefore
	}
}

func TestHeartbeatLoss(t *testing.T) {
	cfg := Config{
		HeartbeatTimeout: time.Minute,
		Kinds:            map[string]Kind{"rebase": {Idempotent: true, MaxAttempts: 2}},
	}
	s, c, _ := newStore(t, cfg)
	idem := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	outward := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#2"})
	mustClaim(t, s, "r1")
	mustClaim(t, s, "r1")
	c.t = c.t.Add(30 * time.Second)
	if _, err := s.Heartbeat(Args{ID: idem.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
	c.t = c.t.Add(45 * time.Second)
	changed, err := s.Expire()
	if err != nil {
		t.Fatal(err)
	}
	if len(changed) != 1 || changed[0].ID != outward.ID || changed[0].State != StateUnknown {
		t.Fatalf("expired = %+v, want only the outward job, unknown", changed)
	}
	c.t = c.t.Add(time.Minute)
	changed, _ = s.Expire()
	if len(changed) != 1 || changed[0].ID != idem.ID || changed[0].State != StateReady || changed[0].Holder != "" {
		t.Fatalf("expired = %+v, want the idempotent job ready", changed)
	}
	// The unknown job is never claimed again.
	if j := mustClaim(t, s, "r2"); j.ID != idem.ID {
		t.Fatalf("claim = %s, want %s", j.ID, idem.ID)
	}
	c.t = c.t.Add(2 * time.Minute)
	// Attempts are used up: an idempotent job is dead, not ready.
	if changed, _ := s.Expire(); len(changed) != 1 || changed[0].State != StateDead {
		t.Fatalf("expire at max attempts = %+v, want dead", changed)
	}
	// A late heartbeat or done from the lost runner is refused.
	if _, err := s.Heartbeat(Args{ID: outward.ID, Runner: "r1"}); err == nil {
		t.Fatal("heartbeat on an unknown job succeeded")
	}
	if _, err := s.Done(Args{ID: outward.ID, Runner: "r1"}); err != nil {
		t.Fatalf("settling an unknown job: %v", err)
	}
}

func TestBlockedJobBlocksItsSubjectOrIsParked(t *testing.T) {
	cfg := Config{
		HeartbeatTimeout: time.Minute,
		Kinds:            map[string]Kind{"comment": {OnBlocked: OnBlockedPark}},
	}
	s, c, _ := newStore(t, cfg)
	merge := mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#1"})
	comment := mustEnqueue(t, s, Args{Kind: "comment", Subject: "o/r#2"})
	mustClaim(t, s, "r1")
	mustClaim(t, s, "r1")
	c.t = c.t.Add(2 * time.Minute)
	if _, err := s.Expire(); err != nil {
		t.Fatal(err)
	}
	if j := show(t, s, merge.ID); j.State != StateUnknown {
		t.Fatalf("merge = %s, want unknown", j.State)
	}
	if j := show(t, s, comment.ID); j.State != StateParked || j.ParkedFrom != StateUnknown {
		t.Fatalf("comment = %s from %s, want parked from unknown", j.State, j.ParkedFrom)
	}
	next1 := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	next2 := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#2"})
	if j := mustClaim(t, s, "r2"); j.ID != next2.ID {
		t.Fatalf("claim = %s, want %s; o/r#1 is blocked by %s", j.ID, next2.ID, merge.ID)
	}
	if r, _ := s.Claim(Args{Runner: "r2"}); r.Job != nil {
		t.Fatalf("claimed %s on a blocked subject", r.Job.ID)
	}
	if _, err := s.Fail(Args{ID: merge.ID, Note: "checked by hand"}); err != nil {
		t.Fatal(err)
	}
	if j := mustClaim(t, s, "r2"); j.ID != next1.ID {
		t.Fatalf("claim after settling = %s, want %s", j.ID, next1.ID)
	}
	// A parked job, settled, stays settled.
	if r, err := s.Done(Args{ID: comment.ID}); err != nil || r.Job.State != StateDone {
		t.Fatalf("settle parked = %+v, %v", r.Job, err)
	}
}

func TestHoldStopsClaims(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	if err := s.Hold("deploy freeze"); err != nil {
		t.Fatal(err)
	}
	r, err := s.Claim(Args{Runner: "r1"})
	if err != nil {
		t.Fatal(err)
	}
	if !r.Held || r.HoldNote != "deploy freeze" || r.Job != nil {
		t.Fatalf("claim under hold = %+v", r)
	}
	if err := s.Unhold(); err != nil {
		t.Fatal(err)
	}
	mustClaim(t, s, "r1")
}

func TestHoldFromTheExternalCheck(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	held := true
	s.SetExternalHold(func() (bool, string) { return held, "decide status jobs busy" })
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	if r, _ := s.Claim(Args{Runner: "r1"}); !r.Held || r.HoldNote != "decide status jobs busy" {
		t.Fatalf("claim = %+v, want held by decide", r)
	}
	held = false
	mustClaim(t, s, "r1")
}

func TestStorePersistsAcrossOpen(t *testing.T) {
	dir := t.TempDir()
	c := &clock{t: time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)}
	s, err := Open(dir, &memLog{}, c.now, Config{})
	if err != nil {
		t.Fatal(err)
	}
	s.SetPolicy(Policy{Default: GateAuto})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "abc"})
	mustClaim(t, s, "r1")
	s2, err := Open(dir, &memLog{}, c.now, Config{})
	if err != nil {
		t.Fatal(err)
	}
	s2.SetPolicy(Policy{Default: GateAuto})
	got := show(t, s2, j.ID)
	if got.State != StateRunning || got.Holder != "r1" {
		t.Fatalf("reopened job = %+v", got)
	}
	if next := mustEnqueue(t, s2, Args{Kind: "rebase", Subject: "o/r#2"}); next.ID != "j-2" {
		t.Fatalf("next id after reopen = %s, want j-2", next.ID)
	}
}

func TestOpenRefusesAnotherVersion(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, stateFile), []byte(`{"version":99}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := Open(dir, &memLog{}, nil, Config{}); err == nil || !strings.Contains(err.Error(), "version") {
		t.Fatalf("Open = %v, want a version error", err)
	}
}

func TestListAndStats(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	a := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#2"})
	mustClaim(t, s, "r1")
	if _, err := s.Done(Args{ID: a.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
	if r, _ := s.List(Args{}); len(r.Jobs) != 1 || r.Jobs[0].Kind != "merge" {
		t.Fatalf("live list = %+v, want only merge", r.Jobs)
	}
	if r, _ := s.List(Args{All: true}); len(r.Jobs) != 2 {
		t.Fatalf("list --all = %d jobs, want 2", len(r.Jobs))
	}
	if r, _ := s.List(Args{State: "done"}); len(r.Jobs) != 1 || r.Jobs[0].ID != a.ID {
		t.Fatalf("list --state done = %+v", r.Jobs)
	}
	if r, _ := s.List(Args{Kind: "merge", Subject: "o/r#2"}); len(r.Jobs) != 1 {
		t.Fatalf("list by kind and subject = %+v", r.Jobs)
	}
	if _, err := s.List(Args{State: "bogus"}); err == nil {
		t.Fatal("list --state bogus succeeded")
	}
	r, err := s.Stats()
	if err != nil {
		t.Fatal(err)
	}
	if r.Stats.States[StateDone] != 1 || r.Stats.States[StateReady] != 1 || r.Stats.Held {
		t.Fatalf("stats = %+v", r.Stats)
	}
}

func TestShowMissing(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	if _, err := s.Show("j-1"); !errors.Is(err, ErrNoJob) {
		t.Fatalf("Show = %v, want ErrNoJob", err)
	}
}

func show(t *testing.T, s *Store, id string) Job {
	t.Helper()
	r, err := s.Show(id)
	if err != nil {
		t.Fatal(err)
	}
	return *r.Job
}
