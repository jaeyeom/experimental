package jobs

import (
	"errors"
	"os"
	"slices"
	"sync"
	"testing"
	"time"
)

// fakeLeases is a lease table that grants each checkout key to one job.
type fakeLeases struct {
	mu     sync.Mutex
	held   map[string]string
	refuse map[string]bool
	takes  []string
	freed  []string
}

func newFakeLeases() *fakeLeases {
	return &fakeLeases{held: map[string]string{}, refuse: map[string]bool{}}
}

func (f *fakeLeases) Take(j Job, machine, runner string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	key := machine + ":" + j.Needs.Checkout
	f.takes = append(f.takes, j.ID+" "+key+" "+runner)
	if f.refuse[key] {
		return "", errors.New(key + " is held by someone else")
	}
	if h, ok := f.held[key]; ok && h != j.ID {
		return "", errors.New(key + " is held by " + h)
	}
	f.held[key] = j.ID
	return key, nil
}

func (f *fakeLeases) Free(key, id string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.held[key] == id {
		delete(f.held, key)
	}
	f.freed = append(f.freed, id+" "+key)
}

func (f *fakeLeases) holder(key string) string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.held[key]
}

func leaseStore(t *testing.T, cfg Config) (*Store, *clock, *fakeLeases) {
	t.Helper()
	s, c, _ := newStore(t, cfg)
	f := newFakeLeases()
	s.SetLeaser(f)
	return s, c, f
}

func TestClaimTakesTheCheckoutLeaseWithTheJob(t *testing.T) {
	s, _, f := leaseStore(t, Config{})
	plain := mustEnqueue(t, s, Args{Kind: "approve", Subject: "o/r#1"})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#2", Needs: Needs{Checkout: "/src/repo"}, Priority: 1})
	r, err := s.Claim(Args{Runner: "tiger-runner", Machine: "tiger"})
	if err != nil || r.Job == nil || r.Job.ID != j.ID {
		t.Fatalf("claim = %+v, %v; want %s", r.Job, err, j.ID)
	}
	if r.Job.Lease != "tiger:/src/repo" || f.holder("tiger:/src/repo") != j.ID {
		t.Fatalf("job lease %q, table holder %q", r.Job.Lease, f.holder("tiger:/src/repo"))
	}
	if !slices.Equal(f.takes, []string{j.ID + " tiger:/src/repo tiger-runner"}) {
		t.Fatalf("takes = %v", f.takes)
	}
	// A job that needs no checkout takes no lease.
	if p := mustClaim(t, s, "tiger-runner"); p.ID != plain.ID || p.Lease != "" || len(f.takes) != 1 {
		t.Fatalf("plain claim = %+v, takes %v", p, f.takes)
	}
	// show names the lease the job holds.
	if got := show(t, s, j.ID); got.Lease != "tiger:/src/repo" {
		t.Fatalf("show lease = %q", got.Lease)
	}
}

func TestLeaseConflictLeavesTheJobReady(t *testing.T) {
	s, _, f := leaseStore(t, Config{})
	f.refuse["tiger:/src/repo"] = true
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Checkout: "/src/repo"}, Priority: 1})
	other := mustEnqueue(t, s, Args{Kind: "approve", Subject: "o/r#2"})
	// The conflicting job is passed over; the next ready one is claimed.
	if got := mustClaimOn(t, s, "r1", "tiger"); got.ID != other.ID {
		t.Fatalf("claim = %s, want %s", got.ID, other.ID)
	}
	if r, err := s.Claim(Args{Runner: "r1", Machine: "tiger"}); err != nil || r.Job != nil {
		t.Fatalf("claim on conflict = %+v, %v; want none", r.Job, err)
	}
	got := show(t, s, j.ID)
	if got.State != StateReady || got.Holder != "" || got.Attempts != 0 || got.Lease != "" {
		t.Fatalf("job after a lease conflict = %+v, want ready and untouched", got)
	}
	delete(f.refuse, "tiger:/src/repo")
	if got := mustClaimOn(t, s, "r1", "tiger"); got.ID != j.ID || got.Attempts != 1 {
		t.Fatalf("claim once free = %+v", got)
	}
}

func TestLeaseIsReleasedOnDoneFailAndPark(t *testing.T) {
	cfg := Config{
		HeartbeatTimeout: time.Minute,
		Kinds: map[string]Kind{
			"rebase":  {Idempotent: true, MaxAttempts: 2, RetryExits: []int{75}},
			"comment": {OnBlocked: OnBlockedPark},
		},
	}
	s, c, f := leaseStore(t, cfg)
	claimOn := func(kind, subject, path string) Job {
		t.Helper()
		mustEnqueue(t, s, Args{Kind: kind, Subject: subject, Needs: Needs{Checkout: path}})
		return mustClaimOn(t, s, "r1", "tiger")
	}
	done := claimOn("approve", "o/r#1", "/a")
	if r, err := s.Done(Args{ID: done.ID, Runner: "r1"}); err != nil || r.Job.Lease != "" {
		t.Fatalf("done = %+v, %v", r.Job, err)
	}
	failed := claimOn("approve", "o/r#2", "/b")
	if _, err := s.Fail(Args{ID: failed.ID, Runner: "r1", Exit: intp(1)}); err != nil {
		t.Fatal(err)
	}
	retried := claimOn("rebase", "o/r#3", "/c")
	if r, err := s.Fail(Args{ID: retried.ID, Runner: "r1", Exit: intp(75)}); err != nil || r.Job.State != StateReady {
		t.Fatalf("retryable fail = %+v, %v", r.Job, err)
	}
	parked := claimOn("comment", "o/r#4", "/d")
	lost := claimOn("merge", "o/r#5", "/e")
	c.t = c.t.Add(2 * time.Minute)
	if _, err := s.Expire(); err != nil {
		t.Fatal(err)
	}
	if got := show(t, s, parked.ID); got.State != StateParked || got.Lease != "" {
		t.Fatalf("parked = %+v", got)
	}
	want := []string{done.ID + " tiger:/a", failed.ID + " tiger:/b", retried.ID + " tiger:/c", parked.ID + " tiger:/d"}
	if !slices.Equal(f.freed, want) {
		t.Fatalf("freed = %v, want %v", f.freed, want)
	}
	// A lost outward job may have acted: it keeps the checkout until it
	// is settled.
	if got := show(t, s, lost.ID); got.State != StateUnknown || f.holder("tiger:/e") != lost.ID {
		t.Fatalf("lost = %+v, holder %q", got, f.holder("tiger:/e"))
	}
	if _, err := s.Fail(Args{ID: lost.ID, Note: "checked by hand"}); err != nil {
		t.Fatal(err)
	}
	if f.holder("tiger:/e") != "" {
		t.Fatal("settling the unknown job kept its lease")
	}
}

func TestAJobNeedingOneMachineTakesOnlyThatMachinesLease(t *testing.T) {
	s, _, f := leaseStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Machine: "tiger", Checkout: "/src/repo"}})
	if r, _ := s.Claim(Args{Runner: "lion-runner", Machine: "lion"}); r.Job != nil {
		t.Fatalf("lion's runner claimed %s", r.Job.ID)
	}
	if len(f.takes) != 0 {
		t.Fatalf("takes = %v, want none for another machine's runner", f.takes)
	}
	if got := mustClaimOn(t, s, "tiger-runner", "tiger"); got.ID != j.ID || got.Lease != "tiger:/src/repo" {
		t.Fatalf("tiger's claim = %+v", got)
	}
}

func TestEnqueueRefusesARelativeCheckout(t *testing.T) {
	s, _, _ := newStore(t, Config{})
	for _, path := range []string{"src/repo", "/src/../repo", "tiger:/src/repo"} {
		if _, err := s.Enqueue(Args{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Checkout: path}}); err == nil {
			t.Errorf("checkout %q accepted", path)
		}
	}
}

func mustClaimOn(t *testing.T, s *Store, runner, machine string) Job {
	t.Helper()
	r, err := s.Claim(Args{Runner: runner, Machine: machine})
	if err != nil || r.Job == nil {
		t.Fatalf("Claim by %s on %s = %+v, %v", runner, machine, r.Job, err)
	}
	return *r.Job
}

func TestAClaimNotSavedReleasesItsLease(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("root writes a read-only directory")
	}
	s, _, f := leaseStore(t, Config{})
	j := mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Needs: Needs{Checkout: "/src/repo"}})
	if err := os.Chmod(s.dir, 0o500); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(s.dir, 0o755) })
	if _, err := s.Claim(Args{Runner: "r1", Machine: "tiger"}); err == nil {
		t.Fatal("claim saved into a read-only directory")
	}
	if f.holder("tiger:/src/repo") != "" {
		t.Fatal("the unsaved claim kept its lease")
	}
	if got := show(t, s, j.ID); got.State != StateReady || got.Lease != "" {
		t.Fatalf("job = %+v, want ready", got)
	}
}
