package daemon

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// jobLeaseWorld is a server whose job queue takes checkout leases.
type jobLeaseWorld struct {
	srv *server
	ad  *fake.Adapter
}

func newJobLeaseWorld(t *testing.T) *jobLeaseWorld {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "state")
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ciAlice: adapter.StateWorking},
		Result:      map[string]adapter.Result{ciAlice: adapter.ResultAccepted},
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	srv, err := newServer(ctx, dir, Config{Adapters: []adapter.Adapter{ad}, Now: (&clock{now: ciT0()}).Now}, eventlog.Open(dir))
	if err != nil {
		t.Fatal(err)
	}
	return &jobLeaseWorld{srv: srv, ad: ad}
}

func (w *jobLeaseWorld) jobs(t *testing.T, op string, a jobs.Args, env ...string) Response {
	t.Helper()
	return w.srv.dispatch(Request{Op: op, Env: env, Jobs: &a}, 0)
}

func (w *jobLeaseWorld) lease(t *testing.T, key string) lease.Status {
	t.Helper()
	r, err := w.srv.leases.Status(lease.Args{Checkout: key})
	if err != nil {
		t.Fatal(err)
	}
	return r.Leases[0]
}

func (w *jobLeaseWorld) enqueue(t *testing.T, a jobs.Args) string {
	t.Helper()
	r := w.jobs(t, "jobs-enqueue", a)
	if !r.OK {
		t.Fatalf("enqueue = %+v", r)
	}
	return r.Jobs.Job.ID
}

func TestJobClaimTakesTheLeaseAndDoneReleasesIt(t *testing.T) {
	w := newJobLeaseWorld(t)
	id := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Needs: jobs.Needs{Checkout: "/home/u/src/repo"}})
	r := w.jobs(t, "jobs-claim", jobs.Args{Runner: "tiger-runner"})
	if !r.OK || r.Jobs.Job == nil || r.Jobs.Job.Lease != ciKey {
		t.Fatalf("claim = %+v", r)
	}
	st := w.lease(t, ciKey)
	if st.State != lease.StateHeld || st.Holder.Handle != lease.JobHolder(id) || !strings.Contains(st.Holder.Why, "tiger-runner") {
		t.Fatalf("lease status = %+v holder %+v, want held by the job", st, st.Holder)
	}
	// An agent waits behind the job and gets the checkout when it is done.
	if _, err := w.srv.leases.Claim(ciAlice, lease.Args{Checkout: ciKey, For: "40m"}); err != nil {
		t.Fatal(err)
	}
	if r := w.jobs(t, "jobs-done", jobs.Args{ID: id, Runner: "tiger-runner"}); !r.OK {
		t.Fatalf("done = %+v", r)
	}
	if st := w.lease(t, ciKey); st.Holder == nil || st.Holder.Handle != ciAlice {
		t.Fatalf("lease after done = %+v, want alice granted", st)
	}
	woken := false
	for _, text := range w.ad.Texts {
		woken = woken || strings.Contains(text, "[lease] granted "+ciKey)
	}
	if !woken {
		t.Fatalf("alice was not woken: %v", w.ad.Texts)
	}
}

func TestJobLeaseConflictLeavesTheJobReady(t *testing.T) {
	w := newJobLeaseWorld(t)
	if _, err := w.srv.leases.Claim(ciAlice, lease.Args{Checkout: ciKey, For: "40m"}); err != nil {
		t.Fatal(err)
	}
	id := w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Needs: jobs.Needs{Checkout: "/home/u/src/repo"}})
	if r := w.jobs(t, "jobs-claim", jobs.Args{Runner: "r1"}); !r.OK || r.Jobs.Job != nil {
		t.Fatalf("claim of a held checkout = %+v", r)
	}
	if r := w.jobs(t, "jobs-show", jobs.Args{ID: id}); r.Jobs.Job.State != jobs.StateReady || r.Jobs.Job.Lease != "" {
		t.Fatalf("job = %+v, want ready", r.Jobs.Job)
	}
	if st := w.lease(t, ciKey); st.Holder.Handle != ciAlice || len(st.Queue) != 0 {
		t.Fatalf("lease = %+v, want alice's and no job queued", st)
	}
}

func TestARemoteRunnerTakesOnlyItsOwnMachinesLease(t *testing.T) {
	w := newJobLeaseWorld(t)
	const lion = "AGENTBUS_AS_HANDLE=herdr:lion/default/w1:p1"
	w.enqueue(t, jobs.Args{Kind: "rebase", Subject: "o/r#1", Needs: jobs.Needs{Checkout: "/src/repo"}})
	r := w.jobs(t, "jobs-claim", jobs.Args{Runner: "lion-runner", Machine: "tiger"}, lion)
	if r.OK || !strings.Contains(r.Error, "machine lion") {
		t.Fatalf("lion's runner claiming as tiger = %+v, want refused", r)
	}
	if st := w.lease(t, "tiger:/src/repo"); st.Holder != nil {
		t.Fatalf("tiger's lease = %+v, want free", st)
	}
	r = w.jobs(t, "jobs-claim", jobs.Args{Runner: "lion-runner"}, lion)
	if !r.OK || r.Jobs.Job == nil || r.Jobs.Job.Lease != "lion:/src/repo" {
		t.Fatalf("lion's own claim = %+v", r)
	}
}
