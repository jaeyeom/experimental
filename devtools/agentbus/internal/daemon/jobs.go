package daemon

import (
	"cmp"
	"context"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// defaultJobInterval is how often the daemon looks for lost runners
// when Config.JobTicks is nil.
const defaultJobInterval = 15 * time.Second

// jobsOp runs one jobs-* op on the job store.
func (s *server) jobsOp(req Request) Response {
	var a jobs.Args
	if req.Jobs != nil {
		a = *req.Jobs
	}
	var (
		r   jobs.Reply
		err error
	)
	switch req.Op {
	case "jobs-enqueue":
		r, err = s.jobs.Enqueue(a)
	case "jobs-claim":
		if err = s.claimMachine(req, &a); err == nil {
			r, err = s.jobs.Claim(a)
		}
	case "jobs-heartbeat":
		r, err = s.jobs.Heartbeat(a)
	case "jobs-done":
		r, err = s.jobs.Done(a)
	case "jobs-fail":
		r, err = s.jobs.Fail(a)
	case "jobs-ls":
		r, err = s.jobs.List(a)
	case "jobs-show":
		r, err = s.jobs.Show(a.ID)
	case "jobs-stats":
		r, err = s.jobs.Stats()
	case "jobs-hold":
		err = s.jobs.Hold(a.Note)
		r.Held, r.HoldNote = s.jobs.Held()
	case "jobs-unhold":
		err = s.jobs.Unhold()
		r.Held, r.HoldNote = s.jobs.Held()
	default:
		return Response{Error: "unknown op"}
	}
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Jobs: &r}
}

// watchJobs applies heartbeat loss, reconciles lost outward jobs, and
// runs the ask gate on every tick until ctx ends. An unclear reconcile
// is put in the user's decide inbox through notify; a gated job's
// question goes there through decider.
func (s *server) watchJobs(ctx context.Context, ticks <-chan time.Time, hook jobs.Hook, decider jobs.Decider) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			// A save that fails is tried again on the next tick.
			_, _ = s.jobs.Expire()
			jobs.Reconcile(ctx, s.jobs, hook, s.notify)
			jobs.RunGate(ctx, s.jobs, decider)
		}
	}
}

// claimMachine holds a runner on another machine to its own machine, so
// it takes only that machine's checkout leases: an empty --machine is
// its machine, and another machine is refused. A runner on this machine
// names its own machine; without one it is the local machine.
func (s *server) claimMachine(req Request, a *jobs.Args) error {
	caller, remote := s.remoteCaller(req.Env)
	if !remote {
		return nil
	}
	machine := handleMachine(caller)
	switch {
	case machine == "":
		return fmt.Errorf("cannot tell the machine of %s", caller)
	case a.Machine == "":
		a.Machine = machine
	case a.Machine != machine:
		return fmt.Errorf("%s is on machine %s; its runner may claim only as %s, not %s", caller, machine, machine, a.Machine)
	}
	return nil
}

// jobLeases takes and releases the checkout leases of jobs in the
// daemon's lease table.
type jobLeases struct{ s *server }

// Take grants the job the lease on its checkout on the runner's
// machine, keyed <machine>:<path>, only if it is free.
func (l jobLeases) Take(j jobs.Job, machine, runner string) (string, error) {
	machine = cmp.Or(machine, hubcall.LocalMachine)
	key := machine + ":" + j.Needs.Checkout
	if _, err := l.s.leases.ClaimForJob(key, j.ID, "runner "+runner+" on "+machine); err != nil {
		return "", fmt.Errorf("lease: %w", err)
	}
	l.s.dropCache(key)
	return key, nil
}

// Free ends the job's hold and grants and wakes the next live waiter,
// like an agent's release, without its git steps: the job's runner
// leaves the checkout as it found it.
func (l jobLeases) Free(key, id string) {
	gone, err := l.s.goneWaiters(key)
	if err != nil {
		gone = map[string]bool{}
	}
	r, err := l.s.leases.Release(lease.JobHolder(id), lease.Args{Checkout: key}, func(handle string) bool { return gone[handle] })
	if err != nil {
		// The lease table logs nothing for a refused release: say why.
		_ = l.s.log.Append(eventlog.Event{Event: eventlog.EventJob, Actor: "daemon", Name: key, Detail: id + " lease release failed", Reason: err.Error()})
		return
	}
	l.s.dropCache(key)
	l.s.wake(lease.Args{Checkout: key}, r)
}
