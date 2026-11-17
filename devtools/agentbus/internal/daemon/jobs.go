package daemon

import (
	"context"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
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
		r, err = s.jobs.Claim(a)
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

// watchJobs applies heartbeat loss and reconciles lost outward jobs on
// every tick until ctx ends. An unclear reconcile is put in the user's
// decide inbox through notify.
func (s *server) watchJobs(ctx context.Context, ticks <-chan time.Time, hook jobs.Hook) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			// A save that fails is tried again on the next tick.
			_, _ = s.jobs.Expire()
			jobs.Reconcile(ctx, s.jobs, hook, s.notify)
		}
	}
}
