package daemon

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
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
	if op, ok := holderOps[req.Op]; ok {
		r, err = op(s.jobs, a)
	} else {
		r, err = s.otherJobsOp(req, a)
	}
	if errors.Is(err, errUnknownOp) {
		return Response{Error: "unknown op"}
	}
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Jobs: &r}
}

// holderOps are the ops a job's holder uses on its running job.
var holderOps = map[string]func(*jobs.Store, jobs.Args) (jobs.Reply, error){
	"jobs-heartbeat": (*jobs.Store).Heartbeat,
	"jobs-done":      (*jobs.Store).Done,
	"jobs-fail":      (*jobs.Store).Fail,
	"jobs-gate":      (*jobs.Store).Gate,
}

var errUnknownOp = errors.New("unknown op")

func (s *server) otherJobsOp(req Request, a jobs.Args) (jobs.Reply, error) {
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
	case "jobs-beat":
		if err = s.claimMachine(req, &a); err == nil {
			r, err = s.jobs.Beat(a)
		}
	case "jobs-park", "jobs-regate":
		r, err = s.parkOp(req, a)
	case "jobs-ls":
		r, err = s.jobs.List(a)
	case "jobs-show":
		r, err = s.jobs.Show(a.ID)
	case "jobs-stats":
		r, err = s.jobs.Stats()
	case "jobs-status":
		v := s.jobsStatus()
		r.Status = &v
	case "jobs-reserve":
		r, err = s.jobs.Reserve(a)
	case "jobs-hold":
		err = s.jobs.Hold(a.Note)
		r.Held, r.HoldNote = s.jobs.Held()
	case "jobs-unhold":
		err = s.jobs.Unhold()
		r.Held, r.HoldNote = s.jobs.Held()
	default:
		return r, errUnknownOp
	}
	return r, err //nolint:wrapcheck // the store's own message is the reply
}

// watchJobs applies heartbeat loss, reconciles lost outward jobs, runs
// the ask gate, reports anomalies, and publishes the status view through
// pub, when it is not nil, on every tick until ctx ends. An unclear
// reconcile is put in the user's decide inbox through notify; a gated
// job's question goes there through decider.
func (s *server) watchJobs(ctx context.Context, ticks <-chan time.Time, hook jobs.Hook, decider jobs.Decider, pub *jobs.StatusPublisher) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			// A save that fails is tried again on the next tick.
			_, _ = s.jobs.Expire()
			jobs.Reconcile(ctx, s.jobs, hook, s.notify)
			jobs.RunAsk(ctx, s.jobs, busReplies{s.box})
			jobs.RunGate(ctx, s.jobs, decider)
			s.reportAnomalies(ctx)
			if pub != nil {
				pub.Publish(ctx, s.jobsStatus())
			}
		}
	}
}

// statusPublisher publishes the jobs status view to cfg.JobStatus, or
// is nil without one.
func statusPublisher(cfg Config) *jobs.StatusPublisher {
	if cfg.JobStatus == nil {
		return nil
	}
	return jobs.NewStatusPublisher(cfg.JobStatus, cfg.Jobs.StatusInterval())
}

// jobsStatus is the jobs status view with the lease table's leases.
func (s *server) jobsStatus() jobs.StatusView {
	var leases []jobs.LeaseView
	if r, err := s.leases.Status(lease.Args{All: true}); err == nil {
		for _, l := range r.Leases {
			v := jobs.LeaseView{Checkout: l.Checkout, State: string(l.State)}
			if h := l.Holder; h != nil {
				v.Holder = cmp.Or(h.Name, h.Handle)
			}
			leases = append(leases, v)
		}
	}
	return s.jobs.Status(leases)
}

// reportAnomalies sends each new anomaly to the anomaly role as an
// agentbus message, once. A message that cannot be queued, such as to a
// role nobody has claimed, is logged by the mailbox and not sent again.
func (s *server) reportAnomalies(ctx context.Context) {
	fired, err := s.jobs.CheckAnomalies()
	if err != nil {
		// The next tick tries again.
		return
	}
	role := s.jobs.Config().Anomalies.Role
	for _, a := range fired {
		_, _ = s.box.Post(ctx, "agentbus", role, a.Text)
	}
}

// parkTab is the pane of the agent that parks a job, which a resume
// goes back to, or empty for a caller outside an agent pane.
func (s *server) parkTab(env []string) string {
	if caller, remote := s.remoteCaller(env); remote {
		return caller
	}
	if envValue(env, "HERDR_PANE_ID") == "" {
		return ""
	}
	handle, _, err := s.reg.WhoAmI(s.ctx, env)
	if err != nil {
		return ""
	}
	return handle
}

// parkOp parks a job, on a decide item or an agentbus message, or
// regates one parked on a message onto a decide item.
func (s *server) parkOp(req Request, a jobs.Args) (jobs.Reply, error) {
	if req.Op == "jobs-regate" {
		return s.jobs.Regate(a.ID, a.Decide) //nolint:wrapcheck // the store's own message is the reply
	}
	a.Tab = s.parkTab(req.Env)
	if err := s.askTo(&a); err != nil {
		return jobs.Reply{}, err
	}
	return s.jobs.Park(a) //nolint:wrapcheck // the store's own message is the reply
}

// askTo sets the name a park's agent bus message was sent to, from the
// mailbox; the parking agent does not say it.
func (s *server) askTo(a *jobs.Args) error {
	a.AskTo = ""
	if a.AskMsg == "" {
		return nil
	}
	to, err := s.box.Recipient(a.AskMsg)
	if err != nil {
		return fmt.Errorf("park on message %s: %w", a.AskMsg, err)
	}
	a.AskTo = to
	return nil
}

// busReplies reads the replies to a park's message from the mailbox.
type busReplies struct{ box *mailbox.Mailbox }

func (b busReplies) Replies(_ context.Context, msg string) ([]jobs.BusReply, error) {
	answers, err := b.box.Replies(msg)
	if err != nil {
		return nil, fmt.Errorf("replies to %s: %w", msg, err)
	}
	out := make([]jobs.BusReply, 0, len(answers))
	for _, a := range answers {
		out = append(out, jobs.BusReply{ID: a.ID, From: a.From, Addressed: a.Addressed, Text: a.Text})
	}
	return out, nil
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
