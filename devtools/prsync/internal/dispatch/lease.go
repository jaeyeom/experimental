package dispatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

// SignalLease holds the dispatch checkout when its agentbus workdir
// lease is not free, or, with gate_lease=required, cannot be read.
const SignalLease = "lease"

// ErrAgentbusMissing is returned by a LeaseSource when agentbus is not
// installed.
var ErrAgentbusMissing = errors.New("agentbus not installed")

// LeaseStatus is the part of `agentbus lease status --json` the gate reads.
type LeaseStatus struct {
	Checkout string       `json:"checkout"`
	State    string       `json:"state"`
	Holder   *LeaseHolder `json:"holder"`
}

// LeaseHolder is the lease holder. Deadline is RFC 3339, or empty for
// the user's pause.
type LeaseHolder struct {
	Handle   string `json:"handle"`
	Name     string `json:"name"`
	Deadline string `json:"deadline"`
}

// LeaseSource reads the agentbus workdir lease of a checkout.
type LeaseSource interface {
	// Guarded reports whether checkout is in agentbus lease_checkouts.
	// It must not contact the agentbus daemon or hub.
	Guarded(ctx context.Context, checkout string) (bool, error)
	// Status is the checkout's lease.
	Status(ctx context.Context, checkout string) (LeaseStatus, error)
}

// AgentbusLease reads the lease with the agentbus CLI.
type AgentbusLease struct {
	Exec executor.Executor
	Bin  string
}

// Guarded runs `agentbus lease status --guarded`, which answers from
// the local agentbus config without contacting the daemon or hub.
func (a AgentbusLease) Guarded(ctx context.Context, checkout string) (bool, error) {
	var out struct {
		Guarded bool `json:"guarded"`
	}
	if err := a.run(ctx, &out, "lease", "status", "--guarded", "--json", "--path", checkout); err != nil {
		return false, err
	}
	return out.Guarded, nil
}

// Status runs `agentbus lease status --json`.
func (a AgentbusLease) Status(ctx context.Context, checkout string) (LeaseStatus, error) {
	var out LeaseStatus
	if err := a.run(ctx, &out, "lease", "status", "--json", "--path", checkout); err != nil {
		return LeaseStatus{}, err
	}
	return out, nil
}

func (a AgentbusLease) run(ctx context.Context, out any, args ...string) error {
	res, err := a.Exec.Execute(ctx, executor.ToolConfig{Command: a.Bin, Args: args, Timeout: probeTimeout})
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return fmt.Errorf("%w: %w", ErrAgentbusMissing, err)
		}
		return fmt.Errorf("run %s: %w", a.Bin, err)
	}
	if res.ExitCode != 0 {
		return fmt.Errorf("%s %s exited %d: %s", a.Bin, strings.Join(args[:2], " "), res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	if err := json.Unmarshal([]byte(res.Output), out); err != nil {
		return fmt.Errorf("decode %s %s: %w", a.Bin, strings.Join(args[:2], " "), err)
	}
	return nil
}

// leaseVerdict is the lease's verdict on the dispatch checkout for one
// sample. When read is true the lease decides and the tab signals are
// skipped; hold is set unless the lease is free. When read is false the
// tab signals apply, plus hold under gate_lease=required, and
// unavailable is set when the lease could not be read.
type leaseVerdict struct {
	read        bool
	hold        *Busy
	unavailable *Unavailable
}

func (s *snapshotProbes) lease(ctx context.Context, policy gatePolicy) leaseVerdict {
	if s.Lease == nil || policy.checkout == "" || policy.leaseMode == "off" {
		return leaseVerdict{}
	}
	guarded, err := s.leaseGuarded(ctx, policy.checkout)
	if err == nil && !guarded {
		return leaseVerdict{}
	}
	var st LeaseStatus
	if err == nil {
		st, err = s.Lease.Status(ctx, policy.checkout)
	}
	if err != nil {
		s.run.warnLease(ctx, err)
		out := leaseVerdict{unavailable: &Unavailable{Checkout: policy.checkout, Signal: SignalLease, Error: err.Error()}}
		if policy.leaseMode == "required" {
			out.hold = &Busy{Checkout: policy.checkout, Signal: SignalLease, Detail: "cannot read the lease: " + err.Error()}
		}
		return out
	}
	s.run.leaseWarned = ""
	out := leaseVerdict{read: true}
	if st.State != "free" {
		out.hold = &Busy{Checkout: policy.checkout, Signal: SignalLease, Detail: leaseDetail(st)}
	}
	return out
}

// leaseGuarded asks once per gate run whether checkout is guarded; an
// error is asked again on the next sample.
func (s *snapshotProbes) leaseGuarded(ctx context.Context, checkout string) (bool, error) {
	if s.run.leaseGuardedKnown {
		return s.run.leaseGuarded, nil
	}
	guarded, err := s.Lease.Guarded(ctx, checkout)
	if err != nil {
		return false, fmt.Errorf("lease guarded: %w", err)
	}
	s.run.leaseGuarded, s.run.leaseGuardedKnown = guarded, true
	return guarded, nil
}

// warnLease logs gate_lease_unavailable when the error changes.
func (r *gateRun) warnLease(ctx context.Context, err error) {
	if r.leaseWarned == err.Error() {
		return
	}
	r.leaseWarned = err.Error()
	runlog.FromContext(ctx).Warn("gate_lease_unavailable", "error", err.Error())
	writeStatus(ctx, "cannot read the agentbus lease ("+err.Error()+")")
}

// leaseDetail names the holder and the bid's end: "held by pm/foo
// until 14:20", "paused by user", or "needs-adopt by pm/foo".
func leaseDetail(st LeaseStatus) string {
	h := st.Holder
	switch {
	case h == nil:
		return st.State
	case st.State == "held":
		out := "held by " + leaseWho(h)
		if h.Deadline != "" {
			out += " until " + leaseClock(h.Deadline)
		}
		return out
	default:
		return st.State + " by " + leaseWho(h)
	}
}

func leaseWho(h *LeaseHolder) string {
	if h.Name != "" {
		return h.Name
	}
	return h.Handle
}

// leaseClock renders an RFC 3339 time as local HH:MM.
func leaseClock(ts string) string {
	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ts
	}
	return t.Local().Format("15:04")
}
