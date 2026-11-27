package gate

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// leaseOutcome is the result of reading the agentbus lease for a checkout.
type leaseOutcome struct {
	reason       string
	notes        []string
	skipCheckout bool
}

// leaseCheck reads the agentbus lease of dir under in.LeaseGate.
// The lease adds to the herdr and git checks; it does not replace them, except
// under queue and queue-required while another agent holds (or is releasing)
// the lease, where the shared checkout's git state belongs to the holder.
// A free lease, or one held by in.LeaseOwner, passes and keeps checkout checks.
// Under queue and queue-required, a lease held (or releasing) by another agent
// passes with notes so the started agent can queue behind the holder without
// taking the lease and checkout checks are skipped, while paused and
// needs-adopt still refuse. Any other state is a reason that names the holder,
// the deadline, and the queue length.
// A lease that cannot be read, including a checkout agentbus does not
// guard, is a note under auto/queue and a reason under required/queue-required.
func leaseCheck(ctx context.Context, in Inputs, dir string) leaseOutcome {
	switch in.LeaseGate {
	case "", config.LeaseGateOff:
		return leaseOutcome{}
	case config.LeaseGateAuto, config.LeaseGateRequired, config.LeaseGateQueue, config.LeaseGateQueueRequired:
	default:
		return leaseOutcome{reason: config.ValidLeaseGate(in.LeaseGate).Error()}
	}
	if dir == "" {
		// checkoutReason already reports the empty path.
		return leaseOutcome{}
	}
	if ctx == nil {
		return leaseOutcome{reason: "agentbus lease: expected a context, found nil"}
	}
	if in.Lease == nil {
		return leaseOutcome{reason: "agentbus lease: expected a reader, found nil"}
	}
	reading, err := in.Lease(ctx, dir)
	if err != nil {
		reason, note := unreadLease(in.LeaseGate, dir, err.Error())
		return leaseOutcomeWithNote(reason, note)
	}
	if !reading.Guarded {
		reason, note := unreadLease(in.LeaseGate, dir, "not a guarded checkout in agentbus lease_checkouts")
		return leaseOutcomeWithNote(reason, note)
	}
	st := reading.Status
	if st.State == agentbus.StateFree || st.HeldBy(in.LeaseOwner) {
		return leaseOutcome{}
	}
	if isQueueLeaseGate(in.LeaseGate) && (st.State == agentbus.StateHeld || st.State == agentbus.StateReleasing) {
		return leaseOutcome{
			notes: []string{
				fmt.Sprintf("%s: lease %s; start will queue behind holder (lease gate: %s)", dir, st.Summary(nil), in.LeaseGate),
				skipCheckoutNote(dir, st),
			},
			skipCheckout: true,
		}
	}
	return leaseOutcome{reason: fmt.Sprintf("%s: lease %s", dir, st.Summary(nil))}
}

func leaseOutcomeWithNote(reason, note string) leaseOutcome {
	if note == "" {
		return leaseOutcome{reason: reason}
	}
	return leaseOutcome{reason: reason, notes: []string{note}}
}

func skipCheckoutNote(dir string, st agentbus.Status) string {
	who := st.Who()
	if who == "" {
		return fmt.Sprintf("%s: checkout checks skipped: lease %s", dir, st.State)
	}
	return fmt.Sprintf("%s: checkout checks skipped: lease %s by %s", dir, st.State, who)
}

func isQueueLeaseGate(mode string) bool {
	return mode == config.LeaseGateQueue || mode == config.LeaseGateQueueRequired
}

func unreadLease(mode, dir, why string) (reason, note string) {
	switch mode {
	case config.LeaseGateRequired:
		return fmt.Sprintf("%s: lease not read (%s); lease gate is required", dir, why), ""
	case config.LeaseGateQueueRequired:
		return fmt.Sprintf("%s: lease not read (%s); lease gate is queue-required", dir, why), ""
	case config.LeaseGateQueue:
		return "", fmt.Sprintf("%s: lease not read (%s); used the herdr and git checks only (lease gate: queue)", dir, why)
	default:
		return "", fmt.Sprintf("%s: lease not read (%s); used the herdr and git checks only", dir, why)
	}
}

// LeaseReader adapts an agentbus client to Inputs.Lease.
func LeaseReader(client *agentbus.Client) func(context.Context, string) (agentbus.Reading, error) {
	if client == nil {
		return nil
	}
	return client.Read
}
