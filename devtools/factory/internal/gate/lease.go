package gate

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// leaseCheck reads the agentbus lease of dir under in.LeaseGate.
// The lease adds to the herdr and git checks; it does not replace them.
// A free lease, or one held by in.LeaseOwner, passes. Under queue and
// queue-required, a lease held (or releasing) by another agent also passes
// with a note so the started agent can queue behind the holder without taking
// the lease, while paused and needs-adopt still refuse. Any other state is
// a reason that names the holder, the deadline, and the queue length.
// A lease that cannot be read, including a checkout agentbus does not
// guard, is a note under auto/queue and a reason under required/queue-required.
func leaseCheck(ctx context.Context, in Inputs, dir string) (reason, note string) {
	switch in.LeaseGate {
	case "", config.LeaseGateOff:
		return "", ""
	case config.LeaseGateAuto, config.LeaseGateRequired, config.LeaseGateQueue, config.LeaseGateQueueRequired:
	default:
		return config.ValidLeaseGate(in.LeaseGate).Error(), ""
	}
	if dir == "" {
		// checkoutReason already reports the empty path.
		return "", ""
	}
	if ctx == nil {
		return "agentbus lease: expected a context, found nil", ""
	}
	if in.Lease == nil {
		return "agentbus lease: expected a reader, found nil", ""
	}
	reading, err := in.Lease(ctx, dir)
	if err != nil {
		return unreadLease(in.LeaseGate, dir, err.Error())
	}
	if !reading.Guarded {
		return unreadLease(in.LeaseGate, dir, "not a guarded checkout in agentbus lease_checkouts")
	}
	st := reading.Status
	if st.State == agentbus.StateFree || st.HeldBy(in.LeaseOwner) {
		return "", ""
	}
	if isQueueLeaseGate(in.LeaseGate) && (st.State == agentbus.StateHeld || st.State == agentbus.StateReleasing) {
		return "", fmt.Sprintf("%s: lease %s; start will queue behind holder (lease gate: %s)", dir, st.Summary(nil), in.LeaseGate)
	}
	return fmt.Sprintf("%s: lease %s", dir, st.Summary(nil)), ""
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
