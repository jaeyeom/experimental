package gate

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// leaseCheck reads the agentbus lease of dir under in.LeaseGate.
// The lease adds to the herdr and git checks; it does not replace them.
// A free lease, or one held by in.LeaseOwner, passes. Any other state is
// a reason that names the holder, the deadline, and the queue length.
// A lease that cannot be read, including a checkout agentbus does not
// guard, is a note under auto and a reason under required.
func leaseCheck(ctx context.Context, in Inputs, dir string) (reason, note string) {
	switch in.LeaseGate {
	case "", config.LeaseGateOff:
		return "", ""
	case config.LeaseGateAuto, config.LeaseGateRequired:
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
	return fmt.Sprintf("%s: lease %s", dir, st.Summary(nil)), ""
}

func unreadLease(mode, dir, why string) (reason, note string) {
	if mode == config.LeaseGateRequired {
		return fmt.Sprintf("%s: lease not read (%s); lease gate is required", dir, why), ""
	}
	return "", fmt.Sprintf("%s: lease not read (%s); used the herdr and git checks only", dir, why)
}

// LeaseReader adapts an agentbus client to Inputs.Lease.
func LeaseReader(client *agentbus.Client) func(context.Context, string) (agentbus.Reading, error) {
	if client == nil {
		return nil
	}
	return client.Read
}
