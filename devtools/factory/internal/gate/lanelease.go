package gate

import (
	"context"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// laneLeaseHolder returns a func that reports whether a claim holds the lane
// checkout's lease, for CountLane. A local lane reads the lease of its
// checkout through in.Lease. A remote lane looks up <machine>:<checkout> in
// in.Leases, the hub's list of every checkout. A lease gate of off reads
// nothing. A lease that cannot be read gives nil: leaseCheck reports a local
// one, and note reports a remote one.
func laneLeaseHolder(ctx context.Context, in Inputs, lane Lane) (held func(store.Claim) bool, note string) {
	if in.LeaseGate == "" || in.LeaseGate == config.LeaseGateOff || ctx == nil || lane.Checkout == "" {
		return nil, ""
	}
	if lane.Machine == "" {
		if in.Lease == nil {
			return nil, ""
		}
		reading, err := in.Lease(ctx, lane.Checkout)
		if err != nil || !reading.Guarded {
			return nil, ""
		}
		return holderOf(reading.Status), ""
	}
	if in.Leases == nil {
		return nil, ""
	}
	list, err := in.Leases(ctx)
	if err != nil {
		return nil, "lease list not read (" + err.Error() + ")"
	}
	key := lane.Machine + ":" + lane.Checkout
	for _, st := range list {
		if st.Checkout == key {
			return holderOf(st), ""
		}
	}
	return nil, ""
}

func holderOf(st agentbus.Status) func(store.Claim) bool {
	return func(claim store.Claim) bool {
		return st.HeldBy(agentbus.Owner{ItemID: claim.ID})
	}
}

// onceLease wraps read so a second read of the same checkout in one lane
// check reuses the first answer instead of running agentbus again.
func onceLease(read func(context.Context, string) (agentbus.Reading, error)) func(context.Context, string) (agentbus.Reading, error) {
	if read == nil {
		return nil
	}
	type answer struct {
		reading agentbus.Reading
		err     error
	}
	seen := map[string]answer{}
	return func(ctx context.Context, dir string) (agentbus.Reading, error) {
		if got, ok := seen[dir]; ok {
			return got.reading, got.err
		}
		reading, err := read(ctx, dir)
		seen[dir] = answer{reading: reading, err: err}
		return reading, err
	}
}
