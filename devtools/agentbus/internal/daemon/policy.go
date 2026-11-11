package daemon

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
)

// newPolicy makes the hibernation policy around this daemon's own
// hibernate ops and event log.
func (s *server) newPolicy(build func(hibernate.Bus) *hibernate.Policy) *hibernate.Policy {
	pol := build(localBus{s: s})
	pol.Log = s.log.Append
	return pol
}

// localBus is the hibernate.Bus of the policy: the daemon's hibernate
// ops, called in process as the local user. It does not log refusals,
// which the policy would repeat on every tick.
type localBus struct{ s *server }

func (b localBus) State(context.Context) (hibernate.Reply, error) {
	return b.s.hibernateState()
}

func (b localBus) Begin(_ context.Context, rec hibernate.Record) (hibernate.Record, error) {
	r, err := b.s.hibernateBegin(Request{}, hibernate.Args{Record: &rec})
	if err != nil {
		return hibernate.Record{}, err
	}
	return *r.Record, nil
}

func (b localBus) End(_ context.Context, handle, status, reason string) error {
	args := hibernate.Args{Handle: handle, Status: status}
	if reason != "" {
		args.Reasons = []string{reason}
	}
	if _, err := b.s.hibernateEnd(Request{}, args); err != nil {
		return fmt.Errorf("end the hibernation of %s: %w", handle, err)
	}
	return nil
}

func (localBus) Refused(context.Context, string, []string) error { return nil }
