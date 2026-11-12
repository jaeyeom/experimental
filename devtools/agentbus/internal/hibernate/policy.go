package hibernate

import (
	"cmp"
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// hibernate_policy modes.
const (
	PolicyOff    = "off"
	PolicyDryRun = "dry-run"
	PolicyOn     = "on"
)

// policyActor is the actor of the policy events.
const policyActor = "hibernate policy"

// Runner is what the policy stops agents with: a *Hibernator.
type Runner interface {
	Run(ctx context.Context, req Request) ([]Result, error)
}

// StateReader reads the daemon's names, idle times, and records.
type StateReader interface {
	State(ctx context.Context) (Reply, error)
}

// Policy hibernates idle agents on this machine while its available
// memory is low. Each Tick under Below stops eligible agents one at a
// time, longest idle first, until memory is back at Target.
type Policy struct {
	// Mode is PolicyOn, PolicyDryRun, or anything else for off.
	Mode string
	// Below is the available memory under which the policy acts, and
	// Target the available memory at which it stops.
	Below, Target int64
	// MinIdle is how long the daemon must have seen an agent idle or
	// done.
	MinIdle time.Duration
	// Machine is this machine's label on the hub, or empty when Bus is
	// the local daemon. Only handles on it are candidates.
	Machine string
	Memory  procinfo.Memory
	Bus     StateReader
	// Hibernator runs every #644 check on one agent and stops it.
	Hibernator Runner
	Log        func(eventlog.Event) error

	// low is set during a low episode: from a tick under Below to the
	// next tick at or above it.
	low bool
	// noCandidate is set once policy_no_candidate is logged in the
	// episode.
	noCandidate bool
	// would are the agents logged as policy_would_hibernate in the
	// episode.
	would map[string]bool
}

// policyCandidate is an agent idle long enough to try.
type policyCandidate struct {
	handle string
	idle   time.Duration
}

// Watch runs Tick on every tick until ctx ends. A failed tick is
// tried again on the next.
func (p *Policy) Watch(ctx context.Context, ticks <-chan time.Time) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			_ = p.Tick(ctx)
		}
	}
}

// Tick reads the available memory and, when it is under Below,
// hibernates agents until it reaches Target or none is left to try.
// With PolicyDryRun an agent's RSS counts as freed and nothing stops.
func (p *Policy) Tick(ctx context.Context) error {
	dryRun := p.Mode == PolicyDryRun
	if p.Mode != PolicyOn && !dryRun {
		return nil
	}
	avail, err := p.Memory.Available(ctx)
	if err != nil {
		return fmt.Errorf("available memory: %w", err)
	}
	if avail >= p.Below {
		p.low, p.noCandidate, p.would = false, false, nil
		return nil
	}
	if !p.low {
		p.low, p.would = true, map[string]bool{}
	}
	state, err := p.Bus.State(ctx)
	if err != nil {
		return fmt.Errorf("agentbus daemon: %w", err)
	}
	acted, avail, err := p.hibernate(ctx, p.candidates(state), avail, dryRun)
	if err != nil || acted || p.noCandidate {
		return err
	}
	p.noCandidate = true
	return p.log(eventlog.Event{
		Event:  eventlog.EventPolicyNoCandidate,
		Detail: fmt.Sprintf("%s available, below %s; no agent eligible", memreport.HumanBytes(avail), memreport.HumanBytes(p.Below)),
	})
}

// hibernate tries the candidates in order until avail reaches Target.
// It reports whether any was hibernated, or would be, and the available
// memory after.
func (p *Policy) hibernate(ctx context.Context, cands []policyCandidate, avail int64, dryRun bool) (bool, int64, error) {
	acted := false
	for _, c := range cands {
		if avail >= p.Target {
			break
		}
		results, err := p.Hibernator.Run(ctx, Request{Targets: []string{c.handle}, DryRun: dryRun})
		if err != nil {
			return acted, avail, err //nolint:wrapcheck // Run names what failed
		}
		for _, r := range results {
			if r.Outcome != OutcomeHibernated && r.Outcome != OutcomeWould {
				continue
			}
			acted = true
			if err := p.logAction(c, r, avail); err != nil {
				return acted, avail, err
			}
			if dryRun {
				avail += r.RSSBytes
			} else if avail, err = p.Memory.Available(ctx); err != nil {
				return acted, avail, fmt.Errorf("available memory: %w", err)
			}
		}
	}
	return acted, avail, nil
}

// logAction logs policy_hibernate, or policy_would_hibernate once per
// agent in an episode.
func (p *Policy) logAction(c policyCandidate, r Result, before int64) error {
	event := eventlog.EventPolicyHibernate
	if r.Outcome == OutcomeWould {
		if p.would[c.handle] {
			return nil
		}
		p.would[c.handle] = true
		event = eventlog.EventPolicyWouldHibernate
	}
	return p.log(eventlog.Event{
		Event: event,
		Name:  r.Name,
		Detail: fmt.Sprintf("idle %s, %s rss, %s available before",
			c.idle, memreport.HumanBytes(r.RSSBytes), memreport.HumanBytes(before)),
	})
}

func (p *Policy) log(ev eventlog.Event) error {
	ev.Actor = policyActor
	if err := p.Log(ev); err != nil {
		return fmt.Errorf("log %s: %w", ev.Event, err)
	}
	return nil
}

// candidates are this machine's agents the daemon has seen idle or
// done for at least MinIdle and that have no hibernation record,
// longest idle first. An agent woken by a message starts its idle time
// over, so it waits MinIdle again.
func (p *Policy) candidates(state Reply) []policyCandidate {
	var out []policyCandidate
	here := cmp.Or(p.Machine, hubcall.LocalMachine)
	for handle, seen := range state.Seen {
		machine, _, _, err := hubcall.ParseHandle(handle)
		if err != nil || machine != here || !resting(seen.Status) || seen.For < p.MinIdle {
			continue
		}
		if _, ok := recordOf(state.Records, handle); ok {
			continue
		}
		out = append(out, policyCandidate{handle: handle, idle: seen.For})
	}
	slices.SortFunc(out, func(a, b policyCandidate) int {
		return cmp.Or(cmp.Compare(b.idle, a.idle), strings.Compare(a.handle, b.handle))
	})
	return out
}
