package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// Tests replace the memory reading and the ticker, and wait on
// policyTicked, called after each tick.
var (
	policyMemory = procinfo.LocalMemory
	policyTicks  = func(d time.Duration) (<-chan time.Time, func()) {
		t := time.NewTicker(d)
		return t.C, t.Stop
	}
	policyTicked = func() {}
)

const policyPrefix = "agentbus hibernate --policy: "

// policySeenFile, in the state directory, holds the runner's idle
// tracker, so a restarted runner keeps its idle times and mem can show
// them.
const policySeenFile = "policy-seen.json"

// cmdHibernatePolicy runs hibernate_policy on a hub-mode client, where
// no agentbusd runs, until ctx ends. It reads this machine's memory and
// panes, and sends the hibernate ops to the hub, as agentbus hibernate
// does. The hub tracks idle time only for the machines it polls, so the
// runner tracks it here from its own inventory reads, and saves it to
// policySeenFile at most once a minute and on exit. It writes its
// events and why a tick did nothing to stderr.
func cmdHibernatePolicy(ctx context.Context, env []string, stderr io.Writer) int {
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "%s%s\n", policyPrefix, err)
		return 1
	}
	if cfg.Hub == "" {
		fmt.Fprintf(stderr, "%snot in hub mode: agentbusd runs hibernate_policy on this machine\n", policyPrefix)
		return 1
	}
	machine, err := leaseMachine(cfg, env)
	if err != nil {
		fmt.Fprintf(stderr, "%s%s\n", policyPrefix, err)
		return 1
	}
	for _, w := range cfg.Warnings {
		fmt.Fprintf(stderr, "%sconfig: %s\n", policyPrefix, w)
	}
	pc := cfg.Policy()
	if pc.Mode == hibernate.PolicyOff {
		fmt.Fprintf(stderr, "%shibernate_policy=off; nothing to run\n", policyPrefix)
		return 0
	}
	fmt.Fprintf(stderr, "%shibernate_policy=%s below=%s target=%s min_idle=%s interval=%s hub=%s machine=%s\n",
		policyPrefix, pc.Mode, memreport.HumanBytes(pc.Below), memreport.HumanBytes(pc.Target), pc.MinIdle, pc.Interval, cfg.Hub, machine)
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "%s%s\n", policyPrefix, err)
		return 1
	}
	r := &policyRunner{
		cfg: cfg, env: env, machine: machine, panes: &herdr.Adapter{},
		seenPath: filepath.Join(dirs.StateDir, policySeenFile), stderr: stderr,
	}
	r.idle.LastTurn = lastturn.Claude(lastturn.ClaudeProjectsDir(env))
	if err := r.idle.Load(r.seenPath); err != nil {
		fmt.Fprintf(stderr, "%sidle times start over: %s\n", policyPrefix, err)
	}
	h := newHibernator(cfg, env, machine)
	r.policy = &hibernate.Policy{
		Mode: pc.Mode, Below: pc.Below, Target: pc.Target, MinIdle: pc.MinIdle,
		Machine:    machine,
		Memory:     policyMemory(),
		Bus:        r,
		Hibernator: h,
		Log: func(ev eventlog.Event) error {
			what := ev.Event
			if ev.Name != "" {
				what += " " + ev.Name
			}
			_, err := fmt.Fprintf(stderr, "%s%s: %s\n", policyPrefix, what, ev.Detail)
			return err //nolint:wrapcheck // the policy names the event
		},
	}
	ticks, stop := policyTicks(pc.Interval)
	defer stop()
	defer r.saveSeen()
	for {
		select {
		case <-ctx.Done():
			return 0
		case now := <-ticks:
			if err := r.tick(ctx, now); err != nil {
				fmt.Fprintf(stderr, "%s%s; nothing more hibernated until the next tick\n", policyPrefix, err)
			}
			policyTicked()
		}
	}
}

// policyRunner is the policy's state reader: the hub's names and
// records, with idle times tracked here.
type policyRunner struct {
	cfg     config.Config
	env     []string
	machine string
	panes   *herdr.Adapter
	idle    hibernate.IdleTracker
	// seenPath is where idle is saved, once it has a snapshot.
	seenPath string
	observed bool
	stderr   io.Writer
	policy   *hibernate.Policy
	// now is the time of the current tick.
	now time.Time
}

// tick reads this machine's panes for idle time, then runs the policy
// with its hub requests routed as agentbus hibernate routes them.
func (r *policyRunner) tick(ctx context.Context, now time.Time) error {
	agents, err := r.panes.Inventory(ctx)
	if err != nil {
		return fmt.Errorf("herdr: %w", err)
	}
	r.idle.Observe(now, agents)
	r.observed = true
	r.now = now
	if err := os.MkdirAll(filepath.Dir(r.seenPath), 0o700); err != nil {
		fmt.Fprintf(r.stderr, "%ssave idle times: %s\n", policyPrefix, err)
	} else if err := r.idle.SaveEvery(r.seenPath, now); err != nil {
		fmt.Fprintf(r.stderr, "%s%s\n", policyPrefix, err)
	}
	routed, err := hibernateHubRoute(ctx, r.cfg, r.env, io.Discard)
	if err != nil {
		return err
	}
	return r.policy.Tick(routed) //nolint:wrapcheck // Tick names what failed
}

// saveSeen saves the idle times on exit, unless the runner never read
// the panes.
func (r *policyRunner) saveSeen() {
	if !r.observed {
		return
	}
	if err := r.idle.Save(r.seenPath, r.now); err != nil {
		fmt.Fprintf(r.stderr, "%s%s\n", policyPrefix, err)
	}
}

// State is the hub's hibernate state, with Seen replaced by this
// machine's idle times under its hub handles.
func (r *policyRunner) State(ctx context.Context) (hibernate.Reply, error) {
	reply, err := busClient{env: r.env}.State(ctx)
	if err != nil {
		return reply, fmt.Errorf("hub %s: %w", r.cfg.Hub, err)
	}
	reply.Seen = map[string]hibernate.Seen{}
	for local, seen := range r.idle.Seen(r.now) {
		if handle, err := hubcall.OnMachine(local, r.machine); err == nil {
			reply.Seen[handle] = seen
		}
	}
	return reply, nil
}
