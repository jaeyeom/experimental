package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// memBusTimeout bounds the daemon request of mem.
const memBusTimeout = 2 * time.Second

// Tests replace the process table and the memory reading.
var (
	memProcs  = procinfo.LocalTable
	memMemory = procinfo.LocalMemory
)

// cmdMem reports each local agent pane's memory. It needs herdr; the
// daemon, or the hub in hub mode, only adds names, leases, and idle
// times, and is never started for it. In hub mode the idle times are
// the policy runner's.
func cmdMem(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	asJSON := len(args) == 1 && args[0] == "--json"
	if len(args) != 0 && !asJSON {
		fmt.Fprintf(stderr, "agentbus mem: unexpected arguments\n")
		return 2
	}
	in, err := memInput(ctx, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus mem: %s\n", err)
		return 1
	}
	report := memreport.Build(in)
	if asJSON {
		if err := json.NewEncoder(stdout).Encode(report); err != nil {
			fmt.Fprintf(stderr, "agentbus mem: %s\n", err)
			return 1
		}
		return 0
	}
	if err := report.WriteText(stdout); err != nil {
		fmt.Fprintf(stderr, "agentbus mem: %s\n", err)
		return 1
	}
	return 0
}

// memInput reads herdr once for the panes and once per agent pane for
// its processes, the process table once, the available memory, and the
// daemon's names, leases, and idle times.
func memInput(ctx context.Context, env []string) (memreport.Input, error) {
	ad := &herdr.Adapter{}
	agents, err := ad.Inventory(ctx)
	if err != nil {
		return memreport.Input{}, fmt.Errorf("herdr: %w", err)
	}
	in := memreport.Input{Agents: agents, Foreground: map[string][]int{}, InfoErrors: map[string]error{}, Now: time.Now()}
	for _, a := range agents {
		if a.Kind == "" {
			continue
		}
		info, err := ad.ProcessInfo(ctx, a.PaneID)
		if err != nil {
			in.InfoErrors[a.PaneID] = err
			continue
		}
		in.Foreground[a.PaneID] = info.ForegroundPIDs()
	}
	if in.Procs, err = memProcs().List(ctx); err != nil {
		return memreport.Input{}, fmt.Errorf("process table: %w", err)
	}
	if avail, err := memMemory().Available(ctx); err == nil {
		in.Available = &avail
	} else {
		in.Notes = append(in.Notes, "available memory unknown: "+err.Error())
	}
	cfg, err := hubSettings(env)
	if err != nil {
		return memreport.Input{}, err
	}
	in.Policy = memPolicy(cfg)
	in.Machine = cfg.Machine
	if in.Machine == "" {
		in.Machine, _ = os.Hostname()
	}
	if cfg.Hub != "" {
		if in.Idle, err = memPolicySeen(env, in.Now, agents); err != nil {
			in.Notes = append(in.Notes, "idle times unknown: "+err.Error())
		}
		bus, err := memHubBus(ctx, cfg, env)
		if err != nil {
			in.Notes = append(in.Notes, fmt.Sprintf("hub %s not reachable (%s); names and leases left out", cfg.Hub, err))
			return in, nil
		}
		in.Bus = bus
		return in, nil
	}
	bus, seen, err := memBus(ctx, env)
	if err != nil {
		in.Notes = append(in.Notes, fmt.Sprintf("agentbus daemon not reachable (%s); names, leases, and idle times left out", err))
		return in, nil
	}
	in.Bus = bus
	in.Idle = memIdle(seen)
	return in, nil
}

// memPolicySeen are the idle times the policy runner of a hub-mode
// client saved, for the agents herdr still reports in the state it saw.
// Without a runner there are none.
func memPolicySeen(env []string, now time.Time, agents []adapter.Agent) (map[string]memreport.Idle, error) {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return nil, fmt.Errorf("%w", err)
	}
	seen, err := hibernate.LoadSeen(filepath.Join(dirs.StateDir, policySeenFile), now, agents)
	if err != nil {
		return nil, fmt.Errorf("%w", err)
	}
	return memIdle(seen), nil
}

func memIdle(seen map[string]hibernate.Seen) map[string]memreport.Idle {
	out := make(map[string]memreport.Idle, len(seen))
	for handle, s := range seen {
		out[handle] = memreport.Idle{Status: s.Status, For: s.For, LowerBound: s.LowerBound}
	}
	return out
}

// memPolicy is the hibernation policy this machine's config sets. It
// runs in agentbusd, or in hub mode in `agentbus hibernate --policy`.
func memPolicy(cfg config.Config) *memreport.Policy {
	p := cfg.Policy()
	if p.Mode == "off" {
		return &memreport.Policy{Mode: p.Mode}
	}
	return &memreport.Policy{
		Mode: p.Mode, BelowBytes: p.Below, TargetBytes: p.Target,
		MinIdleSeconds: int64(p.MinIdle / time.Second),
	}
}

// memHubBus asks the hub for its names and lease table, over the tunnel
// when it answers, else over ssh, within the hook deadline so a slow
// hub never stalls mem. It keeps only this machine's handles, in the
// herdr:local/... form of the local panes. It drops the hub's idle
// times, which are not this machine's.
func memHubBus(ctx context.Context, cfg config.Config, env []string) (*memreport.Bus, error) {
	machine, err := leaseMachine(cfg, env)
	if err != nil {
		return nil, err
	}
	total := hubHookTimeout(env)
	ctx, cancel := context.WithTimeout(ctx, total)
	defer cancel()
	var bus *memreport.Bus
	routed, ok, err := openTunnel(ctx, cfg, "mem", env, io.Discard)
	switch {
	case err != nil:
		return nil, err
	case ok:
		bus, _, err = memBus(routed, env)
	default:
		bus, err = memSSHBus(ctx, cfg, total)
	}
	if err != nil {
		return nil, err
	}
	return machineBus(bus, machine), nil
}

// memSSHBus runs the hub's `list --json` and `lease status --all
// --json` over ssh, side by side.
func memSSHBus(ctx context.Context, cfg config.Config, total time.Duration) (*memreport.Bus, error) {
	hub := hookHub(hubcall.Hub{Target: cfg.Hub, Tool: "agentbus"}, total)
	run := func(args ...string) ([]byte, error) {
		var stdout, stderr bytes.Buffer
		code, err := hub.Run(ctx, hubcall.Call{Args: args, Stdout: &stdout, Stderr: &stderr})
		switch {
		case err != nil:
			return nil, fmt.Errorf("%w", err)
		case ctx.Err() != nil:
			return nil, fmt.Errorf("agentbus %s: %w", strings.Join(args, " "), ctx.Err())
		case code != 0:
			return nil, fmt.Errorf("agentbus %s exited %d: %s", strings.Join(args, " "), code, strings.TrimSpace(stderr.String()))
		}
		return stdout.Bytes(), nil
	}
	var (
		listOut []byte
		listErr error
		done    = make(chan struct{})
	)
	go func() {
		defer close(done)
		listOut, listErr = run("list", "--json")
	}()
	leaseOut, leaseErr := run("lease", "status", "--all", "--json")
	<-done
	if listErr != nil {
		return nil, listErr
	}
	if leaseErr != nil {
		return nil, leaseErr
	}
	var entries []listEntry
	if err := json.Unmarshal(listOut, &entries); err != nil {
		return nil, fmt.Errorf("bad list reply: %w", err)
	}
	bus := &memreport.Bus{Names: map[string][]string{}}
	for _, e := range entries {
		if e.Name != "" {
			bus.Names[e.Handle] = append(bus.Names[e.Handle], e.Name)
		}
	}
	if err := json.Unmarshal(leaseOut, &bus.Leases); err != nil {
		return nil, fmt.Errorf("bad lease status reply: %w", err)
	}
	return bus, nil
}

// machineBus keeps the names and lease roles of machine's handles on
// the hub, rewritten to the herdr:local/... handles of this machine's
// panes. The hub's own herdr:local/... panes are dropped.
func machineBus(bus *memreport.Bus, machine string) *memreport.Bus {
	local := func(handle string) (string, bool) {
		m, session, pane, err := hubcall.ParseHandle(handle)
		if err != nil || m != machine {
			return "", false
		}
		return hubcall.Handle(hubcall.LocalMachine, session, pane), true
	}
	out := &memreport.Bus{Names: map[string][]string{}}
	for handle, names := range bus.Names {
		if l, ok := local(handle); ok {
			out.Names[l] = append(out.Names[l], names...)
		}
	}
	for _, s := range bus.Leases {
		if s.Holder != nil {
			holder := *s.Holder
			s.Holder = nil
			if l, ok := local(holder.Handle); ok {
				holder.Handle = l
				s.Holder = &holder
			}
		}
		queue := s.Queue
		s.Queue = nil
		for _, w := range queue {
			if l, ok := local(w.Handle); ok {
				w.Handle = l
				s.Queue = append(s.Queue, w)
			}
		}
		out.Leases = append(out.Leases, s)
	}
	return out
}

// memBus asks the daemon dialBus reaches, the local one or the hub over
// the tunnel, for its hibernate state: the registered names, the lease
// table, and the idle times hibernate --idle-over reads.
func memBus(ctx context.Context, env []string) (*memreport.Bus, map[string]hibernate.Seen, error) {
	ctx, cancel := context.WithTimeout(ctx, memBusTimeout)
	defer cancel()
	resp, err := dialBus(ctx, env, daemon.Request{Op: "hibernate-state"})
	if err != nil {
		return nil, nil, err
	}
	if !resp.OK {
		return nil, nil, fmt.Errorf("hibernate-state: %s", resp.Error)
	}
	bus := &memreport.Bus{Names: map[string][]string{}}
	if resp.Hibernate == nil {
		return bus, nil, nil
	}
	for handle, names := range resp.Hibernate.Names {
		bus.Names[handle] = append([]string{}, names...)
	}
	bus.Leases = resp.Hibernate.Leases
	return bus, resp.Hibernate.Seen, nil
}
