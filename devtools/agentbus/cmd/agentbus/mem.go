package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// memBusTimeout bounds the two daemon requests of mem.
const memBusTimeout = 2 * time.Second

// Tests replace the process table and the memory reading.
var (
	memProcs  = procinfo.LocalTable
	memMemory = procinfo.LocalMemory
)

// cmdMem reports each local agent pane's memory. It needs herdr; the
// daemon only adds names and leases, and is never started for it.
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
// daemon's names and leases.
func memInput(ctx context.Context, env []string) (memreport.Input, error) {
	ad := &herdr.Adapter{}
	agents, err := ad.Inventory(ctx)
	if err != nil {
		return memreport.Input{}, fmt.Errorf("herdr: %w", err)
	}
	in := memreport.Input{Agents: agents, Foreground: map[string][]int{}, InfoErrors: map[string]error{}}
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
	in.Machine = cfg.Machine
	if in.Machine == "" {
		in.Machine, _ = os.Hostname()
	}
	if cfg.Hub != "" {
		in.Notes = append(in.Notes, fmt.Sprintf("hub mode: names and leases are on the hub %s; left out", cfg.Hub))
		return in, nil
	}
	bus, err := memBus(ctx, env)
	if err != nil {
		in.Notes = append(in.Notes, fmt.Sprintf("agentbus daemon not reachable (%s); names and leases left out", err))
		return in, nil
	}
	in.Bus = bus
	return in, nil
}

// memBus asks a running daemon for the registered names and the lease
// table.
func memBus(ctx context.Context, env []string) (*memreport.Bus, error) {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return nil, fmt.Errorf("locate the daemon: %w", err)
	}
	ctx, cancel := context.WithTimeout(ctx, memBusTimeout)
	defer cancel()
	ask := func(req daemon.Request) (daemon.Response, error) {
		resp, err := dialDaemon(ctx, dirs.SocketPath, req)
		if err != nil {
			return resp, err
		}
		if !resp.OK {
			return resp, fmt.Errorf("%s: %s", req.Op, resp.Error)
		}
		return resp, nil
	}
	list, err := ask(daemon.Request{Op: "list"})
	if err != nil {
		return nil, err
	}
	bus := &memreport.Bus{Names: map[string][]string{}}
	for _, row := range list.Rows {
		if row.Name != "" {
			bus.Names[row.Handle] = append(bus.Names[row.Handle], row.Name)
		}
	}
	status, err := ask(daemon.Request{Op: "lease-status", Lease: &lease.Args{All: true}})
	if err != nil {
		return nil, err
	}
	if status.Lease != nil {
		bus.Leases = status.Lease.Leases
	}
	return bus, nil
}
