package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// Tests replace the process table, the cwd reader, and the clock.
var (
	hibernateProcs = func(ctx context.Context) ([]procinfo.Process, error) {
		return procinfo.LocalTable().List(ctx) //nolint:wrapcheck // hibernate names the process table
	}
	hibernateCwd                   = procinfo.CwdFunc(procinfo.LocalCwd)
	hibernateClock hibernate.Clock = hibernate.RealClock{}
)

// cmdHibernate stops idle agents on this machine and keeps their panes.
// It runs here even in hub mode: it reads this machine's panes and
// processes. Only its daemon requests go to the hub, which owns the
// names, the leases, and the records, over the tunnel when the hub
// answers there, else over ssh through `agentbus lease forward`.
func cmdHibernate(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) > 0 && args[0] == "--policy" {
		if len(args) > 1 {
			fmt.Fprintf(stderr, "agentbus hibernate: --policy takes no other arguments\n")
			return 2
		}
		return cmdHibernatePolicy(ctx, env, stderr)
	}
	req, asJSON, err := parseHibernate(args, stderr)
	if err != nil {
		if !errors.Is(err, flag.ErrHelp) {
			fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
		}
		return 2
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
		return 1
	}
	machine := ""
	if cfg.Hub != "" {
		if machine, err = leaseMachine(cfg, env); err != nil {
			fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
			return 1
		}
		if ctx, err = hibernateHubRoute(ctx, cfg, env, stderr); err != nil {
			fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
			return 1
		}
	}
	results, err := newHibernator(cfg, env, machine).Run(ctx, req)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
		return 1
	}
	if err := writeHibernate(stdout, results, asJSON); err != nil {
		fmt.Fprintf(stderr, "agentbus hibernate: %s\n", err)
		return 1
	}
	return hibernateCode(req, results)
}

// newHibernator stops this machine's agents, with daemon requests sent
// through dialBus. machine is empty unless in hub mode.
func newHibernator(cfg config.Config, env []string, machine string) *hibernate.Hibernator {
	return &hibernate.Hibernator{
		Panes:   &herdr.Adapter{},
		Bus:     busClient{env: env},
		Procs:   hibernateProcs,
		Cwd:     hibernateCwd,
		Clock:   hibernateClock,
		Machine: machine,
		Exempt:  cfg.HibernateExempt,
		Self:    os.Getpid(),
	}
}

func parseHibernate(args []string, stderr io.Writer) (hibernate.Request, bool, error) {
	fs := flag.NewFlagSet("hibernate", flag.ContinueOnError)
	fs.SetOutput(stderr)
	idleOver := fs.Duration("idle-over", 0, "hibernate every agent idle at least this long")
	dryRun := fs.Bool("dry-run", false, "report what would happen and change nothing")
	asJSON := fs.Bool("json", false, "print a JSON array, one object per agent")
	targets, err := parseInterspersed(fs, args)
	if err != nil {
		return hibernate.Request{}, false, err
	}
	req := hibernate.Request{Targets: targets, IdleOver: *idleOver, DryRun: *dryRun}
	switch {
	case *idleOver < 0:
		return req, false, errors.New("--idle-over must not be negative")
	case len(req.Targets) > 0 && *idleOver > 0:
		return req, false, errors.New("give names or handles, or --idle-over, not both")
	case len(req.Targets) == 0 && *idleOver == 0:
		return req, false, errors.New("usage: agentbus hibernate <name|handle>... | --idle-over <duration> [--dry-run] [--json]")
	}
	return req, *asJSON, nil
}

func writeHibernate(w io.Writer, results []hibernate.Result, asJSON bool) error {
	if asJSON {
		if results == nil {
			results = []hibernate.Result{}
		}
		if err := json.NewEncoder(w).Encode(results); err != nil {
			return fmt.Errorf("write results: %w", err)
		}
		return nil
	}
	for _, r := range results {
		line := r.Line()
		if r.Warning != "" {
			line += " (warning: " + r.Warning + ")"
		}
		if _, err := fmt.Fprintln(w, line); err != nil {
			return fmt.Errorf("write results: %w", err)
		}
	}
	return nil
}

// hibernateCode is 1 when a stop failed or a named agent was refused.
// With --idle-over a refusal is expected and still exits 0.
func hibernateCode(req hibernate.Request, results []hibernate.Result) int {
	for _, r := range results {
		if r.Outcome == hibernate.OutcomeFailed || (r.Outcome == hibernate.OutcomeRefused && len(req.Targets) > 0) {
			return 1
		}
	}
	return 0
}

// hibernateHubRoute returns ctx routed to the hub: the tunnel when it
// answers, else ssh, carrying this pane's handle when the caller is an
// agent pane, and none for the user in a plain shell. Inside a sandbox,
// a tunnel that is there but fails is an error instead.
func hibernateHubRoute(ctx context.Context, cfg config.Config, env []string, stderr io.Writer) (context.Context, error) {
	routed, ok, err := openTunnel(ctx, cfg, "hibernate", env, stderr)
	if err != nil {
		return ctx, err
	}
	if ok {
		return routed, nil
	}
	r := sshRoute{hub: agentHub(cfg)}
	if envGet(env, "HERDR_PANE_ID") != "" {
		if caller, err := callerHandle(ctx, cfg, env); err == nil {
			r.handle = caller
		}
	}
	return context.WithValue(ctx, sshRouteKey{}, r), nil
}

// busClient sends hibernate's daemon requests through dialBus: to the
// local daemon, or to the hub when the request's ctx is routed there. It
// never starts a daemon.
type busClient struct {
	env []string
}

// hibernateBusTimeout bounds one daemon request.
const hibernateBusTimeout = 30 * time.Second

func (b busClient) call(ctx context.Context, op string, args *hibernate.Args) (hibernate.Reply, error) {
	ctx, cancel := context.WithTimeout(ctx, hibernateBusTimeout)
	defer cancel()
	resp, err := dialBus(ctx, b.env, daemon.Request{Op: op, Env: b.env, Hibernate: args})
	if err != nil {
		return hibernate.Reply{}, fmt.Errorf("%s: %w", op, err)
	}
	if !resp.OK {
		return hibernate.Reply{}, errors.New(resp.Error)
	}
	if resp.Hibernate == nil {
		return hibernate.Reply{}, nil
	}
	return *resp.Hibernate, nil
}

func (b busClient) State(ctx context.Context) (hibernate.Reply, error) {
	return b.call(ctx, "hibernate-state", nil)
}

func (b busClient) Begin(ctx context.Context, rec hibernate.Record) (hibernate.Record, error) {
	r, err := b.call(ctx, "hibernate-begin", &hibernate.Args{Record: &rec})
	if err != nil {
		return hibernate.Record{}, err
	}
	if r.Record != nil {
		return *r.Record, nil
	}
	return rec, nil
}

func (b busClient) End(ctx context.Context, handle, status, reason string) error {
	args := &hibernate.Args{Handle: handle, Status: status}
	if reason != "" {
		args.Reasons = []string{reason}
	}
	_, err := b.call(ctx, "hibernate-end", args)
	return err
}

func (b busClient) Refused(ctx context.Context, handle string, reasons []string) error {
	_, err := b.call(ctx, "hibernate-refused", &hibernate.Args{Handle: handle, Reasons: reasons})
	return err
}
