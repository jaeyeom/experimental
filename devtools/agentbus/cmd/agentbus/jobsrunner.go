package main

import (
	"bytes"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os/exec"
	"path/filepath"
	"strings"

	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/runner"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// jobsRunner runs `agentbus jobs runner` until ctx ends: SIGINT or
// SIGTERM from the user's service manager.
func jobsRunner(ctx context.Context, args, env []string, stderr io.Writer) int {
	fs := flag.NewFlagSet("jobs runner", flag.ContinueOnError)
	fs.SetOutput(stderr)
	name := fs.String("name", "", "the runner's name in the queue: its unit name")
	machine := fs.String("machine", "", "this machine (default: machine in config.toml in hub mode, else local)")
	path := fs.String("config", "", "the runner config (default: runner.json beside config.toml)")
	dryRun := fs.Bool("dry-run", false, "log what the runner would claim and run, and change nothing")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if *name == "" || fs.NArg() > 0 {
		fmt.Fprintln(stderr, "agentbus jobs runner: want --name and no other arguments")
		return 2
	}
	if *path == "" {
		p, err := config.Path(env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus jobs runner: %s\n", err)
			return 1
		}
		*path = filepath.Join(filepath.Dir(p), "runner.json")
	}
	cfg, err := runner.LoadConfig(*path)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus jobs runner: %s\n", err)
		return 1
	}
	if *machine == "" {
		hub, err := hubSettings(env)
		if err == nil {
			*machine, err = leaseMachine(hub, env)
		}
		if err != nil {
			fmt.Fprintf(stderr, "agentbus jobs runner: %s\n", err)
			return 1
		}
	}
	if cfg.Start.DiskPath == "" {
		cfg.Start.DiskPath = envGet(env, "HOME")
	}
	r := &runner.Runner{
		Name: *name, Machine: *machine, Config: cfg, DryRun: *dryRun, Log: stderr,
		Queue: busQueue{env: env, runner: *name, machine: *machine},
		Host:  localHost{env: env, out: stderr},
		Panes: busPanes{env: env},
	}
	mode := ""
	if *dryRun {
		mode = " (dry-run)"
	}
	fmt.Fprintf(stderr, "agentbus jobs runner %s on %s%s: kinds %s\n", *name, *machine, mode, strings.Join(cfg.KindNames(), ", "))
	if err := r.Run(ctx); err != nil {
		fmt.Fprintf(stderr, "agentbus jobs runner: %s\n", err)
		return 1
	}
	return 0
}

// busQueue is the job queue in the daemon, as runner on machine.
type busQueue struct {
	env     []string
	runner  string
	machine string
}

func (q busQueue) call(ctx context.Context, op string, a jobs.Args) (jobs.Reply, error) {
	var errs bytes.Buffer
	resp, code := callDaemon(ctx, q.env, daemon.Request{Op: "jobs-" + op, Env: q.env, Jobs: &a}, &errs)
	switch {
	case code != 0:
		return jobs.Reply{}, errors.New(strings.TrimSpace(errs.String()))
	case !resp.OK || resp.Jobs == nil:
		return jobs.Reply{}, errors.New(resp.Error)
	}
	return *resp.Jobs, nil
}

func (q busQueue) Claim(ctx context.Context, kinds []string) (*jobs.Job, bool, string, error) {
	r, err := q.call(ctx, "claim", jobs.Args{Runner: q.runner, Machine: q.machine, Kinds: kinds})
	return r.Job, r.Held, r.HoldNote, err
}

func (q busQueue) Beat(ctx context.Context) error {
	_, err := q.call(ctx, "beat", jobs.Args{Runner: q.runner, Machine: q.machine})
	return err
}

func (q busQueue) Heartbeat(ctx context.Context, id string) error {
	_, err := q.call(ctx, "heartbeat", jobs.Args{ID: id, Runner: q.runner})
	return err
}

func (q busQueue) Done(ctx context.Context, id, note string) error {
	_, err := q.call(ctx, "done", jobs.Args{ID: id, Runner: q.runner, Note: note})
	return err
}

func (q busQueue) Fail(ctx context.Context, id string, exit *int, note string) error {
	_, err := q.call(ctx, "fail", jobs.Args{ID: id, Runner: q.runner, Exit: exit, Note: note})
	return err
}

func (q busQueue) Gate(ctx context.Context, id, note string) error {
	_, err := q.call(ctx, "gate", jobs.Args{ID: id, Runner: q.runner, Note: note})
	return err
}

func (q busQueue) Show(ctx context.Context, id string) (jobs.Job, error) {
	r, err := q.call(ctx, "show", jobs.Args{ID: id})
	if err != nil || r.Job == nil {
		return jobs.Job{}, err
	}
	return *r.Job, nil
}

func (q busQueue) List(ctx context.Context, state jobs.State, kind string) ([]jobs.Job, error) {
	r, err := q.call(ctx, "ls", jobs.Args{State: string(state), Kind: kind})
	return r.Jobs, err
}

func (q busQueue) Held(ctx context.Context) (bool, string, error) {
	r, err := q.call(ctx, "stats", jobs.Args{})
	if err != nil || r.Stats == nil {
		return false, "", err
	}
	return r.Stats.Held, r.Stats.HoldNote, nil
}

// localHost runs commands and reads checkouts and disks on this
// machine. Command output goes to the runner's log.
type localHost struct {
	env []string
	out io.Writer
}

func (h localHost) Run(ctx context.Context, argv []string, dir string, env []string) (int, error) {
	// The user's own runner config names the command.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // see above
	cmd.Dir = dir
	cmd.Env = append(append([]string{}, h.env...), env...)
	cmd.Stdout, cmd.Stderr = h.out, h.out
	err := cmd.Run()
	var exit *exec.ExitError
	if errors.As(err, &exit) && ctx.Err() == nil {
		return exit.ExitCode(), nil
	}
	if err != nil {
		return -1, fmt.Errorf("run %s: %w", argv[0], err)
	}
	return 0, nil
}

func (h localHost) Dirty(ctx context.Context, checkout string) (bool, error) {
	out, err := exec.CommandContext(ctx, "git", "-C", checkout, "status", "--porcelain").Output()
	if err != nil {
		return false, fmt.Errorf("git status: %w", err)
	}
	return len(bytes.TrimSpace(out)) > 0, nil
}

func (h localHost) FreeDisk(path string) (uint64, error) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return 0, fmt.Errorf("statfs %s: %w", path, err)
	}
	return st.Bavail * uint64(st.Bsize), nil //nolint:gosec // block size is positive
}

// busPanes reads a tab job's target through herdr and prompts it with
// an agentbus message: the runner never types into a pane.
type busPanes struct{ env []string }

func (p busPanes) handle(ctx context.Context, target string) (string, error) {
	if _, _, _, err := hubcall.ParseHandle(target); err == nil {
		return target, nil
	}
	var errs bytes.Buffer
	resp, code := callDaemon(ctx, p.env, daemon.Request{Op: "list", Env: p.env}, &errs)
	if code != 0 {
		return "", errors.New(strings.TrimSpace(errs.String()))
	}
	for _, row := range resp.Rows {
		if row.Name == target && row.Handle != "" {
			return row.Handle, nil
		}
	}
	return "", fmt.Errorf("no agent named %s", target)
}

func (p busPanes) Dialog(ctx context.Context, target string) (string, error) {
	h, err := p.handle(ctx, target)
	if err != nil {
		return "", err
	}
	panes := &herdr.Adapter{}
	a, ok, err := panes.PaneAgent(ctx, h)
	switch {
	case err != nil:
		return "", fmt.Errorf("read %s: %w", h, err)
	case !ok:
		return "", fmt.Errorf("no agent in the pane of %s", h)
	case a.Status == "blocked":
		return "blocked agent waiting on input", nil
	}
	if m, _, _, _ := hubcall.ParseHandle(h); m != hubcall.LocalMachine {
		return "", nil
	}
	screen, err := panes.Screen(ctx, a.PaneID)
	if err != nil {
		return "", fmt.Errorf("read the screen of %s: %w", h, err)
	}
	dialog, _ := herdr.PendingDialog(a.Kind, screen)
	return dialog, nil
}

func (p busPanes) Live(ctx context.Context, target string) (bool, error) {
	h, err := p.handle(ctx, target)
	if err != nil {
		return false, err
	}
	_, ok, err := (&herdr.Adapter{}).PaneAgent(ctx, h)
	if err != nil {
		return false, fmt.Errorf("read %s: %w", h, err)
	}
	return ok, nil
}

func (p busPanes) Prompt(ctx context.Context, target, key, text string) error {
	var errs bytes.Buffer
	resp, code := callDaemon(ctx, p.env, daemon.Request{Op: "send", Name: target, Body: text, Env: p.env, ClientID: key, LogName: target}, &errs)
	switch {
	case code != 0:
		return errors.New(strings.TrimSpace(errs.String()))
	case !resp.OK:
		return errors.New(resp.Error)
	}
	return nil
}
