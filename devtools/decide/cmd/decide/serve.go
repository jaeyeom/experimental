package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"golang.org/x/sys/unix"
)

// hubInterval is how often a hub-mode inbox lists the hub's ledger.
const hubInterval = time.Second

// followEvery is how often the hub follows up a stale answer. The
// serve process does it, so the item does not have to be open.
const followEvery = time.Minute

// cmdHubInbox runs the inbox here against the hub's ledger, over one
// ssh session to `decide serve --stdio`. Links open here; ledger writes,
// notifications, and compose sends run on the hub.
func cmdHubInbox(settings hubSettings, args []string, stdout, stderr io.Writer) int {
	snoozed, utc, code, ok := inboxFlags(args, stderr)
	if !ok {
		return code
	}
	local, err := decide.LoadConfigFile()
	if err != nil {
		return fail(stderr, err, 1)
	}
	client := &inboxrpc.Client{
		Dial:       inboxrpc.HubDialer(settings.hub),
		Label:      settings.hub.Target,
		Machine:    settings.machine,
		HubMachine: settings.hubMachine,
	}
	if err := client.Connect(); err != nil {
		return fail(stderr, err, 1)
	}
	defer client.Close()
	opts := inboxOptions(local.Config, stdout, snoozed)
	opts.UTC = utc || local.Config.UI.UTC
	opts.Backend = client
	opts.Interval = hubInterval
	// The hub's display settings fill the keys this config does not
	// set, at start and after each reconnect.
	opts.Display = &hubDisplay{local: local, fetch: client.Display}
	opts.LocalConfig = local
	opts.BusRunner = client.BusRunner
	// The hub's own config maps its checkouts on the status board.
	opts.Checkouts = nil
	if err := runInbox(opts); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

// ledgerBackend is this machine's ledger with this machine's config.
func ledgerBackend() (*tui.LocalBackend, decide.Config, error) {
	dir, err := decide.DefaultDir()
	if err != nil {
		return nil, decide.Config{}, fmt.Errorf("decide directory: %w", err)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return nil, decide.Config{}, err //nolint:wrapcheck // LoadConfig names the config
	}
	return &tui.LocalBackend{
		Dir:          dir,
		SendTemplate: cfg.SendTemplate,
		Checkouts:    cfg.Checkouts,
		Machine:      decide.LocalMachine(cfg),
	}, cfg, nil
}

// cmdFollow is `decide follow`: the stale-answer follow-up and delivery
// retries on this machine's ledger, without an inbox or a hub client.
// It runs a pass every --every until SIGINT or SIGTERM, or one pass
// with --once.
func cmdFollow(args []string, _, stderr io.Writer) int {
	fs := flag.NewFlagSet("follow", flag.ContinueOnError)
	fs.SetOutput(stderr)
	every := fs.Duration("every", followEvery, "time between passes")
	once := fs.Bool("once", false, "run one pass and exit")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 || *every <= 0 {
		return fail(stderr, fmt.Errorf("decide follow takes [--every <duration>] [--once], with a positive duration"), 2)
	}
	backend, _, err := ledgerBackend()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *once {
		if err := backend.FollowDue(); err != nil {
			return fail(stderr, err, 1)
		}
		return 0
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	backend.FollowLoop(ctx, *every, func(err error) {
		fmt.Fprintf(stderr, "decide follow: %v\n", err)
	})
	return 0
}

// cmdServe is the hidden `decide serve --stdio`: the hub side of a
// hub-mode inbox. It answers JSON-line requests on stdin against this
// machine's ledger until stdin closes.
func cmdServe(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("serve", flag.ContinueOnError)
	fs.SetOutput(stderr)
	stdio := fs.Bool("stdio", false, "serve JSON lines on stdin and stdout")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if !*stdio || len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide serve needs --stdio and no arguments"), 2)
	}
	backend, cfg, err := ledgerBackend()
	if err != nil {
		return fail(stderr, err, 1)
	}
	server := &inboxrpc.Server{
		Backend:     backend,
		Targets:     cfg.Targets,
		Agentbus:    cfg.Recipients.Agentbus,
		FollowEvery: followEvery,
		Display: func() (decide.DisplaySettings, error) {
			f, err := decide.LoadConfigFile()
			if err != nil {
				return decide.DisplaySettings{}, err //nolint:wrapcheck // LoadConfigFile names the config
			}
			return f.Display(), nil
		},
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := server.Serve(ctx, os.Stdin, stdout); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}
