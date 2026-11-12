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
	opts.BusRunner = client.BusRunner
	// The hub's own config maps its checkouts on the status board.
	opts.Checkouts = nil
	if err := runInbox(opts); err != nil {
		return fail(stderr, err, 1)
	}
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
	dir, err := decide.DefaultDir()
	if err != nil {
		return fail(stderr, fmt.Errorf("decide directory: %w", err), 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	server := &inboxrpc.Server{
		Backend: &tui.LocalBackend{
			Dir:          dir,
			SendTemplate: cfg.SendTemplate,
			Checkouts:    cfg.Checkouts,
			Machine:      decide.LocalMachine(cfg),
		},
		Targets:  cfg.Targets,
		Agentbus: cfg.Recipients.Agentbus,
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
