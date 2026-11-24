package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"github.com/jaeyeom/experimental/devtools/decide/web"
	"golang.org/x/sys/unix"
)

// cmdWeb is `decide web`: the HTTP server for the web inbox, on this
// machine's ledger, or `decide web setup` for the interactive setup wizard.
func cmdWeb(args []string, stdout, stderr io.Writer) int {
	if len(args) > 0 && args[0] == "setup" {
		return cmdWebSetup(args[1:], os.Stdin, stdout, stderr)
	}
	fs := flag.NewFlagSet("web", flag.ContinueOnError)
	fs.SetOutput(stderr)
	addr := fs.String("addr", "", "listen address (default web.addr, else "+decide.DefaultWebAddr+")")
	tokenFile := fs.String("token-file", "", "token auth, with the token read from (or created at) this file")
	insecure := fs.Bool("allow-insecure-unauth", false, "allow auth mode none on a non-loopback address")
	noFollow := fs.Bool("no-follow", false, "do not follow up stale answers")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide web takes no arguments"), 2)
	}
	backend, cfg, err := ledgerBackend()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *addr != "" {
		cfg.Web.Addr = *addr
	}
	if *tokenFile != "" {
		cfg.Web.Auth.Mode = decide.WebAuthToken
		cfg.Web.Auth.TokenFile = *tokenFile
	}
	s := &web.Server{
		Backend:             backend,
		Config:              cfg,
		Display:             localDisplay,
		OrgConfig:           localOrgConfig,
		Orgctl:              tui.ExecOrgctl,
		Targets:             cfg.Targets,
		Agentbus:            cfg.Recipients.Agentbus,
		AllowInsecureUnauth: *insecure,
		Report:              func(err error) { fmt.Fprintf(stderr, "decide web: %v\n", err) },
	}
	if !*noFollow {
		s.FollowEvery = followEvery
	}
	ln, err := s.Listen()
	if err != nil {
		return fail(stderr, err, 1)
	}
	defer ln.Close()
	fmt.Fprintf(stdout, "decide web: listening on http://%s\n", ln.Addr())
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := s.Serve(ctx, ln); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

func cmdWebSetup(args []string, stdin io.Reader, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("web setup", flag.ContinueOnError)
	fs.SetOutput(stderr)
	yes := fs.Bool("yes", false, "accept every step except sudo commands")
	dryRun := fs.Bool("dry-run", false, "print the setup plan without making changes")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide web setup takes no arguments"), 2)
	}
	env := web.SetupEnv{
		Stdin:  stdin,
		Stdout: stdout,
		Stderr: stderr,
	}
	if err := web.RunSetup(context.Background(), env, web.SetupOptions{
		Yes:    *yes,
		DryRun: *dryRun,
	}); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}
