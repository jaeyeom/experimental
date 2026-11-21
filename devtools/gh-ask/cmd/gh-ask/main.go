// Command gh-ask runs a loopback server that hands a question about a
// pull request line range to a configured command.
//
//	gh-ask token [--config PATH]   create the shared secret if missing and print it
//	gh-ask serve [--config PATH]   serve POST /ask, GET /status/<id> and GET /thread
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"net"
	"net/http"
	"os"
	"os/signal"
	"time"

	ghask "github.com/jaeyeom/experimental/devtools/gh-ask"
	"golang.org/x/sys/unix"
)

const usage = `usage:
  gh-ask token [--config PATH]   create the shared secret if missing and print it
  gh-ask serve [--config PATH]   serve POST /ask, GET /status/<id> and GET /thread on the configured 127.0.0.1 port

The default config is $XDG_CONFIG_HOME/gh-ask/config.toml, or
$HOME/.config/gh-ask/config.toml.
`

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, os.Environ()))
}

func run(args []string, stdout, stderr io.Writer, env []string) int {
	if code, ok := newGHAskTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
	if len(args) == 0 {
		fmt.Fprint(stderr, usage)
		return 2
	}
	switch args[0] {
	case "token":
		return cmdToken(args[1:], stdout, stderr, env)
	case "serve":
		return cmdServe(args[1:], stderr, env)
	case "help", "-h", "--help":
		fmt.Fprint(stdout, usage)
		return 0
	default:
		fmt.Fprintf(stderr, "gh-ask: unknown command %q\n%s", args[0], usage)
		return 2
	}
}

func parseConfigFlag(name string, args []string, stderr io.Writer, env []string) (string, bool) {
	fs := flag.NewFlagSet("gh-ask "+name, flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return "", false
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(stderr, "gh-ask %s: unexpected argument %q\n", name, fs.Arg(0))
		return "", false
	}
	if *configPath != "" {
		return *configPath, true
	}
	path, err := ghask.DefaultConfigPath(env)
	if err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return "", false
	}
	return path, true
}

func cmdToken(args []string, stdout, stderr io.Writer, env []string) int {
	configPath, ok := parseConfigFlag("token", args, stderr, env)
	if !ok {
		return 2
	}
	tokenFile := ghask.ResolveTokenFile(configPath, "")
	cfg, err := ghask.LoadConfig(configPath)
	switch {
	case err == nil:
		tokenFile = cfg.TokenFile
	case !errors.Is(err, fs.ErrNotExist):
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 2
	}
	token, err := ghask.EnsureToken(tokenFile)
	if err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 1
	}
	fmt.Fprintln(stdout, token)
	return 0
}

func cmdServe(args []string, stderr io.Writer, env []string) int {
	configPath, ok := parseConfigFlag("serve", args, stderr, env)
	if !ok {
		return 2
	}
	cfg, err := ghask.LoadConfig(configPath)
	if err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 2
	}
	token, err := ghask.ReadToken(cfg.TokenFile)
	if err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v (run gh-ask token first)\n", err)
		return 1
	}
	if cfg.StateFile, err = ghask.DefaultStatePath(env); err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 2
	}
	ln, err := ghask.Listen(cfg.Listen)
	if err != nil {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 1
	}
	addr, ok := ln.Addr().(*net.TCPAddr)
	if !ok {
		_ = ln.Close()
		fmt.Fprintf(stderr, "gh-ask: unexpected listener address %v\n", ln.Addr())
		return 1
	}
	srv := &http.Server{
		Handler:           ghask.NewServer(cfg, token, addr.Port),
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:       30 * time.Second,
		WriteTimeout:      cfg.CommandTimeout + 30*time.Second,
		MaxHeaderBytes:    16 << 10,
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, unix.SIGTERM)
	defer stop()
	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), cfg.CommandTimeout+5*time.Second)
		defer cancel()
		_ = srv.Shutdown(shutdownCtx)
	}()
	fmt.Fprintf(stderr, "gh-ask: listening on %s\n", ln.Addr())
	if err := srv.Serve(ln); err != nil && !errors.Is(err, http.ErrServerClosed) {
		fmt.Fprintf(stderr, "gh-ask: %v\n", err)
		return 1
	}
	return 0
}
