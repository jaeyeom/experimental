// Package main implements gh-approve.
//
// gh-approve approves, as the logged-in gh user, the pull requests that
// request that user as an individual reviewer and match an approve rule in
// the gh-nudge config. Everything else is left for a human. A request only to
// a team the user belongs to is ignored. It never approves a pull request the
// user authored.
//
// Usage:
//
//	# Dry run: print what would be approved. Nothing is approved.
//	gh-approve
//
//	# Approve for real
//	gh-approve --apply
//
//	# Evaluate one pull request and print every condition
//	gh-approve --pr acme/widgets#12 --explain
//
// Every decision is appended to ~/.config/gh-nudge/approve-log.jsonl.
package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/approve"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	executor "github.com/jaeyeom/go-cmdexec"
)

// openAuditLog opens the audit log under home for appending.
func openAuditLog(home string) (*os.File, string, error) {
	dir := filepath.Join(home, ".config", "gh-nudge")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, "", fmt.Errorf("create %s: %w", dir, err)
	}
	path := filepath.Join(dir, "approve-log.jsonl")
	file, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return nil, "", fmt.Errorf("open audit log: %w", err)
	}
	return file, path, nil
}

func main() {
	var (
		configPath string
		opts       approve.Options
	)
	flag.StringVar(&configPath, "config", "", "Path to config file (default ~/.config/gh-nudge/config.pkl)")
	flag.BoolVar(&opts.Apply, "apply", false, "Approve for real. Without it, gh-approve is a dry run")
	flag.StringVar(&opts.PR, "pr", "", "Evaluate one pull request: owner/repo#N or a URL")
	flag.BoolVar(&opts.Explain, "explain", false, "Print every condition of every rule with the value it saw")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [--apply] [--pr <owner/repo#N | URL>] [--explain] [--config <path>]\n\n", os.Args[0])
		flag.PrintDefaults()
	}
	flag.Parse()
	if flag.NArg() > 0 {
		flag.Usage()
		os.Exit(2)
	}
	if err := run(configPath, opts); err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		os.Exit(1)
	}
}

func run(configPath string, opts approve.Options) (err error) {
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		return fmt.Errorf("load config: %w", err)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return fmt.Errorf("find home directory: %w", err)
	}
	logFile, _, err := openAuditLog(home)
	if err != nil {
		return err
	}
	defer func() {
		if closeErr := logFile.Close(); closeErr != nil && err == nil {
			err = fmt.Errorf("close audit log: %w", closeErr)
		}
	}()
	runner := &approve.Runner{
		Client: github.NewClient(context.Background(), executor.NewBasicExecutor()),
		Config: cfg.Approve,
		Out:    os.Stdout,
		Log:    logFile,
		Now:    time.Now,
	}
	if err := runner.Run(opts); err != nil {
		return fmt.Errorf("run: %w", err)
	}
	return nil
}
