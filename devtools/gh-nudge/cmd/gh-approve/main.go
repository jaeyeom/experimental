// Package main implements gh-approve.
//
// gh-approve approves, as the logged-in gh user, the pull requests that
// request that user as an individual reviewer and match an approve rule in
// the gh-nudge config. Everything else is left for a human. A request only to
// a team the user belongs to is ignored. It never approves a pull request the
// user authored.
//
// A rule with action = "handoff" never approves. It requests the rule's
// reviewers, then removes the user as an individual requested reviewer, at
// most once per pull request.
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
//
// Agent command decisions are cached under $XDG_CACHE_HOME/gh-approve/agent
// (default ~/.cache/gh-approve/agent). --no-agent-cache bypasses the cache.
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

// newAgentCache returns the agent decision cache, or nil when disabled.
// It lives under xdgCacheHome, or home/.cache when xdgCacheHome is empty.
func newAgentCache(cfg config.ApproveConfig, disabled bool, home, xdgCacheHome string) *approve.AgentCache {
	if disabled {
		return nil
	}
	base := xdgCacheHome
	if base == "" {
		base = filepath.Join(home, ".cache")
	}
	return &approve.AgentCache{
		Dir:      filepath.Join(base, "gh-approve", "agent"),
		TTL:      cfg.AgentCacheTTL,
		MaxBytes: int64(cfg.AgentCacheMaxMB) << 20,
		Now:      time.Now,
	}
}

func main() {
	var (
		configPath   string
		opts         approve.Options
		noAgentCache bool
	)
	flag.StringVar(&configPath, "config", "", "Path to config file (default ~/.config/gh-nudge/config.pkl)")
	flag.BoolVar(&opts.Apply, "apply", false, "Approve and hand off for real. Without it, gh-approve is a dry run")
	flag.StringVar(&opts.PR, "pr", "", "Evaluate one pull request: owner/repo#N or a URL")
	flag.BoolVar(&opts.Explain, "explain", false, "Print every condition of every rule with the value it saw")
	flag.BoolVar(&noAgentCache, "no-agent-cache", false, "Run agent commands without reading or writing the decision cache")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [--apply] [--pr <owner/repo#N | URL>] [--explain] [--no-agent-cache] [--config <path>]\n\n", os.Args[0])
		flag.PrintDefaults()
	}
	flag.Parse()
	if flag.NArg() > 0 {
		flag.Usage()
		os.Exit(2)
	}
	if err := run(configPath, opts, noAgentCache); err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		os.Exit(1)
	}
}

func run(configPath string, opts approve.Options, noAgentCache bool) (err error) {
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
		Client:     github.NewClient(context.Background(), github.GHExecutor(executor.NewBasicExecutor())),
		Config:     cfg.Approve,
		Out:        os.Stdout,
		Log:        logFile,
		Now:        time.Now,
		AgentCache: newAgentCache(cfg.Approve, noAgentCache, home, os.Getenv("XDG_CACHE_HOME")),
	}
	if err := runner.Run(opts); err != nil {
		return fmt.Errorf("run: %w", err)
	}
	return nil
}
