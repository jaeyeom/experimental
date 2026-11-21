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
//	# Keep colors when relaying the output to a terminal
//	gh-approve --color=always
//
//	# List recent decisions, with each pull request's current state
//	gh-approve history
//
// Every decision is appended to ~/.config/gh-nudge/approve-log.jsonl.
//
// Agent command decisions are cached under $XDG_CACHE_HOME/gh-approve/agent
// (default ~/.cache/gh-approve/agent). --no-agent-cache bypasses the cache.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
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

// newPRCache returns the per-commit PR facts cache, or nil when disabled.
// It lives under xdgCacheHome, or home/.cache when xdgCacheHome is empty.
func newPRCache(cfg config.ApproveConfig, disabled bool, home, xdgCacheHome string) *approve.PRCache {
	if disabled {
		return nil
	}
	base := xdgCacheHome
	if base == "" {
		base = filepath.Join(home, ".cache")
	}
	return &approve.PRCache{
		Dir: filepath.Join(base, "gh-approve", "facts"),
		TTL: cfg.PRCacheTTL,
		Now: time.Now,
	}
}

func main() {
	if code, ok := newGHApproveTool().Handle(context.Background(), os.Args[1:], os.Stdout, os.Stderr); ok {
		os.Exit(code)
	}
	if len(os.Args) > 1 && os.Args[1] == "history" {
		os.Exit(runHistory(os.Args[2:]))
	}
	var (
		configPath      string
		opts            approve.Options
		maxDuration     time.Duration
		loadConcurrency int
		noAgentCache    bool
		noCache         bool
		color           string
	)
	flag.StringVar(&configPath, "config", "", "Path to config file (default ~/.config/gh-nudge/config.pkl)")
	flag.DurationVar(&maxDuration, "max-duration", 0, "Stop starting pull requests after this duration (also configurable as approve.max_duration)")
	flag.IntVar(&loadConcurrency, "load-concurrency", 0, "Maximum concurrent uncached PR loads (default from approve.load_concurrency, 4)")
	flag.BoolVar(&opts.Apply, "apply", false, "Approve, hand off, and mark files viewed for real. Without it, gh-approve is a dry run")
	flag.StringVar(&opts.PR, "pr", "", "Evaluate one pull request: owner/repo#N or a URL")
	flag.BoolVar(&opts.Explain, "explain", false, "Print every condition of every rule with the value it saw")
	flag.BoolVar(&noAgentCache, "no-agent-cache", false, "Run agent commands without reading or writing the decision cache")
	flag.BoolVar(&noCache, "no-cache", false, "Bypass the per-commit PR facts cache")
	flag.StringVar(&color, "color", approve.ColorAuto, "Color the status markers: auto (only on a terminal), always, or never. NO_COLOR turns color off")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [--apply] [--pr <owner/repo#N | URL>] [--explain] [--no-cache] [--no-agent-cache] [--load-concurrency N] [--color auto|always|never] [--config <path>]\n       %s history [--days N] [--action approved|handoff|mark_viewed|all] [--rule NAME] [--json] [--no-state]\n\n", os.Args[0], os.Args[0])
		flag.PrintDefaults()
	}
	flag.Parse()
	if loadConcurrency < 0 {
		fmt.Fprintf(os.Stderr, "gh-approve: --load-concurrency must be >= 0\n")
		flag.Usage()
		os.Exit(2)
	}
	opts.MaxDuration = maxDuration
	opts.LoadConcurrency = loadConcurrency
	if flag.NArg() > 0 {
		flag.Usage()
		os.Exit(2)
	}
	useColor, err := approve.UseColor(color, os.Stdout, os.Getenv("NO_COLOR"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		flag.Usage()
		os.Exit(2)
	}
	opts.Color = useColor
	if err = run(configPath, opts, noAgentCache, noCache); err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		os.Exit(1)
	}
}

func run(configPath string, opts approve.Options, noAgentCache, noCache bool) (err error) {
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
	xdgCacheHome := os.Getenv("XDG_CACHE_HOME")
	runner := &approve.Runner{
		Client:         github.NewClient(context.Background(), github.GHExecutor(executor.NewBasicExecutor())),
		Config:         cfg.Approve,
		Viewed:         cfg.Viewed,
		GeneratedLists: cfg.GeneratedLists,
		Out:            os.Stdout,
		Log:            logFile,
		Now:            time.Now,
		AgentCache:     newAgentCache(cfg.Approve, noAgentCache, home, xdgCacheHome),
		PRCache:        newPRCache(cfg.Approve, noCache, home, xdgCacheHome),
	}
	if err := runner.Run(opts); err != nil {
		return fmt.Errorf("run: %w", err)
	}
	return nil
}

// runHistory lists recent approve-log decisions. It returns the process
// exit code. Flag mistakes are 2. A failed lookup is 1.
func runHistory(args []string) int {
	fs := flag.NewFlagSet("history", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	var (
		days    int
		action  string
		rule    string
		jsonOut bool
		noState bool
	)
	fs.IntVar(&days, "days", 7, "How many days of decisions to list")
	fs.StringVar(&action, "action", approve.ActionApproved, "approved, handoff, mark_viewed, or all; would_* actions are included only with all")
	fs.StringVar(&rule, "rule", "", "List only decisions for this rule")
	fs.BoolVar(&jsonOut, "json", false, "Print a JSON array instead of a table")
	fs.BoolVar(&noState, "no-state", false, "Do not look up the current pull request state")
	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: gh-approve history [--days N] [--action approved|handoff|mark_viewed|all] [--rule NAME] [--json] [--no-state]\n\n")
		fs.SetOutput(os.Stderr)
		fs.PrintDefaults()
	}
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			fs.Usage()
			return 0
		}
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		fs.Usage()
		return 2
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(os.Stderr, "gh-approve: unexpected argument %q\n", fs.Arg(0))
		fs.Usage()
		return 2
	}
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: find home directory: %v\n", err)
		return 1
	}
	client := github.NewClient(context.Background(), github.GHExecutor(executor.NewBasicExecutor()))
	err = approve.History(approve.HistoryOptions{
		LogPath:   filepath.Join(home, ".config", "gh-nudge", "approve-log.jsonl"),
		CachePath: approve.PRStateCachePath(home, os.Getenv("XDG_CACHE_HOME")),
		Days:      days,
		Action:    action,
		Rule:      rule,
		JSON:      jsonOut,
		NoState:   noState,
		Out:       os.Stdout,
		Err:       os.Stderr,
		Lookup:    client.PullRequestSnapshots,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "gh-approve: %v\n", err)
		return 1
	}
	return 0
}
