// Package main implements the gh-merge command-line tool.
//
// gh-merge is a utility for finding and merging GitHub pull requests that have
// no pending review requests and are in a mergeable state. It can automatically
// merge these PRs and optionally delete the source branch after merging.
//
// Usage:
//
//	# List mergeable PRs without taking action
//	gh-merge list
//
//	# List specific PRs with debug timing logs and a deadline
//	gh-merge -v --pr 12 --pr 34 --timeout 30s
//	gh-merge -v 12 34
//	gh-merge --log-level debug list
//
//	# Merge all eligible PRs
//	gh-merge merge
//
//	# Merge and delete branches after merging
//	gh-merge merge -d
//
//	# Dry run to preview what would be merged
//	gh-merge merge --dry-run
//
//	# Use a custom config file
//	gh-merge --config /path/to/config.pkl
//
//	# Enqueue one pull request on the merge queue
//	gh-merge queue acme/widgets#12
//
//	# Enqueue pull request 12 in the current checkout
//	gh-merge queue 12
//
//	# Preview an enqueue. A draft is refused and nothing is enqueued.
//	gh-merge --dry-run queue 12
//
//	# Wait for pending required checks on the current head, then enqueue.
//	# The wait timeout is a Go duration and defaults to 2h. --timeout overrides it.
//	gh-merge queue --when-green acme/widgets#12
//
//	# Watch until each pull request merges, is ejected, or times out.
//	# --timeout is a Go duration and defaults to 30m. watch does not read stdin.
//	gh-merge watch --timeout 30m acme/widgets#12
//
//	# Post an RCA marker for the latest removal
//	gh-merge rca acme/widgets#12 --class flake --file body.md
//
//	# Correct that RCA: edit it in place, or post a superseding one
//	gh-merge rca acme/widgets#12 --update --class infra --file body.md
//	gh-merge rca acme/widgets#12 --supersedes --class infra --file body.md
//
//	# Report whether the latest removal has an RCA, without posting
//	gh-merge rca --check acme/widgets#12 --json
//
//	# Print the failure evidence for the latest removal, or one --event, as JSON
//	gh-merge evidence acme/widgets#12 --json
//
//	# After an ejection: post the RCA if missing, then requeue if the gate
//	# allows it. Rerunning does nothing new; exit 2 means the gate refused.
//	gh-merge handle-ejection acme/widgets#12 --class flake --file body.md
//
//	# Remove one pull request from the merge queue
//	gh-merge dequeue acme/widgets#12
//
//	# A pull request whose base is not the repository's default branch, and
//	# which is not in a native stack, is refused.
//	gh-merge queue acme/widgets#11
//
//	# A repo_settings entry for <owner>/<repo> replaces the global fields it sets.
//	# queue, watch, rca, list, and merge use that entry for the pull request's repository.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	executor "github.com/jaeyeom/go-cmdexec"
)

var (
	configPath            string
	dryRun                bool
	deleteBranch          bool
	verbose               bool
	logLevelFlag          string
	prListFlag            prRefFlag
	force                 bool
	includeDownstack      bool
	jsonOutput            bool
	allowNoRequiredChecks bool
	whenGreen             bool
	watchTimeout          = defaultWatchTimeout
)

const (
	defaultWatchTimeout  = 30 * time.Minute
	defaultWatchInterval = 15 * time.Second
)

func registerFlags() {
	flag.StringVar(&configPath, "config", "", "Path to config file")
	flag.BoolVar(&dryRun, "dry-run", false, dryRunHelp())
	flag.BoolVar(&deleteBranch, "d", false, "Delete branch after merge")
	flag.BoolVar(&verbose, "v", false, "Verbose output (streams skipped PRs and sets debug logging)")
	flag.StringVar(&logLevelFlag, "log-level", "", "Log level to stderr (debug, info, warn, error)")
	flag.Var(&prListFlag, "pr", "Limit list to the given pull request (repeatable; number, owner/repo#N, or URL)")
	flag.BoolVar(&force, "force", false, "Merge even if CI checks have failed or are pending")
	flag.BoolVar(&includeDownstack, "include-downstack", false, "Also enqueue open pull requests below a stack")
	flag.BoolVar(&jsonOutput, "json", false, "Print queue, watch, rca, evidence, and handle-ejection results as JSON")
	flag.DurationVar(&watchTimeout, "timeout", defaultWatchTimeout, timeoutHelp())
	flag.BoolVar(&whenGreen, "when-green", false, whenGreenHelp())
	flag.BoolVar(&allowNoRequiredChecks, "allow-no-required-checks", false, "Queue even when required_checks is empty or the repository is not configured")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] [command]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nCommands:\n")
		fmt.Fprintf(os.Stderr, "  list    List mergeable PRs (default; accepts --pr, --timeout, or positional PR refs)\n")
		fmt.Fprintf(os.Stderr, "  merge   Merge eligible PRs\n")
		fmt.Fprintf(os.Stderr, "  queue   %s\n", queueCommandHelp())
		fmt.Fprintf(os.Stderr, "  watch   %s\n", watchCommandHelp())
		fmt.Fprintf(os.Stderr, "  rca     %s\n", rcaCommandHelp())
		fmt.Fprintf(os.Stderr, "  evidence %s\n", evidenceCommandHelp())
		fmt.Fprintf(os.Stderr, "  dequeue %s\n", dequeueCommandHelp())
		fmt.Fprintf(os.Stderr, "  handle-ejection %s\n", handleEjectionCommandHelp())
	}
}

// applyQueueTimeoutFlag uses --timeout for queue --when-green when the flag is set.
// The queue wait otherwise stays at its own default, not the watch default.
func applyQueueTimeoutFlag() {
	flag.Visit(func(f *flag.Flag) {
		if f.Name == "timeout" {
			whenGreenTimeout = watchTimeout
		}
	})
}

// setupLogging configures the logger with the appropriate level, writing to stderr.
func setupLogging() (*slog.Logger, error) {
	logLevel, err := resolveLogLevel(logLevelFlag, verbose)
	if err != nil {
		return nil, err
	}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{
		Level: logLevel,
	}))
	slog.SetDefault(logger)
	return logger, nil
}

// printCIStatus prints CI check status details for a pull request.
// merge uses this with ChecksStatus. list uses printListCITo.
func printCIStatus(pr *models.PullRequest) {
	switch pr.ChecksStatus() {
	case models.ChecksPass:
		fmt.Printf("         CI: PASS (%d/%d checks passed)\n", len(pr.StatusCheckRollup), len(pr.StatusCheckRollup))
	case models.ChecksFail:
		failed := pr.FailedChecks()
		fmt.Printf("         CI: FAIL (%d failed)\n", len(failed))
		for _, check := range failed {
			fmt.Printf("           - %q %s\n", check.Name, check.Conclusion)
		}
	case models.ChecksPending:
		pending := pr.PendingChecks()
		fmt.Printf("         CI: PENDING (%d checks still running)\n", len(pending))
	case models.ChecksNone:
		fmt.Printf("         CI: NONE (no checks configured)\n")
	}
}

// mergeClient fetches mergeable pull requests and merges one of them.
type mergeClient interface {
	GetMergeablePullRequests() ([]models.PullRequest, error)
	MergePullRequest(prURL string, deleteBranch bool) error
}

// listMergeablePRs lists all PRs that are ready to merge.
func listMergeablePRs(githubClient mergeClient, cfg *config.Config) error {
	return listFetchedPullRequests(githubClient, cfg, nil)
}

type authoredPullRequests interface {
	GetAuthoredPullRequests() ([]models.PullRequest, error)
}

type mergeableReader interface {
	MergeableState(prURL string) (string, error)
}

func listFetchedPullRequests(githubClient mergeClient, cfg *config.Config, sleep func(time.Duration)) error {
	return listPullRequests(context.Background(), githubClient, cfg, listOptions{
		out:    os.Stdout,
		errOut: os.Stderr,
		sleep:  sleep,
	})
}

// statusChecksReader reads one pull request's status checks. The pull
// request list leaves them out, so list and merge read them only for the
// pull requests that pass the cheap filters.
type statusChecksReader interface {
	StatusChecks(prURL string) ([]models.StatusCheck, error)
}

// loadStatusChecks fills StatusCheckRollup for each pull request in prs.
// It prefers batched GraphQL via candidateStatesReader and otherwise falls
// back to bounded concurrent StatusChecks calls.
func loadStatusChecks(githubClient any, prs []models.PullRequest) error {
	if len(prs) == 0 {
		return nil
	}
	if batchReader, ok := githubClient.(candidateStatesReader); ok {
		urls := make([]string, len(prs))
		for i := range prs {
			urls[i] = prs[i].URL
		}
		states, err := batchReader.CandidateStatesContext(context.Background(), urls)
		if err != nil {
			return fmt.Errorf("failed to get status checks: %w", err)
		}
		for i := range prs {
			if st, found := states[prs[i].URL]; found {
				prs[i].StatusCheckRollup = st.StatusCheckRollup
			}
		}
		return nil
	}
	reader, ok := githubClient.(statusChecksReader)
	if !ok {
		return nil
	}
	limit := min(max(1, github.SuggestedConcurrency()), len(prs))
	workCh := make(chan int, len(prs))
	for i := range prs {
		workCh <- i
	}
	close(workCh)
	errCh := make(chan error, len(prs))
	var wg sync.WaitGroup
	for range limit {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for idx := range workCh {
				checks, err := reader.StatusChecks(prs[idx].URL)
				if err != nil {
					errCh <- fmt.Errorf("failed to get status checks for %s: %w", prs[idx].URL, err)
					return
				}
				prs[idx].StatusCheckRollup = checks
			}
		}()
	}
	wg.Wait()
	close(errCh)
	for err := range errCh {
		return err
	}
	return nil
}

// mergeablePoll bounds how resolveMergeable re-reads an UNKNOWN state.
// Delays start at first and double up to max. The total wait stays within
// budget, measured with now, or by the summed sleeps when now is nil.
type mergeablePoll struct {
	first, max, budget time.Duration
	now                func() time.Time
	sleep              func(time.Duration)
}

// resolveMergeable treats state as the first attempt and refreshes it while
// it is UNKNOWN and the poll budget allows.
func resolveMergeable(state, prURL string, refresh func(string) (string, error), poll mergeablePoll) (string, error) {
	return resolveMergeableContext(context.Background(), state, prURL, refresh, poll)
}

func resolveMergeableContext(ctx context.Context, state, prURL string, refresh func(string) (string, error), poll mergeablePoll) (string, error) {
	if refresh == nil {
		return state, nil
	}
	var start time.Time
	if poll.now != nil {
		start = poll.now()
	}
	var slept time.Duration
	delay := poll.first
	for state == "UNKNOWN" {
		if ctx != nil && ctx.Err() != nil {
			return "", fmt.Errorf("resolve mergeable cancelled: %w", ctx.Err())
		}
		elapsed := slept
		if poll.now != nil {
			elapsed = poll.now().Sub(start)
		}
		remaining := poll.budget - elapsed
		if remaining <= 0 {
			break
		}
		delay = min(delay, remaining)
		if err := sleepWithContext(ctx, poll.sleep, delay); err != nil {
			return "", err
		}
		slept += delay
		next, err := refresh(prURL)
		if err != nil {
			return "", fmt.Errorf("failed to read mergeable state: %w", err)
		}
		state = next
		delay = min(2*delay, poll.max)
	}
	return state, nil
}

func listReady(pr *models.PullRequest) bool {
	return !pr.IsDraft && len(pr.ReviewRequests) == 0 && pr.Mergeable == "MERGEABLE"
}

func skipReasons(pr *models.PullRequest, blockLabels []string) []string {
	var reasons []string
	if pr.IsDraft {
		reasons = append(reasons, "draft")
	}
	if len(pr.ReviewRequests) > 0 {
		reasons = append(reasons, "review requested")
	}
	if pr.Mergeable != "" && pr.Mergeable != "MERGEABLE" && pr.Mergeable != "UNKNOWN" {
		reasons = append(reasons, "not mergeable")
	}
	if label, blocked := pr.BlocksMerge(blockLabels); blocked {
		reasons = append(reasons, "blocking label "+label)
	}
	if pr.Mergeable == "UNKNOWN" {
		reasons = append(reasons, "unknown")
	}
	return reasons
}

// mergeResult tracks the outcome of attempting to merge a single PR.
type mergeResult int

const (
	mergeResultMerged mergeResult = iota
	mergeResultSkippedFail
	mergeResultSkippedPending
	mergeResultSkippedLabel
	mergeResultDryRun
	mergeResultError
)

// tryMergePR attempts to merge a single PR, checking block labels and CI status first.
// It prints status and returns the outcome. A block label is not overridden by --force.
func tryMergePR(githubClient mergeClient, pr *models.PullRequest, blockLabels []string) mergeResult {
	fmt.Printf("  %s\n", pr.Title)
	fmt.Printf("         URL: %s\n", pr.URL)
	fmt.Printf("         Branch: %s\n", pr.HeadRefName)
	printCIStatus(pr)

	if label, blocked := pr.BlocksMerge(blockLabels); blocked {
		fmt.Printf("         Skipping: label %q blocks merge\n\n", label)
		return mergeResultSkippedLabel
	}

	ciStatus := pr.ChecksStatus()
	if ciStatus == models.ChecksFail && !force {
		fmt.Printf("         Skipping: CI checks failed — blocking merge (use --force to override)\n\n")
		return mergeResultSkippedFail
	}
	if ciStatus == models.ChecksPending && !force {
		fmt.Printf("         Skipping: CI checks still in progress\n\n")
		return mergeResultSkippedPending
	}
	if force && (ciStatus == models.ChecksFail || ciStatus == models.ChecksPending) {
		fmt.Printf("         Warning: merging despite CI status %s (--force)\n", ciStatus)
	}

	if dryRun {
		branchAction := "keep"
		if deleteBranch {
			branchAction = "delete"
		}
		fmt.Printf("         Would merge PR and %s branch\n", branchAction)
		return mergeResultDryRun
	}

	if err := githubClient.MergePullRequest(pr.URL, deleteBranch); err != nil {
		fmt.Printf("         Error merging: %v\n", err)
		return mergeResultError
	}

	successMsg := "Successfully added to the merge queue"
	if deleteBranch {
		successMsg += " and deleted branch"
	}
	fmt.Printf("         %s\n\n", successMsg)
	return mergeResultMerged
}

// mergePRs merges all eligible PRs.
func mergePRs(githubClient mergeClient, cfg *config.Config) error {
	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}
	if err := loadStatusChecks(githubClient, prs); err != nil {
		return err
	}
	logMergeScan(cfg, prs, "Finding and merging PRs with no review requests",
		"dry_run", dryRun,
		"delete_branch", deleteBranch,
		"force", force)

	if len(prs) == 0 {
		fmt.Println("No PRs with no review requests found.")
		return nil
	}

	fmt.Printf("Found %d PRs with no review requests:\n\n", len(prs))

	var merged, skippedFail, skippedPending, skippedLabel int
	for i := range prs {
		switch tryMergePR(githubClient, &prs[i], blockLabelsFor(cfg, &prs[i])) {
		case mergeResultMerged:
			merged++
		case mergeResultSkippedFail:
			skippedFail++
		case mergeResultSkippedPending:
			skippedPending++
		case mergeResultSkippedLabel:
			skippedLabel++
		}
	}

	if dryRun {
		fmt.Printf("Found %d PRs with no review requests (dry run, no changes made).\n", len(prs))
	} else {
		fmt.Printf("Successfully added to the merge queue %d of %d PRs.\n", merged, len(prs))
	}
	if skippedFail > 0 || skippedPending > 0 {
		fmt.Printf("Skipped: %d due to CI failure, %d due to pending CI.\n", skippedFail, skippedPending)
	}
	if skippedLabel > 0 {
		fmt.Printf("Skipped: %d due to a block label.\n", skippedLabel)
	}

	return nil
}

// logMergeScan logs the label gate for list and merge.
// An empty repo_settings map keeps the global block_merge_labels attribute.
// Any entry logs one effective line per repository in prs, in first-seen order.
func logMergeScan(cfg *config.Config, prs []models.PullRequest, msg string, attrs ...any) {
	if len(cfg.RepoSettings) == 0 {
		slog.Info(msg, append(attrs, "block_merge_labels", cfg.Settings.BlockMergeLabels)...)
		return
	}
	slog.Info(msg, attrs...)
	seen := make(map[string]struct{})
	for i := range prs {
		owner, repo, ok := ownerRepoFromPullURL(prs[i].URL)
		if !ok {
			continue
		}
		key := owner + "/" + repo
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		settings, _ := cfg.EffectiveSettings(owner, repo)
		slog.Info(fmt.Sprintf("%s: block_merge_labels=%s required_checks=%d",
			key, formatBracketList(settings.BlockMergeLabels), len(settings.RequiredChecks)))
	}
}

func formatBracketList(items []string) string {
	return "[" + strings.Join(items, " ") + "]"
}

// requiredChecksFor returns the effective required_checks for the pull request's repository.
// An unparseable URL uses the global list.
func requiredChecksFor(cfg *config.Config, pr *models.PullRequest) []string {
	owner, repo, ok := ownerRepoFromPullURL(pr.URL)
	if !ok {
		return cfg.Settings.RequiredChecks
	}
	settings, _ := cfg.EffectiveSettings(owner, repo)
	return settings.RequiredChecks
}

// blockLabelsFor returns the block labels for the pull request's repository.
// An unparseable URL uses the global list.
func blockLabelsFor(cfg *config.Config, pr *models.PullRequest) []string {
	owner, repo, ok := ownerRepoFromPullURL(pr.URL)
	if !ok {
		return cfg.Settings.BlockMergeLabels
	}
	settings, _ := cfg.EffectiveSettings(owner, repo)
	return settings.BlockMergeLabels
}

// resolveMergeConfigPath picks the gh-merge config file.
// -config wins, then GH_MERGE_CONFIG, then ~/.config/gh-merge/config.pkl
// when that file exists, then the gh-nudge default.
func resolveMergeConfigPath(flagPath string) (string, error) {
	if flagPath != "" {
		return flagPath, nil
	}
	if env := os.Getenv("GH_MERGE_CONFIG"); env != "" {
		return env, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get user home directory: %w", err)
	}
	mergePkl := filepath.Join(home, ".config", "gh-merge", "config.pkl")
	if configFileExists(mergePkl) {
		return mergePkl, nil
	}
	base := filepath.Join(home, ".config", "gh-nudge", "config")
	if configFileExists(base + ".pkl") {
		return base + ".pkl", nil
	}
	return base + ".yaml", nil
}

func configFileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// exitOnUsage prints usage and exits 1 when err is the usage error.
func exitOnUsage(err, usageErr error, usage string) {
	if errors.Is(err, usageErr) {
		fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], usage)
		os.Exit(1)
	}
}

func main() {
	if code, ok := newGHMergeTool().Handle(context.Background(), os.Args[1:], os.Stdout, os.Stderr); ok {
		os.Exit(code)
	}
	os.Exit(runMain())
}

func runMain() int {
	registerFlags()
	flag.Parse()
	applyQueueTimeoutFlag()

	command, args, listOpts, err := resolveInvocation(flag.Args(), initialListOptions())
	if err != nil {
		if errors.Is(err, errUnknownCommand) {
			fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		} else {
			fmt.Fprintf(os.Stderr, "%v\n", err)
		}
		flag.Usage()
		return 1
	}

	// Setup logging
	if _, err := setupLogging(); err != nil {
		fmt.Fprintf(os.Stderr, "gh-merge: %v\n", err)
		return 1
	}

	// Load configuration. gh-merge picks the path; gh-nudge's default stays put.
	path, err := resolveMergeConfigPath(configPath)
	if err != nil {
		slog.Error("Failed to resolve configuration path", "error", err)
		return 1
	}
	cfg, err := config.LoadConfig(path)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		return 1
	}
	cfg.Path = path

	// Initialize GitHub client
	ctx := context.Background()
	exec := github.GHExecutor(executor.NewBasicExecutor())
	githubClient := github.NewClient(ctx, exec)

	if err := dispatchCommand(ctx, command, args, listOpts, githubClient, cfg); err != nil {
		slog.Error("Command failed", "command", command, "error", err)
		return 1
	}
	return 0
}

func dispatchCommand(ctx context.Context, command string, args []string, listOpts listOptions, githubClient *github.Client, cfg *config.Config) error {
	switch command {
	case "list":
		return listPullRequests(ctx, githubClient, cfg, listOpts)
	case "merge":
		return mergePRs(githubClient, cfg)
	case "queue":
		if len(args) != 2 {
			fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], queueUsage())
			os.Exit(1)
		}
		return queuePullRequest(githubClient, cfg, args[1], includeDownstack)
	case "watch":
		return watchCommand(githubClient, cfg, args[1:])
	case "rca":
		err := postRCA(githubClient, cfg, args[1:])
		exitOnUsage(err, errRCAUsage, rcaUsage())
		return err
	case "evidence":
		err := evidenceCommand(githubClient, cfg, args[1:])
		exitOnUsage(err, errEvidenceUsage, evidenceUsage())
		return err
	case "dequeue":
		return dequeueCommand(githubClient, args[1:])
	case "handle-ejection":
		return handleEjectionCommand(githubClient, cfg, args[1:])
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		flag.Usage()
		os.Exit(1)
		return nil
	}
}
