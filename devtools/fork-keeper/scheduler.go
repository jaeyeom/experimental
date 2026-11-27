package forkkeeper

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

const (
	labelApproved    = "fork-keeper:approved"
	labelNeedsReview = "needs-review"
)

// Options configures a fork-keeper scheduler run.
type Options struct {
	ConfigPath  string
	CacheDir    string
	ScriptsDir  string
	GHBin       string
	ReviewerCmd []string
	Reviewer    Reviewer
	Today       string
	Sandbox     string
	ForkNames   []string
	Stdout      io.Writer
	Stderr      io.Writer
}

type ghLabel struct {
	Name string `json:"name"`
}

type ghPR struct {
	Number      int       `json:"number"`
	HeadRefName string    `json:"headRefName"`
	HeadRefOid  string    `json:"headRefOid"`
	Labels      []ghLabel `json:"labels"`
	URL         string    `json:"url"`
}

func (p ghPR) hasLabel(name string) bool {
	for _, l := range p.Labels {
		if l.Name == name {
			return true
		}
	}
	return false
}

// Run executes the fork-keeper scheduler for the configured forks (or the
// subset named in opts.ForkNames).
func Run(ctx context.Context, opts Options) error {
	opts = normalizeOptions(opts)
	logMsg := func(forkName, format string, args ...any) {
		_, _ = fmt.Fprintf(opts.Stdout, "fork-keeper[%s]: %s\n", forkName, fmt.Sprintf(format, args...))
	}
	logErr := func(forkName string, err error) {
		_, _ = fmt.Fprintf(opts.Stderr, "fork-keeper[%s]: error: %v\n", forkName, err)
	}

	if _, err := os.Stat(opts.ConfigPath); err != nil {
		err = fmt.Errorf("no config at %s", opts.ConfigPath)
		logErr("-", err)
		return err
	}
	cfg, err := LoadConfig(opts.ConfigPath)
	if err != nil {
		logErr("-", err)
		return err
	}

	env := os.Environ()
	ghBin, err := cfg.ResolveGHBin(opts.GHBin, env)
	if err != nil {
		logErr("-", err)
		return err
	}
	if err := validatePrerequisites(opts, ghBin); err != nil {
		logErr("-", err)
		return err
	}

	targets := targetForkNames(cfg, opts.ForkNames)
	var firstErr error
	for _, name := range targets {
		fork, ok := findFork(cfg, name)
		if !ok {
			err := fmt.Errorf("no fork named %s in %s", name, opts.ConfigPath)
			logErr(name, err)
			if firstErr == nil {
				firstErr = err
			}
			continue
		}
		reviewer, err := resolveReviewer(opts, cfg, fork, env)
		if err != nil {
			logErr(name, err)
			if firstErr == nil {
				firstErr = err
			}
			continue
		}
		if err := runFork(ctx, fork.withDefaults(), opts, ghBin, reviewer, logMsg); err != nil {
			logErr(name, err)
			if firstErr == nil {
				firstErr = err
			}
		}
	}
	return firstErr
}

func resolveReviewer(opts Options, cfg Config, f Fork, env []string) (Reviewer, error) {
	if opts.Reviewer != nil {
		return opts.Reviewer, nil
	}
	cmd, err := f.EffectiveReviewerCommand(opts.ReviewerCmd, cfg.ReviewerCmd, env)
	if err != nil {
		return nil, err
	}
	if len(cmd) == 0 {
		return StandaloneReviewer{}, nil
	}
	return &CommandReviewer{Command: cmd}, nil
}

func normalizeOptions(opts Options) Options {
	if opts.Stdout == nil {
		opts.Stdout = os.Stdout
	}
	if opts.Stderr == nil {
		opts.Stderr = os.Stderr
	}
	if opts.ConfigPath == "" {
		opts.ConfigPath = DefaultConfigPath()
	}
	if opts.CacheDir == "" {
		opts.CacheDir = DefaultCacheDir()
	}
	if opts.ScriptsDir == "" {
		opts.ScriptsDir = DefaultScriptsDir()
	}
	if opts.Today == "" {
		if envDate := os.Getenv("FORK_KEEPER_DATE"); envDate != "" {
			opts.Today = envDate
		} else {
			opts.Today = time.Now().UTC().Format("2006-01-02")
		}
	}
	if opts.Sandbox == "" {
		opts.Sandbox = DefaultSandboxMode()
	}
	return opts
}

func validatePrerequisites(opts Options, ghBin string) error {
	if _, err := exec.LookPath("git"); err != nil {
		return errors.New("git is required")
	}
	if _, err := exec.LookPath(ghBin); err != nil {
		return fmt.Errorf("%s is required", ghBin)
	}
	switch opts.Sandbox {
	case "", sandboxAuto, sandboxSeatbelt, sandboxBwrap, sandboxNone:
	default:
		return fmt.Errorf("unknown sandbox mode %q (want auto, seatbelt, bwrap, or none)", opts.Sandbox)
	}
	return nil
}

func targetForkNames(cfg Config, requested []string) []string {
	if len(requested) > 0 {
		return requested
	}
	names := make([]string, 0, len(cfg.Forks))
	for _, f := range cfg.Forks {
		names = append(names, f.Name)
	}
	return names
}

func findFork(cfg Config, name string) (Fork, bool) {
	for _, f := range cfg.Forks {
		if f.Name == name {
			return f, true
		}
	}
	return Fork{}, false
}

type forkPaths struct {
	noHooksDir string
	gitDir     string
	workDir    string
	outDir     string
	privDir    string
	rangeFile  string
	diffFile   string
	promptFile string
	schemaFile string
}

func runFork(ctx context.Context, f Fork, opts Options, ghBin string, reviewer Reviewer, logMsg func(string, string, ...any)) error {
	paths, cleanup, err := initForkPaths(opts, f.Name)
	if err != nil {
		return err
	}
	defer cleanup()

	rangeInfo, err := fetchAndComputeRange(ctx, f, opts, paths)
	if err != nil {
		return err
	}
	if rangeInfo.UpToDate {
		logMsg(f.Name, "up-to-date: upstream is already in %s", f.Branch)
		return nil
	}

	pr, err := findOpenSyncPR(ctx, ghBin, f.Repo)
	if err != nil {
		return err
	}

	syncBranch, prURL, sticky, done, err := handleExistingPR(ctx, f, opts, ghBin, paths.gitDir, rangeInfo.UpstreamNew, pr, logMsg)
	if err != nil || done {
		return err
	}

	candidate, err := runAgentCandidate(ctx, f, opts, paths, rangeInfo, reviewer, logMsg)
	if err != nil {
		return err
	}

	return publishAndPromote(ctx, f, opts, ghBin, paths, rangeInfo, pr, syncBranch, prURL, candidate, sticky, logMsg)
}

func initForkPaths(opts Options, forkName string) (forkPaths, func(), error) {
	noHooksDir := filepath.Join(opts.CacheDir, "no-hooks")
	if err := os.MkdirAll(noHooksDir, 0o755); err != nil {
		return forkPaths{}, nil, fmt.Errorf("create no-hooks dir: %w", err)
	}
	privDir, err := os.MkdirTemp("", "fork-keeper.*")
	if err != nil {
		return forkPaths{}, nil, fmt.Errorf("create temp dir: %w", err)
	}
	cleanup := func() { _ = os.RemoveAll(privDir) }

	if opts.ScriptsDir != "" {
		for _, s := range []string{"review-range.sh", "validate-verdict.sh"} {
			src := filepath.Join(opts.ScriptsDir, s)
			if _, statErr := os.Stat(src); statErr == nil {
				if err := copyExecutable(src, filepath.Join(privDir, s)); err != nil {
					cleanup()
					return forkPaths{}, nil, fmt.Errorf("copy %s: %w", s, err)
				}
			}
		}
	}
	return forkPaths{
		noHooksDir: noHooksDir,
		gitDir:     filepath.Join(opts.CacheDir, forkName+".git"),
		workDir:    filepath.Join(opts.CacheDir, forkName+".work"),
		outDir:     filepath.Join(opts.CacheDir, forkName+".out"),
		privDir:    privDir,
		rangeFile:  filepath.Join(privDir, "range.json"),
		diffFile:   filepath.Join(privDir, "upstream.diff"),
		promptFile: filepath.Join(privDir, "prompt.md"),
		schemaFile: filepath.Join(privDir, "verdict-schema.json"),
	}, cleanup, nil
}

func fetchAndComputeRange(ctx context.Context, f Fork, opts Options, paths forkPaths) (RangeInfo, error) {
	if _, err := os.Stat(paths.gitDir); errors.Is(err, os.ErrNotExist) {
		if err := runGit(ctx, "", "init", "-q", "--bare", paths.gitDir); err != nil {
			return RangeInfo{}, fmt.Errorf("init bare repo: %w", err)
		}
	}
	if _, err := gitOutput(ctx, paths.gitDir, "config", "core.hooksPath", paths.noHooksDir); err != nil {
		return RangeInfo{}, fmt.Errorf("configure hooksPath: %w", err)
	}
	if _, err := gitOutput(ctx, paths.gitDir, "fetch", "-q", "--prune", f.ForkURL,
		"+refs/heads/*:refs/remotes/origin/*",
		"+refs/fork-keeper/*:refs/fork-keeper/*"); err != nil {
		return RangeInfo{}, fmt.Errorf("cannot fetch the fork %s: %w", f.ForkURL, err)
	}
	if _, err := gitOutput(ctx, paths.gitDir, "fetch", "-q", "--no-tags", "--prune", f.UpstreamURL,
		"+refs/heads/*:refs/remotes/upstream/*",
		"+refs/tags/*:refs/upstream-tags/*"); err != nil {
		return RangeInfo{}, fmt.Errorf("cannot fetch upstream %s: %w", f.UpstreamURL, err)
	}

	var rangeInfo RangeInfo
	var rangeBytes []byte
	var err error
	legacyScript := filepath.Join(paths.privDir, "review-range.sh")
	if opts.ScriptsDir != "" && fileExists(legacyScript) {
		rangeBytes, err = runReviewRangeScript(ctx, legacyScript, paths.gitDir, f.Branch)
		if err != nil {
			return RangeInfo{}, fmt.Errorf("review-range.sh failed: %w", err)
		}
		if err := json.Unmarshal(rangeBytes, &rangeInfo); err != nil {
			return RangeInfo{}, fmt.Errorf("parse range.json: %w", err)
		}
	} else {
		rangeInfo, err = ComputeRange(ctx, paths.gitDir, "upstream", f.Branch)
		if err != nil {
			return RangeInfo{}, fmt.Errorf("compute range: %w", err)
		}
		rangeBytes, err = json.Marshal(rangeInfo)
		if err != nil {
			return RangeInfo{}, fmt.Errorf("encode range.json: %w", err)
		}
		rangeBytes = append(rangeBytes, '\n')
	}

	if err := os.WriteFile(paths.rangeFile, rangeBytes, 0o600); err != nil {
		return RangeInfo{}, fmt.Errorf("write range.json: %w", err)
	}
	return rangeInfo, nil
}

func promoteFork(ctx context.Context, f Fork, opts Options, gitDir, syncBranch, head, up, tagsPrefix string, logMsg func(string, string, ...any)) error {
	oldHead, err := gitOutput(ctx, gitDir, "rev-parse", "refs/remotes/origin/"+f.Branch)
	if err != nil {
		return fmt.Errorf("resolve origin/%s: %w", f.Branch, err)
	}
	oldHead = strings.TrimSpace(oldHead)
	preSyncTag, err := nextPreSyncTag(ctx, gitDir, f.ForkURL, opts.Today)
	if err != nil {
		return fmt.Errorf("choose pre-sync tag: %w", err)
	}
	if _, err := gitOutput(ctx, gitDir, "push", "-q", "--atomic",
		fmt.Sprintf("--force-with-lease=refs/heads/%s:%s", f.Branch, oldHead),
		f.ForkURL,
		fmt.Sprintf("%s:refs/tags/%s", oldHead, preSyncTag),
		fmt.Sprintf("%s:refs/heads/%s", head, f.Branch)); err != nil {
		return fmt.Errorf("force-push of %s failed; fork-keeper refs left unchanged: %w", f.Branch, err)
	}
	if _, err := gitOutput(ctx, gitDir, "push", "-q", "--prune", f.ForkURL,
		fmt.Sprintf("+%s:refs/fork-keeper/upstream-head", up),
		fmt.Sprintf("+%s/*:refs/fork-keeper/upstream-tags/*", tagsPrefix)); err != nil {
		return fmt.Errorf("promoted %s but could not record refs/fork-keeper/upstream-head: %w", f.Branch, err)
	}
	if err := cleanupPromotedSyncRefs(ctx, gitDir, f.ForkURL, syncBranch, head); err != nil {
		logMsg(f.Name, "warning: promoted %s but failed to clean up old sync refs: %v", f.Branch, err)
	}
	return nil
}

func nextPreSyncTag(ctx context.Context, gitDir, forkURL, today string) (string, error) {
	prefix := "pre-sync/" + today
	out, err := gitOutput(ctx, gitDir, "ls-remote", "--tags", forkURL,
		"refs/tags/"+prefix,
		"refs/tags/"+prefix+".*")
	if err != nil {
		return "", err
	}
	existing := make(map[string]bool)
	for _, line := range splitNonEmptyLines(out) {
		fields := strings.Fields(line)
		if len(fields) < 2 {
			continue
		}
		ref := strings.TrimSuffix(fields[1], "^{}")
		if name, ok := strings.CutPrefix(ref, "refs/tags/"); ok {
			existing[name] = true
		}
	}
	if !existing[prefix] {
		return prefix, nil
	}
	for i := 2; ; i++ {
		candidate := fmt.Sprintf("%s.%d", prefix, i)
		if !existing[candidate] {
			return candidate, nil
		}
	}
}

func cleanupPromotedSyncRefs(ctx context.Context, gitDir, forkURL, syncBranch, promotedHead string) error {
	out, err := gitOutput(ctx, gitDir, "ls-remote", forkURL,
		"refs/heads/sync/*",
		"refs/fork-keeper/sync-base/*",
		"refs/fork-keeper/sync-tags/*")
	if err != nil {
		return err
	}

	// Collect sync branches that are safe to delete: the branch we just
	// promoted, plus any older sync/* branch already merged into promotedHead.
	staleBranches := map[string]bool{syncBranch: true}
	var allRefs []string
	for _, line := range splitNonEmptyLines(out) {
		fields := strings.Fields(line)
		if len(fields) < 2 {
			continue
		}
		sha, ref := fields[0], fields[1]
		allRefs = append(allRefs, ref)
		if b, ok := strings.CutPrefix(ref, "refs/heads/"); ok && strings.HasPrefix(b, "sync/") {
			if gitIsAncestor(ctx, gitDir, sha, promotedHead) {
				staleBranches[b] = true
			}
		}
	}

	var refspecs []string
	for _, ref := range allRefs {
		if isStaleSyncRef(ref, staleBranches) {
			refspecs = append(refspecs, ":"+ref)
		}
	}
	if len(refspecs) == 0 {
		return nil
	}
	args := append([]string{"push", "-q", forkURL}, refspecs...)
	_, err = gitOutput(ctx, gitDir, args...)
	return err
}

func isStaleSyncRef(ref string, staleBranches map[string]bool) bool {
	if b, ok := strings.CutPrefix(ref, "refs/heads/"); ok {
		return staleBranches[b]
	}
	if b, ok := strings.CutPrefix(ref, "refs/fork-keeper/sync-base/"); ok {
		return staleBranches[b]
	}
	if rest, ok := strings.CutPrefix(ref, "refs/fork-keeper/sync-tags/"); ok {
		for b := range staleBranches {
			if strings.HasPrefix(rest, b+"/") {
				return true
			}
		}
	}
	return false
}

func formatSyncLocation(syncBranch, prURL string) string {
	if prURL != "" {
		return syncBranch + " " + prURL
	}
	return syncBranch
}

func handleExistingPR(ctx context.Context, f Fork, opts Options, ghBin, gitDir, upstreamNew string, pr *ghPR, logMsg func(string, string, ...any)) (syncBranch, prURL string, sticky, done bool, err error) {
	syncBranch = "sync/" + opts.Today
	if pr == nil {
		return syncBranch, "", false, false, nil
	}
	syncBranch = pr.HeadRefName
	prURL = pr.URL
	prHead := pr.HeadRefOid
	fetched := gitRevParseOptional(ctx, gitDir, fmt.Sprintf("refs/remotes/origin/%s^{commit}", syncBranch))

	if pr.hasLabel(labelApproved) {
		if fetched != "" && prHead == fetched {
			current, err := isApprovalCurrent(ctx, ghBin, f.Repo, pr.Number)
			if err != nil {
				return "", "", false, false, fmt.Errorf("check approval timeline for #%d: %w", pr.Number, err)
			}
			if current {
				base, err := gitOutput(ctx, gitDir, "rev-parse", "-q", "--verify",
					fmt.Sprintf("refs/fork-keeper/sync-base/%s^{commit}", syncBranch))
				if err != nil {
					return "", "", false, false, fmt.Errorf("approved %s has no refs/fork-keeper/sync-base/%s", syncBranch, syncBranch)
				}
				if err := promoteFork(ctx, f, opts, gitDir, syncBranch, prHead, strings.TrimSpace(base), "refs/fork-keeper/sync-tags/"+syncBranch, logMsg); err != nil {
					return "", "", false, false, err
				}
				logMsg(f.Name, "promoted approved %s (%s) to %s", formatSyncLocation(syncBranch, prURL), prHead, f.Branch)
				return syncBranch, prURL, false, true, nil
			}
			logMsg(f.Name, "ignoring %s: PR #%d changed after approval was added", labelApproved, pr.Number)
		} else {
			logMsg(f.Name, "ignoring %s: PR head %s is not the fetched %s", labelApproved, prHead, syncBranch)
		}
	}

	if pr.hasLabel(labelNeedsReview) {
		sticky = true
		if fetched != "" && gitIsAncestor(ctx, gitDir, upstreamNew, fetched) {
			logMsg(f.Name, "waiting for review: %s already has upstream %s", formatSyncLocation(syncBranch, prURL), upstreamNew)
			return syncBranch, prURL, true, true, nil
		}
	}
	return syncBranch, prURL, sticky, false, nil
}

func runAgentCandidate(ctx context.Context, f Fork, opts Options, paths forkPaths, rangeInfo RangeInfo, reviewer Reviewer, logMsg func(string, string, ...any)) (string, error) {
	if err := prepareAgentWorkdir(ctx, paths.gitDir, paths.workDir, paths.outDir, opts.CacheDir, paths.noHooksDir, f.Branch); err != nil {
		return "", err
	}

	diffText, err := BuildUpstreamDiff(ctx, paths.gitDir, rangeInfo)
	if err != nil {
		return "", err
	}
	if err := os.WriteFile(paths.diffFile, []byte(diffText), 0o600); err != nil {
		return "", fmt.Errorf("write upstream diff: %w", err)
	}
	if err := os.WriteFile(paths.schemaFile, []byte(VerdictOutputSchema+"\n"), 0o600); err != nil {
		return "", fmt.Errorf("write verdict schema: %w", err)
	}

	agentLogPath := filepath.Join(opts.CacheDir, f.Name+".agent.log")
	if cmdRev, ok := reviewer.(*CommandReviewer); ok {
		logMsg(f.Name, "running reviewer: %s", cmdRev.Command[0])
	} else {
		logMsg(f.Name, "running built-in standalone rebase (no external reviewer configured)")
	}

	input := ReviewInput{
		Fork:        f,
		WorkDir:     paths.workDir,
		OutDir:      paths.outDir,
		GitDir:      paths.gitDir,
		CacheDir:    opts.CacheDir,
		LogPath:     agentLogPath,
		SandboxMode: opts.Sandbox,
		Range:       rangeInfo,
		RangeFile:   paths.rangeFile,
		DiffFile:    paths.diffFile,
		PromptFile:  paths.promptFile,
		SchemaFile:  paths.schemaFile,
	}
	res, err := reviewer.Review(ctx, input)
	if err != nil {
		return "", fmt.Errorf("review %s: %w", f.Name, err)
	}
	if res.AgentExited != 0 {
		logMsg(f.Name, "agent exited %d (see %s)", res.AgentExited, agentLogPath)
	}

	candidate := gitRevParseDirOptional(ctx, paths.workDir, "refs/heads/fork-keeper/candidate^{commit}")
	if candidate == "" {
		return "", errors.New("agent left no fork-keeper/candidate branch")
	}
	if _, err := gitOutput(ctx, paths.gitDir, "fetch", "-q", paths.workDir,
		"+refs/heads/fork-keeper/candidate:refs/fork-keeper-run/candidate"); err != nil {
		return "", fmt.Errorf("cannot import the candidate: %w", err)
	}
	imported, err := gitOutput(ctx, paths.gitDir, "rev-parse", "refs/fork-keeper-run/candidate")
	if err != nil || strings.TrimSpace(imported) != candidate {
		return "", errors.New("fork-keeper/candidate moved while importing it")
	}
	return candidate, nil
}

func publishAndPromote(ctx context.Context, f Fork, opts Options, ghBin string, paths forkPaths, rangeInfo RangeInfo, pr *ghPR, syncBranch, prURL, candidate string, sticky bool, logMsg func(string, string, ...any)) error {
	verdict, valid, bodyFile, twRC, err := preparePRBody(ctx, opts, paths, rangeInfo, candidate, sticky)
	if err != nil {
		return err
	}

	if pr != nil && pr.hasLabel(labelApproved) {
		if _, err := ghOutput(ctx, ghBin, "pr", "edit", syncBranch, "-R", f.Repo, "--remove-label", labelApproved); err != nil {
			return fmt.Errorf("cannot remove %s before refreshing %s: %w", labelApproved, syncBranch, err)
		}
	}

	if _, err := gitOutput(ctx, paths.gitDir, "push", "-q", "--prune", f.ForkURL,
		fmt.Sprintf("+%s:refs/heads/%s", candidate, syncBranch),
		fmt.Sprintf("+%s:refs/fork-keeper/sync-base/%s", rangeInfo.UpstreamNew, syncBranch),
		fmt.Sprintf("+refs/upstream-tags/*:refs/fork-keeper/sync-tags/%s/*", syncBranch)); err != nil {
		return fmt.Errorf("cannot push %s: %w", syncBranch, err)
	}

	prURL, err = upsertSyncPR(ctx, ghBin, f, opts.Today, pr, syncBranch, prURL, bodyFile, logMsg)
	if err != nil {
		return err
	}

	if valid && verdict == VerdictClean && twRC == 0 && !sticky {
		if err := promoteFork(ctx, f, opts, paths.gitDir, syncBranch, candidate, rangeInfo.UpstreamNew, "refs/upstream-tags", logMsg); err != nil {
			return err
		}
		logMsg(f.Name, "promoted %s (%s) to %s", formatSyncLocation(syncBranch, prURL), candidate, f.Branch)
		return nil
	}

	if _, err := ghOutput(ctx, ghBin, "pr", "edit", syncBranch, "-R", f.Repo, "--add-label", labelNeedsReview); err != nil {
		return fmt.Errorf("cannot label %s %s: %w", syncBranch, labelNeedsReview, err)
	}
	logMsg(f.Name, "needs-review: %s (verdict %s, validator %t, tripwires rc %d, sticky %t)",
		formatSyncLocation(syncBranch, prURL), verdict, valid, twRC, sticky)
	return nil
}

func preparePRBody(ctx context.Context, opts Options, paths forkPaths, rangeInfo RangeInfo, candidate string, sticky bool) (verdict string, valid bool, bodyFile string, twRC int, err error) {
	verdictPath := filepath.Join(paths.outDir, "verdict.json")
	var vmsg string
	legacyValidator := filepath.Join(paths.privDir, "validate-verdict.sh")
	if opts.ScriptsDir != "" && fileExists(legacyValidator) {
		valid, vmsg = validateVerdictScript(ctx, legacyValidator, verdictPath, candidate, paths.rangeFile)
	} else {
		_, valid, vmsg = ValidateVerdictFile(verdictPath, candidate, &rangeInfo)
	}
	verdict, summary := readVerdictMeta(verdictPath)
	hits, twErr := CheckTripwires(ctx, paths.gitDir, rangeInfo, candidate)
	if twErr != nil || len(hits) > 0 {
		twRC = 1
	}

	bodyFile = filepath.Join(paths.privDir, "body.md")
	bodyContent := buildPRBody(filepath.Join(paths.outDir, "report.md"), verdict, summary, valid, vmsg, hits, twErr, sticky)
	if err := os.WriteFile(bodyFile, []byte(bodyContent), 0o600); err != nil {
		return "", false, "", 0, fmt.Errorf("write PR body: %w", err)
	}
	return verdict, valid, bodyFile, twRC, nil
}

func upsertSyncPR(ctx context.Context, ghBin string, f Fork, today string, pr *ghPR, syncBranch, prURL, bodyFile string, logMsg func(string, string, ...any)) (string, error) {
	if pr != nil {
		if _, err := ghOutput(ctx, ghBin, "pr", "edit", syncBranch, "-R", f.Repo, "--body-file", bodyFile); err != nil {
			return prURL, fmt.Errorf("cannot refresh the PR for %s: %w", syncBranch, err)
		}
		target := prURL
		if target == "" {
			target = syncBranch
		}
		logMsg(f.Name, "refreshed %s", target)
		return prURL, nil
	}

	createdURL, err := ghOutput(ctx, ghBin, "pr", "create", "-R", f.Repo,
		"--base", f.Branch,
		"--head", syncBranch,
		"--title", "fork-keeper: sync upstream "+today,
		"--body-file", bodyFile)
	if err != nil {
		return prURL, fmt.Errorf("cannot open a PR for %s: %w", syncBranch, err)
	}
	prURL = strings.TrimSpace(createdURL)
	target := prURL
	if target == "" {
		target = syncBranch
	}
	logMsg(f.Name, "opened %s", target)
	return prURL, nil
}

func findOpenSyncPR(ctx context.Context, ghBin, repo string) (*ghPR, error) {
	out, err := ghOutput(ctx, ghBin, "pr", "list", "-R", repo, "--state", "open",
		"--json", "number,headRefName,headRefOid,labels,url")
	if err != nil {
		return nil, fmt.Errorf("gh pr list failed: %w", err)
	}
	var prs []ghPR
	if err := json.Unmarshal([]byte(out), &prs); err != nil {
		return nil, fmt.Errorf("parse gh pr list output: %w", err)
	}
	var syncPRs []ghPR
	for _, p := range prs {
		if strings.HasPrefix(p.HeadRefName, "sync/") {
			syncPRs = append(syncPRs, p)
		}
	}
	if len(syncPRs) == 0 {
		return nil, nil
	}
	sort.Slice(syncPRs, func(i, j int) bool {
		return syncPRs[i].Number < syncPRs[j].Number
	})
	chosen := syncPRs[len(syncPRs)-1]
	return &chosen, nil
}

type timelineEvent struct {
	Event string `json:"event"`
	Label struct {
		Name string `json:"name"`
	} `json:"label"`
}

func isApprovalCurrent(ctx context.Context, ghBin, repo string, prNumber int) (bool, error) {
	endpoint := fmt.Sprintf("repos/%s/issues/%d/timeline", repo, prNumber)
	out, err := ghOutput(ctx, ghBin, "api", "--paginate", endpoint)
	if err != nil {
		return false, err
	}
	return approvalCurrentFromTimeline(out)
}

func approvalCurrentFromTimeline(raw string) (bool, error) {
	dec := json.NewDecoder(strings.NewReader(raw))
	approved := false
	for {
		var page []timelineEvent
		if err := dec.Decode(&page); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return false, fmt.Errorf("parse PR timeline: %w", err)
		}
		for _, ev := range page {
			switch ev.Event {
			case "labeled":
				if ev.Label.Name == labelApproved {
					approved = true
				}
			case "unlabeled":
				if ev.Label.Name == labelApproved {
					approved = false
				}
			case "committed", "head_ref_force_pushed":
				approved = false
			}
		}
	}
	return approved, nil
}

func prepareAgentWorkdir(ctx context.Context, gitDir, workDir, outDir, cacheDir, noHooksDir, branch string) error {
	if err := os.RemoveAll(workDir); err != nil {
		return fmt.Errorf("remove workdir: %w", err)
	}
	if err := os.RemoveAll(outDir); err != nil {
		return fmt.Errorf("remove outdir: %w", err)
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return fmt.Errorf("create outdir: %w", err)
	}
	if err := os.MkdirAll(filepath.Join(cacheDir, "agent-gh"), 0o755); err != nil {
		return fmt.Errorf("create agent-gh dir: %w", err)
	}
	if err := runGit(ctx, "", "init", "-q", workDir); err != nil {
		return fmt.Errorf("init agent workdir: %w", err)
	}
	if err := runGit(ctx, workDir, "config", "core.hooksPath", noHooksDir); err != nil {
		return fmt.Errorf("set agent hooksPath: %w", err)
	}
	if err := runGit(ctx, workDir, "fetch", "-q", gitDir,
		"+refs/remotes/*:refs/remotes/*",
		"+refs/upstream-tags/*:refs/upstream-tags/*",
		"+refs/fork-keeper/*:refs/fork-keeper/*"); err != nil {
		return fmt.Errorf("fetch into agent workdir: %w", err)
	}
	if err := runGit(ctx, workDir, "checkout", "-q", "--detach", "refs/remotes/origin/"+branch); err != nil {
		return fmt.Errorf("checkout branch in agent workdir: %w", err)
	}

	userName := gitConfigOrDefault(ctx, "user.name", "fork-keeper")
	userEmail := gitConfigOrDefault(ctx, "user.email", "fork-keeper@localhost")
	gitconfig := fmt.Sprintf("[user]\n\tname = %s\n\temail = %s\n[core]\n\thooksPath = %s\n",
		userName, userEmail, noHooksDir)
	if err := os.WriteFile(filepath.Join(cacheDir, "agent-gitconfig"), []byte(gitconfig), 0o600); err != nil {
		return fmt.Errorf("write agent-gitconfig: %w", err)
	}
	return nil
}

func scrubbedAgentEnv(base []string, input ReviewInput) []string {
	blocked := map[string]bool{
		"GH_TOKEN":                   true,
		"GITHUB_TOKEN":               true,
		"GH_ENTERPRISE_TOKEN":        true,
		"GITHUB_ENTERPRISE_TOKEN":    true,
		"GH_BIN":                     true,
		"SSH_AUTH_SOCK":              true,
		"GIT_ASKPASS":                true,
		"SSH_ASKPASS":                true,
		"GIT_SSH":                    true,
		"GIT_SSH_COMMAND":            true,
		"GH_CONFIG_DIR":              true,
		"GIT_CONFIG_GLOBAL":          true,
		"GIT_CONFIG_NOSYSTEM":        true,
		"GIT_TERMINAL_PROMPT":        true,
		"FORK_KEEPER_OUT":            true,
		"FORK_KEEPER_BRANCH":         true,
		"FORK_KEEPER_REPO":           true,
		"FORK_KEEPER_WORK_DIR":       true,
		"FORK_KEEPER_RANGE_FILE":     true,
		"FORK_KEEPER_DIFF_FILE":      true,
		"FORK_KEEPER_PROMPT_FILE":    true,
		"FORK_KEEPER_SCHEMA_FILE":    true,
		"FORK_KEEPER_UPSTREAM_OLD":   true,
		"FORK_KEEPER_UPSTREAM_NEW":   true,
		"FORK_KEEPER_REVIEW_BASE":    true,
		"FORK_KEEPER_CANDIDATE_HEAD": true,
	}
	env := make([]string, 0, len(base)+16)
	for _, entry := range base {
		k, _, ok := strings.Cut(entry, "=")
		if !ok || blocked[k] {
			continue
		}
		env = append(env, entry)
	}
	reviewBase := ""
	if input.Range.ReviewBase != nil {
		reviewBase = *input.Range.ReviewBase
	}
	env = append(env,
		"GH_CONFIG_DIR="+filepath.Join(input.CacheDir, "agent-gh"),
		"GIT_CONFIG_GLOBAL="+filepath.Join(input.CacheDir, "agent-gitconfig"),
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_TERMINAL_PROMPT=0",
		"FORK_KEEPER_OUT="+input.OutDir,
		"FORK_KEEPER_BRANCH="+input.Fork.Branch,
		"FORK_KEEPER_REPO="+input.Fork.Repo,
		"FORK_KEEPER_WORK_DIR="+input.WorkDir,
		"FORK_KEEPER_RANGE_FILE="+input.RangeFile,
		"FORK_KEEPER_DIFF_FILE="+input.DiffFile,
		"FORK_KEEPER_PROMPT_FILE="+input.PromptFile,
		"FORK_KEEPER_SCHEMA_FILE="+input.SchemaFile,
		"FORK_KEEPER_UPSTREAM_OLD="+input.Range.UpstreamOld,
		"FORK_KEEPER_UPSTREAM_NEW="+input.Range.UpstreamNew,
		"FORK_KEEPER_REVIEW_BASE="+reviewBase,
		"FORK_KEEPER_CANDIDATE_HEAD="+input.CandidateHead,
	)
	return env
}

func runReviewRangeScript(ctx context.Context, scriptPath, gitDir, branch string) ([]byte, error) {
	cmd := exec.CommandContext(ctx, "bash", scriptPath, "-u", "upstream", "-b", branch)
	cmd.Env = append(os.Environ(), "GIT_DIR="+gitDir)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("%w: %s", err, strings.TrimSpace(stderr.String()))
	}
	return stdout.Bytes(), nil
}

func validateVerdictScript(ctx context.Context, scriptPath, verdictPath, candidate, rangeFile string) (bool, string) {
	cmd := exec.CommandContext(ctx, "bash", scriptPath, verdictPath, "--candidate-head", candidate, "--range", rangeFile)
	out, err := cmd.CombinedOutput()
	return err == nil, strings.TrimSpace(string(out))
}

func readVerdictMeta(verdictPath string) (verdict, summary string) {
	data, err := os.ReadFile(verdictPath)
	if err != nil {
		return "none", ""
	}
	var v struct {
		Verdict string `json:"verdict"`
		Summary string `json:"summary"`
	}
	if err := json.Unmarshal(data, &v); err != nil || v.Verdict == "" {
		return "none", ""
	}
	return v.Verdict, strings.TrimSpace(v.Summary)
}

func buildPRBody(reportPath, verdict, summary string, valid bool, vmsg string, hits []TripwireHit, twErr error, sticky bool) string {
	var b strings.Builder
	if data, err := os.ReadFile(reportPath); err == nil && len(bytes.TrimSpace(data)) > 0 {
		b.Write(data)
		if data[len(data)-1] != '\n' {
			b.WriteByte('\n')
		}
	} else if summary != "" {
		b.WriteString(summary)
		b.WriteByte('\n')
	} else {
		b.WriteString("No report.md written.\n")
	}

	b.WriteString("\n## Scheduler checks\n\n")
	fmt.Fprintf(&b, "- Verdict: `%s`\n", verdict)
	if valid {
		b.WriteString("- Verdict validation: passed\n")
	} else {
		b.WriteString("- Verdict validation: failed\n")
		for _, line := range strings.Split(vmsg, "\n") {
			if line != "" {
				fmt.Fprintf(&b, "  - %s\n", line)
			}
		}
	}

	if twErr == nil && len(hits) == 0 {
		b.WriteString("- Tripwires: none\n")
	} else {
		b.WriteString("- Tripwires:\n")
		if twErr != nil {
			fmt.Fprintf(&b, "  - %v\n", twErr)
		}
		for _, h := range hits {
			fmt.Fprintf(&b, "  - %s\n", h.String())
		}
	}

	if sticky {
		fmt.Fprintf(&b, "- Already `%s`: waits for `%s`.\n", labelNeedsReview, labelApproved)
	}
	return b.String()
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

func copyExecutable(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("read %s: %w", src, err)
	}
	//nolint:gosec // Helper scripts copied to a private temp dir must be executable.
	if err := os.WriteFile(dst, data, 0o755); err != nil {
		return fmt.Errorf("write %s: %w", dst, err)
	}
	return nil
}

func runGit(ctx context.Context, dir string, args ...string) error {
	cmd := exec.CommandContext(ctx, "git", args...)
	if dir != "" {
		cmd.Dir = dir
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("git %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return nil
}

func ghOutput(ctx context.Context, ghBin string, args ...string) (string, error) {
	if ghBin == "" {
		ghBin = DefaultGHBin
	}
	//nolint:gosec // ghBin is resolved from config (gh_bin) or GH_BIN without a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, ghBin, args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%s %s: %w: %s", ghBin, strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}

func gitRevParseOptional(ctx context.Context, gitDir, rev string) string {
	out, err := gitOutput(ctx, gitDir, "rev-parse", "-q", "--verify", rev)
	if err != nil {
		return ""
	}
	return strings.TrimSpace(out)
}

func gitRevParseDirOptional(ctx context.Context, dir, rev string) string {
	cmd := exec.CommandContext(ctx, "git", "-C", dir, "rev-parse", "-q", "--verify", rev)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	if err := cmd.Run(); err != nil {
		return ""
	}
	return strings.TrimSpace(stdout.String())
}

func gitConfigOrDefault(ctx context.Context, key, fallback string) string {
	cmd := exec.CommandContext(ctx, "git", "config", key)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	if err := cmd.Run(); err != nil {
		return fallback
	}
	val := strings.TrimSpace(stdout.String())
	if val == "" {
		return fallback
	}
	return val
}
