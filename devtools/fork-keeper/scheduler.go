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
	ConfigPath string
	CacheDir   string
	ScriptsDir string
	Today      string
	ForkNames  []string
	Stdout     io.Writer
	Stderr     io.Writer
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

	if err := validatePrerequisites(opts); err != nil {
		logErr("-", err)
		return err
	}
	cfg, err := LoadConfig(opts.ConfigPath)
	if err != nil {
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
		if err := runFork(ctx, fork.withDefaults(), opts, logMsg); err != nil {
			logErr(name, err)
			if firstErr == nil {
				firstErr = err
			}
		}
	}
	return firstErr
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
	return opts
}

func validatePrerequisites(opts Options) error {
	for _, tool := range []string{"git", "jq", "gh"} {
		if _, err := exec.LookPath(tool); err != nil {
			return fmt.Errorf("%s is required", tool)
		}
	}
	if _, err := os.Stat(opts.ConfigPath); err != nil {
		return fmt.Errorf("no config at %s", opts.ConfigPath)
	}
	for _, s := range []string{"review-range.sh", "validate-verdict.sh"} {
		p := filepath.Join(opts.ScriptsDir, s)
		if _, err := os.Stat(p); err != nil {
			return fmt.Errorf("missing %s", p)
		}
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
}

func runFork(ctx context.Context, f Fork, opts Options, logMsg func(string, string, ...any)) error {
	paths, cleanup, err := initForkPaths(opts, f.Name)
	if err != nil {
		return err
	}
	defer cleanup()

	rangeInfo, err := fetchAndComputeRange(ctx, f, paths)
	if err != nil {
		return err
	}
	if rangeInfo.UpToDate {
		logMsg(f.Name, "up-to-date: upstream is already in %s", f.Branch)
		return nil
	}

	pr, err := findOpenSyncPR(ctx, f.Repo)
	if err != nil {
		return err
	}

	syncBranch, prURL, sticky, done, err := handleExistingPR(ctx, f, opts, paths.gitDir, rangeInfo.UpstreamNew, pr, logMsg)
	if err != nil || done {
		return err
	}

	candidate, err := runAgentCandidate(ctx, f, opts, paths, logMsg)
	if err != nil {
		return err
	}

	return publishAndPromote(ctx, f, opts, paths, rangeInfo, pr, syncBranch, prURL, candidate, sticky, logMsg)
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

	for _, s := range []string{"review-range.sh", "validate-verdict.sh"} {
		if err := copyExecutable(filepath.Join(opts.ScriptsDir, s), filepath.Join(privDir, s)); err != nil {
			cleanup()
			return forkPaths{}, nil, fmt.Errorf("copy %s: %w", s, err)
		}
	}
	return forkPaths{
		noHooksDir: noHooksDir,
		gitDir:     filepath.Join(opts.CacheDir, forkName+".git"),
		workDir:    filepath.Join(opts.CacheDir, forkName+".work"),
		outDir:     filepath.Join(opts.CacheDir, forkName+".out"),
		privDir:    privDir,
		rangeFile:  filepath.Join(privDir, "range.json"),
	}, cleanup, nil
}

func fetchAndComputeRange(ctx context.Context, f Fork, paths forkPaths) (RangeInfo, error) {
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

	rangeBytes, err := runReviewRange(ctx, filepath.Join(paths.privDir, "review-range.sh"), paths.gitDir, f.Branch)
	if err != nil {
		return RangeInfo{}, fmt.Errorf("review-range.sh failed: %w", err)
	}
	if err := os.WriteFile(paths.rangeFile, rangeBytes, 0o600); err != nil {
		return RangeInfo{}, fmt.Errorf("write range.json: %w", err)
	}
	var rangeInfo RangeInfo
	if err := json.Unmarshal(rangeBytes, &rangeInfo); err != nil {
		return RangeInfo{}, fmt.Errorf("parse range.json: %w", err)
	}
	return rangeInfo, nil
}

func promoteFork(ctx context.Context, f Fork, opts Options, gitDir, head, up, tagsPrefix string) error {
	oldHead, err := gitOutput(ctx, gitDir, "rev-parse", "refs/remotes/origin/"+f.Branch)
	if err != nil {
		return fmt.Errorf("resolve origin/%s: %w", f.Branch, err)
	}
	oldHead = strings.TrimSpace(oldHead)
	if _, err := gitOutput(ctx, gitDir, "push", "-q", "--atomic",
		fmt.Sprintf("--force-with-lease=refs/heads/%s:%s", f.Branch, oldHead),
		f.ForkURL,
		fmt.Sprintf("%s:refs/tags/pre-sync/%s", oldHead, opts.Today),
		fmt.Sprintf("%s:refs/heads/%s", head, f.Branch)); err != nil {
		return fmt.Errorf("force-push of %s failed; fork-keeper refs left unchanged: %w", f.Branch, err)
	}
	if _, err := gitOutput(ctx, gitDir, "push", "-q", "--prune", f.ForkURL,
		fmt.Sprintf("+%s:refs/fork-keeper/upstream-head", up),
		fmt.Sprintf("+%s/*:refs/fork-keeper/upstream-tags/*", tagsPrefix)); err != nil {
		return fmt.Errorf("promoted %s but could not record refs/fork-keeper/upstream-head: %w", f.Branch, err)
	}
	return nil
}

func formatSyncLocation(syncBranch, prURL string) string {
	if prURL != "" {
		return syncBranch + " " + prURL
	}
	return syncBranch
}

func handleExistingPR(ctx context.Context, f Fork, opts Options, gitDir, upstreamNew string, pr *ghPR, logMsg func(string, string, ...any)) (syncBranch, prURL string, sticky, done bool, err error) {
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
			base, err := gitOutput(ctx, gitDir, "rev-parse", "-q", "--verify",
				fmt.Sprintf("refs/fork-keeper/sync-base/%s^{commit}", syncBranch))
			if err != nil {
				return "", "", false, false, fmt.Errorf("approved %s has no refs/fork-keeper/sync-base/%s", syncBranch, syncBranch)
			}
			if err := promoteFork(ctx, f, opts, gitDir, prHead, strings.TrimSpace(base), "refs/fork-keeper/sync-tags/"+syncBranch); err != nil {
				return "", "", false, false, err
			}
			logMsg(f.Name, "promoted approved %s (%s) to %s", formatSyncLocation(syncBranch, prURL), prHead, f.Branch)
			return syncBranch, prURL, false, true, nil
		}
		logMsg(f.Name, "ignoring %s: PR head %s is not the fetched %s", labelApproved, prHead, syncBranch)
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

func runAgentCandidate(ctx context.Context, f Fork, opts Options, paths forkPaths, logMsg func(string, string, ...any)) (string, error) {
	if err := prepareAgentWorkdir(ctx, paths.gitDir, paths.workDir, paths.outDir, opts.CacheDir, paths.noHooksDir, f.Branch); err != nil {
		return "", err
	}

	agentArgs := make([]string, len(f.Agent))
	for i, a := range f.Agent {
		a = strings.ReplaceAll(a, "{out}", paths.outDir)
		a = strings.ReplaceAll(a, "{branch}", f.Branch)
		agentArgs[i] = a
	}
	logMsg(f.Name, "running agent: %s", agentArgs[0])
	agentLogPath := filepath.Join(opts.CacheDir, f.Name+".agent.log")
	agentRC, err := runAgent(ctx, paths.workDir, paths.outDir, opts.CacheDir, agentLogPath, agentArgs)
	if err != nil {
		return "", err
	}
	if agentRC != 0 {
		logMsg(f.Name, "agent exited %d (see %s)", agentRC, agentLogPath)
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

func publishAndPromote(ctx context.Context, f Fork, opts Options, paths forkPaths, rangeInfo RangeInfo, pr *ghPR, syncBranch, prURL, candidate string, sticky bool, logMsg func(string, string, ...any)) error {
	valid, vmsg := validateVerdict(ctx, filepath.Join(paths.privDir, "validate-verdict.sh"),
		filepath.Join(paths.outDir, "verdict.json"), candidate, paths.rangeFile)
	verdict := readVerdictField(filepath.Join(paths.outDir, "verdict.json"))
	hits, twErr := CheckTripwires(ctx, paths.gitDir, rangeInfo, candidate)
	twRC := 0
	if twErr != nil || len(hits) > 0 {
		twRC = 1
	}

	bodyFile := filepath.Join(paths.privDir, "body.md")
	bodyContent := buildPRBody(filepath.Join(paths.outDir, "report.md"), verdict, valid, vmsg, hits, twErr, sticky)
	if err := os.WriteFile(bodyFile, []byte(bodyContent), 0o600); err != nil {
		return fmt.Errorf("write PR body: %w", err)
	}

	if pr != nil && pr.hasLabel(labelApproved) {
		if _, err := ghOutput(ctx, "pr", "edit", syncBranch, "-R", f.Repo, "--remove-label", labelApproved); err != nil {
			return fmt.Errorf("cannot remove %s before refreshing %s: %w", labelApproved, syncBranch, err)
		}
	}

	if _, err := gitOutput(ctx, paths.gitDir, "push", "-q", "--prune", f.ForkURL,
		fmt.Sprintf("+%s:refs/heads/%s", candidate, syncBranch),
		fmt.Sprintf("+%s:refs/fork-keeper/sync-base/%s", rangeInfo.UpstreamNew, syncBranch),
		fmt.Sprintf("+refs/upstream-tags/*:refs/fork-keeper/sync-tags/%s/*", syncBranch)); err != nil {
		return fmt.Errorf("cannot push %s: %w", syncBranch, err)
	}

	var err error
	prURL, err = upsertSyncPR(ctx, f, opts.Today, pr, syncBranch, prURL, bodyFile, logMsg)
	if err != nil {
		return err
	}

	if valid && verdict == "clean" && twRC == 0 && !sticky {
		if err := promoteFork(ctx, f, opts, paths.gitDir, candidate, rangeInfo.UpstreamNew, "refs/upstream-tags"); err != nil {
			return err
		}
		logMsg(f.Name, "promoted %s (%s) to %s", formatSyncLocation(syncBranch, prURL), candidate, f.Branch)
		return nil
	}

	if _, err := ghOutput(ctx, "pr", "edit", syncBranch, "-R", f.Repo, "--add-label", labelNeedsReview); err != nil {
		return fmt.Errorf("cannot label %s %s: %w", syncBranch, labelNeedsReview, err)
	}
	logMsg(f.Name, "needs-review: %s (verdict %s, validator %t, tripwires rc %d, sticky %t)",
		formatSyncLocation(syncBranch, prURL), verdict, valid, twRC, sticky)
	return nil
}

func upsertSyncPR(ctx context.Context, f Fork, today string, pr *ghPR, syncBranch, prURL, bodyFile string, logMsg func(string, string, ...any)) (string, error) {
	if pr != nil {
		if _, err := ghOutput(ctx, "pr", "edit", syncBranch, "-R", f.Repo, "--body-file", bodyFile); err != nil {
			return prURL, fmt.Errorf("cannot refresh the PR for %s: %w", syncBranch, err)
		}
		target := prURL
		if target == "" {
			target = syncBranch
		}
		logMsg(f.Name, "refreshed %s", target)
		return prURL, nil
	}

	createdURL, err := ghOutput(ctx, "pr", "create", "-R", f.Repo,
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

func findOpenSyncPR(ctx context.Context, repo string) (*ghPR, error) {
	out, err := ghOutput(ctx, "pr", "list", "-R", repo, "--state", "open",
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

func runAgent(ctx context.Context, workDir, outDir, cacheDir, logPath string, args []string) (int, error) {
	logFile, err := os.Create(logPath)
	if err != nil {
		return 0, fmt.Errorf("create agent log %s: %w", logPath, err)
	}
	defer logFile.Close()

	//nolint:gosec // Configured agent command from forks.json.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, args[0], args[1:]...)
	cmd.Dir = workDir
	cmd.Env = scrubbedAgentEnv(os.Environ(), cacheDir, outDir)
	cmd.Stdout = logFile
	cmd.Stderr = logFile
	if err := cmd.Run(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			return exitErr.ExitCode(), nil
		}
		return 1, nil
	}
	return 0, nil
}

func scrubbedAgentEnv(base []string, cacheDir, outDir string) []string {
	blocked := map[string]bool{
		"GH_TOKEN":                true,
		"GITHUB_TOKEN":            true,
		"GH_ENTERPRISE_TOKEN":     true,
		"GITHUB_ENTERPRISE_TOKEN": true,
		"SSH_AUTH_SOCK":           true,
		"GIT_ASKPASS":             true,
		"SSH_ASKPASS":             true,
		"GH_CONFIG_DIR":           true,
		"GIT_CONFIG_GLOBAL":       true,
		"GIT_CONFIG_NOSYSTEM":     true,
		"GIT_TERMINAL_PROMPT":     true,
		"FORK_KEEPER_OUT":         true,
	}
	env := make([]string, 0, len(base)+5)
	for _, entry := range base {
		k, _, ok := strings.Cut(entry, "=")
		if !ok || blocked[k] {
			continue
		}
		env = append(env, entry)
	}
	env = append(env,
		"GH_CONFIG_DIR="+filepath.Join(cacheDir, "agent-gh"),
		"GIT_CONFIG_GLOBAL="+filepath.Join(cacheDir, "agent-gitconfig"),
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_TERMINAL_PROMPT=0",
		"FORK_KEEPER_OUT="+outDir,
	)
	return env
}

func runReviewRange(ctx context.Context, scriptPath, gitDir, branch string) ([]byte, error) {
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

func validateVerdict(ctx context.Context, scriptPath, verdictPath, candidate, rangeFile string) (bool, string) {
	cmd := exec.CommandContext(ctx, "bash", scriptPath, verdictPath, "--candidate-head", candidate, "--range", rangeFile)
	out, err := cmd.CombinedOutput()
	return err == nil, strings.TrimSpace(string(out))
}

func readVerdictField(verdictPath string) string {
	data, err := os.ReadFile(verdictPath)
	if err != nil {
		return "none"
	}
	var v struct {
		Verdict string `json:"verdict"`
	}
	if err := json.Unmarshal(data, &v); err != nil || v.Verdict == "" {
		return "none"
	}
	return v.Verdict
}

func buildPRBody(reportPath, verdict string, valid bool, vmsg string, hits []TripwireHit, twErr error, sticky bool) string {
	var b strings.Builder
	if data, err := os.ReadFile(reportPath); err == nil && len(data) > 0 {
		b.Write(data)
		if data[len(data)-1] != '\n' {
			b.WriteByte('\n')
		}
	} else {
		b.WriteString("The agent wrote no report.md.\n")
	}

	b.WriteString("\n## Scheduler checks\n\n")
	fmt.Fprintf(&b, "- Verdict: `%s`\n", verdict)
	if valid {
		b.WriteString("- validate-verdict.sh: passed\n")
	} else {
		b.WriteString("- validate-verdict.sh: failed\n")
		for _, line := range strings.Split(vmsg, "\n") {
			fmt.Fprintf(&b, "  - %s\n", line)
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

func ghOutput(ctx context.Context, args ...string) (string, error) {
	cmd := exec.CommandContext(ctx, "gh", args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("gh %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
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
