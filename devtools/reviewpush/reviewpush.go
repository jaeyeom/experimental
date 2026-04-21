// Package reviewpush implements the review-and-push loop that reviews and
// pushes local commits one at a time using Codex.
package reviewpush

import (
	"context"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

// CodexResult represents the JSON response from a Codex review pass.
type CodexResult struct {
	Status        string  `json:"status"`
	Summary       string  `json:"summary"`
	MultiCommits  bool    `json:"multi_commits"`  //nolint:tagliatelle // Codex API uses snake_case
	PushTarget    *string `json:"push_target"`    //nolint:tagliatelle // Codex API uses snake_case
	BlockedReason *string `json:"blocked_reason"` //nolint:tagliatelle // Codex API uses snake_case
}

// CodexPassInput contains the input parameters for a Codex review pass.
type CodexPassInput struct {
	RepoDir         string
	BaseBranch      string
	RemoteRef       string
	RemoteRefExists bool
	Iteration       int
	AheadCount      int
	TargetCommit    string
}

// CodexRunner executes a Codex review pass and returns the parsed result.
type CodexRunner interface {
	RunPass(ctx context.Context, input *CodexPassInput) (*CodexResult, error)
}

// Config holds configuration for the review-and-push loop.
type Config struct {
	RepoDir            string
	BaseBranchOverride string
	MaxIterations      int
	GitBin             string
	CodexBin           string
	CodexTimeoutSecs   int
}

// RemoteBase describes the remote base branch and whether its remote tracking
// ref currently exists in the local Git repository. Exists is false when the
// remote base branch is unborn, such as on the first push to a brand-new
// remote repository or branch.
type RemoteBase struct {
	Branch string
	Ref    string
	Exists bool
}

// Runner executes the review-and-push loop.
type Runner struct {
	Cfg    Config
	Exec   executor.Executor
	Codex  CodexRunner
	Stdout io.Writer
	Stderr io.Writer
}

// NewRunner creates a new Runner with default I/O writers.
func NewRunner(cfg Config, exec executor.Executor, codex CodexRunner) *Runner {
	return &Runner{
		Cfg:    cfg,
		Exec:   exec,
		Codex:  codex,
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}
}

func (r *Runner) log(format string, args ...any) {
	fmt.Fprintf(r.Stdout, format+"\n", args...)
}

func (r *Runner) gitOutput(ctx context.Context, args ...string) (string, error) {
	output, err := executor.OutputWithWorkDir(ctx, r.Exec, r.Cfg.RepoDir, r.Cfg.GitBin, args...)
	if err != nil {
		return "", fmt.Errorf("git %s: %w", args[0], err)
	}
	return strings.TrimSpace(string(output)), nil
}

func (r *Runner) gitRun(ctx context.Context, args ...string) error {
	if err := executor.RunWithWorkDir(ctx, r.Exec, r.Cfg.RepoDir, r.Cfg.GitBin, args...); err != nil {
		return fmt.Errorf("git %s: %w", args[0], err)
	}
	return nil
}

func (r *Runner) remoteRefExists(ctx context.Context, branch string) bool {
	return r.gitRun(ctx, "show-ref", "--verify", "--quiet", "refs/remotes/origin/"+branch) == nil
}

// ResolveRemoteBase detects the remote base branch and whether its remote
// tracking ref currently exists. It honors Cfg.BaseBranchOverride first, then
// falls back to origin/HEAD, then to well-known branch names (main, master,
// trunk). When no remote refs exist and no override is provided, it falls
// back to the current local branch name with Exists=false so the first-push
// path can bootstrap a brand-new remote branch.
func (r *Runner) ResolveRemoteBase(ctx context.Context) (*RemoteBase, error) {
	if r.Cfg.BaseBranchOverride != "" {
		branch := r.Cfg.BaseBranchOverride
		return &RemoteBase{
			Branch: branch,
			Ref:    "origin/" + branch,
			Exists: r.remoteRefExists(ctx, branch),
		}, nil
	}

	detected, err := r.gitOutput(ctx, "symbolic-ref", "--short", "refs/remotes/origin/HEAD")
	if err == nil && detected != "" {
		branch := strings.TrimPrefix(detected, "origin/")
		return &RemoteBase{Branch: branch, Ref: "origin/" + branch, Exists: true}, nil
	}

	for _, candidate := range []string{"main", "master", "trunk"} {
		if r.remoteRefExists(ctx, candidate) {
			return &RemoteBase{Branch: candidate, Ref: "origin/" + candidate, Exists: true}, nil
		}
	}

	currentBranch, err := r.gitOutput(ctx, "rev-parse", "--abbrev-ref", "HEAD")
	if err != nil || currentBranch == "" || currentBranch == "HEAD" {
		return nil, fmt.Errorf("could not determine remote base branch; pass --base to set one explicitly")
	}
	return &RemoteBase{
		Branch: currentBranch,
		Ref:    "origin/" + currentBranch,
		Exists: false,
	}, nil
}

// AheadCount returns the number of commits HEAD is ahead of the remote base.
// When the remote base is unborn (Exists=false), every commit reachable from
// HEAD counts as ahead.
func (r *Runner) AheadCount(ctx context.Context, base *RemoteBase) (int, error) {
	args := []string{"rev-list", "--count"}
	if base.Exists {
		args = append(args, base.Ref+"..HEAD")
	} else {
		args = append(args, "HEAD")
	}
	output, err := r.gitOutput(ctx, args...)
	if err != nil {
		return 0, fmt.Errorf("failed to get ahead count: %w", err)
	}
	n, err := strconv.Atoi(output)
	if err != nil {
		return 0, fmt.Errorf("parse ahead count %q: %w", output, err)
	}
	return n, nil
}

// OldestAheadCommit returns the oldest commit that is ahead of the remote
// base. When the remote base is unborn, it returns the root commit reachable
// from HEAD.
func (r *Runner) OldestAheadCommit(ctx context.Context, base *RemoteBase) (string, error) {
	args := []string{"rev-list"}
	if base.Exists {
		args = append(args, base.Ref+"..HEAD")
	} else {
		args = append(args, "HEAD")
	}
	output, err := r.gitOutput(ctx, args...)
	if err != nil {
		return "", fmt.Errorf("failed to list ahead commits: %w", err)
	}
	lines := strings.Split(strings.TrimSpace(output), "\n")
	if len(lines) == 0 || lines[0] == "" {
		return "", fmt.Errorf("no ahead commits found")
	}
	return lines[len(lines)-1], nil
}

// ValidatePushTarget checks that pushTarget is a valid commit that lies
// between the remote base and HEAD, inclusive of oldestTarget. When the
// remote base is unborn, the remote-ancestor check is skipped because there
// is no remote commit to compare against.
func (r *Runner) ValidatePushTarget(ctx context.Context, pushTarget string, base *RemoteBase, oldestTarget string) bool {
	if pushTarget == "" {
		return false
	}
	if err := r.gitRun(ctx, "rev-parse", "--verify", "--quiet", pushTarget+"^{commit}"); err != nil {
		return false
	}
	if base.Exists {
		if err := r.gitRun(ctx, "merge-base", "--is-ancestor", base.Ref, pushTarget); err != nil {
			return false
		}
	}
	if err := r.gitRun(ctx, "merge-base", "--is-ancestor", oldestTarget, pushTarget); err != nil {
		return false
	}
	if err := r.gitRun(ctx, "merge-base", "--is-ancestor", pushTarget, "HEAD"); err != nil {
		return false
	}
	return true
}

// PrintCommitBanner prints a visually prominent commit banner before a
// Codex review pass, showing git log --stat --format=fuller output.
func (r *Runner) PrintCommitBanner(ctx context.Context, commit string) {
	output, err := r.gitOutput(ctx, "log", "--stat", "--format=fuller", "-1", commit)
	if err != nil {
		return
	}
	sep := strings.Repeat("=", 40)
	fmt.Fprintf(r.Stdout, "%s\nTARGET COMMIT\n%s\n%s\n%s\n", sep, sep, output, sep)
}

// handlePush validates and pushes commits based on the Codex result.
// Returns an error message on failure, empty string on success.
func (r *Runner) handlePush(ctx context.Context, result *CodexResult, targetCommit string, base *RemoteBase) string {
	pushTarget := targetCommit
	if result.MultiCommits && result.PushTarget != nil {
		pushTarget = *result.PushTarget
	}

	if !r.ValidatePushTarget(ctx, pushTarget, base, targetCommit) {
		return fmt.Sprintf("BLOCKED: Codex proposed invalid push target '%s' for oldest commit %s.", pushTarget, targetCommit)
	}

	refspec := pushTarget + ":refs/heads/" + base.Branch
	if err := r.gitRun(ctx, "push", "origin", refspec); err != nil {
		return fmt.Sprintf("BLOCKED: push failed for %s.", pushTarget)
	}

	r.log("Pushed through %s.", pushTarget)
	return ""
}

// Run executes the review-and-push loop. Returns 0 on success (all commits
// pushed), 1 on configuration errors, and 2 when blocked.
func (r *Runner) Run(ctx context.Context) int {
	base, err := r.ResolveRemoteBase(ctx)
	if err != nil {
		fmt.Fprintf(r.Stderr, "Error: %v\n", err)
		return 1
	}

	for iteration := 1; iteration <= r.Cfg.MaxIterations; iteration++ {
		r.log("Fetching %s before iteration %d...", base.Ref, iteration)
		if err := r.gitRun(ctx, "fetch", "origin"); err != nil {
			fmt.Fprintf(r.Stderr, "Error: fetch failed: %v\n", err)
			return 2
		}
		if !base.Exists {
			base.Exists = r.remoteRefExists(ctx, base.Branch)
		}

		ahead, err := r.AheadCount(ctx, base)
		if err != nil {
			fmt.Fprintf(r.Stderr, "Error: %v\n", err)
			return 2
		}
		if ahead == 0 {
			r.log("DONE: HEAD is not ahead of %s.", base.Ref)
			return 0
		}

		targetCommit, err := r.OldestAheadCommit(ctx, base)
		if err != nil {
			fmt.Fprintf(r.Stderr, "Error: %v\n", err)
			return 2
		}

		r.log("Iteration %d: reviewing and pushing %s (%d commit(s) ahead of %s).", iteration, targetCommit, ahead, base.Ref)
		r.PrintCommitBanner(ctx, targetCommit)

		result, err := r.Codex.RunPass(ctx, &CodexPassInput{
			RepoDir:         r.Cfg.RepoDir,
			BaseBranch:      base.Branch,
			RemoteRef:       base.Ref,
			RemoteRefExists: base.Exists,
			Iteration:       iteration,
			AheadCount:      ahead,
			TargetCommit:    targetCommit,
		})
		if err != nil {
			r.log("BLOCKED: Codex execution failed during iteration %d.", iteration)
			return 2
		}

		switch result.Status {
		case "PUSH":
			if msg := r.handlePush(ctx, result, targetCommit, base); msg != "" {
				r.log("%s", msg)
				return 2
			}
		case "BLOCKED":
			r.log("BLOCKED: review-and-push stopped on %s.", targetCommit)
			if result.BlockedReason != nil {
				r.log("Reason: %s", *result.BlockedReason)
			}
			return 2
		default:
			r.log("BLOCKED: unexpected Codex status '%s' for %s.", result.Status, targetCommit)
			return 2
		}
	}

	r.log("BLOCKED: reached max iterations (%d) before the branch was fully pushed.", r.Cfg.MaxIterations)
	return 2
}
