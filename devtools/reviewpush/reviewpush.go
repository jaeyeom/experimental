// Package reviewpush implements the review-and-push loop that reviews and
// pushes local commits one at a time using a read-only reviewer.
package reviewpush

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	// OutputModeHuman preserves the interactive terminal output.
	OutputModeHuman = "human"
	// OutputModeCompact minimizes output for line-oriented agent workflows.
	OutputModeCompact = "compact"
	// OutputModeJSON emits a single structured JSON RunReport on stdout.
	OutputModeJSON = "json"
)

// RunReport is the structured summary emitted on stdout when OutputMode is OutputModeJSON.
type RunReport struct {
	Status        string            `json:"status"`
	Base          string            `json:"base,omitempty"`
	PushedCommits []string          `json:"pushed_commits,omitempty"` //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
	Iterations    []IterationReport `json:"iterations,omitempty"`
	BlockedCommit string            `json:"blocked_commit,omitempty"` //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
	Reason        string            `json:"reason,omitempty"`
	ExitCode      int               `json:"exit_code"` //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
}

// IterationReport describes a single review-and-push pass in RunReport.
type IterationReport struct {
	Iteration    int    `json:"iteration"`
	TargetCommit string `json:"target_commit"` //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
	AheadCount   int    `json:"ahead_count"`   //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
	Status       string `json:"status"`
	PushTarget   string `json:"push_target,omitempty"` //nolint:tagliatelle // reviewpush JSON output contract uses snake_case
	Summary      string `json:"summary,omitempty"`
	Reason       string `json:"reason,omitempty"`
}

// Config holds configuration for the review-and-push loop.
type Config struct {
	RepoDir            string
	BaseBranchOverride string
	MaxIterations      int
	OutputMode         string
	GitBin             string
	ReviewerCmd        string
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
	Cfg      Config
	Exec     executor.Executor
	Reviewer Reviewer
	// Codex is a backwards-compatible alias field; prefer Reviewer.
	Codex  CodexRunner
	Stdout io.Writer
	Stderr io.Writer
}

// NewRunner creates a new Runner with default I/O writers.
func NewRunner(cfg Config, exec executor.Executor, reviewer Reviewer) *Runner {
	return &Runner{
		Cfg:      cfg,
		Exec:     exec,
		Reviewer: reviewer,
		Stdout:   os.Stdout,
		Stderr:   os.Stderr,
	}
}

func (r *Runner) reviewer() Reviewer {
	if r.Reviewer != nil {
		return r.Reviewer
	}
	return r.Codex
}

func (r *Runner) log(format string, args ...any) {
	fmt.Fprintf(r.Stdout, format+"\n", args...)
}

func (r *Runner) isCompactOutput() bool {
	return r.Cfg.OutputMode == OutputModeCompact
}

func (r *Runner) isJSONOutput() bool {
	return r.Cfg.OutputMode == OutputModeJSON
}

func (r *Runner) isQuietProgress() bool {
	return r.isCompactOutput() || r.isJSONOutput()
}

func shortCommit(commit string) string {
	if len(commit) <= 12 {
		return commit
	}
	return commit[:12]
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
// review pass, showing git log --stat --format=fuller output.
func (r *Runner) PrintCommitBanner(ctx context.Context, commit string) {
	output, err := r.gitOutput(ctx, "log", "--stat", "--format=fuller", "-1", commit)
	if err != nil {
		return
	}
	sep := strings.Repeat("=", 40)
	fmt.Fprintf(r.Stdout, "%s\nTARGET COMMIT\n%s\n%s\n%s\n", sep, sep, output, sep)
}

func (r *Runner) logCompactIteration(iteration, ahead int, targetCommit, status, detail string) {
	msg := fmt.Sprintf(
		"ITERATION %d target=%s ahead=%d status=%s",
		iteration,
		shortCommit(targetCommit),
		ahead,
		status,
	)
	if detail != "" {
		msg += " " + detail
	}
	r.log(msg)
}

func (r *Runner) logDone(base *RemoteBase) {
	if r.isCompactOutput() {
		r.log("DONE base=%s", base.Ref)
		return
	}
	if !r.isJSONOutput() {
		r.log("DONE: HEAD is not ahead of %s.", base.Ref)
	}
}

func (r *Runner) emitReport(report RunReport) int {
	if r.isJSONOutput() {
		enc := json.NewEncoder(r.Stdout)
		enc.SetEscapeHTML(false)
		_ = enc.Encode(report)
	}
	return report.ExitCode
}

// handlePush validates and pushes commits based on the review result.
// Returns the commit that was pushed, a machine-readable reason code, and a
// human error message on failure.
func (r *Runner) handlePush(ctx context.Context, result *ReviewResult, targetCommit string, base *RemoteBase) (string, string, string) {
	pushTarget := targetCommit
	if result.MultiCommits && result.PushTarget != nil {
		pushTarget = *result.PushTarget
	}

	if !r.ValidatePushTarget(ctx, pushTarget, base, targetCommit) {
		return "", "invalid_push_target", fmt.Sprintf("BLOCKED: reviewer proposed invalid push target '%s' for oldest commit %s.", pushTarget, targetCommit)
	}

	refspec := pushTarget + ":refs/heads/" + base.Branch
	if err := r.gitRun(ctx, "push", "origin", refspec); err != nil {
		return "", "push_failed", fmt.Sprintf("BLOCKED: push failed for %s.", pushTarget)
	}

	if !r.isQuietProgress() {
		r.log("Pushed through %s.", pushTarget)
	}
	return pushTarget, "", ""
}

func (r *Runner) runReviewPass(ctx context.Context, base *RemoteBase, iteration, ahead int, targetCommit string) (*ReviewResult, error) {
	if !r.isQuietProgress() {
		r.log("Iteration %d: reviewing and pushing %s (%d commit(s) ahead of %s).", iteration, targetCommit, ahead, base.Ref)
		r.PrintCommitBanner(ctx, targetCommit)
	}

	rev := r.reviewer()
	if rev == nil {
		return nil, fmt.Errorf("no reviewer configured")
	}

	result, err := rev.RunPass(ctx, &ReviewPassInput{
		RepoDir:         r.Cfg.RepoDir,
		BaseBranch:      base.Branch,
		RemoteRef:       base.Ref,
		RemoteRefExists: base.Exists,
		Iteration:       iteration,
		AheadCount:      ahead,
		TargetCommit:    targetCommit,
	})
	if err != nil {
		return nil, fmt.Errorf("run reviewer pass: %w", err)
	}
	if err := ValidateReviewResult(result); err != nil {
		return nil, err
	}
	return result, nil
}

func (r *Runner) handlePassFailure(report *RunReport, iteration, ahead int, targetCommit string, err error) {
	reason := err.Error()
	report.Status = StatusBlocked
	report.BlockedCommit = targetCommit
	report.Reason = reason
	report.ExitCode = 2
	report.Iterations = append(report.Iterations, IterationReport{
		Iteration:    iteration,
		TargetCommit: targetCommit,
		AheadCount:   ahead,
		Status:       StatusBlocked,
		Reason:       reason,
	})
	if r.isCompactOutput() {
		r.logCompactIteration(iteration, ahead, targetCommit, StatusBlocked, "reason=reviewer_execution_failed")
		return
	}
	if !r.isJSONOutput() {
		r.log("BLOCKED: reviewer execution failed during iteration %d: %v.", iteration, err)
	}
}

func (r *Runner) handlePassResult(ctx context.Context, report *RunReport, result *ReviewResult, base *RemoteBase, iteration, ahead int, targetCommit string) bool {
	switch result.Status {
	case StatusPush:
		pushTarget, code, msg := r.handlePush(ctx, result, targetCommit, base)
		if msg != "" {
			report.Status = StatusBlocked
			report.BlockedCommit = targetCommit
			report.Reason = msg
			report.ExitCode = 2
			report.Iterations = append(report.Iterations, IterationReport{
				Iteration:    iteration,
				TargetCommit: targetCommit,
				AheadCount:   ahead,
				Status:       StatusBlocked,
				Summary:      result.Summary,
				Reason:       msg,
			})
			if r.isCompactOutput() {
				r.logCompactIteration(iteration, ahead, targetCommit, StatusBlocked, "reason="+code)
			} else if !r.isJSONOutput() {
				r.log("%s", msg)
			}
			return true
		}
		report.PushedCommits = append(report.PushedCommits, pushTarget)
		report.Iterations = append(report.Iterations, IterationReport{
			Iteration:    iteration,
			TargetCommit: targetCommit,
			AheadCount:   ahead,
			Status:       StatusPush,
			PushTarget:   pushTarget,
			Summary:      result.Summary,
		})
		if r.isCompactOutput() {
			r.logCompactIteration(iteration, ahead, targetCommit, StatusPush, fmt.Sprintf("push_target=%s", shortCommit(pushTarget)))
		}
		return false
	case StatusBlocked:
		reason := "blocked"
		if result.BlockedReason != nil {
			reason = *result.BlockedReason
		}
		report.Status = StatusBlocked
		report.BlockedCommit = targetCommit
		report.Reason = reason
		report.ExitCode = 2
		report.Iterations = append(report.Iterations, IterationReport{
			Iteration:    iteration,
			TargetCommit: targetCommit,
			AheadCount:   ahead,
			Status:       StatusBlocked,
			Summary:      result.Summary,
			Reason:       reason,
		})
		if r.isCompactOutput() {
			r.logCompactIteration(iteration, ahead, targetCommit, StatusBlocked, fmt.Sprintf("reason=%q", reason))
			return true
		}
		if !r.isJSONOutput() {
			r.log("BLOCKED: review-and-push stopped on %s.", targetCommit)
			if result.BlockedReason != nil {
				r.log("Reason: %s", *result.BlockedReason)
			}
		}
		return true
	default:
		reason := fmt.Sprintf("unexpected reviewer status '%s'", result.Status)
		report.Status = StatusBlocked
		report.BlockedCommit = targetCommit
		report.Reason = reason
		report.ExitCode = 2
		report.Iterations = append(report.Iterations, IterationReport{
			Iteration:    iteration,
			TargetCommit: targetCommit,
			AheadCount:   ahead,
			Status:       StatusBlocked,
			Reason:       reason,
		})
		if r.isCompactOutput() {
			r.logCompactIteration(iteration, ahead, targetCommit, StatusBlocked, "reason=unexpected_status")
			return true
		}
		if !r.isJSONOutput() {
			r.log("BLOCKED: unexpected reviewer status '%s' for %s.", result.Status, targetCommit)
		}
		return true
	}
}

func (r *Runner) reportRuntimeError(report *RunReport, msg string) int {
	if r.isJSONOutput() {
		report.Status = "ERROR"
		report.Reason = msg
		report.ExitCode = 2
		return r.emitReport(*report)
	}
	fmt.Fprintf(r.Stderr, "Error: %s\n", msg)
	return 2
}

func (r *Runner) runIteration(ctx context.Context, report *RunReport, base *RemoteBase, iteration int) (bool, int) {
	if !r.isQuietProgress() {
		r.log("Fetching %s before iteration %d...", base.Ref, iteration)
	}
	if err := r.gitRun(ctx, "fetch", "origin"); err != nil {
		return true, r.reportRuntimeError(report, fmt.Sprintf("fetch failed: %v", err))
	}
	if !base.Exists {
		base.Exists = r.remoteRefExists(ctx, base.Branch)
	}

	ahead, err := r.AheadCount(ctx, base)
	if err != nil {
		return true, r.reportRuntimeError(report, err.Error())
	}
	if ahead == 0 {
		r.logDone(base)
		report.Status = "DONE"
		report.ExitCode = 0
		return true, r.emitReport(*report)
	}

	targetCommit, err := r.OldestAheadCommit(ctx, base)
	if err != nil {
		return true, r.reportRuntimeError(report, err.Error())
	}
	result, err := r.runReviewPass(ctx, base, iteration, ahead, targetCommit)
	if err != nil {
		r.handlePassFailure(report, iteration, ahead, targetCommit, err)
		return true, r.emitReport(*report)
	}
	if r.handlePassResult(ctx, report, result, base, iteration, ahead, targetCommit) {
		return true, r.emitReport(*report)
	}
	return false, 0
}

// Run executes the review-and-push loop. Returns 0 on success (all commits
// pushed), 1 on configuration errors, and 2 when blocked.
func (r *Runner) Run(ctx context.Context) int {
	base, err := r.ResolveRemoteBase(ctx)
	if err != nil {
		if r.isJSONOutput() {
			return r.emitReport(RunReport{Status: "ERROR", Reason: err.Error(), ExitCode: 1})
		}
		fmt.Fprintf(r.Stderr, "Error: %v\n", err)
		return 1
	}

	report := RunReport{Base: base.Ref}
	for iteration := 1; iteration <= r.Cfg.MaxIterations; iteration++ {
		if done, code := r.runIteration(ctx, &report, base, iteration); done {
			return code
		}
	}

	msg := fmt.Sprintf("reached max iterations (%d) before the branch was fully pushed", r.Cfg.MaxIterations)
	if r.isJSONOutput() {
		report.Status = StatusBlocked
		report.Reason = msg
		report.ExitCode = 2
		return r.emitReport(report)
	}
	r.log("BLOCKED: %s.", msg)
	return 2
}
