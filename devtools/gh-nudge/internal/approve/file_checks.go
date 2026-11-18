package approve

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const fileChecksCondition = "file_checks"

// maxFileCheckOutput caps stdout and stderr captured from a file_checks script.
const maxFileCheckOutput = 64 * 1024

// FileCheckRunner runs a file_checks script with env and stdin.
type FileCheckRunner func(ctx context.Context, env []string, stdin []byte, argv []string) (remotecall.Result, error)

// FileCheckFunc evaluates file_checks for a single rule and returns its
// condition verdict plus the passing files (path -> pattern) when any passed.
type FileCheckFunc func(rule config.ApproveRule, pr github.ApprovalPullRequest) (pass bool, saw string, details []string, passed []FileCheckPass)

// FileCheckPass records one file whose file_checks check approved.
type FileCheckPass struct {
	Path    string
	Pattern string
}

// limitedBuffer caps writes at limit bytes, discarding any excess.
type limitedBuffer struct {
	buf   bytes.Buffer
	limit int
}

func (w *limitedBuffer) Write(p []byte) (int, error) {
	origLen := len(p)
	if w.limit <= 0 {
		return origLen, nil
	}
	remaining := w.limit - w.buf.Len()
	if remaining <= 0 {
		return origLen, nil
	}
	if len(p) > remaining {
		p = p[:remaining]
	}
	_, _ = w.buf.Write(p)
	return origLen, nil
}

func (w *limitedBuffer) String() string {
	return w.buf.String()
}

// ExecFileCheckCommand runs a file_checks script directly without a shell,
// inheriting the process environment plus env, and capping stdout and stderr.
func ExecFileCheckCommand(ctx context.Context, env []string, stdin []byte, argv []string) (remotecall.Result, error) {
	stdout := &limitedBuffer{limit: maxFileCheckOutput}
	stderr := &limitedBuffer{limit: maxFileCheckOutput}
	fullEnv := append(os.Environ(), env...)
	res, err := remotecall.RunIOEnv(ctx, fullEnv, bytes.NewReader(stdin), stdout, stderr, argv[0], argv[1:]...)
	res.Stdout, res.Stderr = stdout.String(), stderr.String()
	if err != nil {
		return res, fmt.Errorf("start %s: %w", argv[0], err)
	}
	return res, nil
}

type fileCheckOutcome struct {
	Pattern string
	Check   string
	Pass    bool
	Reason  string
	Stderr  string
}

func (r *Runner) fileChecker() FileCheckFunc {
	run := r.RunFileCheckCommand
	if run == nil {
		run = ExecFileCheckCommand
	}
	return func(rule config.ApproveRule, targetPR github.ApprovalPullRequest) (bool, string, []string, []FileCheckPass) {
		return evaluateFileChecks(rule, targetPR, run, r.AgentCache)
	}
}

func evaluateFileChecks(rule config.ApproveRule, pr github.ApprovalPullRequest, run FileCheckRunner, cache *AgentCache) (bool, string, []string, []FileCheckPass) {
	if pr.FilesErr != "" {
		pass, saw := unreadable(pr.FilesErr)
		return pass, saw, nil, nil
	}
	patches := make(map[string]github.PullFile, len(pr.Patches))
	for _, p := range pr.Patches {
		patches[p.Path] = p
	}
	var (
		details []string
		passed  []FileCheckPass
		failed  []string
		covered int
	)
	for _, file := range pr.Files {
		pattern, check, ok := config.MatchFileCheck(rule.FileChecks, file.Path)
		if !ok {
			details = append(details, file.Path+": not covered")
			continue
		}
		covered++
		out := runSingleFileCheck(rule, pr, file, pattern, check, patches, run, cache)
		details = append(details, formatFileCheckDetail(file.Path, out)...)
		if out.Pass {
			passed = append(passed, FileCheckPass{Path: file.Path, Pattern: pattern})
		} else {
			failed = append(failed, fmt.Sprintf("%s (%s)", file.Path, out.Reason))
		}
	}
	if len(failed) > 0 {
		return false, "deferred: " + strings.Join(failed, "; "), details, passed
	}
	return true, fmt.Sprintf("%d/%d covered files approved", covered, len(pr.Files)), details, passed
}

func formatFileCheckDetail(path string, out fileCheckOutcome) []string {
	var lines []string
	if out.Pass {
		lines = append(lines, fmt.Sprintf("pass %s [%s -> %s]: approve (%s)", path, out.Pattern, out.Check, out.Reason))
	} else {
		lines = append(lines, fmt.Sprintf("FAIL %s [%s -> %s]: defer (%s)", path, out.Pattern, out.Check, out.Reason))
	}
	if out.Stderr != "" {
		lines = append(lines, fmt.Sprintf("  stderr: %s", out.Stderr))
	}
	return lines
}

func runSingleFileCheck(
	rule config.ApproveRule,
	pr github.ApprovalPullRequest,
	file models.File,
	pattern string,
	check config.FileCheck,
	patches map[string]github.PullFile,
	run FileCheckRunner,
	cache *AgentCache,
) fileCheckOutcome {
	if check.Builtin != "" {
		return runBuiltinFileCheck(pr, file, pattern, check.Builtin, patches)
	}
	return runScriptFileCheck(rule, pr, file, pattern, check.Argv, patches, run, cache)
}

func runBuiltinFileCheck(
	pr github.ApprovalPullRequest,
	file models.File,
	pattern string,
	builtin string,
	patches map[string]github.PullFile,
) fileCheckOutcome {
	out := fileCheckOutcome{Pattern: pattern, Check: builtin}
	switch builtin {
	case config.FileCheckOnlyDeletions:
		if file.Additions > 0 {
			out.Reason = fmt.Sprintf("%d added lines", file.Additions)
			return out
		}
		if file.Deletions <= 0 {
			out.Reason = "0 deletions"
			return out
		}
		out.Pass = true
		out.Reason = fmt.Sprintf("%d deletions", file.Deletions)
		return out
	case config.FileCheckDeletionsAndCommentsOnly:
		if pr.PatchesErr != "" {
			out.Reason = unreadableText(pr.PatchesErr)
			return out
		}
		patch, ok := patches[file.Path]
		if !ok || !patch.PatchSet {
			out.Reason = "patch unavailable for " + file.Path
			return out
		}
		if patch.Deletions <= 0 {
			out.Reason = "0 deletions"
			return out
		}
		adds, reason, pass := fileCommentAdditions(patch)
		if !pass {
			out.Reason = reason
			return out
		}
		out.Pass = true
		out.Reason = fmt.Sprintf("%d deletions, %d comment/blank additions", patch.Deletions, adds)
		return out
	default:
		out.Reason = fmt.Sprintf("unknown built-in %q", builtin)
		return out
	}
}

func runScriptFileCheck(
	rule config.ApproveRule,
	pr github.ApprovalPullRequest,
	file models.File,
	pattern string,
	argv []string,
	patches map[string]github.PullFile,
	run FileCheckRunner,
	cache *AgentCache,
) fileCheckOutcome {
	out := fileCheckOutcome{Pattern: pattern, Check: filepath.Base(argv[0])}
	if pr.PatchesErr != "" {
		out.Reason = unreadableText(pr.PatchesErr)
		return out
	}
	patch, ok := patches[file.Path]
	if !ok || !patch.PatchSet {
		out.Reason = "patch unavailable for " + file.Path
		return out
	}
	diffBytes := []byte(patch.Patch)
	if cache != nil {
		key := fileCheckCacheKey(diffBytes, argv)
		if entry, hit := cache.get(key); hit {
			out.Pass = entry.Decision == AgentApprove
			out.Reason = entry.Reason
			out.Stderr = entry.Stderr
			return out
		}
		res, cacheable := execScriptFileCheck(rule, pr, file, argv, diffBytes, run)
		out.Pass, out.Reason, out.Stderr = res.pass, res.reason, res.stderr
		if cacheable {
			decision := AgentDefer
			if out.Pass {
				decision = AgentApprove
			}
			_ = cache.put(key, agentCacheEntry{
				Decision:   decision,
				Reason:     out.Reason,
				Stderr:     out.Stderr,
				Time:       cache.Now(),
				DurationMs: res.durationMs,
			})
		}
		return out
	}
	res, _ := execScriptFileCheck(rule, pr, file, argv, diffBytes, run)
	out.Pass, out.Reason, out.Stderr = res.pass, res.reason, res.stderr
	return out
}

type scriptExecResult struct {
	pass       bool
	reason     string
	stderr     string
	durationMs int64
}

func execScriptFileCheck(
	rule config.ApproveRule,
	pr github.ApprovalPullRequest,
	file models.File,
	argv []string,
	diffBytes []byte,
	run FileCheckRunner,
) (scriptExecResult, bool) {
	timeout := rule.FileChecksTimeout
	if timeout <= 0 {
		timeout = config.DefaultFileCheckTimeout
	}
	env := []string{
		"GH_APPROVE_REPO=" + pr.Ref.Owner + "/" + pr.Ref.Repo,
		"GH_APPROVE_PR=" + strconv.Itoa(pr.Ref.Number),
		"GH_APPROVE_PATH=" + file.Path,
		"GH_APPROVE_HEAD_SHA=" + pr.HeadSHA,
		"GH_APPROVE_BASE_REF=" + pr.BaseRef,
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	start := time.Now()
	res, err := run(ctx, env, diffBytes, argv)
	durationMs := time.Since(start).Milliseconds()
	stderr := strings.TrimSpace(res.Stderr)
	if len(stderr) > maxStderrInReason {
		stderr = "..." + stderr[len(stderr)-maxStderrInReason:]
	}
	switch {
	case err != nil:
		return scriptExecResult{reason: err.Error(), stderr: stderr, durationMs: durationMs}, false
	case res.TimedOut:
		return scriptExecResult{reason: fmt.Sprintf("timed out after %s", timeout), stderr: stderr, durationMs: durationMs}, false
	case res.Code != 0:
		return scriptExecResult{reason: fmt.Sprintf("exited %d", res.Code), stderr: stderr, durationMs: durationMs}, false
	}
	fields := strings.Fields(res.Stdout)
	if len(fields) == 0 {
		return scriptExecResult{reason: "empty stdout", stderr: stderr, durationMs: durationMs}, false
	}
	switch fields[0] {
	case AgentApprove:
		reason := "script approved"
		if len(fields) > 1 {
			reason = strings.Join(fields[1:], " ")
		}
		return scriptExecResult{pass: true, reason: reason, stderr: stderr, durationMs: durationMs}, true
	case AgentDefer:
		reason := "script deferred"
		if len(fields) > 1 {
			reason = strings.Join(fields[1:], " ")
		}
		return scriptExecResult{reason: reason, stderr: stderr, durationMs: durationMs}, true
	default:
		return scriptExecResult{reason: fmt.Sprintf("unexpected output %q", fields[0]), stderr: stderr, durationMs: durationMs}, false
	}
}
