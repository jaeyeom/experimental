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
	"strings"
	"unicode"
)

const previewCandidateRef = "refs/fork-keeper-preview/candidate"

// ReviewInput carries all self-contained context needed for a reviewer or agent
// pass in a credential-scrubbed throwaway worktree.
type ReviewInput struct {
	Fork          Fork
	WorkDir       string
	OutDir        string
	GitDir        string
	CacheDir      string
	LogPath       string
	SandboxMode   string
	Range         RangeInfo
	RangeFile     string
	DiffFile      string
	PromptFile    string
	SchemaFile    string
	Prompt        string
	CandidateHead string
}

// ReviewResult reports the candidate commit and exit status from a reviewer pass.
type ReviewResult struct {
	CandidateHead string
	AgentExited   int
}

// Reviewer performs the rebase/review step inside a prepared throwaway worktree.
// Implementations must leave branch refs/heads/fork-keeper/candidate in WorkDir
// and write verdict.json (plus optional report.md) in OutDir.
type Reviewer interface {
	Review(ctx context.Context, input ReviewInput) (ReviewResult, error)
}

// StandaloneReviewer is the self-contained deterministic default when no
// external reviewer command is configured. It cherry-picks the fork's user
// commits onto upstream_new and emits a "needs-review" verdict so unreviewed
// upstream code is never auto-promoted.
type StandaloneReviewer struct{}

// Review rebases user_commits onto upstream_new and writes verdict.json and report.md.
func (StandaloneReviewer) Review(ctx context.Context, input ReviewInput) (ReviewResult, error) {
	candidate, err := rebaseCandidate(ctx, input.WorkDir, input.Range)
	if err != nil {
		return ReviewResult{}, err
	}

	const summary = "No external reviewer configured; candidate rebased cleanly and awaits manual review."
	v := Verdict{
		Schema:        VerdictSchemaVersion,
		Verdict:       VerdictNeedsReview,
		CandidateHead: candidate,
		Range:         input.Range,
		Conflicts:     []VerdictConflict{},
		Findings:      []VerdictFinding{},
		Summary:       summary,
	}
	vBytes, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return ReviewResult{}, fmt.Errorf("encode standalone verdict: %w", err)
	}
	vBytes = append(vBytes, '\n')
	if err := os.WriteFile(filepath.Join(input.OutDir, "verdict.json"), vBytes, 0o600); err != nil {
		return ReviewResult{}, fmt.Errorf("write standalone verdict.json: %w", err)
	}

	report := buildStandaloneReport(ctx, input.GitDir, input.Range, candidate, summary)
	if err := os.WriteFile(filepath.Join(input.OutDir, "report.md"), []byte(report), 0o600); err != nil {
		return ReviewResult{}, fmt.Errorf("write standalone report.md: %w", err)
	}
	return ReviewResult{CandidateHead: candidate}, nil
}

// CommandReviewer runs an external reviewer or coding-agent command inside the
// configured OS sandbox with scrubbed credentials.
type CommandReviewer struct {
	Command []string
}

// Review executes CommandReviewer.Command in input.WorkDir. It supports both
// autonomous agents (that create fork-keeper/candidate and write {out}/verdict.json)
// and read-only JSON-over-stdio reviewers (when the clean preview rebase succeeds).
func (c *CommandReviewer) Review(ctx context.Context, input ReviewInput) (ReviewResult, error) {
	if len(c.Command) == 0 || strings.TrimSpace(c.Command[0]) == "" {
		return ReviewResult{}, errors.New("reviewer command must not be empty")
	}

	previewSHA := tryPreviewRebase(ctx, input.WorkDir, input.Range, input.Fork.Branch)
	if input.CandidateHead == "" {
		input.CandidateHead = previewSHA
	}
	if input.Prompt == "" {
		input.Prompt = BuildReviewPrompt(input)
	}
	if input.PromptFile != "" {
		if err := os.WriteFile(input.PromptFile, []byte(input.Prompt), 0o600); err != nil {
			return ReviewResult{}, fmt.Errorf("write prompt file: %w", err)
		}
	}

	expanded := expandCommandPlaceholders(c.Command, input)
	agentRC, stdoutBytes, err := runSandboxedReviewer(ctx, input, expanded)
	if err != nil {
		return ReviewResult{}, err
	}

	verdictPath := filepath.Join(input.OutDir, "verdict.json")
	if _, statErr := os.Stat(verdictPath); errors.Is(statErr, os.ErrNotExist) && agentRC == 0 {
		maybeWriteStdoutVerdict(stdoutBytes, verdictPath, filepath.Join(input.OutDir, "report.md"))
	}

	candidate, err := resolveCandidateAfterReview(ctx, input, previewSHA, verdictPath, agentRC)
	if err != nil {
		return ReviewResult{}, err
	}

	return ReviewResult{
		CandidateHead: candidate,
		AgentExited:   agentRC,
	}, nil
}

func resolveCandidateAfterReview(ctx context.Context, input ReviewInput, previewSHA, verdictPath string, agentRC int) (string, error) {
	if candidate := gitRevParseDirOptional(ctx, input.WorkDir, "refs/heads/fork-keeper/candidate^{commit}"); candidate != "" {
		return candidate, nil
	}
	if agentRC == 0 && previewSHA != "" {
		if v, ok, _ := ValidateVerdictFile(verdictPath, previewSHA, &input.Range); ok && v != nil {
			if err := runGit(ctx, input.WorkDir, "branch", "-f", "fork-keeper/candidate", previewSHA); err == nil {
				return previewSHA, nil
			}
		}
	}
	if agentRC != 0 {
		return "", fmt.Errorf("reviewer exited %d and left no fork-keeper/candidate branch (see %s)", agentRC, input.LogPath)
	}
	return "", errors.New("agent left no fork-keeper/candidate branch")
}

// BuildReviewPrompt renders the self-contained threat-review prompt passed on
// stdin (and written to FORK_KEEPER_PROMPT_FILE) for external reviewers.
func BuildReviewPrompt(input ReviewInput) string {
	reviewBase := "(none - unrelated histories)"
	if input.Range.ReviewBase != nil && *input.Range.ReviewBase != "" {
		reviewBase = *input.Range.ReviewBase
	}
	previewLine := "(not pre-created; rebase user_commits onto upstream_new)"
	if input.CandidateHead != "" {
		previewLine = input.CandidateHead + " (clean preview at " + previewCandidateRef + ")"
	}
	userCommits := "(none)"
	if len(input.Range.UserCommits) > 0 {
		userCommits = strings.Join(input.Range.UserCommits, ", ")
	}

	return fmt.Sprintf(`You are reviewing an upstream sync for personal fork %s (branch %s).

Metadata:
- Worktree: %s
- Output directory: %s
- Range JSON file: %s
- Upstream diff file: %s
- Verdict JSON Schema file: %s
- upstream_old: %s
- upstream_new: %s
- review_base: %s
- first_run: %t
- rewritten: %t
- fork_has_merges: %t
- fork_behind_recorded_head: %t
- moved_tags: %d
- deleted_tags: %d
- user_commits: %s
- candidate_preview: %s

Instructions:
1. Perform a threat-only security review of the upstream diff between review_base and upstream_new.
   Focus on these categories only:
   - secret-leak
   - destructive
   - concealment
   - supply-chain
   - repo-signal
   - agent-manipulation
   - uncertain
2. If fork-keeper/candidate does not already exist in the worktree, create branch
   fork-keeper/candidate starting at upstream_new and cherry-pick user_commits onto it.
   Record any rebase conflicts in "conflicts" with "confident": true/false.
3. Copy the exact range object from %s into "range".
4. Write %s matching %s (or emit that single JSON object on stdout when running read-only against the preview candidate) and write a concise markdown summary to %s.
5. Use "verdict": "clean" only when there are zero findings, every conflict has "confident": true, and none of first_run, rewritten, fork_has_merges, fork_behind_recorded_head, moved_tags, or deleted_tags are flagged. Otherwise use "verdict": "needs-review".
`,
		input.Fork.Repo,
		input.Fork.Branch,
		input.WorkDir,
		input.OutDir,
		input.RangeFile,
		input.DiffFile,
		input.SchemaFile,
		input.Range.UpstreamOld,
		input.Range.UpstreamNew,
		reviewBase,
		input.Range.FirstRun,
		input.Range.Rewritten,
		input.Range.ForkHasMerges,
		input.Range.ForkBehindRecordedHead,
		len(input.Range.MovedTags),
		len(input.Range.DeletedTags),
		userCommits,
		previewLine,
		input.RangeFile,
		filepath.Join(input.OutDir, "verdict.json"),
		input.SchemaFile,
		filepath.Join(input.OutDir, "report.md"),
	)
}

func buildStandaloneReport(ctx context.Context, gitDir string, r RangeInfo, candidate, summary string) string {
	var b strings.Builder
	b.WriteString(summary)
	b.WriteString("\n\n")
	fmt.Fprintf(&b, "- Upstream: `%s` -> `%s`\n", shortSHA(r.UpstreamOld), shortSHA(r.UpstreamNew))
	fmt.Fprintf(&b, "- Candidate: `%s`\n", shortSHA(candidate))
	fmt.Fprintf(&b, "- Rebased fork commits: %d\n", len(r.UserCommits))

	if upSummary := BuildUpstreamSummary(ctx, gitDir, r); upSummary != "" {
		b.WriteByte('\n')
		b.WriteString(upSummary)
		b.WriteByte('\n')
	}
	return b.String()
}

func shortSHA(s string) string {
	if len(s) > 12 {
		return s[:12]
	}
	return s
}

func rebaseCandidate(ctx context.Context, workDir string, r RangeInfo) (string, error) {
	if err := runGit(ctx, workDir, "checkout", "-q", "-B", "fork-keeper/candidate", r.UpstreamNew); err != nil {
		return "", fmt.Errorf("checkout upstream_new %s: %w", r.UpstreamNew, err)
	}
	for _, c := range r.UserCommits {
		if err := runGit(ctx, workDir, "cherry-pick", c); err != nil {
			_ = runGit(ctx, workDir, "cherry-pick", "--abort")
			return "", fmt.Errorf("cherry-pick %s onto %s failed (configure an external reviewer/agent to resolve conflicts): %w", c, r.UpstreamNew, err)
		}
	}
	cand := gitRevParseDirOptional(ctx, workDir, "refs/heads/fork-keeper/candidate^{commit}")
	if cand == "" {
		return "", errors.New("failed to resolve fork-keeper/candidate after rebase")
	}
	return cand, nil
}

func tryPreviewRebase(ctx context.Context, workDir string, r RangeInfo, branch string) string {
	if err := runGit(ctx, workDir, "checkout", "-q", "--detach", r.UpstreamNew); err != nil {
		_ = runGit(ctx, workDir, "checkout", "-q", "--detach", "refs/remotes/origin/"+branch)
		return ""
	}
	for _, c := range r.UserCommits {
		if err := runGit(ctx, workDir, "cherry-pick", c); err != nil {
			_ = runGit(ctx, workDir, "cherry-pick", "--abort")
			_ = runGit(ctx, workDir, "checkout", "-q", "--detach", "refs/remotes/origin/"+branch)
			return ""
		}
	}
	sha := gitRevParseDirOptional(ctx, workDir, "HEAD^{commit}")
	if sha != "" {
		_ = runGit(ctx, workDir, "update-ref", previewCandidateRef, sha)
	}
	_ = runGit(ctx, workDir, "checkout", "-q", "--detach", "refs/remotes/origin/"+branch)
	return sha
}

func expandCommandPlaceholders(args []string, input ReviewInput) []string {
	reviewBase := ""
	if input.Range.ReviewBase != nil {
		reviewBase = *input.Range.ReviewBase
	}
	replacer := strings.NewReplacer(
		"{out}", input.OutDir,
		"{branch}", input.Fork.Branch,
		"{repo}", input.Fork.Repo,
		"{workdir}", input.WorkDir,
		"{range}", input.RangeFile,
		"{diff}", input.DiffFile,
		"{prompt}", input.PromptFile,
		"{schema}", input.SchemaFile,
		"{candidate}", input.CandidateHead,
		"{upstream_old}", input.Range.UpstreamOld,
		"{upstream_new}", input.Range.UpstreamNew,
		"{review_base}", reviewBase,
	)
	out := make([]string, len(args))
	for i, a := range args {
		out[i] = replacer.Replace(a)
	}
	return out
}

func runSandboxedReviewer(ctx context.Context, input ReviewInput, args []string) (int, []byte, error) {
	homeDir, _ := os.UserHomeDir()
	execArgs, err := wrapAgentSandbox(input.SandboxMode, homeDir, input.GitDir, args)
	if err != nil {
		return 0, nil, fmt.Errorf("confine agent: %w", err)
	}

	logFile, err := os.Create(input.LogPath)
	if err != nil {
		return 0, nil, fmt.Errorf("create agent log %s: %w", input.LogPath, err)
	}
	defer func() { _ = logFile.Close() }()

	//nolint:gosec // Configured reviewer command, wrapped in sandbox when enabled.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, execArgs[0], execArgs[1:]...)
	cmd.Dir = input.WorkDir
	cmd.Env = scrubbedAgentEnv(os.Environ(), input)
	cmd.Stdin = strings.NewReader(input.Prompt)

	var stdoutBuf bytes.Buffer
	cmd.Stdout = io.MultiWriter(logFile, &stdoutBuf)
	cmd.Stderr = logFile

	if err := cmd.Run(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			return exitErr.ExitCode(), stdoutBuf.Bytes(), nil
		}
		return 0, stdoutBuf.Bytes(), fmt.Errorf("run reviewer %s: %w", execArgs[0], err)
	}
	return 0, stdoutBuf.Bytes(), nil
}

func maybeWriteStdoutVerdict(stdout []byte, verdictPath, reportPath string) {
	trimmed := bytes.TrimSpace(stdout)
	if len(trimmed) == 0 || trimmed[0] != '{' {
		return
	}
	v, errs := ParseAndValidateVerdict(trimmed, "", nil)
	if v == nil && len(errs) > 0 {
		return
	}
	// Accept JSON object on stdout even if validation later rejects clean/range;
	// scheduler's ValidateVerdictFile enforces the full contract and reports reasons.
	if err := os.WriteFile(verdictPath, append(trimmed, '\n'), 0o600); err != nil {
		return
	}
	if _, err := os.Stat(reportPath); errors.Is(err, os.ErrNotExist) && v != nil && strings.TrimSpace(v.Summary) != "" {
		_ = os.WriteFile(reportPath, []byte(strings.TrimSpace(v.Summary)+"\n"), 0o600)
	}
}

// SplitCommand parses a command string into argv tokens without invoking a shell.
// It supports single quotes, double quotes, and backslash escapes outside single quotes.
func SplitCommand(raw string) ([]string, error) {
	var l cmdLexer
	for _, r := range raw {
		l.step(r)
	}
	return l.finish()
}

type cmdLexer struct {
	args    []string
	cur     strings.Builder
	inToken bool
	quote   rune
	escaped bool
}

func (l *cmdLexer) step(r rune) {
	if l.escaped {
		l.cur.WriteRune(r)
		l.escaped = false
		l.inToken = true
		return
	}
	if l.quote != '\'' && r == '\\' {
		l.escaped = true
		l.inToken = true
		return
	}
	if l.quote != 0 {
		l.stepQuoted(r)
		return
	}
	l.stepUnquoted(r)
}

func (l *cmdLexer) stepQuoted(r rune) {
	if r == l.quote {
		l.quote = 0
		return
	}
	l.cur.WriteRune(r)
}

func (l *cmdLexer) stepUnquoted(r rune) {
	switch {
	case r == '\'' || r == '"':
		l.quote = r
		l.inToken = true
	case unicode.IsSpace(r):
		if l.inToken {
			l.args = append(l.args, l.cur.String())
			l.cur.Reset()
			l.inToken = false
		}
	default:
		l.cur.WriteRune(r)
		l.inToken = true
	}
}

func (l *cmdLexer) finish() ([]string, error) {
	if l.escaped {
		return nil, errors.New("reviewer command ends with trailing backslash")
	}
	if l.quote != 0 {
		return nil, errors.New("reviewer command has unclosed quote")
	}
	if l.inToken {
		l.args = append(l.args, l.cur.String())
	}
	if len(l.args) == 0 {
		return nil, errors.New("reviewer command must not be empty")
	}
	return l.args, nil
}
