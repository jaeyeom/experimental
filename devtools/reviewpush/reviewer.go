package reviewpush

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
	"unicode"
)

const (
	// StatusPush indicates the reviewer approved pushing through push_target.
	StatusPush = "PUSH"
	// StatusBlocked indicates the reviewer found a hard blocker and stopped the loop.
	StatusBlocked = "BLOCKED"
)

// ReviewResult represents the validated JSON verdict from a review pass.
type ReviewResult struct {
	Status        string  `json:"status"`
	Summary       string  `json:"summary"`
	MultiCommits  bool    `json:"multi_commits"`  //nolint:tagliatelle // reviewer API uses snake_case
	PushTarget    *string `json:"push_target"`    //nolint:tagliatelle // reviewer API uses snake_case
	BlockedReason *string `json:"blocked_reason"` //nolint:tagliatelle // reviewer API uses snake_case
}

// CodexResult is a backwards-compatible alias for ReviewResult.
type CodexResult = ReviewResult

// ReviewPassInput contains the input parameters for a single review pass.
type ReviewPassInput struct {
	RepoDir         string
	BaseBranch      string
	RemoteRef       string
	RemoteRefExists bool
	Iteration       int
	AheadCount      int
	TargetCommit    string
}

// CodexPassInput is a backwards-compatible alias for ReviewPassInput.
type CodexPassInput = ReviewPassInput

// Reviewer executes one read-only review pass and returns the parsed verdict.
type Reviewer interface {
	RunPass(ctx context.Context, input *ReviewPassInput) (*ReviewResult, error)
}

// CodexRunner is a backwards-compatible alias for Reviewer.
type CodexRunner = Reviewer

// OutputSchema is the JSON schema for the reviewer response contract.
const OutputSchema = `{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "additionalProperties": false,
  "required": ["status", "summary", "multi_commits", "push_target", "blocked_reason"],
  "properties": {
    "status": {
      "type": "string",
      "enum": ["PUSH", "BLOCKED"]
    },
    "summary": {
      "type": "string"
    },
    "multi_commits": {
      "type": "boolean"
    },
    "push_target": {
      "type": ["string", "null"]
    },
    "blocked_reason": {
      "type": ["string", "null"]
    }
  }
}`

// BuildPrompt constructs the read-only review prompt for a given pass.
func BuildPrompt(input *ReviewPassInput) string {
	remoteState := input.RemoteRef
	if !input.RemoteRefExists {
		remoteState = fmt.Sprintf("%s (unborn: remote branch does not exist yet; do not assume %s is a valid Git object)", input.RemoteRef, input.RemoteRef)
	}
	return fmt.Sprintf(`You are running one isolated pass of the review-and-push workflow.

Repository: %s
Base branch: %s
Remote ref: %s
Iteration: %d
Ahead count before review: %d
Target commit: %s

Instructions:
- Review the target commit above. You may also inspect its descendants through HEAD if needed to decide whether the wrapper should push exactly one commit or several contiguous commits.
- Treat the current working tree as potentially dirty and ignore unrelated untracked or unstaged files unless they directly affect the target commit review.
- Keep the review phase read-only and inspect Git objects directly.
- Do not run git push or any other network-changing command. The wrapper will validate your JSON response and perform any push itself.
- Treat only hard blockers as push-stopping issues: security bugs, leaked secrets, obvious destructive data loss or corruption, clearly broken startup or primary-path behavior, or similarly severe issues.
- Non-blocking concerns should become concise follow-up items, not a reason to stop the push.
- Your final response must be a JSON object and nothing else.
- Use status PUSH when there are no hard blockers and the wrapper should push commits through push_target.
- Set push_target to %s to push exactly one commit, or to a descendant commit hash to push multiple contiguous commits in one step.
- Set multi_commits to true if you are recommending pushing multiple contiguous commits (push_target != target_commit). Set multi_commits to false if you are recommending pushing only the target commit. This field serves as a cross-check: the wrapper will only push multiple commits when multi_commits is explicitly true.
- Use status BLOCKED if a hard blocker or execution failure stopped progress.
- Always include push_target. Use null when status is BLOCKED.
- Always include blocked_reason. Use null when status is PUSH.`,
		input.RepoDir, input.BaseBranch, remoteState,
		input.Iteration, input.AheadCount, input.TargetCommit,
		input.TargetCommit)
}

type rawReviewResult struct {
	Status        *string `json:"status"`
	Summary       *string `json:"summary"`
	MultiCommits  *bool   `json:"multi_commits"`  //nolint:tagliatelle // reviewer API uses snake_case
	PushTarget    *string `json:"push_target"`    //nolint:tagliatelle // reviewer API uses snake_case
	BlockedReason *string `json:"blocked_reason"` //nolint:tagliatelle // reviewer API uses snake_case
}

// ParseReviewResult decodes and strictly validates a JSON verdict payload.
func ParseReviewResult(data []byte) (*ReviewResult, error) {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 {
		return nil, errors.New("reviewer output is empty")
	}
	if trimmed[0] != '{' {
		return nil, errors.New("reviewer output must be a JSON object")
	}

	dec := json.NewDecoder(bytes.NewReader(trimmed))
	dec.DisallowUnknownFields()

	var raw rawReviewResult
	if err := dec.Decode(&raw); err != nil {
		return nil, fmt.Errorf("parse reviewer output: %w", err)
	}
	var trailing any
	if err := dec.Decode(&trailing); !errors.Is(err, io.EOF) {
		return nil, errors.New("reviewer output contains trailing data after JSON object")
	}
	if raw.Status == nil {
		return nil, errors.New("reviewer output missing required field \"status\"")
	}
	if raw.Summary == nil {
		return nil, errors.New("reviewer output missing required field \"summary\"")
	}

	result := &ReviewResult{
		Status:        *raw.Status,
		Summary:       *raw.Summary,
		MultiCommits:  raw.MultiCommits != nil && *raw.MultiCommits,
		PushTarget:    raw.PushTarget,
		BlockedReason: raw.BlockedReason,
	}
	if err := ValidateReviewResult(result); err != nil {
		return nil, err
	}
	return result, nil
}

// ValidateReviewResult checks that result represents an unambiguous,
// internally consistent PUSH or BLOCKED verdict.
func ValidateReviewResult(result *ReviewResult) error {
	if result == nil {
		return errors.New("reviewer returned nil result")
	}
	switch result.Status {
	case StatusPush:
		if result.BlockedReason != nil && strings.TrimSpace(*result.BlockedReason) != "" {
			return errors.New("PUSH verdict must not include a blocked_reason")
		}
		if result.PushTarget != nil && strings.TrimSpace(*result.PushTarget) == "" {
			return errors.New("PUSH verdict push_target must be null or a non-empty commit ref")
		}
		if result.MultiCommits && (result.PushTarget == nil || strings.TrimSpace(*result.PushTarget) == "") {
			return errors.New("PUSH verdict with multi_commits=true requires a non-empty push_target")
		}
		return nil
	case StatusBlocked:
		if result.PushTarget != nil && strings.TrimSpace(*result.PushTarget) != "" {
			return errors.New("BLOCKED verdict must not include a push_target")
		}
		return nil
	default:
		return fmt.Errorf("unexpected reviewer status %q", result.Status)
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

// CommandReviewer runs an arbitrary executable reviewer command without a shell.
// It writes the read-only review prompt to stdin, passes pass metadata and a
// schema file path via REVIEWPUSH_* environment variables, and reads and strictly
// validates the JSON verdict from stdout.
type CommandReviewer struct {
	Command      []string
	TimeoutSecs  int
	Stderr       io.Writer
	StreamStderr bool
}

// RunPass executes the configured reviewer command for a single review pass.
func (c *CommandReviewer) RunPass(ctx context.Context, input *ReviewPassInput) (*ReviewResult, error) {
	if len(c.Command) == 0 || c.Command[0] == "" {
		return nil, errors.New("reviewer command is empty")
	}
	schemaFile, err := writeSchemaTempFile("reviewpush-schema-*.json")
	if err != nil {
		return nil, err
	}
	defer os.Remove(schemaFile)

	runCtx, cancel := withOptionalTimeout(ctx, c.TimeoutSecs)
	defer cancel()

	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(runCtx, c.Command[0], c.Command[1:]...) //nolint:gosec // Command is user-configured argv without shell
	cmd.Dir = input.RepoDir
	cmd.Stdin = strings.NewReader(BuildPrompt(input))
	cmd.Env = reviewerEnv(os.Environ(), input, schemaFile)

	var stdoutBuf, stderrBuf bytes.Buffer
	cmd.Stdout = &stdoutBuf
	stderr := c.Stderr
	if stderr == nil {
		stderr = os.Stderr
	}
	if c.StreamStderr {
		cmd.Stderr = stderr
	} else {
		cmd.Stderr = &stderrBuf
	}

	if err := cmd.Run(); err != nil {
		if !c.StreamStderr && stderrBuf.Len() > 0 {
			fmt.Fprint(stderr, stderrBuf.String())
		}
		if runCtx.Err() == context.DeadlineExceeded {
			return nil, fmt.Errorf("reviewer execution timed out after %ds", c.TimeoutSecs)
		}
		return nil, fmt.Errorf("reviewer execution failed: %w", err)
	}

	return ParseReviewResult(stdoutBuf.Bytes())
}

func reviewerEnv(base []string, input *ReviewPassInput, schemaFile string) []string {
	env := make([]string, 0, len(base)+8)
	env = append(env, base...)
	env = append(env,
		"REVIEWPUSH_REPO_DIR="+input.RepoDir,
		"REVIEWPUSH_BASE_BRANCH="+input.BaseBranch,
		"REVIEWPUSH_REMOTE_REF="+input.RemoteRef,
		"REVIEWPUSH_REMOTE_REF_EXISTS="+strconv.FormatBool(input.RemoteRefExists),
		"REVIEWPUSH_ITERATION="+strconv.Itoa(input.Iteration),
		"REVIEWPUSH_AHEAD_COUNT="+strconv.Itoa(input.AheadCount),
		"REVIEWPUSH_TARGET_COMMIT="+input.TargetCommit,
		"REVIEWPUSH_SCHEMA_FILE="+schemaFile,
	)
	return env
}

func writeSchemaTempFile(pattern string) (string, error) {
	f, err := os.CreateTemp("", pattern)
	if err != nil {
		return "", fmt.Errorf("create schema temp file: %w", err)
	}
	name := f.Name()
	if _, err := f.WriteString(OutputSchema); err != nil {
		f.Close()
		_ = os.Remove(name)
		return "", fmt.Errorf("write schema: %w", err)
	}
	if err := f.Close(); err != nil {
		_ = os.Remove(name)
		return "", fmt.Errorf("close schema temp file: %w", err)
	}
	return name, nil
}

func withOptionalTimeout(ctx context.Context, timeoutSecs int) (context.Context, context.CancelFunc) {
	if timeoutSecs > 0 {
		return context.WithTimeout(ctx, time.Duration(timeoutSecs)*time.Second)
	}
	return context.WithCancel(ctx)
}
