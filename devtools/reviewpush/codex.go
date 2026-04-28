package reviewpush

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"time"
)

// OutputSchema is the JSON schema for the Codex review response.
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

// BuildPrompt constructs the Codex review prompt for a given pass.
func BuildPrompt(input *CodexPassInput) string {
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

// RealCodexRunner runs the real codex binary via os/exec.
type RealCodexRunner struct {
	CodexBin     string
	TimeoutSecs  int
	Stderr       io.Writer
	StreamStderr bool
	helpCache    string
	helpCached   bool
}

func (r *RealCodexRunner) getHelp() string {
	if r.helpCached {
		return r.helpCache
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(r.CodexBin, "exec", "--help") //nolint:gosec // CodexBin is user-configured
	output, _ := cmd.CombinedOutput()
	r.helpCache = string(output)
	r.helpCached = true
	return r.helpCache
}

func (r *RealCodexRunner) supportsFlag(flag string) bool {
	return strings.Contains(r.getHelp(), flag)
}

func (r *RealCodexRunner) buildArgs(input *CodexPassInput, schemaFile, outputFile string) []string {
	args := []string{"exec", "--cd", input.RepoDir}

	if r.supportsFlag("--ask-for-approval") {
		args = append(args, "--ask-for-approval", "never")
	} else if r.supportsFlag("--full-auto") {
		args = append(args, "--full-auto")
	}

	if r.supportsFlag("--sandbox") {
		args = append(args, "--sandbox", "workspace-write")
	}
	if r.supportsFlag("--ephemeral") {
		args = append(args, "--ephemeral")
	}
	if r.supportsFlag("--color") {
		args = append(args, "--color", "never")
	}
	if r.supportsFlag("--output-schema") {
		args = append(args, "--output-schema", schemaFile)
	}
	if r.supportsFlag("--output-last-message") {
		args = append(args, "--output-last-message", outputFile)
	}

	args = append(args, "-")
	return args
}

// RunPass executes a single Codex review pass.
func (r *RealCodexRunner) RunPass(ctx context.Context, input *CodexPassInput) (*CodexResult, error) {
	schemaFile, err := os.CreateTemp("", "codex-schema-*.json")
	if err != nil {
		return nil, fmt.Errorf("create schema temp file: %w", err)
	}
	defer os.Remove(schemaFile.Name())
	if _, err := schemaFile.WriteString(OutputSchema); err != nil {
		schemaFile.Close()
		return nil, fmt.Errorf("write schema: %w", err)
	}
	schemaFile.Close()

	outputFile, err := os.CreateTemp("", "codex-output-*.json")
	if err != nil {
		return nil, fmt.Errorf("create output temp file: %w", err)
	}
	outputFile.Close()
	defer os.Remove(outputFile.Name())

	args := r.buildArgs(input, schemaFile.Name(), outputFile.Name())
	prompt := BuildPrompt(input)

	var runCtx context.Context
	var cancel context.CancelFunc
	if r.TimeoutSecs > 0 {
		runCtx, cancel = context.WithTimeout(ctx, time.Duration(r.TimeoutSecs)*time.Second)
	} else {
		runCtx, cancel = context.WithCancel(ctx)
	}
	defer cancel()

	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(runCtx, r.CodexBin, args...) //nolint:gosec // CodexBin is user-configured
	cmd.Stdin = strings.NewReader(prompt)
	stderr := r.Stderr
	if stderr == nil {
		stderr = os.Stderr
	}
	var stderrBuf bytes.Buffer
	if r.StreamStderr {
		cmd.Stderr = stderr
	} else {
		cmd.Stderr = &stderrBuf
	}

	if err := cmd.Run(); err != nil {
		if !r.StreamStderr && stderrBuf.Len() > 0 {
			fmt.Fprint(stderr, stderrBuf.String())
		}
		if runCtx.Err() == context.DeadlineExceeded {
			return nil, fmt.Errorf("codex execution timed out after %ds", r.TimeoutSecs)
		}
		return nil, fmt.Errorf("codex execution failed: %w", err)
	}

	data, err := os.ReadFile(outputFile.Name())
	if err != nil {
		return nil, fmt.Errorf("read codex output: %w", err)
	}

	var result CodexResult
	if err := json.Unmarshal(data, &result); err != nil {
		return nil, fmt.Errorf("parse codex output: %w", err)
	}

	return &result, nil
}
