package reviewpush

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

// RealCodexRunner is the default Reviewer adapter that runs the Codex CLI via os/exec.
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

func (r *RealCodexRunner) buildArgs(input *ReviewPassInput, schemaFile, outputFile string) []string {
	args := []string{"exec", "--cd", input.RepoDir}

	if r.supportsFlag("--ask-for-approval") {
		args = append(args, "--ask-for-approval", "never")
	} else if r.supportsFlag("--full-auto") {
		args = append(args, "--full-auto")
	}

	if r.supportsFlag("--sandbox") {
		sandboxMode := "workspace-write"
		if os.Getenv("TERMUX_VERSION") != "" {
			sandboxMode = "danger-full-access"
		}
		args = append(args, "--sandbox", sandboxMode)
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

// RunPass executes a single Codex review pass and validates the returned JSON verdict.
func (r *RealCodexRunner) RunPass(ctx context.Context, input *ReviewPassInput) (*ReviewResult, error) {
	schemaFile, err := writeSchemaTempFile("codex-schema-*.json")
	if err != nil {
		return nil, err
	}
	defer os.Remove(schemaFile)

	outputFile, err := os.CreateTemp("", "codex-output-*.json")
	if err != nil {
		return nil, fmt.Errorf("create output temp file: %w", err)
	}
	outputFile.Close()
	defer os.Remove(outputFile.Name())

	args := r.buildArgs(input, schemaFile, outputFile.Name())
	prompt := BuildPrompt(input)

	runCtx, cancel := withOptionalTimeout(ctx, r.TimeoutSecs)
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

	return ParseReviewResult(data)
}
