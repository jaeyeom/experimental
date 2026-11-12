package ghlimit

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os/exec"
)

// stdoutHeadSize is how much of stdout a Result keeps for detecting a
// rate limit.
const stdoutHeadSize = 64 << 10

// ExecRunner runs the real gh as a child process.
type ExecRunner struct {
	Path string
	Args []string
	// Env is the child's environment; nil inherits this process's.
	Env []string
	// StdoutDirect hands stdout to gh as is, so gh can see a terminal.
	// Result.StdoutLen is then 0.
	StdoutDirect bool
	// StderrLive writes stderr out as gh runs, as well as capturing it.
	StderrLive bool

	stdin          io.Reader
	stdinData      []byte
	replayStdin    bool
	stdout, stderr io.Writer
}

// NewExecRunner runs gh at path with args. When args tell gh to read
// stdin, stdin is read up front so a retry gets the same input;
// otherwise it is passed to gh as is.
func NewExecRunner(path string, args []string, stdin io.Reader, stdout, stderr io.Writer) (*ExecRunner, error) {
	r := &ExecRunner{Path: path, Args: args, stdin: stdin, stdout: stdout, stderr: stderr}
	if UsesStdin(args) {
		data, err := io.ReadAll(stdin)
		if err != nil {
			return nil, fmt.Errorf("read stdin: %w", err)
		}
		r.stdinData, r.replayStdin = data, true
	}
	return r, nil
}

// Run runs gh once. It is a Runner.
func (r *ExecRunner) Run(ctx context.Context) (Result, error) {
	// The path is the user's gh, and args are the caller's gh arguments.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, r.Path, r.Args...) //nolint:gosec // G204: the user's gh with the caller's argument list, not a shell
	cmd.Env = r.Env
	cmd.Stdin = r.stdin
	if r.replayStdin {
		cmd.Stdin = bytes.NewReader(r.stdinData)
	}
	out := &headWriter{w: r.stdout}
	cmd.Stdout = out
	if r.StdoutDirect {
		cmd.Stdout = r.stdout
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if r.StderrLive {
		cmd.Stderr = io.MultiWriter(r.stderr, &stderr)
	}
	err := cmd.Run()
	res := Result{Stderr: stderr.Bytes(), StderrShown: r.StderrLive, StdoutLen: out.n, StdoutHead: out.head}
	var exitErr *exec.ExitError
	switch {
	case err == nil:
	case errors.As(err, &exitErr):
		res.ExitCode = exitErr.ExitCode()
		if res.ExitCode < 0 {
			res.ExitCode = 1
		}
	default:
		return res, fmt.Errorf("run %s: %w", r.Path, err)
	}
	return res, nil
}

// headWriter passes writes through, counting them and keeping the
// first stdoutHeadSize bytes.
type headWriter struct {
	w    io.Writer
	n    int64
	head []byte
}

func (h *headWriter) Write(p []byte) (int, error) {
	if room := stdoutHeadSize - len(h.head); room > 0 {
		h.head = append(h.head, p[:min(room, len(p))]...)
	}
	n, err := h.w.Write(p)
	h.n += int64(n)
	if err != nil {
		return n, fmt.Errorf("write stdout: %w", err)
	}
	return n, nil
}
