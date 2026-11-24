// Package sandbox defines the sandbox runner boundary and a cplt adapter for
// isolated agent execution on Linux and macOS.
package sandbox

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"time"
)

const defaultTerminateGrace = 5 * time.Second

// GitHubAuthMode controls how GitHub authentication is exposed to the sandbox.
type GitHubAuthMode string

const (
	// GitHubAuthCpltGH injects the volunteer's gh session via cplt
	// --inject-github-token with gh_guard enforced.
	GitHubAuthCpltGH GitHubAuthMode = "cplt-gh"
	// GitHubAuthNone injects no GitHub token into the sandbox.
	GitHubAuthNone GitHubAuthMode = "none"
)

// Spec describes a single isolated agent execution request.
type Spec struct {
	// Agent is the cplt agent name (e.g. "claude").
	Agent string
	// ProjectDir is the ephemeral detached git worktree path.
	ProjectDir string
	// IsolatedHome is the isolated HOME directory (mode 0700).
	IsolatedHome string
	// RealHome is the volunteer's real HOME, used for explicit --deny-path rules.
	RealHome string
	// ClaudeConfigDir is the isolated Claude config directory (<IsolatedHome>/.claude).
	ClaudeConfigDir string
	// ClaudeOAuthToken is an optional OAuth token for macOS keychain_substitute.
	// It is passed only via environment and must never be logged.
	ClaudeOAuthToken string
	// GitHubAuth selects "cplt-gh" or "none".
	GitHubAuth GitHubAuthMode
	// ExtraAllowedHosts lists optional extra proxy allowlist domains.
	ExtraAllowedHosts []string
	// AgentArgs is the exact argv passed to the agent after "--".
	AgentArgs []string
	// TargetOS overrides runtime.GOOS ("linux" or "darwin") for tests.
	TargetOS string
}

// Invocation is a prepared, direct-exec command specification for a sandbox.
type Invocation struct {
	Binary string
	Args   []string
	Env    []string
	Dir    string
}

// Runner prepares and starts sandboxed agent processes.
type Runner interface {
	Prepare(spec Spec) (Invocation, func() error, error)
	Start(ctx context.Context, inv Invocation) (*Process, error)
}

// Launch prepares spec with r and starts the resulting Invocation. If Start
// fails, Launch runs cleanup before returning the error.
func Launch(ctx context.Context, r Runner, spec Spec) (*Process, func() error, error) {
	inv, cleanup, err := r.Prepare(spec)
	if err != nil {
		return nil, nil, fmt.Errorf("prepare sandbox: %w", err)
	}
	proc, err := r.Start(ctx, inv)
	if err != nil {
		return nil, nil, errors.Join(fmt.Errorf("start sandbox: %w", err), cleanup())
	}
	return proc, cleanup, nil
}

// Process represents a running sandboxed command with its own process group.
type Process struct {
	Stdout io.ReadCloser
	Stderr io.ReadCloser

	cmd     *exec.Cmd
	pid     int
	done    chan struct{}
	waitErr error
}

// Wait blocks until the process exits and returns its exit error, if any.
func (p *Process) Wait() error {
	<-p.done
	return p.waitErr
}

func (p *Process) exited() bool {
	select {
	case <-p.done:
		return true
	default:
		return false
	}
}

func startProcess(ctx context.Context, inv Invocation) (*Process, error) {
	if inv.Binary == "" {
		return nil, errors.New("sandbox invocation binary is empty")
	}

	stdoutR, stdoutW, err := os.Pipe()
	if err != nil {
		return nil, fmt.Errorf("create stdout pipe: %w", err)
	}
	stderrR, stderrW, err := os.Pipe()
	if err != nil {
		_ = stdoutR.Close()
		_ = stdoutW.Close()
		return nil, fmt.Errorf("create stderr pipe: %w", err)
	}

	// Direct execve of the configured sandbox runner binary (no shell).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, inv.Binary, inv.Args...)
	cmd.Dir = inv.Dir
	cmd.Env = inv.Env
	cmd.Stdout = stdoutW
	cmd.Stderr = stderrW
	configureProcessGroup(cmd)

	if err := cmd.Start(); err != nil {
		_ = stdoutR.Close()
		_ = stdoutW.Close()
		_ = stderrR.Close()
		_ = stderrW.Close()
		return nil, fmt.Errorf("start sandbox process %q: %w", inv.Binary, err)
	}
	_ = stdoutW.Close()
	_ = stderrW.Close()

	p := &Process{
		Stdout: stdoutR,
		Stderr: stderrR,
		cmd:    cmd,
		pid:    cmd.Process.Pid,
		done:   make(chan struct{}),
	}
	go func() {
		if err := cmd.Wait(); err != nil {
			p.waitErr = fmt.Errorf("wait sandbox process: %w", err)
		}
		close(p.done)
	}()
	return p, nil
}
