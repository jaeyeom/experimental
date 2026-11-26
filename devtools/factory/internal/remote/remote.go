// Package remote runs commands on another machine through a lane's exec
// prefix, such as ["ssh", "mac1"].
//
// A command run over ssh reaches the remote machine as one string that its
// shell parses. So every argument after the prefix is single-quoted for a
// POSIX shell, and arrives as exactly one argument with nothing expanded.
package remote

import (
	"context"
	"errors"
	"fmt"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"
)

// Executor runs each command as prefix + quoted command + quoted args
// through an inner executor. It implements go-cmdexec's Executor.
type Executor struct {
	inner  executor.Executor
	prefix []string
}

// New returns an Executor that runs commands through prefix on inner.
func New(inner executor.Executor, prefix []string) (*Executor, error) {
	if inner == nil {
		return nil, errors.New("remote: expected an executor, found nil")
	}
	if len(prefix) == 0 || prefix[0] == "" {
		return nil, fmt.Errorf("remote: expected an exec prefix such as [\"ssh\", \"HOST\"], found %q", prefix)
	}
	return &Executor{inner: inner, prefix: append([]string(nil), prefix...)}, nil
}

// Execute runs cfg on the remote machine. A WorkingDir cannot be applied
// there and is an error: callers pass the directory as an argument, such
// as git -C DIR.
func (e *Executor) Execute(ctx context.Context, cfg executor.ToolConfig) (*executor.ExecutionResult, error) {
	if cfg.Command == "" {
		return nil, errors.New("remote: expected a command, found empty")
	}
	if cfg.WorkingDir != "" {
		return nil, fmt.Errorf("remote: expected no working directory, found %q; pass the directory as an argument", cfg.WorkingDir)
	}
	args := append([]string(nil), e.prefix[1:]...)
	args = append(args, Quote(cfg.Command))
	for _, arg := range cfg.Args {
		args = append(args, Quote(arg))
	}
	cfg.Command = e.prefix[0]
	cfg.Args = args
	return e.inner.Execute(ctx, cfg) //nolint:wrapcheck // transparent executor wrapper
}

// IsAvailable reports whether the prefix command is available locally.
func (e *Executor) IsAvailable(string) bool {
	return e.inner.IsAvailable(e.prefix[0])
}

// Quote single-quotes s for a POSIX shell.
func Quote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}
