package dispatch

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

// wakeTimeout bounds one `agentbus wake`. agentbus waits up to its own
// wake_timeout (60s by default) for the relaunched agent, and may reach
// a hub over ssh first.
const wakeTimeout = 3 * time.Minute

// Waker wakes a hibernated agent before a prompt is typed into its
// pane: in a hibernated pane the prompt would land in the shell.
type Waker interface {
	// WakeIfHibernated relaunches the agent in pane on machine (empty
	// for the local herdr server) when it is hibernated, and returns
	// once it is ready. woke reports whether it relaunched one. A
	// missing agentbus is ErrAgentbusMissing.
	WakeIfHibernated(ctx context.Context, machine, pane string) (woke bool, err error)
}

// AgentbusWaker wakes with `agentbus wake --if-hibernated --wait`.
type AgentbusWaker struct {
	Exec executor.Executor
	Bin  string
}

// WakeIfHibernated runs `agentbus wake --if-hibernated --wait --machine
// <machine> <pane>`. agentbus prints nothing when the agent was not
// hibernated, and "woke ..." when it relaunched it.
func (a AgentbusWaker) WakeIfHibernated(ctx context.Context, machine, pane string) (bool, error) {
	if machine == "" {
		machine = config.LocalMachine
	}
	res, err := a.Exec.Execute(ctx, executor.ToolConfig{
		Command: a.Bin,
		Args:    []string{"wake", "--if-hibernated", "--wait", "--machine", machine, pane},
		Timeout: wakeTimeout,
	})
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return false, fmt.Errorf("%w: %w", ErrAgentbusMissing, err)
		}
		return false, fmt.Errorf("run %s wake: %w", a.Bin, err)
	}
	if res.ExitCode != 0 {
		return false, fmt.Errorf("%s wake exited %d: %s", a.Bin, res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	return strings.TrimSpace(res.Output) != "", nil
}

// wakeBeforePrompt wakes pane's agent through req.Waker. A missing
// agentbus is not an error: the prompt is sent as without one.
func wakeBeforePrompt(ctx context.Context, req Request, machine, pane string) (bool, error) {
	if req.Waker == nil {
		return false, nil
	}
	woke, err := req.Waker.WakeIfHibernated(ctx, machine, pane)
	if errors.Is(err, ErrAgentbusMissing) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("wake before prompt: %w", err)
	}
	return woke, nil
}
