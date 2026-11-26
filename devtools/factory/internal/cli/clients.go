package cli

import (
	"context"
	"errors"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
)

// newCommandExec builds the executor used for herdr and gh.
// Tests replace it so commands do not run real binaries.
var newCommandExec = func() executor.Executor {
	return executor.NewBasicExecutor()
}

// newWaker returns the wake hook a start runs before its prompt under
// wake_before_prompt = "auto". Tests replace it so they never run agentbus.
var newWaker = agentbusWaker

// agentbusWaker runs `agentbus wake --if-hibernated --wait` through exec.
func agentbusWaker(exec executor.Executor) func(context.Context, string, string) (bool, error) {
	return agentbus.NewClient(exec, "agentbus").Wake
}

// promptWaker is newWaker(exec) when wake_before_prompt is "auto", and nil
// (never wake) when it is "off".
func promptWaker(cfg *config.Config, exec executor.Executor) func(context.Context, string, string) (bool, error) {
	if cfg == nil || cfg.Defaults.WakeBeforePrompt != config.WakeAuto {
		return nil
	}
	return newWaker(exec)
}

// startSleep is start.Starter.Sleep for every start: nil sleeps for real.
// Tests replace it so the agent-ready settle pause does not slow them.
var startSleep func(ctx context.Context, d time.Duration) error

// externalClients builds the herdr client and a gh client that runs ghBin.
func externalClients(ghBin string) (*herdr.Client, *gh.Client, error) {
	exec := newCommandExec()
	if exec == nil {
		return nil, nil, errors.New("clients: expected an executor, found nil")
	}
	client, err := ghClient(exec, ghBin)
	if err != nil {
		return nil, nil, err
	}
	return herdr.NewClient(exec, "herdr"), client, nil
}

// ghClient returns a gh client that runs ghBin, the resolved gh_bin.
func ghClient(exec executor.Executor, ghBin string) (*gh.Client, error) {
	if ghBin == "" {
		return nil, errors.New("gh: expected a resolved gh binary, found empty")
	}
	return gh.NewClient(exec, ghBin), nil
}
