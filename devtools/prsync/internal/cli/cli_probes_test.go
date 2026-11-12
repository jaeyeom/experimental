package cli

import (
	"context"
	"encoding/json"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

// useFakeGateProbes keeps the gate off the real process table and
// decide ledger. PRSYNC_FAKE_DECIDE_OUT is the open and answered
// `decide list --json` items; unset is []. TestMain calls it before any test runs.
func useFakeGateProbes() {
	gateProbes = func(executor.Executor, *herdr.Client) dispatch.Probes {
		return dispatch.Probes{Decide: envDecide{}}
	}
	promptWaker = func(executor.Executor) dispatch.Waker { return envWaker{} }
}

// envWaker never runs agentbus, so a test cannot wake a real agent. It
// appends "<machine>|<pane>" to PRSYNC_FAKE_WAKE_LOG when that is set,
// and reports the pane not hibernated.
type envWaker struct{}

func (envWaker) WakeIfHibernated(_ context.Context, machine, pane string) (bool, error) {
	path := os.Getenv("PRSYNC_FAKE_WAKE_LOG")
	if path == "" {
		return false, nil
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return false, err //nolint:wrapcheck // test fake
	}
	defer f.Close()
	_, err = f.WriteString(strings.Join([]string{machine, pane}, "|") + "\n")
	return false, err //nolint:wrapcheck // test fake
}

type envDecide struct{}

func (envDecide) Items(context.Context) ([]dispatch.DecideItem, error) {
	raw := os.Getenv("PRSYNC_FAKE_DECIDE_OUT")
	if raw == "" {
		return nil, nil
	}
	var items []dispatch.DecideItem
	if err := json.Unmarshal([]byte(raw), &items); err != nil {
		return nil, err //nolint:wrapcheck // test fake
	}
	return items, nil
}
