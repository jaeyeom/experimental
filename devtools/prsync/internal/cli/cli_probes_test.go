package cli

import (
	"context"
	"encoding/json"
	"os"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

// useFakeGateProbes keeps the gate off the real process table and
// decide ledger. PRSYNC_FAKE_DECIDE_OUT is the `decide list --json`
// output; unset is []. TestMain calls it before any test runs.
func useFakeGateProbes() {
	gateProbes = func(executor.Executor, *herdr.Client) dispatch.Probes {
		return dispatch.Probes{Decide: envDecide{}}
	}
}

type envDecide struct{}

func (envDecide) OpenItems(context.Context) ([]dispatch.DecideItem, error) {
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
