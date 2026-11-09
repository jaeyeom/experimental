// Package fake is an adapter whose results are set by tests.
package fake

import (
	"context"
	"errors"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// Adapter returns the states and delivery results it was given.
// A non-nil SelfTestErr marks this adapter broken.
type Adapter struct {
	AdapterName string
	SelfTestErr error
	Reach       map[string]adapter.State
	Result      map[string]adapter.Result
	Calls       int
	LastText    string
}

// Name is the adapter name recorded on adapter_error.
func (a *Adapter) Name() string { return a.AdapterName }

// Inventory lists the handles this fake can reach.
func (a *Adapter) Inventory(context.Context) ([]adapter.Agent, error) {
	out := make([]adapter.Agent, 0, len(a.Reach))
	for handle, state := range a.Reach {
		out = append(out, adapter.Agent{Handle: handle, Status: string(state)})
	}
	return out, nil
}

// Identify returns the only handle when there is exactly one.
func (a *Adapter) Identify(context.Context, []string) (string, error) {
	if len(a.Reach) != 1 {
		return "", errors.New("fake adapter cannot identify the caller")
	}
	var handle string
	for candidate := range a.Reach {
		handle = candidate
	}
	return handle, nil
}

// State reports a configured handle, or gone.
func (a *Adapter) State(_ context.Context, handle string) (adapter.State, error) {
	state, ok := a.Reach[handle]
	if !ok {
		return adapter.StateGone, nil
	}
	return state, nil
}

// Deliver records the call and returns the configured result.
func (a *Adapter) Deliver(_ context.Context, handle, text string) (adapter.Result, error) {
	a.Calls++
	a.LastText = text
	if result, ok := a.Result[handle]; ok {
		return result, nil
	}
	return adapter.ResultFailed, nil
}

// SelfTest returns SelfTestErr.
func (a *Adapter) SelfTest(context.Context) error { return a.SelfTestErr }
