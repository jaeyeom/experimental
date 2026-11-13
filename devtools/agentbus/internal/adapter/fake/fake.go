// Package fake is an adapter whose results are set by tests.
package fake

import (
	"context"
	"errors"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

// Adapter returns the states and delivery results it was given.
// A non-nil SelfTestErr marks this adapter broken.
// PaneHandles, when non-nil, identifies the caller from HERDR_PANE_ID.
// InventoryAgents, when non-nil, is the inventory instead of Reach.
type Adapter struct {
	AdapterName     string
	SelfTestErr     error
	Reach           map[string]adapter.State
	Result          map[string]adapter.Result
	PaneHandles     map[string]string
	InventoryAgents []adapter.Agent
	Calls           int
	LastText        string
	// Texts is every paste, in delivery order.
	Texts []string
	// Composer, when non-nil, answers ComposerEmpty. Otherwise it is empty.
	Composer func(handle string) (empty bool, reason string, err error)
	// DeliverErr is returned after OnDeliver, instead of Result.
	DeliverErr error
	// OnDeliver runs after the paste is recorded and before the result.
	OnDeliver func(text string)
	// Screen is the recent scrollback Probe returns.
	Screen string
	// ComposerText is the unsubmitted box Probe returns.
	ComposerText string
	// ScreenUnreadable makes Probe report that the screen cannot be checked.
	ScreenUnreadable bool
	// ProbeErr is Probe's error.
	ProbeErr error
	// Submits counts Enter-only submits.
	Submits int
	// SubmitErr is Submit's error.
	SubmitErr error
}

// ComposerEmpty calls Composer, or reports ComposerText as a user draft.
func (a *Adapter) ComposerEmpty(_ context.Context, handle string) (bool, string, error) {
	if a.Composer != nil {
		return a.Composer(handle)
	}
	if strings.TrimSpace(a.ComposerText) != "" {
		return false, adapter.HoldDraft, nil
	}
	return true, "", nil
}

// Name is the adapter name recorded on adapter_error.
func (a *Adapter) Name() string { return a.AdapterName }

// Inventory lists the handles this fake can reach.
func (a *Adapter) Inventory(context.Context) ([]adapter.Agent, error) {
	if a.InventoryAgents != nil {
		return a.InventoryAgents, nil
	}
	out := make([]adapter.Agent, 0, len(a.Reach))
	for handle, state := range a.Reach {
		out = append(out, adapter.Agent{Handle: handle, Status: string(state)})
	}
	return out, nil
}

// Identify returns the caller. PaneHandles uses HERDR_PANE_ID.
// Otherwise the only Reach handle is the caller.
func (a *Adapter) Identify(_ context.Context, env []string) (string, error) {
	if a.PaneHandles != nil {
		pane := envValue(env, "HERDR_PANE_ID")
		if pane == "" {
			return "", errors.New("HERDR_PANE_ID is not set")
		}
		handle, ok := a.PaneHandles[pane]
		if !ok || handle == "" {
			return "", errors.New("herdr pane " + pane + " is not in the agent list")
		}
		return handle, nil
	}
	if len(a.Reach) != 1 {
		return "", errors.New("fake adapter cannot identify the caller")
	}
	var handle string
	for candidate := range a.Reach {
		handle = candidate
	}
	return handle, nil
}

func envValue(env []string, key string) string {
	for _, entry := range env {
		name, value, ok := strings.Cut(entry, "=")
		if ok && name == key {
			return value
		}
	}
	return ""
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
	a.Texts = append(a.Texts, text)
	if a.OnDeliver != nil {
		a.OnDeliver(text)
	}
	if a.DeliverErr != nil {
		return adapter.ResultFailed, a.DeliverErr
	}
	if result, ok := a.Result[handle]; ok {
		return result, nil
	}
	return adapter.ResultFailed, nil
}

// Probe returns the scrollback and composer a test stored.
func (a *Adapter) Probe(context.Context, string, string) (string, string, bool, error) {
	if a.ProbeErr != nil {
		return "", "", false, a.ProbeErr
	}
	return a.Screen, a.ComposerText, a.ScreenUnreadable, nil
}

// Submit records one Enter and does not append another paste.
func (a *Adapter) Submit(context.Context, string) error {
	a.Submits++
	if a.SubmitErr != nil {
		return a.SubmitErr
	}
	if a.ComposerText != "" {
		if a.Screen != "" && !strings.HasSuffix(a.Screen, "\n") {
			a.Screen += "\n"
		}
		a.Screen += a.ComposerText
		a.ComposerText = ""
	}
	return nil
}

// SelfTest returns SelfTestErr.
func (a *Adapter) SelfTest(context.Context) error { return a.SelfTestErr }
