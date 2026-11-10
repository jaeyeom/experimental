// Package adapter is the transport boundary for agentbus delivery.
package adapter

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// State is the normalized agent state every adapter reports.
type State string

// Normalized states. Names do not expire, and gone means the handle is not there.
const (
	StateIdle    State = "idle"
	StateWorking State = "working"
	StateBlocked State = "blocked"
	StateDone    State = "done"
	StateNoAgent State = "no-agent"
	StateUnknown State = "unknown"
	StateGone    State = "gone"
)

// Result is what deliver reports for one attempt.
type Result string

// Delivery results. Held is a router outcome, not an adapter result.
const (
	ResultAccepted         Result = "accepted"
	ResultRefusedRetryable Result = "refused-retryable"
	ResultFailed           Result = "failed"
	ResultHeld             Result = "held"
)

// Agent is one agent an adapter can see. Fields stay stable for later search.
type Agent struct {
	Handle    string
	PaneID    string
	TabID     string
	TabLabel  string
	Kind      string
	Status    string
	SessionID string
	Title     string
}

// InventoryWarning is the error Inventory returns with the agents it
// could list when some machines could not be listed. Each line names one
// machine and why, for example "machine m2 unreachable: host unreachable".
type InventoryWarning struct {
	Lines []string
}

func (w *InventoryWarning) Error() string {
	return strings.Join(w.Lines, "; ")
}

// Hold reasons ComposerEmpty reports when a paste would mix with user input.
const (
	HoldDraft        = "user draft in composer"
	HoldDraftUnknown = "draft-unknown"
	HoldFocused      = "pane focused"
)

// Adapter is one transport. Identify reads the caller's environment.
// The caller does not type its own handle.
//
// ComposerEmpty is a paste precondition: delivery needs an empty
// composer. When it is not empty, or cannot be read, it reports false
// and a hold reason.
type Adapter interface {
	Name() string
	Inventory(ctx context.Context) ([]Agent, error)
	Identify(ctx context.Context, env []string) (string, error)
	State(ctx context.Context, handle string) (State, error)
	ComposerEmpty(ctx context.Context, handle string) (empty bool, reason string, err error)
	Deliver(ctx context.Context, handle, text string) (Result, error)
	SelfTest(ctx context.Context) error
}

// Set tries adapters in order. A failed self-test logs adapter_error and
// that adapter is skipped. With nobody left who can take the message, it is held.
type Set struct {
	Adapters []Adapter
	Log      *eventlog.Log
	broken   map[string]string
	tested   bool
}

// Deliver runs self-tests once, then the first adapter that accepts the text.
func (s *Set) Deliver(ctx context.Context, handle, text string) (Result, error) {
	if err := s.test(ctx); err != nil {
		return "", err
	}
	for _, item := range s.Adapters {
		if _, skip := s.broken[item.Name()]; skip {
			continue
		}
		state, err := item.State(ctx, handle)
		if err != nil || state == StateGone || state == StateUnknown {
			continue
		}
		result, err := item.Deliver(ctx, handle, text)
		if err != nil {
			return "", fmt.Errorf("deliver through %s: %w", item.Name(), err)
		}
		if result == ResultAccepted {
			return ResultAccepted, nil
		}
	}
	if s.Log != nil {
		if err := s.Log.Append(eventlog.Event{
			Event:       eventlog.EventHeld,
			Actor:       "agentbus",
			Name:        handle,
			Detail:      handle,
			Reason:      "no adapter accepted delivery",
			TargetState: "held",
		}); err != nil {
			return "", fmt.Errorf("log held: %w", err)
		}
	}
	return ResultHeld, nil
}

func (s *Set) test(ctx context.Context) error {
	if s.tested {
		return nil
	}
	s.tested = true
	if s.broken == nil {
		s.broken = map[string]string{}
	}
	for _, item := range s.Adapters {
		err := item.SelfTest(ctx)
		if err == nil {
			continue
		}
		s.broken[item.Name()] = err.Error()
		if s.Log == nil {
			continue
		}
		if logErr := s.Log.Append(eventlog.Event{
			Event:  eventlog.EventAdapterError,
			Actor:  "agentbus",
			Name:   item.Name(),
			Detail: "self-test failed",
			Reason: err.Error(),
		}); logErr != nil {
			return fmt.Errorf("log adapter_error: %w", logErr)
		}
	}
	return nil
}
