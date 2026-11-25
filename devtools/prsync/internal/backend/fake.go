package backend

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// SentPrompt records one prompt sent through FakeBackend.
type SentPrompt struct {
	SessionID string
	Prompt    string
	Timeout   time.Duration
}

// FakeBackend is an in-memory Backend for tests.
type FakeBackend struct {
	mu       sync.Mutex
	Sessions []Session
	ListErr  error
	IdleErr  error
	// SendResults maps sessionID to a fixed SendResult. If absent, returns SendDone.
	SendResults map[string]SendResult
	// OnSend, when non-nil, is invoked during SendPrompt before returning.
	OnSend func(sessionID, prompt string)
	Sent   []SentPrompt
}

// Name returns "fake".
func (f *FakeBackend) Name() string {
	return "fake"
}

// ListSessions returns configured Sessions or ListErr.
func (f *FakeBackend) ListSessions(_ context.Context) ([]Session, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.ListErr != nil {
		return nil, f.ListErr
	}
	out := make([]Session, len(f.Sessions))
	copy(out, f.Sessions)
	return out, nil
}

// FindSession finds the session matching pr in Sessions.
func (f *FakeBackend) FindSession(ctx context.Context, pr scan.PR, labelTemplate string) (*Session, error) {
	sessions, err := f.ListSessions(ctx)
	if err != nil {
		return nil, err
	}
	if pr.Tab != nil && pr.Tab.PaneID != nil && *pr.Tab.PaneID != "" {
		for _, s := range sessions {
			if s.ID == *pr.Tab.PaneID {
				hit := s
				return &hit, nil
			}
		}
		return &Session{
			ID:        *pr.Tab.PaneID,
			TabID:     pr.Tab.TabID,
			Label:     pr.Tab.Label,
			Machine:   pr.Tab.Machine,
			Status:    pr.Tab.AgentStatus,
			HasDialog: pr.Tab.AgentStatus == "blocked",
		}, nil
	}
	return matchSession(sessions, pr, labelTemplate), nil
}

// ReadIdleState returns the idle state of sessionID.
func (f *FakeBackend) ReadIdleState(_ context.Context, sessionID string) (IdleState, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.IdleErr != nil {
		return IdleState{}, f.IdleErr
	}
	for _, s := range f.Sessions {
		if s.ID == sessionID {
			return IdleState{
				Idle:      IsIdleStatus(s.Status),
				HasDialog: s.HasDialog || s.Status == "blocked",
				Status:    s.Status,
			}, nil
		}
	}
	return IdleState{}, fmt.Errorf("session %q not found", sessionID)
}

// SendPrompt records the prompt and returns the configured SendResult.
func (f *FakeBackend) SendPrompt(_ context.Context, sessionID, prompt string, timeout time.Duration) SendResult {
	f.mu.Lock()
	f.Sent = append(f.Sent, SentPrompt{SessionID: sessionID, Prompt: prompt, Timeout: timeout})
	onSend := f.OnSend
	res, ok := f.SendResults[sessionID]
	f.mu.Unlock()
	if onSend != nil {
		onSend(sessionID, prompt)
	}
	if ok {
		return res
	}
	return SendResult{Status: SendDone}
}
