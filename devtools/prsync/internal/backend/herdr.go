package backend

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// HerdrClient is the narrow herdr client surface used by HerdrBackend.
type HerdrClient interface {
	TabList(ctx context.Context) ([]herdr.Tab, error)
	AgentList(ctx context.Context) ([]herdr.Agent, error)
	Prompt(ctx context.Context, paneID, text string, until []string, timeout time.Duration) herdr.PromptOutcome
}

// HerdrBackend talks to agent sessions through herdr.
type HerdrBackend struct {
	Local     HerdrClient
	WaitUntil []string
}

// Name returns "herdr".
func (b *HerdrBackend) Name() string {
	return "herdr"
}

// ListSessions returns every agent session reported by herdr.
func (b *HerdrBackend) ListSessions(ctx context.Context) ([]Session, error) {
	tabs, err := b.Local.TabList(ctx)
	if err != nil {
		return nil, fmt.Errorf("herdr tab list: %w", err)
	}
	agents, err := b.Local.AgentList(ctx)
	if err != nil {
		return nil, fmt.Errorf("herdr agent list: %w", err)
	}
	tabsByID := make(map[string]herdr.Tab, len(tabs))
	for _, t := range tabs {
		tabsByID[t.TabID] = t
	}
	out := make([]Session, 0, len(agents))
	for _, a := range agents {
		tab := tabsByID[a.TabID]
		out = append(out, Session{
			ID:        a.PaneID,
			TabID:     a.TabID,
			Label:     tab.Label,
			Machine:   a.Machine,
			Status:    a.AgentStatus,
			HasDialog: a.AgentStatus == "blocked",
			Cwd:       a.Cwd,
		})
	}
	return out, nil
}

// FindSession locates the session for pr. If pr.Tab already carries a matched
// pane from scan, it uses that directly; otherwise it searches ListSessions
// by label template or #N label.
func (b *HerdrBackend) FindSession(ctx context.Context, pr scan.PR, labelTemplate string) (*Session, error) {
	if pr.Tab != nil && pr.Tab.PaneID != nil && *pr.Tab.PaneID != "" {
		return &Session{
			ID:        *pr.Tab.PaneID,
			TabID:     pr.Tab.TabID,
			Label:     pr.Tab.Label,
			Machine:   pr.Tab.Machine,
			Status:    pr.Tab.AgentStatus,
			HasDialog: pr.Tab.AgentStatus == "blocked",
		}, nil
	}
	sessions, err := b.ListSessions(ctx)
	if err != nil {
		return nil, err
	}
	return matchSession(sessions, pr, labelTemplate), nil
}

// ReadIdleState reads the current status of sessionID from herdr.
func (b *HerdrBackend) ReadIdleState(ctx context.Context, sessionID string) (IdleState, error) {
	agents, err := b.Local.AgentList(ctx)
	if err != nil {
		return IdleState{}, fmt.Errorf("herdr agent list: %w", err)
	}
	for _, a := range agents {
		if a.PaneID == sessionID {
			return IdleState{
				Idle:      IsIdleStatus(a.AgentStatus),
				HasDialog: a.AgentStatus == "blocked",
				Status:    a.AgentStatus,
			}, nil
		}
	}
	return IdleState{}, fmt.Errorf("session %q not found", sessionID)
}

// SendPrompt delivers prompt to sessionID via herdr agent prompt --wait.
func (b *HerdrBackend) SendPrompt(ctx context.Context, sessionID, prompt string, timeout time.Duration) SendResult {
	until := b.WaitUntil
	if len(until) == 0 {
		until = []string{"idle", "done"}
	}
	out := b.Local.Prompt(ctx, sessionID, prompt, until, timeout)
	switch out.Status {
	case herdr.PromptMatched:
		if out.Agent.AgentStatus == "blocked" {
			return SendResult{Status: SendBlocked, Detail: "agent blocked awaiting input"}
		}
		return SendResult{Status: SendDone}
	case herdr.PromptTimeout:
		return SendResult{Status: SendTimeout, Detail: "prompt timed out"}
	case herdr.PromptStalled:
		return SendResult{Status: SendStalled, Detail: "prompt stalled"}
	default:
		detail := "herdr prompt failed"
		if out.Err != nil {
			detail = out.Err.Error()
		}
		return SendResult{Status: SendError, Detail: detail, Err: out.Err}
	}
}

func matchSession(sessions []Session, pr scan.PR, labelTemplate string) *Session {
	wantLabel := ""
	if pr.Identifier != nil && *pr.Identifier != "" {
		tmpl := labelTemplate
		if tmpl == "" {
			tmpl = "{id}"
		}
		wantLabel = strings.ReplaceAll(tmpl, "{id}", *pr.Identifier)
	}
	numLabel := fmt.Sprintf("#%d", pr.Number)
	var matches []Session
	for _, s := range sessions {
		if wantLabel != "" && s.Label == wantLabel {
			matches = append(matches, s)
			continue
		}
		if s.Label == numLabel {
			matches = append(matches, s)
		}
	}
	if len(matches) != 1 {
		return nil
	}
	hit := matches[0]
	return &hit
}
