package backend

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// CommandBackend delegates agent-messaging operations to an external executable
// using JSON over stdout/stdin:
//
//	<cmd> list-sessions
//	<cmd> read-idle <session-id>
//	<cmd> send-prompt <session-id> <timeout-ms>  (prompt on stdin)
type CommandBackend struct {
	Exec executor.Executor
	Bin  string
}

// Name returns "command".
func (b *CommandBackend) Name() string {
	return "command"
}

// ListSessions runs `<bin> list-sessions` and decodes a JSON array of Session.
func (b *CommandBackend) ListSessions(ctx context.Context) ([]Session, error) {
	res, err := b.Exec.Execute(ctx, executor.ToolConfig{
		Command: b.Bin,
		Args:    []string{"list-sessions"},
		Timeout: 15 * time.Second,
	})
	if err != nil {
		return nil, fmt.Errorf("backend %s list-sessions: %w", b.Bin, err)
	}
	if res.ExitCode != 0 {
		return nil, fmt.Errorf("backend %s list-sessions exited %d: %s", b.Bin, res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	var sessions []Session
	if err := json.Unmarshal([]byte(res.Output), &sessions); err != nil {
		return nil, fmt.Errorf("decode backend list-sessions: %w", err)
	}
	return sessions, nil
}

// FindSession searches ListSessions for a session matching pr.
func (b *CommandBackend) FindSession(ctx context.Context, pr scan.PR, labelTemplate string) (*Session, error) {
	sessions, err := b.ListSessions(ctx)
	if err != nil {
		return nil, err
	}
	return matchSession(sessions, pr, labelTemplate), nil
}

// ReadIdleState runs `<bin> read-idle <session-id>` and decodes JSON
// `{"idle":bool,"has_dialog":bool,"status":string}`.
func (b *CommandBackend) ReadIdleState(ctx context.Context, sessionID string) (IdleState, error) {
	res, err := b.Exec.Execute(ctx, executor.ToolConfig{
		Command: b.Bin,
		Args:    []string{"read-idle", sessionID},
		Timeout: 15 * time.Second,
	})
	if err != nil {
		return IdleState{}, fmt.Errorf("backend %s read-idle: %w", b.Bin, err)
	}
	if res.ExitCode != 0 {
		return IdleState{}, fmt.Errorf("backend %s read-idle exited %d: %s", b.Bin, res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	var out struct {
		Idle      bool   `json:"idle"`
		HasDialog bool   `json:"has_dialog"` //nolint:tagliatelle // wire format
		Status    string `json:"status"`
	}
	if err := json.Unmarshal([]byte(res.Output), &out); err != nil {
		return IdleState{}, fmt.Errorf("decode backend read-idle: %w", err)
	}
	return IdleState{Idle: out.Idle, HasDialog: out.HasDialog, Status: out.Status}, nil
}

// SendPrompt runs `<bin> send-prompt <session-id> <timeout-ms>` with prompt on stdin.
func (b *CommandBackend) SendPrompt(ctx context.Context, sessionID, prompt string, timeout time.Duration) SendResult {
	res, err := b.Exec.Execute(ctx, executor.ToolConfig{
		Command: b.Bin,
		Args:    []string{"send-prompt", sessionID, fmt.Sprintf("%d", timeout.Milliseconds())},
		Stdin:   strings.NewReader(prompt),
		Timeout: timeout + 5*time.Second,
	})
	if err != nil {
		return SendResult{Status: SendError, Detail: err.Error(), Err: err}
	}
	if res.ExitCode != 0 {
		return SendResult{
			Status: SendError,
			Detail: fmt.Sprintf("exited %d: %s", res.ExitCode, strings.TrimSpace(res.Stderr)),
		}
	}
	var out struct {
		Status string `json:"status"`
		Detail string `json:"detail"`
	}
	if strings.TrimSpace(res.Output) != "" {
		if err := json.Unmarshal([]byte(res.Output), &out); err == nil && out.Status != "" {
			return SendResult{Status: out.Status, Detail: out.Detail}
		}
	}
	return SendResult{Status: SendDone}
}
