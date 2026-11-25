// Package backend provides pluggable agent-messaging backend implementations
// for listing sessions, finding the session that owns a PR, checking idle
// state, and sending prompts.
package backend

// Send status values returned by SendPrompt.
const (
	SendDone    = "done"
	SendTimeout = "timeout"
	SendBlocked = "blocked"
	SendStalled = "stalled"
	SendError   = "error"
)

// Session is one agent session exposed by a messaging backend.
type Session struct {
	ID        string `json:"id"`
	TabID     string `json:"tab_id,omitempty"` //nolint:tagliatelle // wire format
	Label     string `json:"label,omitempty"`
	Machine   string `json:"machine,omitempty"`
	Status    string `json:"status,omitempty"`
	HasDialog bool   `json:"has_dialog,omitempty"` //nolint:tagliatelle // wire format
	Cwd       string `json:"cwd,omitempty"`
}

// IdleState reports whether a session can accept a prompt right now.
type IdleState struct {
	Idle      bool
	HasDialog bool
	Status    string
}

// SendResult is the outcome of sending a prompt to an agent session.
type SendResult struct {
	Status string
	Detail string
	Err    error
}

// IsIdleStatus reports whether an agent status string represents an idle/ready agent.
func IsIdleStatus(status string) bool {
	return status == "idle" || status == "done"
}
