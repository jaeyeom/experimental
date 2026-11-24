// Package harness defines the coding-agent execution interface and adapters
// for running headless agent CLIs and parsing their structured event streams.
package harness

import (
	"encoding/json"
	"io"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

// JobSpec holds all inputs needed to build a headless agent invocation for a
// single skill execution.
type JobSpec struct {
	SkillName        string
	SkillMarkdown    string
	CanonicalArgs    []byte          // Output of catalog.Skill.ValidateArgs.
	OutputSchema     json.RawMessage // Compact JSON Schema string.
	EmptyMCPPath     string          // Path to {"mcpServers":{}}.
	MaxTurns         int
	MaxEffectiveToks int64
}

// CommandSpec describes an agent binary and its exact argv slice (no shell).
type CommandSpec struct {
	Agent     string   // e.g. "claude".
	AgentArgs []string // Exact argv passed to the agent CLI (no shell).
}

// StreamProgress reports incremental turn, token, and model telemetry while an
// agent stream is being consumed.
type StreamProgress struct {
	TurnsUsed      int
	Tokens         protocol.TokenUsage
	EffectiveUnits int64
	Model          string
}

// StreamCallbacks receives real-time progress updates and immediate budget
// violation notifications while parsing an agent output stream.
type StreamCallbacks struct {
	OnProgress       func(p StreamProgress)
	OnBudgetExceeded func(p StreamProgress, reason string) // Called immediately when turns or tokens exceed cap.
}

// RunResult summarizes the parsed outcome, usage, and deliverable from an agent
// stream.
type RunResult struct {
	Provider       string
	Model          string
	Deliverable    json.RawMessage
	Tokens         protocol.TokenUsage
	EffectiveUnits int64
	TurnsUsed      int
	BudgetExceeded bool
	ExceedReason   string
	IsError        bool
	ErrorMessage   string
}

// Runner abstracts a headless coding-agent CLI harness.
type Runner interface {
	Name() string
	BuildCommand(spec JobSpec) (CommandSpec, error)
	ParseStream(r io.Reader, limits catalog.Limits, cb StreamCallbacks) (RunResult, error)
}
