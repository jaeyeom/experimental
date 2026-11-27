// Package outbox implements a channel-agnostic confirm-before-doing gate backed by decide.
package outbox

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"
	"time"
	"unicode/utf8"
)

// Confirmation states shared across outbox records.
const (
	StatePending      = "pending"
	StateApproved     = "approved"
	StateExecuting    = "executing"
	StateExecuted     = "executed"
	StateNeedsRedraft = "needs-redraft"
	StateCancelled    = "cancelled"
	StateRejected     = "rejected"
	StateFailed       = "failed"
	StateUncertain    = "uncertain"
)

// IsFinalState reports whether state needs no further polling or execution.
func IsFinalState(state string) bool {
	switch state {
	case StateExecuted, StateNeedsRedraft, StateCancelled, StateRejected, StateUncertain:
		return true
	default:
		return false
	}
}

// Digest returns the lowercase hex SHA-256 of text.
func Digest(text string) string {
	sum := sha256.Sum256([]byte(text))
	return hex.EncodeToString(sum[:])
}

// UsageError reports invalid caller input or draft configuration.
type UsageError struct{ Msg string }

// NewUsageError returns a *UsageError with msg.
func NewUsageError(msg string) *UsageError {
	return &UsageError{Msg: msg}
}

// UsageErrorf formats and returns a *UsageError.
func UsageErrorf(format string, args ...any) *UsageError {
	return &UsageError{Msg: fmt.Sprintf(format, args...)}
}

// Error returns the underlying usage error message.
func (e *UsageError) Error() string { return e.Msg }

// Receipt records one completed external execution step.
type Receipt struct {
	Where string `json:"where"`
	URL   string `json:"url"`
}

// ExecutionState tracks crash-safe multi-step execution progress.
type ExecutionState struct {
	InFlight *int
	Receipts []Receipt
}

// Draft holds an agnostic outbound action submitted for human approval.
type Draft struct {
	Kind     string            `json:"kind"`
	Target   string            `json:"target"`
	Owner    string            `json:"owner"`
	Ref      string            `json:"ref,omitempty"`
	Question string            `json:"question"`
	Verbatim string            `json:"verbatim,omitempty"`
	Digest   string            `json:"digest,omitempty"`
	Payload  json.RawMessage   `json:"payload,omitempty"`
	Metadata map[string]string `json:"metadata,omitempty"`
}

// ExpectedDigest returns d.Digest when set, or the digest of Verbatim or Question.
func (d Draft) ExpectedDigest() string {
	if d.Digest != "" {
		return d.Digest
	}
	return d.ContentDigest()
}

// ContentDigest computes the SHA-256 digest of Verbatim when non-empty, else Question.
func (d Draft) ContentDigest() string {
	if d.Verbatim != "" {
		return Digest(d.Verbatim)
	}
	return Digest(d.Question)
}

// Validate checks that required fields are present and UTF-8 clean.
func (d Draft) Validate() error {
	switch {
	case strings.TrimSpace(d.Target) == "":
		return NewUsageError("target is required")
	case strings.TrimSpace(d.Owner) == "":
		return NewUsageError("owner is required")
	case strings.TrimSpace(d.Question) == "":
		return NewUsageError("question preview is required")
	case !utf8.ValidString(d.Question):
		return NewUsageError("question preview is not valid UTF-8")
	case !utf8.ValidString(d.Verbatim):
		return NewUsageError("verbatim text is not valid UTF-8")
	default:
		return nil
	}
}

// Record stores an outbox Draft and its confirmation and execution progress.
type Record struct {
	ID int64 `json:"id"`
	Draft
	State        string    `json:"state"`
	DecideID     string    `json:"decideId,omitempty"`
	Receipts     []Receipt `json:"receipts,omitempty"`
	InFlight     *int      `json:"inFlight,omitempty"`
	Outcome      string    `json:"outcome,omitempty"`
	Notes        string    `json:"notes,omitempty"`
	Notified     bool      `json:"notified,omitempty"`
	DecideClosed bool      `json:"decideClosed,omitempty"`
	CreatedAt    time.Time `json:"createdAt"`
	UpdatedAt    time.Time `json:"updatedAt"`
}
