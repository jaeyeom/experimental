// Package policy implements configurable auto-dispatch policy evaluation,
// guards, conflict classification, nit-reply matching, failure-marker pickup,
// lease hooks, and ledger recording for PR maintenance.
package policy

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// Policy dispatch kinds.
const (
	KindCatchUp     = "catch-up"
	KindRebase      = "rebase"
	KindNitReply    = "nit-reply"
	KindCIFix       = "ci-fix"
	KindQueueFix    = "queue-fix"
	KindBotRereview = "bot-rereview"
)

// ConflictClass classifies a PR's merge/branch state relative to its base.
type ConflictClass string

// Conflict class values.
const (
	ClassUpToDate    ConflictClass = "up-to-date"
	ClassBehind      ConflictClass = "behind"
	ClassCleanRebase ConflictClass = "clean-rebase"
	ClassConflicting ConflictClass = "conflicting"
)

// DecisionAction is the top-level verdict for a candidate (PR, kind) pair.
type DecisionAction string

// Decision action values.
const (
	DecisionDispatch DecisionAction = "dispatch"
	DecisionDryRun   DecisionAction = "dry-run"
	DecisionSkip     DecisionAction = "skip"
	DecisionReport   DecisionAction = "report"
	DecisionError    DecisionAction = "error"
)

// Decision is one evaluated policy decision for a PR and kind.
type Decision struct {
	PR            string              `json:"pr"`
	Repo          string              `json:"repo"`
	Number        int                 `json:"number"`
	HeadSHA       string              `json:"head_sha"` //nolint:tagliatelle // wire format
	Kind          string              `json:"kind"`
	Mode          string              `json:"mode"`
	Decision      DecisionAction      `json:"decision"`
	Reason        string              `json:"reason,omitempty"`
	ConflictClass ConflictClass       `json:"conflict_class,omitempty"` //nolint:tagliatelle // wire format
	Marker        *scan.FailureMarker `json:"marker,omitempty"`
	SessionID     string              `json:"session_id,omitempty"` //nolint:tagliatelle // wire format
	Outcome       string              `json:"outcome,omitempty"`
	Detail        string              `json:"detail,omitempty"`
	Prompt        string              `json:"prompt,omitempty"`
}

// Document is the structured JSON output of a policy maintenance run.
type Document struct {
	GeneratedAt string     `json:"generated_at"` //nolint:tagliatelle // wire format
	Backend     string     `json:"backend"`
	Decisions   []Decision `json:"decisions"`
}

// RunOptions controls one maintenance policy run.
type RunOptions struct {
	Force  bool
	DryRun bool
}

// GuardContext is passed to a GuardRunner before each live dispatch.
type GuardContext struct {
	PR      string
	HeadSHA string
	Kind    string
}

// GuardRunner runs the optional user-configured guard command (exit 0 = allowed).
type GuardRunner interface {
	Check(ctx context.Context, command string, gctx GuardContext) (allowed bool, detail string, err error)
}

// ExecGuardRunner executes the guard command via sh -c with PRSYNC_* env vars.
type ExecGuardRunner struct {
	Exec executor.Executor
}

// Check runs command via sh -c. Exit 0 means allowed.
func (r ExecGuardRunner) Check(ctx context.Context, command string, gctx GuardContext) (bool, string, error) {
	if strings.TrimSpace(command) == "" {
		return true, "", nil
	}
	res, err := r.Exec.Execute(ctx, executor.ToolConfig{
		Command: "sh",
		Args:    []string{"-c", command},
		Env: map[string]string{
			"PRSYNC_PR":       gctx.PR,
			"PRSYNC_HEAD_SHA": gctx.HeadSHA,
			"PRSYNC_KIND":     gctx.Kind,
		},
		Timeout: 30 * time.Second,
	})
	if err != nil {
		return false, "", fmt.Errorf("guard command: %w", err)
	}
	if res.ExitCode == 0 {
		return true, "", nil
	}
	detail := strings.TrimSpace(res.Stderr)
	if detail == "" {
		detail = strings.TrimSpace(res.Output)
	}
	if detail == "" {
		detail = fmt.Sprintf("exit %d", res.ExitCode)
	}
	return false, detail, nil
}

// TouchesWorkingCopy reports whether dispatching kind mutates the shared git
// working copy and therefore requires the lease hook when configured.
func TouchesWorkingCopy(kind string) bool {
	switch kind {
	case KindCatchUp, KindRebase, KindCIFix, KindQueueFix:
		return true
	default:
		return false
	}
}

// FormatLines formats Document as one human-readable line per decision.
func FormatLines(doc Document) string {
	var b strings.Builder
	for _, d := range doc.Decisions {
		verb := strings.ToUpper(string(d.Decision))
		sha := d.HeadSHA
		if len(sha) > 7 {
			sha = sha[:7]
		}
		line := fmt.Sprintf("%-8s  %-24s  %-10s  sha=%-7s", verb, d.PR, d.Kind, sha)
		if d.ConflictClass != "" && d.ConflictClass != ClassUpToDate {
			line += "  class=" + string(d.ConflictClass)
		}
		if d.SessionID != "" {
			line += "  session=" + d.SessionID
		}
		if d.Outcome != "" {
			line += "  outcome=" + d.Outcome
		}
		if d.Reason != "" {
			line += "  reason=" + d.Reason
		}
		if d.Detail != "" {
			line += "  detail=" + d.Detail
		}
		b.WriteString(line)
		b.WriteByte('\n')
	}
	return b.String()
}
