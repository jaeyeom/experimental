package start

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// OutcomeStatus is the unambiguous result of a start attempt.
type OutcomeStatus string

const (
	// OutcomeStarted means the item was claimed and the agent started.
	OutcomeStarted OutcomeStatus = "started"
	// OutcomeRefused means start refused before changing the tracker, store, or herdr.
	OutcomeRefused OutcomeStatus = "refused"
	// OutcomeFailedClaimKept means start failed after claiming and kept the claim.
	OutcomeFailedClaimKept OutcomeStatus = "failed_claim_kept"
	// OutcomeFailedReleased means start failed after claiming and released the claim.
	OutcomeFailedReleased OutcomeStatus = "failed_released"
	// OutcomeDryRun means start printed a dry-run plan without applying.
	OutcomeDryRun OutcomeStatus = "dry_run"
)

// Exit codes for start outcomes.
const (
	ExitStarted   = 0
	ExitRefused   = 3
	ExitClaimKept = 4
	ExitReleased  = 5
)

// Outcome is the structured result of a start attempt.
type Outcome struct {
	ID         string            `json:"id"`
	Status     OutcomeStatus     `json:"status"`
	Spawner    string            `json:"spawner,omitempty"`
	OutputMode string            `json:"output_mode,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	KeepWorker *bool             `json:"keep_worker,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	Source     string            `json:"source,omitempty"`
	TabID      string            `json:"tab_id,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	Cwd        string            `json:"cwd,omitempty"`
	Agent      string            `json:"agent,omitempty"`
	AgentArgs  []string          `json:"agent_args,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	Env        map[string]string `json:"env,omitempty"`
	Worktree   string            `json:"worktree,omitempty"`
	Brief      string            `json:"brief,omitempty"`
	ClaimTime  string            `json:"claim_time,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	Reason     string            `json:"reason,omitempty"`
	NextStep   string            `json:"next_step,omitempty"` //nolint:tagliatelle // documented factory JSON output field
	ExitCode   int               `json:"exit_code"`           //nolint:tagliatelle // documented factory JSON output field
}

func keepWorkerForMode(mode string) *bool {
	keep := mode == config.OutputModePR
	return &keep
}

// Line returns the single-line summary for stdout.
func (o Outcome) Line() string {
	switch o.Status {
	case OutcomeStarted:
		if o.TabID == "" {
			return fmt.Sprintf("factory start %s: started (subagent, claim %s)", o.ID, o.ClaimTime)
		}
		return fmt.Sprintf("factory start %s: started (tab %s, claim %s)", o.ID, o.TabID, o.ClaimTime)
	case OutcomeRefused:
		return fmt.Sprintf("factory start %s: refused, nothing changed: %s", o.ID, o.Reason)
	case OutcomeFailedClaimKept:
		return fmt.Sprintf("factory start %s: failed after claim; claim kept: %s", o.ID, o.NextStep)
	case OutcomeFailedReleased:
		return fmt.Sprintf("factory start %s: failed after claim; claim released: %s", o.ID, o.NextStep)
	case OutcomeDryRun:
		return fmt.Sprintf("factory start %s: dry run", o.ID)
	default:
		return fmt.Sprintf("factory start %s: %s", o.ID, o.Status)
	}
}

// OutcomeError wraps an error with its start Outcome and exit status.
type OutcomeError struct {
	Outcome Outcome
	Err     error
}

func (e *OutcomeError) Error() string {
	if e.Err != nil {
		return e.Err.Error()
	}
	return e.Outcome.Line()
}

func (e *OutcomeError) Unwrap() error { return e.Err }

// ExitStatus returns the process exit code for this outcome.
func (e *OutcomeError) ExitStatus() int { return e.Outcome.ExitCode }

func (s *Starter) writeOutcome(opts Options, out Outcome) error {
	if s == nil || s.Out == nil {
		return nil
	}
	if opts.JSON {
		raw, err := json.Marshal(out)
		if err != nil {
			return fmt.Errorf("marshal start outcome: %w", err)
		}
		if _, err := io.WriteString(s.Out, string(raw)+"\n"); err != nil {
			return fmt.Errorf("write start: %w", err)
		}
		return nil
	}
	if out.Status == OutcomeDryRun {
		return nil
	}
	if _, err := io.WriteString(s.Out, out.Line()+"\n"); err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	return nil
}

func (s *Starter) finishOutcome(opts Options, out Outcome, err error) error {
	if werr := s.writeOutcome(opts, out); werr != nil {
		if err != nil {
			return errors.Join(err, werr)
		}
		return werr
	}
	if err == nil {
		return nil
	}
	return &OutcomeError{Outcome: out, Err: err}
}

func refusedOutcome(id string, err error) Outcome {
	return Outcome{
		ID:       id,
		Status:   OutcomeRefused,
		Reason:   err.Error(),
		ExitCode: ExitRefused,
	}
}
