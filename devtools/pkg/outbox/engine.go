package outbox

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/chorong-sh/chorong/decide"
)

// PreflightHook checks a Draft before it is stored or filed for approval.
type PreflightHook func(ctx context.Context, d Draft) error

// HeadMoveAction is returned by BeforeExecuteHook to pause or stop execution
// without error when external precondition state changes after approval.
type HeadMoveAction int

const (
	// Proceed continues normal execution.
	Proceed HeadMoveAction = iota
	// Pause leaves the record as saved by the hook (for example re-asked or needs-redraft).
	Pause
)

// BeforeExecuteHook runs after human-UI approval and digest verification, just
// before transitioning a record into execution. Returning Pause stops execution
// cleanly without calling Executor.
type BeforeExecuteHook func(ctx context.Context, r *Record) (HeadMoveAction, error)

// Engine runs the canonical confirm-before-doing workflow over an agnostic
// Store, Approver, Executor, and Transport.
type Engine struct {
	Store         Store
	Decide        Approver
	Executor      Executor
	Transport     Transport
	Preflight     PreflightHook
	BeforeExecute BeforeExecuteHook
	NotifyTarget  string
	AskOptions    []decide.Option
	AskClass      string
	LongLines     bool
	Policy        EvalPolicy
	Lock          func() (func(), error)
	Now           func() time.Time
}

func (e *Engine) now() time.Time {
	if e.Now != nil {
		return e.Now().UTC()
	}
	return time.Now().UTC()
}

func (e *Engine) transport() Transport {
	if e.Transport != nil {
		return e.Transport
	}
	return PullTransport{}
}

func (e *Engine) policy() EvalPolicy {
	if len(e.Policy.ApproveKeys) == 0 {
		return DefaultEvalPolicy()
	}
	return e.Policy
}

func (e *Engine) askOptions() []decide.Option {
	if len(e.AskOptions) > 0 {
		return e.AskOptions
	}
	return []decide.Option{
		{Key: "post", Label: "Post exactly this"},
		{Key: "edit", Label: "Edit (write notes for a redraft)"},
		{Key: "cancel", Label: "Cancel"},
	}
}

// Submit validates d, runs Preflight if configured, stores the pending Record,
// and files its decide approval item.
func (e *Engine) Submit(ctx context.Context, d Draft) (Record, error) {
	if err := d.Validate(); err != nil {
		return Record{}, err
	}
	if e.Preflight != nil {
		if err := e.Preflight(ctx, d); err != nil {
			return Record{}, err
		}
	}
	if d.Digest == "" {
		d.Digest = d.ContentDigest()
	}
	now := e.now()
	r, err := e.Store.Create(Record{
		Draft:     d,
		State:     StatePending,
		CreatedAt: now,
		UpdatedAt: now,
	})
	if err != nil {
		return Record{}, fmt.Errorf("store draft: %w", err)
	}
	if err := e.ask(ctx, &r); err != nil {
		return r, fmt.Errorf("record %d stored; poll will file its approval: %w", r.ID, err)
	}
	return r, nil
}

func (e *Engine) ask(ctx context.Context, r *Record) error {
	req := AskRequest{
		Owner:        r.Owner,
		NotifyTarget: e.NotifyTarget,
		Class:        e.AskClass,
		Kind:         decide.KindRelay,
		LongLines:    e.LongLines,
		Options:      e.askOptions(),
		FreeText:     true,
		Verbatim:     r.Verbatim,
		Question:     r.Question,
	}
	id, err := e.Decide.Ask(ctx, req)
	if err != nil {
		return fmt.Errorf("file approval: %w", err)
	}
	r.DecideID = id
	return e.save(r)
}

func (e *Engine) save(r *Record) error {
	r.UpdatedAt = e.now()
	if err := e.Store.Save(*r); err != nil {
		return fmt.Errorf("save record %d: %w", r.ID, err)
	}
	return nil
}

// Poll processes every stored record, filing missing approvals, acting on
// human UI verdicts, resuming interrupted executions, and closing notifications.
func (e *Engine) Poll(ctx context.Context) error {
	if e.Lock != nil {
		unlock, err := e.Lock()
		if err != nil {
			return err
		}
		defer unlock()
	}
	recs, err := e.Store.List()
	if err != nil {
		return fmt.Errorf("list records: %w", err)
	}
	var errs []error
	for i := range recs {
		if err := e.StepRecord(ctx, &recs[i]); err != nil {
			errs = append(errs, fmt.Errorf("record %d: %w", recs[i].ID, err))
		}
	}
	return errors.Join(errs...)
}

// StepRecord advances one record through its state machine.
func (e *Engine) StepRecord(ctx context.Context, r *Record) error {
	switch r.State {
	case StatePending:
		if r.DecideID == "" {
			return e.ask(ctx, r)
		}
		return e.stepPending(ctx, r)
	case StateApproved, StateExecuting:
		if err := e.execute(ctx, r); err != nil {
			return err
		}
	}
	return e.finish(ctx, r)
}

func (e *Engine) stepPending(ctx context.Context, r *Record) error {
	it, err := e.Decide.Show(ctx, r.DecideID)
	if err != nil {
		return fmt.Errorf("read approval: %w", err)
	}
	v := EvaluateApproval(it, e.policy())
	switch v.Action {
	case ActionApprove:
		return e.approve(ctx, r)
	case ActionEdit:
		r.State, r.Notes = StateNeedsRedraft, v.Notes
		r.Outcome = "edit requested; redraft notice sent to " + r.Owner
	case ActionCancel:
		r.State, r.Outcome = StateCancelled, "cancelled; nothing was executed"
	case ActionRejected:
		r.State, r.Outcome = StateRejected, v.Refusal
	case ActionClosedWithoutApproval:
		r.State, r.DecideClosed = StateCancelled, true
		r.Outcome = "the approval item was " + it.State + " without approval; nothing was executed"
	default:
		return nil
	}
	if err := e.save(r); err != nil {
		return err
	}
	return e.finish(ctx, r)
}

func (e *Engine) approve(ctx context.Context, r *Record) error {
	if r.Digest != "" && r.Digest != r.ContentDigest() {
		r.State = StateRejected
		r.Outcome = "rejected: draft content digest no longer matches recorded digest"
		if err := e.save(r); err != nil {
			return err
		}
		return e.finish(ctx, r)
	}
	if e.BeforeExecute != nil {
		act, err := e.BeforeExecute(ctx, r)
		if err != nil {
			return err
		}
		if act == Pause {
			if IsFinalState(r.State) {
				return e.finish(ctx, r)
			}
			return nil
		}
	}
	r.State = StateExecuting
	if err := e.save(r); err != nil {
		return err
	}
	if err := e.execute(ctx, r); err != nil {
		return err
	}
	return e.finish(ctx, r)
}

func (e *Engine) execute(ctx context.Context, r *Record) error {
	if e.Executor == nil {
		return errors.New("executor is not configured")
	}
	steps, err := e.Executor.Steps(ctx, *r)
	if err != nil {
		return fmt.Errorf("plan execution: %w", err)
	}
	state := ExecutionState{InFlight: r.InFlight, Receipts: r.Receipts}
	err = ExecuteSteps(ctx, &state, steps, func() error {
		r.InFlight = state.InFlight
		r.Receipts = state.Receipts
		return e.save(r)
	})
	r.InFlight = state.InFlight
	r.Receipts = state.Receipts
	var uerr *UncertainError
	if errors.As(err, &uerr) {
		r.State = StateUncertain
		r.Outcome = uerr.Error()
		return e.save(r)
	}
	if err != nil {
		return err
	}
	r.State = StateExecuted
	r.Outcome = "executed"
	return e.save(r)
}

func (e *Engine) finish(ctx context.Context, r *Record) error {
	if !IsFinalState(r.State) {
		return nil
	}
	if !r.Notified {
		n := Notice{
			RecordID: r.ID,
			State:    r.State,
			Kind:     r.Kind,
			Target:   r.Target,
			Owner:    r.Owner,
			Ref:      r.Ref,
			Outcome:  r.Outcome,
			Notes:    r.Notes,
			Receipts: r.Receipts,
			Message:  formatNotice(*r),
		}
		if err := e.transport().Notify(ctx, n); err != nil {
			return fmt.Errorf("notify %s: %w", r.Owner, err)
		}
		r.Notified = true
		if err := e.save(r); err != nil {
			return err
		}
	}
	if !r.DecideClosed && r.DecideID != "" {
		outcome := r.Outcome + formatReceiptLines(r.Receipts)
		if err := e.Decide.Done(ctx, r.DecideID, outcome); err != nil {
			return fmt.Errorf("close approval: %w", err)
		}
		r.DecideClosed = true
		return e.save(r)
	}
	return nil
}

func formatReceiptLines(receipts []Receipt) string {
	var b strings.Builder
	for _, rc := range receipts {
		if rc.URL != "" {
			fmt.Fprintf(&b, "\n%s %s", rc.Where, rc.URL)
		} else if rc.Where != "" {
			fmt.Fprintf(&b, "\n%s", rc.Where)
		}
	}
	return b.String()
}

func formatNotice(r Record) string {
	head := "outbox record " + strconv.FormatInt(r.ID, 10) + " for " + r.Target
	if r.Ref != "" {
		head += " (ref " + r.Ref + ")"
	}
	switch r.State {
	case StateExecuted:
		return head + " executed:" + formatReceiptLines(r.Receipts)
	case StateNeedsRedraft:
		notes := r.Notes
		if notes == "" {
			notes = "(no notes)"
		}
		return head + ": the user asked for a redraft. Nothing was executed. Notes:\n" + notes
	case StateUncertain:
		return head + ": " + r.Outcome + formatReceiptLines(r.Receipts)
	default:
		return head + " was not executed: " + r.Outcome
	}
}
