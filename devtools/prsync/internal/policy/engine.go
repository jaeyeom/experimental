package policy

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/backend"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/lease"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Backend is the pluggable agent-messaging backend surface used by Engine.
type Backend interface {
	Name() string
	ListSessions(ctx context.Context) ([]backend.Session, error)
	FindSession(ctx context.Context, pr scan.PR, labelTemplate string) (*backend.Session, error)
	ReadIdleState(ctx context.Context, sessionID string) (backend.IdleState, error)
	SendPrompt(ctx context.Context, sessionID, prompt string, timeout time.Duration) backend.SendResult
}

// LeaseProvider is the pluggable lock/lease hook surface used by Engine.
type LeaseProvider interface {
	Name() string
	Claim(ctx context.Context, name string, timeout time.Duration) (lease.ClaimResult, error)
	Release(ctx context.Context, name string) error
}

// Engine evaluates configured auto-dispatch policies across a scan document.
type Engine struct {
	Backend Backend
	Lease   LeaseProvider
	Ledger  *ledger.Store
	Guard   GuardRunner
	Now     func() time.Time
}

func (e *Engine) now() time.Time {
	if e.Now != nil {
		return e.Now().UTC()
	}
	return time.Now().UTC()
}

func (e *Engine) leaseProvider() LeaseProvider {
	if e.Lease != nil {
		return e.Lease
	}
	return lease.NoopProvider{}
}

// Run evaluates all PRs in scanDoc against cfg.Policy and returns the
// decisions document. Read-only modes (off, dry-run) never send prompts and
// never claim leases.
func (e *Engine) Run(ctx context.Context, cfg config.Config, scanDoc scan.Document, opts RunOptions) (Document, error) {
	now := e.now()
	backendName := ""
	if e.Backend != nil {
		backendName = e.Backend.Name()
	}
	out := Document{
		GeneratedAt: now.Format(time.RFC3339),
		Backend:     backendName,
		Decisions:   []Decision{},
	}

	todayCount := 0
	if e.Ledger != nil && cfg.Policy.DailyCap > 0 {
		var err error
		todayCount, err = e.Ledger.CountToday(now)
		if err != nil {
			return out, fmt.Errorf("count daily dispatches: %w", err)
		}
	}
	dispatchedInRun := 0

	for _, pr := range scanDoc.PRs {
		if pr.State != "" && pr.State != "open" {
			continue
		}
		for _, cand := range e.detectCandidates(pr, cfg) {
			dec, sent, err := e.evaluateCandidate(ctx, cfg, pr, cand, opts, now, todayCount, dispatchedInRun)
			if err != nil {
				return out, err
			}
			out.Decisions = append(out.Decisions, dec)
			if sent {
				dispatchedInRun++
				todayCount++
			}
		}
	}
	return out, nil
}

type workCandidate struct {
	kind          string
	conflictClass ConflictClass
	marker        *scan.FailureMarker
	nitThreads    []scan.Comment
}

// detectCandidates returns the maintenance kinds applicable to pr in priority
// order: queue-fix, ci-fix, nit-reply, rebase, catch-up.
func (e *Engine) detectCandidates(pr scan.PR, cfg config.Config) []workCandidate {
	var out []workCandidate
	p := cfg.Policy

	if fm := effectiveFailureMarker(pr); fm != nil && p.QueueFix != config.PolicyOff {
		out = append(out, workCandidate{kind: KindQueueFix, marker: fm})
	}
	if pr.CIState == "failing" && p.CIFix != config.PolicyOff {
		out = append(out, workCandidate{kind: KindCIFix})
	}
	if p.NitReply != config.PolicyOff {
		if nits := MatchNitThreads(pr, p.NitReplyBots, p.NitReplyPatterns); len(nits) > 0 {
			out = append(out, workCandidate{kind: KindNitReply, nitThreads: nits})
		}
	}
	class := ClassifyConflict(pr)
	if class != ClassUpToDate {
		if p.Rebase != config.PolicyOff {
			out = append(out, workCandidate{kind: KindRebase, conflictClass: class})
		}
		if p.CatchUp != config.PolicyOff {
			out = append(out, workCandidate{kind: KindCatchUp, conflictClass: class})
		}
	}
	return out
}

func effectiveFailureMarker(pr scan.PR) *scan.FailureMarker {
	if pr.FailureMarker != nil {
		return pr.FailureMarker
	}
	for _, a := range pr.Actions {
		if a.Name == scan.ActionMergeQueueFix {
			return &scan.FailureMarker{
				Kind:       scan.FailureKindOurCode,
				HeadSHA:    pr.HeadSHA,
				QueueRunID: a.EventID,
				CommentURL: a.CommentURL,
			}
		}
	}
	return nil
}

func (e *Engine) evaluateCandidate(
	ctx context.Context,
	cfg config.Config,
	pr scan.PR,
	cand workCandidate,
	opts RunOptions,
	now time.Time,
	todayCount int,
	dispatchedInRun int,
) (Decision, bool, error) {
	prID := fmt.Sprintf("%s#%d", pr.Repo, pr.Number)
	mode := cfg.Policy.ModeFor(cand.kind)
	dec := Decision{
		PR:            prID,
		Repo:          pr.Repo,
		Number:        pr.Number,
		HeadSHA:       pr.HeadSHA,
		Kind:          cand.kind,
		Mode:          mode,
		ConflictClass: cand.conflictClass,
		Marker:        cand.marker,
	}

	if stopped := checkPreGuards(cfg.Policy, pr, cand, &dec); stopped {
		return dec, false, nil
	}
	if stopped, err := e.checkLedgerAndCaps(cfg.Policy, prID, pr.HeadSHA, cand.kind, opts.Force, todayCount, dispatchedInRun, &dec); err != nil || stopped {
		return dec, false, err
	}
	session, stopped, err := e.checkSessionAndGuardCmd(ctx, cfg, pr, cand.kind, prID, &dec)
	if err != nil || stopped {
		return dec, false, err
	}

	dec.Prompt = renderPolicyPrompt(cfg, pr, cand)
	if mode == config.PolicyDryRun {
		dec.Decision = DecisionDryRun
		dec.Outcome = ledger.OutcomeDryRun
		return dec, false, nil
	}
	return e.executeAutoDispatch(ctx, cfg, pr, cand, session, now, dec)
}

func checkPreGuards(p config.PolicyConfig, pr scan.PR, cand workCandidate, dec *Decision) bool {
	if dec.Mode == config.PolicyOff {
		dec.Decision = DecisionSkip
		dec.Reason = "mode_off"
		return true
	}
	if checkMarkerDecision(pr, cand, dec) {
		return true
	}
	if (cand.kind == KindRebase || cand.kind == KindCatchUp) &&
		cand.conflictClass == ClassConflicting && !p.DispatchConflicting {
		dec.Decision = DecisionReport
		dec.Reason = "conflicting"
		return true
	}
	return checkStaticPRGuards(p, pr, dec)
}

func checkMarkerDecision(pr scan.PR, cand workCandidate, dec *Decision) bool {
	if cand.kind != KindQueueFix || cand.marker == nil {
		return false
	}
	switch cand.marker.Kind {
	case scan.FailureKindInfraFlake:
		dec.Decision = DecisionSkip
		dec.Reason = "infra_flake"
		return true
	case scan.FailureKindUnknown:
		dec.Decision = DecisionReport
		dec.Reason = "unknown_failure_marker"
		return true
	case scan.FailureKindOurCode:
		if cand.marker.HeadSHA != "" && pr.HeadSHA != "" && cand.marker.HeadSHA != pr.HeadSHA {
			dec.Decision = DecisionSkip
			dec.Reason = "head_changed"
			return true
		}
	}
	return false
}

func checkStaticPRGuards(p config.PolicyConfig, pr scan.PR, dec *Decision) bool {
	if p.SkipDrafts && pr.IsDraft {
		dec.Decision = DecisionSkip
		dec.Reason = "draft"
		return true
	}
	if held := hasHoldLabel(pr.Labels, p.HoldLabels); held != "" {
		dec.Decision = DecisionSkip
		dec.Reason = "held_label:" + held
		return true
	}
	if p.SkipStacked && isStackedPR(pr, p.DefaultBranches) {
		dec.Decision = DecisionSkip
		dec.Reason = "stacked_pr"
		return true
	}
	return false
}

func (e *Engine) checkLedgerAndCaps(p config.PolicyConfig, prID, headSHA, kind string, force bool, todayCount, dispatchedInRun int, dec *Decision) (bool, error) {
	if !force && e.Ledger != nil {
		already, err := e.Ledger.HasDispatched(prID, headSHA, kind)
		if err != nil {
			return false, fmt.Errorf("check ledger: %w", err)
		}
		if already {
			dec.Decision = DecisionSkip
			dec.Reason = "already_dispatched"
			return true, nil
		}
	}
	if p.DailyCap > 0 && todayCount >= p.DailyCap {
		dec.Decision = DecisionSkip
		dec.Reason = "daily_cap"
		return true, nil
	}
	if p.MaxPerRun > 0 && dispatchedInRun >= p.MaxPerRun {
		dec.Decision = DecisionSkip
		dec.Reason = "max_per_run"
		return true, nil
	}
	return false, nil
}

func (e *Engine) checkSessionAndGuardCmd(ctx context.Context, cfg config.Config, pr scan.PR, kind, prID string, dec *Decision) (*backend.Session, bool, error) {
	if e.Backend == nil {
		dec.Decision = DecisionSkip
		dec.Reason = "no_backend"
		return nil, true, nil
	}
	session, err := e.Backend.FindSession(ctx, pr, cfg.TabLabelTemplate)
	if err != nil {
		return nil, false, fmt.Errorf("find session for %s: %w", prID, err)
	}
	if session == nil {
		dec.Decision = DecisionSkip
		dec.Reason = "no_session"
		return nil, true, nil
	}
	dec.SessionID = session.ID
	if cfg.Policy.SkipOtherMachine && session.Machine != "" && session.Machine != config.LocalMachine {
		dec.Decision = DecisionSkip
		dec.Reason = "other_machine:" + session.Machine
		return nil, true, nil
	}
	if stopped, err := e.checkIdle(ctx, cfg.Policy, session.ID, dec); err != nil || stopped {
		return nil, stopped, err
	}
	if stopped, err := e.checkGuardCommand(ctx, cfg.Policy, prID, pr.HeadSHA, kind, dec); err != nil || stopped {
		return nil, stopped, err
	}
	return session, false, nil
}

func (e *Engine) checkIdle(ctx context.Context, p config.PolicyConfig, sessionID string, dec *Decision) (bool, error) {
	if !p.RequireIdle {
		return false, nil
	}
	st, err := e.Backend.ReadIdleState(ctx, sessionID)
	if err != nil {
		return false, fmt.Errorf("read idle state for %s: %w", sessionID, err)
	}
	if st.HasDialog {
		dec.Decision = DecisionSkip
		dec.Reason = "open_dialog"
		return true, nil
	}
	if !st.Idle {
		dec.Decision = DecisionSkip
		dec.Reason = "tab_not_idle:" + st.Status
		return true, nil
	}
	return false, nil
}

func (e *Engine) checkGuardCommand(ctx context.Context, p config.PolicyConfig, prID, headSHA, kind string, dec *Decision) (bool, error) {
	if p.GuardCommand == "" || e.Guard == nil {
		return false, nil
	}
	allowed, detail, err := e.Guard.Check(ctx, p.GuardCommand, GuardContext{
		PR:      prID,
		HeadSHA: headSHA,
		Kind:    kind,
	})
	if err != nil {
		return false, fmt.Errorf("guard command: %w", err)
	}
	if !allowed {
		dec.Decision = DecisionSkip
		dec.Reason = "guard_command_failed"
		dec.Detail = detail
		return true, nil
	}
	return false, nil
}

func (e *Engine) executeAutoDispatch(ctx context.Context, cfg config.Config, pr scan.PR, cand workCandidate, session *backend.Session, now time.Time, dec Decision) (Decision, bool, error) {
	lp := e.leaseProvider()
	leaseName := cfg.Policy.LeaseName
	if leaseName == "" {
		leaseName = pr.Repo
	}
	claimed, stopped, err := claimDispatchLease(ctx, lp, leaseName, cand.kind, cfg.Policy.LeaseTimeout, &dec)
	if err != nil || stopped {
		return dec, false, err
	}
	if claimed {
		defer func() {
			_ = lp.Release(ctx, leaseName)
		}()
	}

	sendRes := e.Backend.SendPrompt(ctx, session.ID, dec.Prompt, cfg.DispatchTimeout)
	outcome := mapSendOutcome(sendRes.Status)
	dec.Outcome = outcome
	dec.Detail = sendRes.Detail
	if sendRes.Status == backend.SendError {
		dec.Decision = DecisionError
		dec.Reason = "send_failed"
	} else {
		dec.Decision = DecisionDispatch
	}

	if e.Ledger != nil {
		if err := e.Ledger.Append(ledger.Entry{
			PR:      dec.PR,
			HeadSHA: pr.HeadSHA,
			Kind:    cand.kind,
			Time:    now,
			Outcome: outcome,
			Detail:  sendRes.Detail,
		}); err != nil {
			return dec, false, fmt.Errorf("append ledger: %w", err)
		}
	}
	return dec, sendRes.Status != backend.SendError, nil
}

func claimDispatchLease(ctx context.Context, lp LeaseProvider, name, kind string, timeout time.Duration, dec *Decision) (bool, bool, error) {
	if !TouchesWorkingCopy(kind) || lp.Name() == config.LeaseOff {
		return false, false, nil
	}
	res, err := lp.Claim(ctx, name, timeout)
	if err != nil {
		return false, false, fmt.Errorf("claim lease %q: %w", name, err)
	}
	switch res.Status {
	case lease.ClaimQueued:
		dec.Decision = DecisionSkip
		dec.Reason = "lease_queued"
		dec.Detail = res.Reason
		return false, true, nil
	case lease.ClaimRefused:
		dec.Decision = DecisionSkip
		dec.Reason = "lease_refused"
		dec.Detail = res.Reason
		if dec.Detail == "" && res.Holder != "" {
			dec.Detail = "held by " + res.Holder
		}
		return false, true, nil
	default:
		return true, false, nil
	}
}

func mapSendOutcome(status string) string {
	switch status {
	case backend.SendDone:
		return ledger.OutcomeDispatched
	case backend.SendTimeout, backend.SendStalled:
		return ledger.OutcomeTimeout
	case backend.SendBlocked:
		return ledger.OutcomeBlocked
	default:
		return ledger.OutcomeFailed
	}
}

func renderPolicyPrompt(cfg config.Config, pr scan.PR, cand workCandidate) string {
	var body string
	switch cand.kind {
	case KindRebase:
		body = dispatch.Render(cfg.RebasePromptTemplate, pr, cfg)
	case KindCatchUp:
		if cfg.DispatchCatchup == "merge" {
			body = dispatch.Render(cfg.MergePromptTemplate, pr, cfg)
		} else {
			body = dispatch.Render(cfg.RebasePromptTemplate, pr, cfg)
		}
	case KindCIFix:
		body = dispatch.RenderHint(cfg.CIFixPromptTemplate, pr, cfg, "")
	case KindNitReply:
		nitPR := pr
		nitPR.BlockingComments = cand.nitThreads
		body = dispatch.Render(cfg.Policy.NitReplyPromptTemplate, nitPR, cfg)
	case KindQueueFix:
		commentURL := ""
		if cand.marker != nil {
			commentURL = cand.marker.CommentURL
		}
		tmpl := fmt.Sprintf("The merge queue ejected PR #{number} ({url}). State: {state}.\nRead the failure marker comment and fix the cause:\n%s\n\nDo not touch other PRs.", commentURL)
		body = dispatch.Render(tmpl, pr, cfg)
	default:
		body = dispatch.Render(cfg.PromptTemplate, pr, cfg)
	}
	if strings.TrimSpace(cfg.PromptSuffix) != "" {
		return body + "\n\n" + cfg.PromptSuffix
	}
	return body
}
