package dispatch

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var prFlagPattern = regexp.MustCompile(`^([^/#]+/[^/#]+)#(\d+)$`)

// StateStore loads and saves dedupe state. Dry-run loads but never writes.
type StateStore interface {
	Load() (State, error)
	Save(State) error
}

// locker is implemented by FileStore for exclusive live-path writes.
type locker interface {
	WithLock(func() error) error
}

// ErrFailed is returned when a live prompt is PromptError (exit 3).
var ErrFailed = errors.New("dispatch failed")

// ErrSettleTimeout is returned when the per-send herdr wait expires.
var ErrSettleTimeout = errors.New("settle timeout")

// Request is the candidate-set input to Run.
type Request struct {
	Doc        scan.Document
	PRs        []string
	RunnerPane string
	Rebase     bool
	CIFix      bool
	Hint       string
	Force      bool
	// Remote returns the herdr client for a tab on another machine. The
	// prompt, wait, and settle polls for such a tab run there. Nil cannot
	// send to a remote tab.
	Remote func(machine string) Herdr
	// Waker wakes a hibernated agent before its prompt
	// (wake_before_prompt). Nil never wakes.
	Waker Waker
}

// Candidate is one PR considered for dispatch.
type Candidate struct {
	Repo   string
	Number int
	PR     *scan.PR
}

// ParsePR parses an owner/repo#N flag value.
func ParsePR(s string) (string, int, error) {
	m := prFlagPattern.FindStringSubmatch(s)
	if m == nil {
		return "", 0, fmt.Errorf("invalid --pr %q", s)
	}
	n, err := strconv.Atoi(m[2])
	if err != nil {
		return "", 0, fmt.Errorf("invalid --pr %q: %w", s, err)
	}
	return m[1], n, nil
}

// Candidates builds the sorted candidate set from a scan document and --pr flags.
// An empty PRs list means every PR in the document. A --pr absent from the
// document is still returned (PR == nil) so it is never silently dropped.
func Candidates(doc scan.Document, prs []string) ([]Candidate, error) {
	byKey := make(map[string]scan.PR, len(doc.PRs))
	for _, pr := range doc.PRs {
		byKey[prKey(pr.Repo, pr.Number)] = pr
	}
	var out []Candidate
	if len(prs) == 0 {
		out = make([]Candidate, 0, len(doc.PRs))
		for i := range doc.PRs {
			pr := doc.PRs[i]
			out = append(out, Candidate{Repo: pr.Repo, Number: pr.Number, PR: &pr})
		}
	} else {
		out = make([]Candidate, 0, len(prs))
		for _, raw := range prs {
			repo, n, err := ParsePR(raw)
			if err != nil {
				return nil, err
			}
			c := Candidate{Repo: repo, Number: n}
			if pr, ok := byKey[prKey(repo, n)]; ok {
				p := pr
				c.PR = &p
			}
			out = append(out, c)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Repo != out[j].Repo {
			return out[i].Repo < out[j].Repo
		}
		return out[i].Number < out[j].Number
	})
	return out, nil
}

// Evaluate returns the skip action for a candidate, or a zero Action if eligible.
// Rebase, CI-fix, and a scan merge-queue-fix action skip the unaddressed-comment
// gate. --rebase and --ci-fix keep their own prompts when that action is also
// present. A rebase is deduped only when dispatched_head_sha equals the current
// head and the PR is not behind, dirty, unknown, or mergeable CONFLICTING. A
// CI-fix is deduped only when dispatched_ci_fix_sha equals the current head and
// CI is not failing. A merge-queue-fix is deduped when
// dispatched_merge_queue_fix_sha equals the current head, and is not prompted
// again until the head changes. A skipped rebase or CI-fix sets Detail to the
// recorded SHA and status. A skipped merge-queue-fix sets Detail to the
// recorded SHA. Comment dedupe holds while the same unaddressed ids are
// inside dedupe_ttl and, when dedupe_scans is non-zero, under that many
// later evaluations. That hold is dispatched_unresolved, does not send, and
// sets Detail to the recorded time. force skips comment, rebase, CI-fix, and
// merge-queue-fix dedupe. A PR out of the run's tab_machine scope is
// skipped_out_of_scope before any other check.
func Evaluate(c Candidate, cfg config.Config, st State, rebase, ciFix, force bool, now time.Time) Item {
	var item Item
	if c.PR != nil && c.PR.OutOfScope != "" {
		item = Item{Action: ActionSkippedOutOfScope, Detail: c.PR.OutOfScope}
	} else {
		item = skipReason(c, cfg, st, rebase, ciFix, force, now)
	}
	item.Repo = c.Repo
	item.Number = c.Number
	return item
}

func skipReason(c Candidate, cfg config.Config, st State, rebase, ciFix, force bool, now time.Time) Item {
	if c.PR == nil {
		return Item{Action: ActionSkippedNotFound}
	}
	pr := *c.PR
	key := prKey(c.Repo, c.Number)
	queueFix := mergeQueueFixMode(pr, rebase, ciFix)
	switch {
	case !rebase && !ciFix && !queueFix && !pr.Unaddressed:
		return Item{Action: ActionSkippedAddressed}
	case pr.IsDraft && !cfg.IncludeDrafts:
		return Item{Action: ActionSkippedDraft}
	case pr.Tab == nil && pr.TabUnknown != "":
		return Item{Action: ActionSkippedTabUnknown, Detail: "unknown tab (" + pr.TabUnknown + ")"}
	case pr.Tab == nil:
		return Item{Action: ActionSkippedNoTab}
	case pr.Tab.PaneID == nil:
		return Item{Action: ActionSkippedNoAgent}
	case !readyStatus(pr.Tab.AgentStatus):
		return Item{Action: ActionSkippedBusy}
	default:
		if action, detail := dedupeAction(st, key, pr, rebase, ciFix, force, now, cfg); action != "" {
			return Item{Action: action, Detail: detail}
		}
		return Item{}
	}
}

// commentWindowOpen reports whether these unaddressed ids are still inside
// the dedupe window. A dedupe_scans of 0 does not expire by scan count.
// The returned time is dispatched_at.
func commentWindowOpen(st State, key string, commentIDs []string, now time.Time, ttl time.Duration, scanLimit int) (bool, string) {
	if !st.Deduped(key, commentIDs) {
		return false, ""
	}
	entry := st[key]
	at, err := time.Parse(time.RFC3339, entry.DispatchedAt)
	if err != nil {
		return false, ""
	}
	if !now.Before(at.Add(ttl)) {
		return false, ""
	}
	if scanLimit > 0 && entry.DedupeScanCount >= scanLimit {
		return false, ""
	}
	return true, entry.DispatchedAt
}

// MarkUnresolved sets Dispatch to dispatched_unresolved on pull requests
// whose unaddressed comment ids are still inside the dedupe window.
func MarkUnresolved(doc *scan.Document, st State, cfg config.Config, now time.Time) {
	if doc == nil {
		return
	}
	for i := range doc.PRs {
		pr := &doc.PRs[i]
		if !pr.Unaddressed {
			continue
		}
		open, _ := commentWindowOpen(st, prKey(pr.Repo, pr.Number), commentIDs(*pr), now, cfg.DedupeTTL, cfg.DedupeScans)
		if !open {
			continue
		}
		pr.Dispatch = ActionDispatchedUnresolved
	}
}

func dedupeDetail(st State, key string, pr scan.PR, rebase, ciFix bool) string {
	if st == nil {
		return ""
	}
	entry := st[key]
	switch {
	case rebase:
		return fmt.Sprintf("recorded at %s, merge_state=%s", entry.DispatchedHeadSHA, pr.MergeStateStatus)
	case ciFix:
		return fmt.Sprintf("recorded at %s, ci_state=%s", entry.DispatchedCIFixSHA, pr.CIState)
	}
	if mergeQueueFixMode(pr, rebase, ciFix) {
		return fmt.Sprintf("recorded at %s", entry.DispatchedMergeQueueFixSHA)
	}
	return ""
}

// mergeQueueAction returns the scan merge-queue-fix action, if the PR has one.
func mergeQueueAction(pr scan.PR) (scan.Action, bool) {
	for _, action := range pr.Actions {
		if action.Name == scan.ActionMergeQueueFix {
			return action, true
		}
	}
	return scan.Action{}, false
}

// mergeQueueFixMode reports whether this candidate is dispatched as a
// merge-queue-fix. Explicit rebase and CI-fix modes keep their own prompts.
func mergeQueueFixMode(pr scan.PR, rebase, ciFix bool) bool {
	if rebase || ciFix {
		return false
	}
	_, ok := mergeQueueAction(pr)
	return ok
}

func dedupeAction(st State, key string, pr scan.PR, rebase, ciFix, force bool, now time.Time, cfg config.Config) (string, string) {
	if force {
		return "", ""
	}
	switch {
	case rebase:
		if rebaseDeduped(st, key, pr) {
			return ActionSkippedDeduped, dedupeDetail(st, key, pr, rebase, ciFix)
		}
		return "", ""
	case ciFix:
		if ciFixDeduped(st, key, pr) {
			return ActionSkippedDeduped, dedupeDetail(st, key, pr, rebase, ciFix)
		}
		return "", ""
	}
	if mergeQueueFixMode(pr, rebase, ciFix) {
		if st.DedupedMergeQueueFix(key, pr.HeadSHA) {
			return ActionSkippedDeduped, dedupeDetail(st, key, pr, rebase, ciFix)
		}
		return "", ""
	}
	if open, at := commentWindowOpen(st, key, commentIDs(pr), now, cfg.DedupeTTL, cfg.DedupeScans); open {
		return ActionDispatchedUnresolved, fmt.Sprintf("recorded at %s", at)
	}
	return "", ""
}

// rebaseDeduped reports whether a prior rebase dispatch on this head is
// already satisfied. BEHIND, DIRTY, UNKNOWN, and mergeable CONFLICTING always
// retry. Any other merge state, including empty, is satisfied only when
// dispatched_head_sha equals the current head.
func rebaseDeduped(st State, key string, pr scan.PR) bool {
	if rebaseIncomplete(pr) {
		return false
	}
	return st.DedupedHead(key, pr.HeadSHA)
}

func rebaseIncomplete(pr scan.PR) bool {
	switch pr.MergeStateStatus {
	case "BEHIND", "DIRTY", "UNKNOWN":
		return true
	default:
		return pr.Mergeable == "CONFLICTING"
	}
}

// ciFixDeduped reports whether a prior CI-fix dispatch on this head is
// already satisfied. failing always retries. Any other CI state, including
// empty, is satisfied only when dispatched_ci_fix_sha equals the current head.
func ciFixDeduped(st State, key string, pr scan.PR) bool {
	if ciFixIncomplete(pr.CIState) {
		return false
	}
	return st.DedupedCIFix(key, pr.HeadSHA)
}

func ciFixIncomplete(status string) bool {
	return status == "failing"
}

// Run evaluates the candidate set. Dry-run does a one-shot gate.Check, never
// polls, never emits queued, and never writes state. Live send polls the gate
// one PR at a time (empty busy set must hold for settleDebouncePolls samples),
// then blocks on herdr agent prompt --wait, debounce-confirms idle/done, and
// agent-waits leftover time if the tab resumes working.
// It writes state on dispatched / dispatched_timeout, and returns ErrTimeout,
// ErrDirtyTree, ErrSettleTimeout, or ErrFailed after emitting partial
// results. A dirty dispatch checkout emits gate_unsafe for the current PR,
// without polling, and queued for the rest. A gate
// timeout emits gate_timeout for the current PR (naming the tab being
// waited on) and queued for the rest. A blocked settlement does not write
// dedupe state and stops the batch so a re-run can retry the same comment
// after the user answers.
func Run(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, now time.Time) (Document, error) {
	doc := Document{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		DryRun:      cfg.DryRun,
		Results:     []Item{},
	}
	cands, err := Candidates(req.Doc, req.PRs)
	if err != nil {
		return doc, err
	}
	if cfg.DryRun {
		return runDry(ctx, h, store, cfg, req, cands, doc, now)
	}
	return runLive(ctx, h, store, cfg, req, now, cands, doc)
}

func runDry(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, cands []Candidate, doc Document, now time.Time) (Document, error) {
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	for _, c := range cands {
		doc.Results = append(doc.Results, evaluateDry(c, cfg, st, req, now))
	}
	if err := annotateGate(ctx, h, cfg, req, &doc); err != nil {
		return doc, err
	}
	for _, item := range doc.Results {
		logResult(ctx, item)
	}
	return doc, nil
}

func runLive(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, now time.Time, cands []Candidate, doc Document) (Document, error) {
	if l, ok := store.(locker); ok {
		var liveErr error
		lockErr := l.WithLock(func() error {
			doc, liveErr = dispatchLive(ctx, h, store, cfg, req, now, cands, doc)
			return nil
		})
		if lockErr != nil {
			return doc, fmt.Errorf("lock state: %w", lockErr)
		}
		return doc, liveErr
	}
	return dispatchLive(ctx, h, store, cfg, req, now, cands, doc)
}

func dispatchLive(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, now time.Time, cands []Candidate, doc Document) (Document, error) {
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	matched := MatchedTabs(req.Doc)
	ctx = WithRoundStart(ctx, now)
	clock := Clock(realClock{})
	sleeper := Sleeper(realSleeper{})
	timedOut := false
	for i, c := range cands {
		var step liveStep
		doc, step = dispatchCandidate(ctx, h, store, cfg, req, now, st, matched, clock, sleeper, cands, i, c, doc)
		timedOut = timedOut || step.timedOut
		if step.done {
			return doc, step.err
		}
	}
	if timedOut {
		return doc, ErrSettleTimeout
	}
	return doc, nil
}

// liveStep is the control result of one candidate. done stops the batch.
type liveStep struct {
	done     bool
	err      error
	timedOut bool
}

func dispatchCandidate(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, now time.Time, st State, matched map[string]struct{}, clock Clock, sleeper Sleeper, cands []Candidate, i int, c Candidate, doc Document) (Document, liveStep) {
	if err := ctx.Err(); err != nil {
		doc.Results = append(doc.Results, failItem(c, err))
		return doc, liveStep{done: true, err: fmt.Errorf("dispatch: %w", err)}
	}
	item := Evaluate(c, cfg, st, req.Rebase, req.CIFix, req.Force, now)
	if item.Action != "" {
		logResult(ctx, item)
		doc.Results = append(doc.Results, item)
		if item.Action == ActionDispatchedUnresolved {
			st.NoteUnresolvedScan(prKey(c.Repo, c.Number))
			if err := saveEntry(store, st, prKey(c.Repo, c.Number)); err != nil {
				return doc, liveStep{done: true, err: err}
			}
		}
		return doc, liveStep{}
	}
	res, err := Wait(ctx, h, gateConfig(cfg, c.Repo), req.RunnerPane, matched, clock, sleeper)
	if errors.Is(err, ErrTimeout) {
		item = gateTimeoutItem(c, res.Busy, req.Doc)
		logResult(ctx, item)
		doc.Results = append(doc.Results, item)
		queueRest(ctx, &doc, cands, i+1)
		return doc, liveStep{done: true, err: fmt.Errorf("%w: %s", ErrTimeout, item.Detail)}
	}
	if errors.Is(err, ErrDirtyTree) {
		item = Item{Repo: c.Repo, Number: c.Number, Action: ActionGateUnsafe, Detail: res.Reason}
		logResult(ctx, item)
		doc.Results = append(doc.Results, item)
		queueRest(ctx, &doc, cands, i+1)
		return doc, liveStep{done: true, err: fmt.Errorf("%w: %s", ErrDirtyTree, res.Reason)}
	}
	if err != nil {
		failed := failItem(c, err)
		logResult(ctx, failed)
		doc.Results = append(doc.Results, failed)
		return doc, liveStep{done: true, err: err}
	}
	sendArgs := []any{"repo", c.Repo, "number", c.Number, "pane_id", *c.PR.Tab.PaneID}
	if c.PR.Tab.Machine != "" {
		sendArgs = append(sendArgs, "machine", c.PR.Tab.Machine)
	}
	runlog.FromContext(ctx).Info("dispatch_send", sendArgs...)
	item = sendPrompt(ctx, h, cfg, c, req, clock, sleeper)
	logResult(ctx, item)
	doc.Results = append(doc.Results, item)
	if item.Action == ActionDispatched || item.Action == ActionDispatchedTimeout {
		recordDispatch(st, c, req, now)
		if err := saveEntry(store, st, prKey(c.Repo, c.Number)); err != nil {
			return doc, liveStep{done: true, err: err}
		}
	}
	step := liveStep{timedOut: item.Action == ActionDispatchedTimeout}
	if item.Action == ActionDispatchedBlocked {
		queueRest(ctx, &doc, cands, i+1)
		return doc, liveStep{done: true}
	}
	if item.Action == ActionFailed {
		if err := ctx.Err(); err != nil {
			return doc, liveStep{done: true, err: fmt.Errorf("dispatch: %w", err)}
		}
		return doc, liveStep{done: true, err: fmt.Errorf("%w: %s", ErrFailed, item.Detail)}
	}
	return doc, step
}

// tabHerdr is the herdr client for tab's machine: h for the local server,
// or req.Remote for another machine.
func tabHerdr(h Herdr, req Request, tab scan.Tab) (Herdr, error) {
	if tab.Machine == "" {
		return h, nil
	}
	if req.Remote == nil {
		return nil, fmt.Errorf("no herdr client for machine %s", tab.Machine)
	}
	return req.Remote(tab.Machine), nil
}

// gateConfig resolves the checkout of repo for the gate. The dirty-tree
// check uses it in every concurrency_cwd mode.
func gateConfig(cfg config.Config, repo string) config.Config {
	if cfg.Checkout == "" {
		cfg.Checkout = checkoutOfRepo(repo)
	}
	return cfg
}

func recordDispatch(st State, c Candidate, req Request, now time.Time) {
	key := prKey(c.Repo, c.Number)
	switch {
	case req.CIFix:
		st.RecordCIFix(key, c.PR.HeadSHA, now)
	case req.Rebase:
		st.RecordHead(key, c.PR.HeadSHA, now)
	case mergeQueueFixMode(*c.PR, req.Rebase, req.CIFix):
		st.RecordMergeQueueFix(key, c.PR.HeadSHA, now)
	default:
		st.Record(key, commentIDs(*c.PR), now)
	}
}

func sendPrompt(ctx context.Context, h Herdr, cfg config.Config, c Candidate, req Request, clock Clock, sleeper Sleeper) Item {
	pr := *c.PR
	pane := *pr.Tab.PaneID
	rendered := renderPrompt(cfg, req, pr)
	item := Item{Repo: c.Repo, Number: c.Number, Machine: pr.Tab.Machine}
	h, err := tabHerdr(h, req, *pr.Tab)
	if err != nil {
		item.Action = ActionFailed
		item.Detail = err.Error()
		return item
	}
	woke, err := wakeBeforePrompt(ctx, req, pr.Tab.Machine, pane)
	if err != nil {
		item.Action = ActionFailed
		item.PaneID = pane
		item.Detail = err.Error()
		return item
	}
	if woke {
		item.Woke = true
		runlog.FromContext(ctx).Info("wake", "pane_id", pane, "machine", pr.Tab.Machine)
	}
	deadline := clock.Now().Add(cfg.DispatchTimeout)
	out := h.Prompt(ctx, pane, rendered, cfg.WaitUntil, cfg.DispatchTimeout)
	return finishAgentOutcome(ctx, h, cfg, clock, sleeper, item, pane, rendered, out, deadline)
}

func finishAgentOutcome(ctx context.Context, h Herdr, cfg config.Config, clock Clock, sleeper Sleeper, item Item, pane, rendered string, out herdr.PromptOutcome, deadline time.Time) Item {
	for {
		switch out.Status {
		case herdr.PromptStalled:
			runlog.FromContext(ctx).Info("settle",
				"pane_id", pane,
				"decision", ActionDispatchedTimeout,
				"reason", "herdr prompt stalled",
			)
			return timeoutItem(item, pane, rendered)
		case herdr.PromptMatched, herdr.PromptTimeout:
			snap, stable, err := confirmSettled(ctx, h, cfg, clock, sleeper, pane, deadline)
			if err != nil {
				item.Action = ActionFailed
				item.Detail = err.Error()
				return item
			}
			if stable {
				return dispatchedFromAgent(ctx, item, pane, rendered, snap)
			}
			remaining := deadline.Sub(clock.Now())
			if remaining <= 0 || !holdsTurn(snap.AgentStatus) {
				runlog.FromContext(ctx).Info("settle",
					"pane_id", pane,
					"decision", ActionDispatchedTimeout,
					"agent_status", snap.AgentStatus,
					"reason", "settle not stable",
				)
				return timeoutItem(item, pane, rendered)
			}
			out = h.Wait(ctx, pane, cfg.WaitUntil, remaining)
			if out.Status == herdr.PromptTimeout {
				snap, stable, err = confirmSettled(ctx, h, cfg, clock, sleeper, pane, deadline)
				if err != nil {
					item.Action = ActionFailed
					item.Detail = err.Error()
					return item
				}
				if stable {
					return dispatchedFromAgent(ctx, item, pane, rendered, snap)
				}
				runlog.FromContext(ctx).Info("settle",
					"pane_id", pane,
					"decision", ActionDispatchedTimeout,
					"agent_status", snap.AgentStatus,
					"reason", "herdr wait timeout",
				)
				return timeoutItem(item, pane, rendered)
			}
		default:
			item.Action = ActionFailed
			if out.Err != nil {
				item.Detail = out.Err.Error()
			} else {
				item.Detail = "herdr prompt failed"
			}
			return item
		}
	}
}

// confirmSettled polls agent list until a terminal status holds for
// settleDebouncePolls samples. A single idle/done from herdr --wait is
// not completion: idle between answering a blocked question and the
// agent resuming is transient.
func confirmSettled(ctx context.Context, h Herdr, cfg config.Config, clock Clock, sleeper Sleeper, pane string, deadline time.Time) (herdr.Agent, bool, error) {
	held := 0
	var last herdr.Agent
	for {
		snap, err := snapshotPane(ctx, h, pane)
		if err != nil {
			return herdr.Agent{}, false, err
		}
		last = snap
		if settleTerminal(snap.AgentStatus, cfg.WaitUntil) {
			held++
			if held >= settleDebouncePolls {
				return snap, true, nil
			}
		} else {
			return snap, false, nil
		}
		if deadline.Sub(clock.Now()) <= 0 {
			return last, false, nil
		}
		if err := sleeper.Sleep(ctx, cfg.GatePoll); err != nil {
			return last, false, fmt.Errorf("settle sleep: %w", err)
		}
	}
}

func settleTerminal(status string, until []string) bool {
	if readyStatus(status) {
		return true
	}
	return status == "blocked" && matchesUntil("blocked", until)
}

func dispatchedFromAgent(ctx context.Context, item Item, pane, rendered string, agent herdr.Agent) Item {
	status := agent.AgentStatus
	if status == "" {
		status = "idle"
	}
	item.Action = ActionDispatched
	if status == "blocked" {
		item.Action = ActionDispatchedBlocked
	}
	item.PaneID = pane
	item.RenderedPrompt = rendered
	runlog.FromContext(ctx).Info("settle",
		"pane_id", pane,
		"decision", item.Action,
		"agent_status", status,
	)
	return item
}

func timeoutItem(item Item, pane, rendered string) Item {
	item.Action = ActionDispatchedTimeout
	item.PaneID = pane
	item.RenderedPrompt = rendered
	return item
}

func holdsTurn(status string) bool {
	return status == "working" || status == "blocked"
}

func snapshotPane(ctx context.Context, h Herdr, paneID string) (herdr.Agent, error) {
	agents, err := h.AgentList(ctx)
	if err != nil {
		return herdr.Agent{}, fmt.Errorf("baseline agent list: %w", err)
	}
	if a, ok := findAgent(agents, paneID); ok {
		return a, nil
	}
	return herdr.Agent{PaneID: paneID}, nil
}

func gateTimeoutItem(c Candidate, busy []Busy, doc scan.Document) Item {
	return Item{
		Repo:   c.Repo,
		Number: c.Number,
		Action: ActionGateTimeout,
		Detail: gateTimeoutDetail(busy, tabLabels(doc), c),
	}
}

func tabLabels(doc scan.Document) map[string]string {
	out := make(map[string]string)
	for _, pr := range doc.PRs {
		if pr.Tab != nil && pr.Tab.TabID != "" && pr.Tab.Label != "" {
			out[scan.TabKey(pr.Tab.Machine, pr.Tab.TabID)] = pr.Tab.Label
		}
	}
	return out
}

func gateTimeoutDetail(busy []Busy, labels map[string]string, c Candidate) string {
	target := prKey(c.Repo, c.Number)
	if len(busy) == 0 {
		return fmt.Sprintf("gate timeout; not dispatching %s", target)
	}
	b := busy[0]
	name := holdName(b)
	if label := labels[scan.TabKey(b.Machine, b.TabID)]; label != "" {
		name = fmt.Sprintf("%s [%s]", name, label)
	}
	if extraSignal(b) {
		return fmt.Sprintf("waiting on %s — %s %s; not dispatching %s until it clears", name, b.Signal, b.Detail, target)
	}
	if b.Status == "blocked" {
		return fmt.Sprintf("waiting on %s — blocked awaiting your input; not dispatching %s until it clears", name, target)
	}
	if b.Status != "" {
		return fmt.Sprintf("waiting on %s — %s; not dispatching %s until it clears", name, b.Status, target)
	}
	return fmt.Sprintf("waiting on %s; not dispatching %s until it clears", name, target)
}

func queueRest(ctx context.Context, doc *Document, cands []Candidate, from int) {
	for _, c := range cands[from:] {
		item := Item{
			Repo:   c.Repo,
			Number: c.Number,
			Action: ActionQueued,
		}
		logResult(ctx, item)
		doc.Results = append(doc.Results, item)
	}
}

func failItem(c Candidate, err error) Item {
	item := Item{Repo: c.Repo, Number: c.Number, Action: ActionFailed}
	if err != nil {
		item.Detail = err.Error()
	}
	return item
}

func logResult(ctx context.Context, item Item) {
	args := []any{"repo", item.Repo, "number", item.Number, "action", item.Action}
	if item.PaneID != "" {
		args = append(args, "pane_id", item.PaneID)
	}
	if item.Machine != "" {
		args = append(args, "machine", item.Machine)
	}
	if item.Detail != "" {
		args = append(args, "detail", item.Detail)
	}
	if item.Woke {
		args = append(args, "woke", true)
	}
	runlog.FromContext(ctx).Info("result", args...)
}

type realClock struct{}

func (realClock) Now() time.Time { return time.Now() }

type realSleeper struct{}

func (realSleeper) Sleep(ctx context.Context, d time.Duration) error {
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("sleep: %w", ctx.Err())
	case <-timer.C:
		return nil
	}
}

// entryMerger saves one entry over the store's current state, so runs
// that do not share a run lock keep each other's entries.
type entryMerger interface {
	MergeEntry(key string, entry Entry) error
}

// saveEntry saves st after a change to key alone.
func saveEntry(store StateStore, st State, key string) error {
	if m, ok := store.(entryMerger); ok {
		if err := m.MergeEntry(key, st[key]); err != nil {
			return fmt.Errorf("save state: %w", err)
		}
		return nil
	}
	return saveState(store, st)
}

func saveState(store StateStore, st State) error {
	if store == nil {
		return errors.New("save state: nil store")
	}
	if err := store.Save(st); err != nil {
		return fmt.Errorf("save state: %w", err)
	}
	return nil
}

func evaluateDry(c Candidate, cfg config.Config, st State, req Request, now time.Time) Item {
	r := Evaluate(c, cfg, st, req.Rebase, req.CIFix, req.Force, now)
	if r.Action != "" {
		return r
	}
	pr := *c.PR
	r.Action = ActionWouldDispatch
	r.PaneID = *pr.Tab.PaneID
	r.Machine = pr.Tab.Machine
	r.RenderedPrompt = renderPrompt(cfg, req, pr)
	return r
}

// renderPrompt renders the prompt body and appends dispatch_prompt_suffix
// after a blank line when it is set.
func renderPrompt(cfg config.Config, req Request, pr scan.PR) string {
	body := renderPromptBody(cfg, req, pr)
	if cfg.PromptSuffix == "" {
		return body
	}
	return body + "\n\n" + cfg.PromptSuffix
}

func renderPromptBody(cfg config.Config, req Request, pr scan.PR) string {
	switch {
	case req.CIFix:
		return RenderHint(cfg.CIFixPromptTemplate, pr, cfg, req.Hint)
	case req.Rebase:
		return RenderHint(cfg.RebasePromptTemplate, pr, cfg, req.Hint)
	}
	if mergeQueueFixMode(pr, req.Rebase, req.CIFix) {
		return renderMergeQueueFix(pr, cfg)
	}
	return RenderHint(cfg.PromptTemplate, pr, cfg, req.Hint)
}

func annotateGate(ctx context.Context, h Herdr, cfg config.Config, req Request, doc *Document) error {
	res, err := GateCheck(ctx, h, cfg, req.RunnerPane, MatchedTabs(req.Doc))
	if err != nil {
		return fmt.Errorf("gate: %w", err)
	}
	busy := ""
	if len(res.Busy) > 0 {
		b := res.Busy[0]
		busy = "gate currently busy: pane " + scan.TabKey(b.Machine, b.PaneID)
		if b.PaneID == "" {
			busy = "gate currently busy: " + holdName(b)
		}
		if extraSignal(b) {
			busy += fmt.Sprintf(" (%s %s)", b.Signal, b.Detail)
		}
	}
	for i := range doc.Results {
		item := &doc.Results[i]
		if item.Action != ActionWouldDispatch {
			continue
		}
		reason := res.Reason
		if cfg.GateMachine() == "" {
			reason = dirtyTreeReason(ctx, gateConfig(cfg, item.Repo).Checkout)
		}
		if reason != "" {
			item.Detail = reason
		} else if busy != "" {
			item.Detail = busy
		}
	}
	return nil
}

func loadState(store StateStore) (State, error) {
	if store == nil {
		return State{}, nil
	}
	st, err := store.Load()
	if err != nil {
		return nil, fmt.Errorf("load state: %w", err)
	}
	if st == nil {
		st = State{}
	}
	return st, nil
}

func readyStatus(status string) bool {
	return status == "idle" || status == "done"
}

func findAgent(agents []herdr.Agent, paneID string) (herdr.Agent, bool) {
	for _, a := range agents {
		if a.PaneID == paneID {
			return a, true
		}
	}
	return herdr.Agent{}, false
}

func matchesUntil(status string, until []string) bool {
	for _, tok := range until {
		if status == tok {
			return true
		}
	}
	return false
}

func commentIDs(pr scan.PR) []string {
	ids := make([]string, 0, len(pr.BlockingComments))
	for _, c := range pr.BlockingComments {
		ids = append(ids, c.CommentID)
	}
	return ids
}

func prKey(repo string, number int) string {
	return fmt.Sprintf("%s#%d", repo, number)
}
