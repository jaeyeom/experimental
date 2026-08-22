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

// Request is the candidate-set input to Run.
type Request struct {
	Doc        scan.Document
	PRs        []string
	RunnerPane string
	Rebase     bool
	Force      bool
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
// Rebase mode skips the unaddressed-comment gate. A rebase dispatch is
// satisfied only when the PR is no longer behind or conflicting; a matching
// head SHA is not enough, so a failed rebase can be retried. force skips
// both comment and rebase dedupe.
func Evaluate(c Candidate, cfg config.Config, st State, rebase, force bool) Item {
	r := Item{Repo: c.Repo, Number: c.Number}
	if c.PR == nil {
		r.Action = ActionSkippedNotFound
		return r
	}
	pr := *c.PR
	switch {
	case !rebase && !pr.Unaddressed:
		r.Action = ActionSkippedAddressed
	case pr.IsDraft && !cfg.IncludeDrafts:
		r.Action = ActionSkippedDraft
	case pr.Tab == nil:
		r.Action = ActionSkippedNoTab
	case pr.Tab.PaneID == nil:
		r.Action = ActionSkippedNoAgent
	case !readyStatus(pr.Tab.AgentStatus):
		r.Action = ActionSkippedBusy
	case !force && rebase && rebaseDeduped(st, prKey(c.Repo, c.Number), pr):
		r.Action = ActionSkippedDeduped
	case !force && !rebase && st.Deduped(prKey(c.Repo, c.Number), commentIDs(pr)):
		r.Action = ActionSkippedDeduped
	}
	return r
}

// rebaseDeduped reports whether a prior rebase dispatch is already satisfied.
// BEHIND and DIRTY always retry. A recorded rebase plus a known up-to-date
// merge state (CLEAN and similar) is satisfied even if the head SHA moved.
// Empty merge state falls back to head-SHA equality so older scan documents
// keep the previous behavior.
func rebaseDeduped(st State, key string, pr scan.PR) bool {
	if rebaseIncomplete(pr.MergeStateStatus) {
		return false
	}
	if pr.MergeStateStatus != "" && rebaseRecorded(st, key) {
		return true
	}
	return st.DedupedHead(key, pr.HeadSHA)
}

func rebaseIncomplete(status string) bool {
	return status == "BEHIND" || status == "DIRTY"
}

func rebaseRecorded(st State, key string) bool {
	if st == nil {
		return false
	}
	entry, ok := st[key]
	return ok && entry.DispatchedHeadSHA != ""
}

// Run evaluates the candidate set. Dry-run does a one-shot gate.Check, never
// polls, never emits queued, and never writes state. Live send polls the gate
// one PR at a time, writes state on dispatched / dispatched_timeout, and
// returns ErrTimeout or ErrFailed after emitting partial results. A blocked
// settlement does not write dedupe state and stops the batch so a re-run can
// retry the same comment after the user answers.
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
		return runDry(ctx, h, store, cfg, req, cands, doc)
	}
	return runLive(ctx, h, store, cfg, req, now, cands, doc)
}

func runDry(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, cands []Candidate, doc Document) (Document, error) {
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	for _, c := range cands {
		doc.Results = append(doc.Results, evaluateDry(c, cfg, st, req.Rebase, req.Force))
	}
	if err := annotateGate(ctx, h, cfg, req, &doc); err != nil {
		return doc, err
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
	clock := Clock(realClock{})
	sleeper := Sleeper(realSleeper{})
	for i, c := range cands {
		if err := ctx.Err(); err != nil {
			doc.Results = append(doc.Results, failItem(c, err))
			return doc, fmt.Errorf("dispatch: %w", err)
		}
		item := Evaluate(c, cfg, st, req.Rebase, req.Force)
		if item.Action != "" {
			doc.Results = append(doc.Results, item)
			continue
		}
		_, err := Wait(ctx, h, cfg, req.RunnerPane, matched, clock, sleeper)
		if errors.Is(err, ErrTimeout) {
			queueRest(&doc, cands, i)
			return doc, err
		}
		if err != nil {
			doc.Results = append(doc.Results, failItem(c, err))
			return doc, err
		}
		item = sendPrompt(ctx, h, cfg, c, req.Rebase)
		doc.Results = append(doc.Results, item)
		if item.Action == ActionDispatched || item.Action == ActionDispatchedTimeout {
			if req.Rebase {
				st.RecordHead(prKey(c.Repo, c.Number), c.PR.HeadSHA, now)
			} else {
				st.Record(prKey(c.Repo, c.Number), commentIDs(*c.PR), now)
			}
			if err := saveState(store, st); err != nil {
				return doc, err
			}
		}
		if item.Action == ActionDispatchedBlocked {
			queueRest(&doc, cands, i+1)
			return doc, nil
		}
		if item.Action == ActionFailed {
			if err := ctx.Err(); err != nil {
				return doc, fmt.Errorf("dispatch: %w", err)
			}
			return doc, fmt.Errorf("%w: %s", ErrFailed, item.Detail)
		}
	}
	return doc, nil
}

func sendPrompt(ctx context.Context, h Herdr, cfg config.Config, c Candidate, rebase bool) Item {
	pr := *c.PR
	pane := *pr.Tab.PaneID
	rendered := Render(promptTemplate(cfg, rebase), pr)
	out := h.Prompt(ctx, pane, rendered, cfg.WaitUntil, cfg.DispatchTimeout)
	item := Item{Repo: c.Repo, Number: c.Number}
	switch out.Status {
	case herdr.PromptMatched:
		item.Action = ActionDispatched
		if out.Agent.AgentStatus == "blocked" {
			item.Action = ActionDispatchedBlocked
		}
		item.PaneID = pane
		item.RenderedPrompt = rendered
	case herdr.PromptStalled:
		item.Action = ActionSkippedStalled
	case herdr.PromptTimeout:
		item.Action = ActionDispatchedTimeout
		item.PaneID = pane
		item.RenderedPrompt = rendered
	default:
		item.Action = ActionFailed
		if out.Err != nil {
			item.Detail = out.Err.Error()
		} else {
			item.Detail = "herdr prompt failed"
		}
	}
	return item
}

func queueRest(doc *Document, cands []Candidate, from int) {
	for _, c := range cands[from:] {
		doc.Results = append(doc.Results, Item{
			Repo:   c.Repo,
			Number: c.Number,
			Action: ActionQueued,
		})
	}
}

func failItem(c Candidate, err error) Item {
	item := Item{Repo: c.Repo, Number: c.Number, Action: ActionFailed}
	if err != nil {
		item.Detail = err.Error()
	}
	return item
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

func saveState(store StateStore, st State) error {
	if store == nil {
		return errors.New("save state: nil store")
	}
	if err := store.Save(st); err != nil {
		return fmt.Errorf("save state: %w", err)
	}
	return nil
}

func evaluateDry(c Candidate, cfg config.Config, st State, rebase, force bool) Item {
	r := Evaluate(c, cfg, st, rebase, force)
	if r.Action != "" {
		return r
	}
	pr := *c.PR
	r.Action = ActionWouldDispatch
	r.PaneID = *pr.Tab.PaneID
	r.RenderedPrompt = Render(promptTemplate(cfg, rebase), pr)
	return r
}

func promptTemplate(cfg config.Config, rebase bool) string {
	if rebase {
		return cfg.RebasePromptTemplate
	}
	return cfg.PromptTemplate
}

func annotateGate(ctx context.Context, h Herdr, cfg config.Config, req Request, doc *Document) error {
	res, err := Check(ctx, h, cfg.ConcurrencyWaitOn, req.RunnerPane, MatchedTabs(req.Doc))
	if err != nil {
		return fmt.Errorf("gate: %w", err)
	}
	if res.Safe || len(res.Busy) == 0 {
		return nil
	}
	detail := fmt.Sprintf("gate currently busy: pane %s", res.Busy[0].PaneID)
	for i := range doc.Results {
		if doc.Results[i].Action == ActionWouldDispatch {
			doc.Results[i].Detail = detail
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
