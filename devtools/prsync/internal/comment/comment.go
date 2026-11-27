// Package comment posts deterministic PR comments via gh, without a herdr tab.
package comment

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// GH is the GitHub surface comment uses.
type GH interface {
	CommentPR(ctx context.Context, repo string, number int, body string) error
}

type commentIDPoster interface {
	CommentPRWithID(ctx context.Context, repo string, number int, body string) (string, error)
}

// Request is the candidate-set input to Run.
type Request struct {
	Doc            scan.Document
	PRs            []string
	Body           string
	AllowDuplicate bool
	RepostWaives   bool
	Ledger         *ledger.Store
}

// Run evaluates the candidate set and posts comments. Dry-run never calls
// GitHub and never writes state. Live send is a gh API call per eligible PR
// and does not wait on the concurrency gate or require a herdr tab. A second
// live run against the same (repo, number, head SHA, body) is skipped_deduped.
func Run(ctx context.Context, g GH, store dispatch.StateStore, cfg config.Config, req Request, now time.Time) (dispatch.Document, error) {
	doc := dispatch.Document{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		DryRun:      cfg.DryRun,
		Results:     []dispatch.Item{},
	}
	cands, err := dispatch.Candidates(req.Doc, req.PRs)
	if err != nil {
		return doc, fmt.Errorf("comment candidates: %w", err)
	}
	if cfg.DryRun {
		return runDry(ctx, store, cfg, req, cands, doc)
	}
	return runLive(ctx, g, store, cfg, req, now, cands, doc)
}

func runDry(ctx context.Context, store dispatch.StateStore, cfg config.Config, req Request, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	for _, c := range cands {
		if err := ctx.Err(); err != nil {
			item := dispatch.Item{Repo: c.Repo, Number: c.Number, Action: dispatch.ActionFailed, Detail: err.Error()}
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("comment: %w", err)
		}
		item, _, err := evaluateCandidate(c, st, cfg, req, true)
		if err != nil {
			return doc, err
		}
		doc.Results = append(doc.Results, item)
	}
	return doc, nil
}

type locker interface {
	WithLock(func() error) error
}

func runLive(ctx context.Context, g GH, store dispatch.StateStore, cfg config.Config, req Request, now time.Time, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
	if l, ok := store.(locker); ok {
		var liveErr error
		lockErr := l.WithLock(func() error {
			doc, liveErr = commentLive(ctx, g, store, cfg, req, now, cands, doc)
			return nil
		})
		if lockErr != nil {
			return doc, fmt.Errorf("lock state: %w", lockErr)
		}
		return doc, liveErr
	}
	return commentLive(ctx, g, store, cfg, req, now, cands, doc)
}

func commentLive(ctx context.Context, g GH, store dispatch.StateStore, cfg config.Config, req Request, now time.Time, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	for _, c := range cands {
		if err := ctx.Err(); err != nil {
			item := dispatch.Item{Repo: c.Repo, Number: c.Number, Action: dispatch.ActionFailed, Detail: err.Error()}
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("comment: %w", err)
		}
		item, postBody, err := evaluateCandidate(c, st, cfg, req, false)
		if err != nil {
			return doc, err
		}
		if item.Action != "" {
			doc.Results = append(doc.Results, item)
			continue
		}
		commentID, err := postComment(ctx, g, c.Repo, c.Number, postBody)
		if err != nil {
			item.Action = dispatch.ActionFailed
			item.Detail = err.Error()
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("%w: %w", dispatch.ErrFailed, err)
		}
		item.Action = dispatch.ActionDispatched
		item.RenderedPrompt = postBody
		if req.RepostWaives && commentID != "" {
			item.Detail += " comment_id=" + commentID
		}
		doc.Results = append(doc.Results, item)
		headSHA := postedHeadSHA(c, req)
		st.RecordPosted(prKey(c.Repo, c.Number), headSHA, postBody, now)
		if err := saveState(store, st); err != nil {
			return doc, err
		}
		if err := recordWaiveLedger(req, c, headSHA, commentID, item.Detail, now); err != nil {
			return doc, err
		}
	}
	return doc, nil
}

func postComment(ctx context.Context, g GH, repo string, number int, body string) (string, error) {
	if poster, ok := g.(commentIDPoster); ok {
		return poster.CommentPRWithID(ctx, repo, number, body) //nolint:wrapcheck // caller classifies comment error
	}
	return "", g.CommentPR(ctx, repo, number, body) //nolint:wrapcheck // caller classifies comment error
}

func postedHeadSHA(c dispatch.Candidate, req Request) string {
	if req.RepostWaives && c.PR != nil && c.PR.StaleWaive != nil && c.PR.StaleWaive.NewHead != "" {
		return c.PR.StaleWaive.NewHead
	}
	if c.PR != nil {
		return c.PR.HeadSHA
	}
	return ""
}

func recordWaiveLedger(req Request, c dispatch.Candidate, headSHA, commentID, detail string, now time.Time) error {
	if !req.RepostWaives || req.Ledger == nil || c.PR == nil || c.PR.StaleWaive == nil {
		return nil
	}
	sw := c.PR.StaleWaive
	if err := req.Ledger.Append(ledger.Entry{
		PR:         prKey(c.Repo, c.Number),
		HeadSHA:    headSHA,
		OldHeadSHA: sw.OldHead,
		NewHeadSHA: headSHA,
		CommentID:  commentID,
		Kind:       ledger.KindWaiveRepost,
		Time:       now,
		Outcome:    ledger.OutcomeDispatched,
		Detail:     detail,
	}); err != nil {
		return fmt.Errorf("append ledger: %w", err)
	}
	return nil
}

func evaluateCandidate(c dispatch.Candidate, st dispatch.State, cfg config.Config, req Request, dryRun bool) (dispatch.Item, string, error) {
	if req.RepostWaives {
		return evaluateWaiveRepost(c, st, cfg, req, dryRun)
	}
	return evaluate(c, st, req, dryRun), req.Body, nil
}

func evaluateWaiveRepost(c dispatch.Candidate, st dispatch.State, cfg config.Config, req Request, dryRun bool) (dispatch.Item, string, error) {
	item := dispatch.Item{Repo: c.Repo, Number: c.Number}
	if c.PR == nil {
		item.Action = dispatch.ActionSkippedNotFound
		return item, "", nil
	}
	if c.PR.OutOfScope != "" {
		item.Action = dispatch.ActionSkippedOutOfScope
		item.Detail = c.PR.OutOfScope
		return item, "", nil
	}
	if reason := waiveConfigDisabledReason(cfg); reason != "" {
		item.Action = dispatch.ActionSkippedWaiveDisabled
		item.Detail = reason
		return item, "", nil
	}
	sw := c.PR.StaleWaive
	if sw == nil {
		item.Action = dispatch.ActionSkippedNoStaleWaive
		return item, "", nil
	}
	if sw.Class != scan.WaiveClassBaseOnly {
		item.Action = dispatch.ActionSkippedAuthorChanges
		item.Detail = fmt.Sprintf("class=%s (%s -> %s); manual review required", sw.Class, sw.OldHead, sw.NewHead)
		return item, "", nil
	}
	failing := matchingWaiveChecks(sw.FailingChecks, cfg.WaiveChecks)
	if len(failing) == 0 {
		item.Action = dispatch.ActionSkippedNoFailingWaivedChecks
		item.Detail = fmt.Sprintf("no failing checks in waive_checks (%s -> %s)", sw.OldHead, sw.NewHead)
		return item, "", nil
	}
	if sw.CommentBody == "" {
		item.Action = dispatch.ActionSkippedNoStaleWaive
		item.Detail = "stale waive has empty comment body"
		return item, "", nil
	}
	return checkWaiveDedupeAndCap(c, st, cfg, req, sw, failing, dryRun)
}

func waiveConfigDisabledReason(cfg config.Config) string {
	switch {
	case cfg.WaiveCommentPrefix == "":
		return "waive_comment_prefix is empty"
	case !cfg.WaiveRepost:
		return "waive_repost is false"
	case len(cfg.WaiveChecks) == 0:
		return "waive_checks is empty"
	default:
		return ""
	}
}

func checkWaiveDedupeAndCap(c dispatch.Candidate, st dispatch.State, cfg config.Config, req Request, sw *scan.StaleWaive, failing []string, dryRun bool) (dispatch.Item, string, error) {
	item := dispatch.Item{Repo: c.Repo, Number: c.Number}
	key := prKey(c.Repo, c.Number)
	newHead := sw.NewHead
	if newHead == "" {
		newHead = c.PR.HeadSHA
	}
	if req.Ledger != nil {
		already, err := req.Ledger.HasWaiveReposted(key, newHead)
		if err != nil {
			return item, "", fmt.Errorf("check waive ledger: %w", err)
		}
		if already {
			item.Action = dispatch.ActionSkippedDeduped
			item.Detail = fmt.Sprintf("already re-posted for head %s", newHead)
			return item, "", nil
		}
	}
	if st.DedupedPosted(key, newHead, sw.CommentBody) {
		item.Action = dispatch.ActionSkippedDeduped
		item.Detail = fmt.Sprintf("already re-posted for head %s", newHead)
		return item, "", nil
	}
	if req.Ledger != nil {
		count, err := req.Ledger.CountWaiveReposts(key)
		if err != nil {
			return item, "", fmt.Errorf("count waive ledger: %w", err)
		}
		if count >= cfg.WaiveMaxPerPR {
			item.Action = dispatch.ActionSkippedWaiveCap
			item.Detail = fmt.Sprintf("waive_max_per_pr=%d reached", cfg.WaiveMaxPerPR)
			return item, "", nil
		}
	} else if cfg.WaiveMaxPerPR <= 0 {
		item.Action = dispatch.ActionSkippedWaiveCap
		item.Detail = fmt.Sprintf("waive_max_per_pr=%d reached", cfg.WaiveMaxPerPR)
		return item, "", nil
	}
	if blockedItem, blocked := checkWaiveRequireGreen(c.Repo, c.Number, cfg.WaiveRequireGreen, sw); blocked {
		return blockedItem, "", nil
	}
	item.Detail = fmt.Sprintf("base-only old=%s new=%s failing=%s", sw.OldHead, newHead, strings.Join(failing, ","))
	if dryRun {
		item.Action = dispatch.ActionWouldDispatch
		item.RenderedPrompt = sw.CommentBody
	}
	return item, sw.CommentBody, nil
}

func checkWaiveRequireGreen(repo string, number int, required []string, sw *scan.StaleWaive) (dispatch.Item, bool) {
	if len(required) == 0 {
		return dispatch.Item{}, false
	}
	outcome := scan.EvaluateRequiredChecks(sw.StatusChecks, required)
	if len(outcome.Failing) > 0 {
		return dispatch.Item{
			Repo:   repo,
			Number: number,
			Action: dispatch.ActionSkippedWaiveChecksFailing,
			Detail: "waive_require_green failing: " + strings.Join(outcome.Failing, ", "),
		}, true
	}
	if len(outcome.Pending) == 0 && len(outcome.Missing) == 0 {
		return dispatch.Item{}, false
	}
	var parts []string
	if len(outcome.Pending) > 0 {
		parts = append(parts, "pending: "+strings.Join(outcome.Pending, ", "))
	}
	if len(outcome.Missing) > 0 {
		parts = append(parts, "missing: "+strings.Join(outcome.Missing, ", "))
	}
	return dispatch.Item{
		Repo:   repo,
		Number: number,
		Action: dispatch.ActionSkippedWaiveChecksPending,
		Detail: "waive_require_green " + strings.Join(parts, "; "),
	}, true
}

func matchingWaiveChecks(failing, allowed []string) []string {
	var out []string
	for _, name := range failing {
		if slices.Contains(allowed, name) && !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	return out
}

func evaluate(c dispatch.Candidate, st dispatch.State, req Request, dryRun bool) dispatch.Item {
	item := dispatch.Item{Repo: c.Repo, Number: c.Number}
	if c.PR == nil {
		item.Action = dispatch.ActionSkippedNotFound
		return item
	}
	if !req.AllowDuplicate && st.DedupedPosted(prKey(c.Repo, c.Number), c.PR.HeadSHA, req.Body) {
		item.Action = dispatch.ActionSkippedDeduped
		return item
	}
	if dryRun {
		item.Action = dispatch.ActionWouldDispatch
		item.RenderedPrompt = req.Body
	}
	return item
}

func loadState(store dispatch.StateStore) (dispatch.State, error) {
	if store == nil {
		return dispatch.State{}, nil
	}
	st, err := store.Load()
	if err != nil {
		return nil, fmt.Errorf("load state: %w", err)
	}
	if st == nil {
		st = dispatch.State{}
	}
	return st, nil
}

func saveState(store dispatch.StateStore, st dispatch.State) error {
	if store == nil {
		return nil
	}
	if err := store.Save(st); err != nil {
		return fmt.Errorf("save state: %w", err)
	}
	return nil
}

func prKey(repo string, number int) string {
	return fmt.Sprintf("%s#%d", repo, number)
}
