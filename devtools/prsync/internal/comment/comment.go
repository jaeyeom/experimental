// Package comment posts deterministic PR comments via gh, without a herdr tab.
package comment

import (
	"context"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// GH is the GitHub surface comment uses.
type GH interface {
	CommentPR(ctx context.Context, repo string, number int, body string) error
}

// Request is the candidate-set input to Run.
type Request struct {
	Doc            scan.Document
	PRs            []string
	Body           string
	AllowDuplicate bool
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
		return runDry(ctx, store, req, cands, doc)
	}
	return runLive(ctx, g, store, req, now, cands, doc)
}

func runDry(ctx context.Context, store dispatch.StateStore, req Request, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
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
		doc.Results = append(doc.Results, evaluate(c, st, req, true))
	}
	return doc, nil
}

type locker interface {
	WithLock(func() error) error
}

func runLive(ctx context.Context, g GH, store dispatch.StateStore, req Request, now time.Time, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
	if l, ok := store.(locker); ok {
		var liveErr error
		lockErr := l.WithLock(func() error {
			doc, liveErr = commentLive(ctx, g, store, req, now, cands, doc)
			return nil
		})
		if lockErr != nil {
			return doc, fmt.Errorf("lock state: %w", lockErr)
		}
		return doc, liveErr
	}
	return commentLive(ctx, g, store, req, now, cands, doc)
}

func commentLive(ctx context.Context, g GH, store dispatch.StateStore, req Request, now time.Time, cands []dispatch.Candidate, doc dispatch.Document) (dispatch.Document, error) {
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
		item := evaluate(c, st, req, false)
		if item.Action != "" {
			doc.Results = append(doc.Results, item)
			continue
		}
		if err := g.CommentPR(ctx, c.Repo, c.Number, req.Body); err != nil {
			item.Action = dispatch.ActionFailed
			item.Detail = err.Error()
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("%w: %w", dispatch.ErrFailed, err)
		}
		item.Action = dispatch.ActionDispatched
		item.RenderedPrompt = req.Body
		doc.Results = append(doc.Results, item)
		st.RecordPosted(prKey(c.Repo, c.Number), c.PR.HeadSHA, req.Body, now)
		if err := saveState(store, st); err != nil {
			return doc, err
		}
	}
	return doc, nil
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
