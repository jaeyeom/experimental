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
	Doc  scan.Document
	PRs  []string
	Body string
}

// Run evaluates the candidate set and posts comments. Dry-run never calls
// GitHub. Live send is a gh API call per eligible PR and does not wait on
// the concurrency gate or require a herdr tab.
func Run(ctx context.Context, g GH, cfg config.Config, req Request, now time.Time) (dispatch.Document, error) {
	doc := dispatch.Document{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		DryRun:      cfg.DryRun,
		Results:     []dispatch.Item{},
	}
	cands, err := dispatch.Candidates(req.Doc, req.PRs)
	if err != nil {
		return doc, fmt.Errorf("comment candidates: %w", err)
	}
	for _, c := range cands {
		if err := ctx.Err(); err != nil {
			item := dispatch.Item{Repo: c.Repo, Number: c.Number, Action: dispatch.ActionFailed, Detail: err.Error()}
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("comment: %w", err)
		}
		item := dispatch.Item{Repo: c.Repo, Number: c.Number}
		if c.PR == nil {
			item.Action = dispatch.ActionSkippedNotFound
			doc.Results = append(doc.Results, item)
			continue
		}
		if cfg.DryRun {
			item.Action = dispatch.ActionWouldDispatch
			item.RenderedPrompt = req.Body
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
	}
	return doc, nil
}
