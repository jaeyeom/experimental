// Package prlog records pull request observations in the store's prs table.
// Callers pass pull requests they already read from gh. It makes no calls.
package prlog

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Record stores prs as seen at now for it. Every pull request must come
// from a gh search, so it has an open time. No pull requests writes nothing.
func Record(ctx context.Context, st *store.Store, now time.Time, it source.Item, prs []gh.PR) error {
	if len(prs) == 0 {
		return nil
	}
	if st == nil {
		return errors.New("record prs: expected a store, found nil")
	}
	if it.Source == "" || it.ID == "" {
		return fmt.Errorf("record prs: expected an item source and id, found source %q id %q", it.Source, it.ID)
	}
	if now.IsZero() {
		return errors.New("record prs: expected a clock time, found zero")
	}
	rows := make([]store.PR, len(prs))
	for i, pr := range prs {
		if pr.Merged && pr.MergedAt.IsZero() {
			return fmt.Errorf("record pr %s#%d: expected mergedAt on a merged pull request, found zero", pr.Repo, pr.Number)
		}
		rows[i] = store.PR{
			Repo:     pr.Repo,
			Number:   pr.Number,
			Source:   it.Source,
			ItemID:   it.ID,
			OpenedAt: pr.CreatedAt,
			MergedAt: pr.MergedAt,
			ClosedAt: pr.ClosedAt,
			Draft:    pr.Draft,
			LastSeen: now,
		}
	}
	return st.UpsertPRs(ctx, rows) //nolint:wrapcheck // propagate context cancellation
}
