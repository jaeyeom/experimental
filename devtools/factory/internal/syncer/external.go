package syncer

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// External counts one sync's blockers outside the mirror.
type External struct {
	// Referenced is how many distinct blocker ids of mirrored items are not
	// in the mirror.
	Referenced int `json:"referenced"`
	// Fetched is how many of them this sync fetched by id.
	Fetched int `json:"fetched"`
	// Done is how many records, fetched now or kept, are done.
	Done int `json:"done"`
	// Skipped is how many were over the lookup bound and not fetched.
	Skipped int `json:"skipped"`
}

// refreshExternal fetches each blocker of a mirrored item that is not
// itself in the mirror, once per id and at most bound in all, and makes the
// result the source's whole set of external blocker records. A blocker over
// the bound keeps its previous record, or gets an unchecked one. A failed
// fetch records unchecked, never done. A record no longer referenced is
// dropped.
func refreshExternal(ctx context.Context, st *store.Store, name string, src source.Source, bound int, now time.Time) (External, error) {
	items, err := st.Items(ctx, name)
	if err != nil {
		return External{}, err //nolint:wrapcheck // propagate context cancellation
	}
	prev, err := st.ExternalBlockers(ctx, name)
	if err != nil {
		return External{}, err //nolint:wrapcheck // propagate context cancellation
	}
	ids := outsideBlockers(items)
	out := External{Referenced: len(ids)}
	recs := make([]store.ExternalBlocker, 0, len(ids))
	for i, id := range ids {
		var rec store.ExternalBlocker
		if i < bound {
			rec, err = fetchExternal(ctx, src, id, now)
			if err != nil {
				return External{}, err
			}
			out.Fetched++
		} else {
			out.Skipped++
			old, ok := prev[id]
			if ok {
				rec = old
			} else {
				rec = store.ExternalBlocker{
					ID:        id,
					Status:    store.ExternalUnchecked,
					Reason:    fmt.Sprintf("over external_blocker_lookups (%d)", bound),
					FetchedAt: now,
				}
			}
		}
		if rec.Status == store.ExternalFound && rec.Done {
			out.Done++
		}
		recs = append(recs, rec)
	}
	if err := st.ReplaceExternalBlockers(ctx, name, recs); err != nil {
		return External{}, err //nolint:wrapcheck // propagate context cancellation
	}
	return out, nil
}

func fetchExternal(ctx context.Context, src source.Source, id string, now time.Time) (store.ExternalBlocker, error) {
	it, err := src.Get(ctx, id)
	rec := store.ExternalBlocker{ID: id, FetchedAt: now}
	switch {
	case err == nil:
		rec.Status = store.ExternalFound
		rec.Done = it.Done
		rec.State = it.State
		rec.Assignee = it.Assignee
		rec.Title = it.Title
	case ctx.Err() != nil:
		return store.ExternalBlocker{}, fmt.Errorf("look up blocker %s: %w", id, ctx.Err())
	case errors.Is(err, source.ErrNotFound):
		rec.Status = store.ExternalNotFound
	default:
		rec.Status = store.ExternalUnchecked
		rec.Reason = "lookup failed: " + strings.Join(strings.Fields(err.Error()), " ")
	}
	return rec, nil
}

// outsideBlockers lists, sorted and once each, the blocker ids of mirrored
// items that are not in items. Done items count too, so doctor sees every
// link it reports.
func outsideBlockers(items []source.Item) []string {
	inMirror := make(map[string]struct{}, len(items))
	for i := range items {
		inMirror[items[i].ID] = struct{}{}
	}
	seen := make(map[string]struct{})
	for i := range items {
		for _, ref := range items[i].BlockedBy {
			if _, ok := inMirror[ref.ID]; !ok {
				seen[ref.ID] = struct{}{}
			}
		}
	}
	ids := make([]string, 0, len(seen))
	for id := range seen {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	return ids
}
