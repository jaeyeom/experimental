package syncer

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Refresh rereads one item after a local write, such as release or finish,
// so the next command sees the change before any sync.
//
// It reads the item with Get and asks the source which IDs match its query
// now. When id is in that set, the row is updated. Otherwise the row and its
// links are deleted, as a full sync would delete them. Unlike Sync, an empty
// ID set still deletes the row: Get just succeeded with the same credentials,
// and only this one row is at stake.
//
// When the membership query fails, the row is updated from Get and the error
// is returned, so the caller can report it. Refresh reports whether the row
// was deleted.
func Refresh(ctx context.Context, st *store.Store, name string, src source.Source, id string) (bool, error) {
	if ctx == nil {
		return false, errors.New("refresh: expected a context, found nil")
	}
	if st == nil {
		return false, errors.New("refresh: expected a store, found nil")
	}
	if src == nil {
		return false, fmt.Errorf("refresh %s: expected a source, found nil", name)
	}
	if id == "" {
		return false, errors.New("refresh: expected a non-empty id, found empty")
	}
	it, err := src.Get(ctx, id)
	if err != nil {
		return false, err //nolint:wrapcheck // propagate context cancellation
	}
	if it.ID != id {
		return false, fmt.Errorf("get %s: expected id %q, found %q", id, id, it.ID)
	}
	if it.Source != name {
		return false, fmt.Errorf("get %s: expected source %q, found %q", id, name, it.Source)
	}
	ids, idsErr := src.IDs(ctx)
	if idsErr != nil || slices.Contains(ids, id) {
		if err := st.UpsertItems(ctx, name, []source.Item{it}); err != nil {
			return false, errors.Join(err, idsErr)
		}
		if idsErr != nil {
			return false, fmt.Errorf("refresh %s: membership check: %w", id, idsErr)
		}
		return false, nil
	}
	return st.DeleteItem(ctx, name, id) //nolint:wrapcheck // propagate context cancellation
}
