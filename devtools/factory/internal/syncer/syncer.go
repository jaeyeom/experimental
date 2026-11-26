// Package syncer refreshes the local mirror from source backends.
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

// Options selects which sources Sync reads and when a sync is full.
type Options struct {
	// Full forces a full sync: since is the zero time, and items the source
	// no longer returns are deleted.
	Full bool
	// Only, when set, is the one source key to sync. The key is kind:name,
	// for example jira:acme. An unknown key is an error.
	Only string
	// FullSyncEvery is how old the last full sync may be before the next one
	// is full. A zero last full sync is always full.
	FullSyncEvery time.Duration
	// AfterSync, when set, runs after each source syncs successfully, with
	// that source key and now. Its error fails that source's Result.
	AfterSync func(ctx context.Context, sourceName string, now time.Time) error
	// ExternalLookups maps a source key to how many blockers outside the
	// mirror its sync fetches by id. A source not in the map fetches none
	// and its external blocker records are left as they are.
	ExternalLookups map[string]int
}

// Result is the outcome of syncing one source.
// Err is set when that source failed. Other sources still run.
type Result struct {
	Source  string
	Full    bool
	Items   int
	Removed int
	Since   time.Time // The time passed to Source.Sync. Zero when Full is true.
	// Warning is set when an incremental sync upserted items but skipped the
	// membership check that removes items which left the source query.
	// It is not a failure: Err stays nil.
	Warning string
	// External counts the blockers outside the mirror this sync looked at.
	// Nil when the source is not in Options.ExternalLookups.
	External *External
	Err      error
}

// Sync updates the mirror from each source.
// A successful source also stores Source.Me in sync_state.me
// and now in sync_state.last_sync.
// A failure is recorded on that Result and the remaining sources still sync.
// Sync returns an error only when every selected source failed, or when the
// arguments or the --source filter are unusable.
func Sync(ctx context.Context, st *store.Store, srcs map[string]source.Source, now time.Time, opts Options) ([]Result, error) {
	if ctx == nil {
		return nil, errors.New("sync: expected a context, found nil")
	}
	if st == nil {
		return nil, errors.New("sync: expected a store, found nil")
	}
	if srcs == nil {
		return nil, errors.New("sync: expected a source map, found nil")
	}
	names, err := selectedSources(srcs, opts.Only)
	if err != nil {
		return nil, err
	}
	results := make([]Result, 0, len(names))
	failed := 0
	for _, name := range names {
		res := afterSync(ctx, syncOne(ctx, st, name, srcs[name], now, opts), now, opts)
		if res.Err != nil {
			failed++
		}
		results = append(results, res)
	}
	if len(names) > 0 && failed == len(names) {
		return results, everySourceFailed(results)
	}
	return results, nil
}

func selectedSources(srcs map[string]source.Source, only string) ([]string, error) {
	names := make([]string, 0, len(srcs))
	for name := range srcs {
		names = append(names, name)
	}
	slices.Sort(names)
	if only == "" {
		return names, nil
	}
	if _, ok := srcs[only]; !ok {
		known := "(none)"
		if len(names) > 0 {
			known = strings.Join(names, ", ")
		}
		return nil, fmt.Errorf("sync --source: expected one of %s, found %q", known, only)
	}
	return []string{only}, nil
}

func syncOne(ctx context.Context, st *store.Store, name string, src source.Source, now time.Time, opts Options) Result {
	res := Result{Source: name}
	if src == nil {
		res.Err = fmt.Errorf("sync %s: expected a source, found nil", name)
		return res
	}
	lastSeen, lastFull, err := st.SyncState(ctx, name)
	if err != nil {
		res.Err = err
		return res
	}
	me, err := sourceMe(ctx, name, src)
	if err != nil {
		res.Err = err
		return res
	}
	full := needsFull(lastFull, now, opts)
	since := time.Time{}
	if !full {
		since = lastSeen
	}
	res.Full = full
	res.Since = since
	items, err := src.Sync(ctx, since)
	if err != nil {
		res.Err = err
		return res
	}
	if err := st.UpsertItems(ctx, name, items); err != nil {
		res.Err = err
		return res
	}
	res.Items = len(items)
	if full {
		removed, err := st.DeleteItemsNotIn(ctx, name, keepIDs(items))
		if err != nil {
			res.Err = err
			return res
		}
		res.Removed = removed
	} else {
		removed, warning, err := pruneLeft(ctx, st, name, src)
		if err != nil {
			res.Err = err
			return res
		}
		res.Removed = removed
		res.Warning = warning
	}
	if bound, ok := opts.ExternalLookups[name]; ok {
		ext, err := refreshExternal(ctx, st, name, src, bound, now)
		if err != nil {
			res.Err = err
			return res
		}
		res.External = &ext
	}
	if err := recordState(ctx, st, name, items, lastSeen, lastFull, full, me, now); err != nil {
		res.Err = err
	}
	return res
}

// recordState stores the sync cursor, the last full sync, me, and now.
func recordState(ctx context.Context, st *store.Store, name string, items []source.Item, lastSeen, lastFull time.Time, full bool, me string, now time.Time) error {
	// Keep the previous cursor when this sync returned no items.
	// Do not store now: a later update stamped earlier than our clock
	// would then be skipped.
	seen := lastSeen
	if len(items) > 0 {
		seen = maxUpdated(items)
	}
	fullAt := lastFull
	if full {
		fullAt = now
	}
	if err := st.SetSyncState(ctx, name, seen, fullAt); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if err := st.SetMe(ctx, name, me); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	return st.SetLastSync(ctx, name, now) //nolint:wrapcheck // propagate context cancellation
}

// pruneLeft deletes mirrored items of name that the source query no longer
// matches. An incremental Sync only returns items that still match, so this
// is the only way it notices an item that left the query.
// A failed membership query, or an empty one while the mirror has rows,
// deletes nothing and returns a warning instead of an error, so an auth or
// query glitch cannot wipe the mirror.
func pruneLeft(ctx context.Context, st *store.Store, name string, src source.Source) (int, string, error) {
	ids, err := src.IDs(ctx)
	if err != nil {
		return 0, "membership check failed, removed nothing: " + err.Error(), nil
	}
	keep := make(map[string]bool, len(ids))
	for i, id := range ids {
		if id == "" {
			return 0, fmt.Sprintf("membership check: expected an item id, found empty at position %d; removed nothing", i), nil
		}
		keep[id] = true
	}
	if len(keep) == 0 {
		n, err := st.ItemCount(ctx, name)
		if err != nil {
			return 0, "", err //nolint:wrapcheck // propagate context cancellation
		}
		if n > 0 {
			return 0, fmt.Sprintf("membership check: expected item ids, found none while the mirror has %d items; removed nothing", n), nil
		}
		return 0, "", nil
	}
	removed, err := st.DeleteItemsNotIn(ctx, name, keep)
	if err != nil {
		return 0, "", err //nolint:wrapcheck // propagate context cancellation
	}
	return removed, "", nil
}

// afterSync runs opts.AfterSync for a source that synced successfully.
func afterSync(ctx context.Context, res Result, now time.Time, opts Options) Result {
	if res.Err != nil || opts.AfterSync == nil {
		return res
	}
	if err := opts.AfterSync(ctx, res.Source, now); err != nil {
		res.Err = fmt.Errorf("after sync %s: %w", res.Source, err)
	}
	return res
}

// sourceMe reads the account next and show will use offline.
// An empty account is not stored: a later command would treat it as unsynced.
func sourceMe(ctx context.Context, name string, src source.Source) (string, error) {
	me, err := src.Me(ctx)
	if err != nil {
		return "", err //nolint:wrapcheck // propagate context cancellation
	}
	if me == "" {
		return "", fmt.Errorf("sync %s: expected a non-empty me, found empty", name)
	}
	return me, nil
}

func needsFull(lastFull, now time.Time, opts Options) bool {
	if opts.Full || lastFull.IsZero() {
		return true
	}
	return now.Sub(lastFull) >= opts.FullSyncEvery
}

func keepIDs(items []source.Item) map[string]bool {
	keep := make(map[string]bool, len(items))
	for i := range items {
		keep[items[i].ID] = true
	}
	return keep
}

func maxUpdated(items []source.Item) time.Time {
	latest := items[0].Updated
	for i := 1; i < len(items); i++ {
		if items[i].Updated.After(latest) {
			latest = items[i].Updated
		}
	}
	return latest
}

func everySourceFailed(results []Result) error {
	parts := make([]string, len(results))
	for i, res := range results {
		msg := "expected an error, found nil"
		if res.Err != nil {
			msg = res.Err.Error()
		}
		parts[i] = res.Source + ": " + msg
	}
	return fmt.Errorf("sync: every source failed: %s", strings.Join(parts, "; "))
}
