package scan

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"log/slog"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// prViewRound is one `tabs --orphans` round over "#N" tabs in acme/widgets.
type prViewRound struct {
	cache     PRViewCache
	closedTTL time.Duration
	numbers   []int
	views     map[string]gh.PRSearchItem
	viewErr   error
}

func (r prViewRound) run(t *testing.T) (*orphanGH, OrphanDocument, string) {
	t.Helper()
	g := &orphanGH{views: r.views, viewErr: r.viewErr}
	var h stubHerdr
	for _, number := range r.numbers {
		tab := fmt.Sprintf("w2:t%d", number)
		h.tabs = append(h.tabs, liveTab(tab, "w2", fmt.Sprintf("#%d", number)))
		h.agents = append(h.agents, liveAgentAt(fmt.Sprintf("w2:p%d", number), tab, "idle", "/work/widgets"))
	}
	lookup := NewPRViewLookup(g, r.cache, r.closedTTL)
	var logs bytes.Buffer
	ctx := runlog.WithLogger(context.Background(), slog.New(slog.NewJSONHandler(&logs, nil)))
	doc, err := Orphans(ctx, OrphanDeps{GH: lookup, Herdr: h, RepoOf: repoFromCWD}, orphanCfg(), nil, fixtureNow)
	if err != nil {
		t.Fatalf("Orphans() unexpected error: %v", err)
	}
	lookup.Prune()
	lookup.Report(ctx)
	return g, doc, logs.String()
}

func withState(item gh.PRSearchItem, state string) gh.PRSearchItem {
	item.State = state
	return item
}

func TestPRViewLookupReusesMergedResult(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	round := prViewRound{
		cache: cache, closedTTL: 24 * time.Hour, numbers: []int{42},
		views: map[string]gh.PRSearchItem{"acme/widgets#42": mergedNumberItem(42)},
	}
	first, firstDoc, logs := round.run(t)
	if len(first.viewed) != 1 {
		t.Fatalf("first round viewed = %v, want one call", first.viewed)
	}
	if !strings.Contains(logs, "pr view 0 hits/1 calls") {
		t.Fatalf("logs = %s, want the pr view summary", logs)
	}

	clock = clock.Add(30 * 24 * time.Hour)
	round.views = nil
	second, doc, logs := round.run(t)
	if len(second.viewed) != 0 {
		t.Fatalf("second round viewed = %v, want none", second.viewed)
	}
	if !strings.Contains(logs, "pr view 1 hits/0 calls") {
		t.Fatalf("logs = %s, want one hit", logs)
	}
	if len(doc.OrphanTabs) != 1 || doc.OrphanTabs[0].Bucket != BucketMerged ||
		!reflect.DeepEqual(doc.OrphanTabs[0].PR, firstDoc.OrphanTabs[0].PR) {
		t.Fatalf("orphan_tabs = %+v, want the round-1 merged item", doc.OrphanTabs)
	}
}

func TestPRViewLookupClosedResultExpires(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	round := prViewRound{
		cache: cache, closedTTL: 24 * time.Hour, numbers: []int{42},
		views: map[string]gh.PRSearchItem{"acme/widgets#42": withState(mergedNumberItem(42), "closed")},
	}
	round.run(t)

	clock = clock.Add(23 * time.Hour)
	if g, _, _ := round.run(t); len(g.viewed) != 0 {
		t.Fatalf("viewed inside the TTL = %v, want none", g.viewed)
	}
	clock = clock.Add(2 * time.Hour)
	if g, _, _ := round.run(t); len(g.viewed) != 1 {
		t.Fatalf("viewed past the TTL = %v, want one call", g.viewed)
	}
}

func TestPRViewLookupClosedTTLZeroFetchesClosed(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	round := prViewRound{
		cache: cache, numbers: []int{42, 43},
		views: map[string]gh.PRSearchItem{
			"acme/widgets#42": withState(mergedNumberItem(42), "closed"),
			"acme/widgets#43": mergedNumberItem(43),
		},
	}
	round.run(t)
	g, _, _ := round.run(t)
	if !slices.Equal(g.viewed, []string{"acme/widgets#42"}) {
		t.Fatalf("viewed = %v, want only the closed PR", g.viewed)
	}
}

func TestPRViewLookupFetchesOpenEveryRound(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	open := withState(mergedNumberItem(42), "open")
	open.ClosedAt = ""
	round := prViewRound{
		cache: cache, closedTTL: 24 * time.Hour, numbers: []int{42},
		views: map[string]gh.PRSearchItem{"acme/widgets#42": open},
	}
	for i := range 2 {
		if g, _, _ := round.run(t); len(g.viewed) != 1 {
			t.Fatalf("round %d viewed = %v, want one call", i+1, g.viewed)
		}
	}
}

func TestPRViewLookupDoesNotStoreErrors(t *testing.T) {
	t.Parallel()

	for name, viewErr := range map[string]error{
		"not found": nil,
		"failure":   errors.New("gh: boom"),
	} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			clock := fixtureNow
			cache := newRoundCache(t, &clock)
			round := prViewRound{cache: cache, closedTTL: 24 * time.Hour, numbers: []int{42}, viewErr: viewErr}
			for i := range 2 {
				if g, _, _ := round.run(t); len(g.viewed) != 1 {
					t.Fatalf("round %d viewed = %v, want one call", i+1, g.viewed)
				}
			}
		})
	}
}

func TestPRViewLookupPrunesUnreferencedNumbers(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	views := map[string]gh.PRSearchItem{
		"acme/widgets#42": mergedNumberItem(42),
		"acme/widgets#43": mergedNumberItem(43),
	}
	prViewRound{cache: cache, closedTTL: 24 * time.Hour, numbers: []int{42, 43}, views: views}.run(t)
	prViewRound{cache: cache, closedTTL: 24 * time.Hour, numbers: []int{43}, views: views}.run(t)
	if _, ok := cache.PRView("acme/widgets", 42); ok {
		t.Fatal("acme/widgets#42 still stored after no tab referenced it")
	}
	if _, ok := cache.PRView("acme/widgets", 43); !ok {
		t.Fatal("acme/widgets#43 pruned while a tab references it")
	}
}

func TestCloseMergedReverifyUsesStoredMergedView(t *testing.T) {
	t.Parallel()

	clock := fixtureNow
	cache := newRoundCache(t, &clock)
	cache.PutPRView("acme/widgets", 42, mergedNumberItem(42))
	cfg := orphanCfg()
	cfg.DryRun = false
	g := &orphanGH{}
	h := &closeHerdr{tabs: []herdr.Tab{liveTab("w2:tA", "w2", "#42")}}
	lookup := NewPRViewLookup(g, cache, 24*time.Hour)
	got, err := CloseMerged(context.Background(), CloseDeps{GH: lookup, Herdr: h}, cfg, "alice",
		[]OrphanTab{numberMergedOrphan("w2:tA", 42)}, fixtureNow)
	if err != nil {
		t.Fatalf("CloseMerged() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionClosed {
		t.Fatalf("results = %+v, want closed", got.Results)
	}
	if len(g.viewed) != 0 {
		t.Fatalf("viewed = %v, want none", g.viewed)
	}
}
