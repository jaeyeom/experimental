// Package snapshot loads one source's readiness inputs from the store, herdr, and gh.
package snapshot

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Build loads the readiness snapshot for one source.
// Items and active claims come from st. Tabs and workspaces come from herdrClient.
// Open pull requests come from ghClient. me is the account stored for the source.
// A herdr or gh failure is returned. Build does not replace that failure with
// empty tabs or pull requests, because an empty tab list would make every item look free.
func Build(ctx context.Context, st *store.Store, herdrClient *herdr.Client, ghClient *gh.Client, sourceName, me string) (ready.Snapshot, error) {
	if ctx == nil {
		return ready.Snapshot{}, errors.New("snapshot: expected a context, found nil")
	}
	if st == nil {
		return ready.Snapshot{}, errors.New("snapshot: expected a store, found nil")
	}
	if herdrClient == nil {
		return ready.Snapshot{}, errors.New("snapshot: expected a herdr client, found nil")
	}
	if ghClient == nil {
		return ready.Snapshot{}, errors.New("snapshot: expected a gh client, found nil")
	}
	if sourceName == "" {
		return ready.Snapshot{}, errors.New("snapshot: expected a non-empty source, found empty")
	}
	if me == "" {
		return ready.Snapshot{}, errors.New("snapshot: expected a non-empty me, found empty")
	}
	items, err := st.Items(ctx, sourceName)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("snapshot %s: %w", sourceName, err)
	}
	claims, err := activeByID(ctx, st, sourceName)
	if err != nil {
		return ready.Snapshot{}, err
	}
	workspaces, err := herdrClient.Workspaces(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("herdr workspaces: %w", err)
	}
	tabs, err := herdrClient.Tabs(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("herdr tabs: %w", err)
	}
	prs, err := ghClient.OpenByMe(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("gh open prs: %w", err)
	}
	external, err := st.ExternalBlockers(ctx, sourceName)
	if err != nil {
		return ready.Snapshot{}, err //nolint:wrapcheck // propagate context cancellation
	}
	approvals, err := st.Approvals(ctx, sourceName)
	if err != nil {
		return ready.Snapshot{}, err //nolint:wrapcheck // propagate context cancellation
	}
	byID := make(map[string]source.Item, len(items))
	for _, it := range items {
		byID[it.ID] = it
	}
	return ready.Snapshot{
		External:   external,
		Approvals:  approvals,
		Items:      byID,
		Claims:     claims,
		Tabs:       tabs,
		Workspaces: workspaces,
		OpenPRs:    prs,
		Me:         me,
		Now:        time.Now(),
	}, nil
}

func activeByID(ctx context.Context, st *store.Store, sourceName string) (map[string]store.ClaimState, error) {
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return nil, fmt.Errorf("snapshot %s: %w", sourceName, err)
	}
	out := make(map[string]store.ClaimState)
	for _, claim := range claims {
		if claim.Source != sourceName {
			continue
		}
		if _, ok := out[claim.ID]; ok {
			return nil, fmt.Errorf("active claims for %s/%s: expected one, found more than one", sourceName, claim.ID)
		}
		out[claim.ID] = claim.State
	}
	return out, nil
}
