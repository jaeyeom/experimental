package scan

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// Close actions in a --close-merged result.
const (
	ActionWouldClose       = "would_close"
	ActionClosed           = "closed"
	ActionSkippedNotMerged = "skipped_not_merged"
	ActionSkippedNotFound  = "skipped_not_found"
	ActionFailed           = "failed"
)

// ErrCloseFailed is returned when a live tab close fails (exit 3).
var ErrCloseFailed = errors.New("close merged failed")

// ErrCloseMergedUntrusted is returned when --close-merged must not proceed
// because classification is an empty-sweep unknown or all no_pr.
var ErrCloseMergedUntrusted = errors.New("close-merged refused")

// UntrustedCloseReason reports whether --close-merged should refuse this
// classification. An empty orphan list is fine (nothing to close). Unknown
// buckets mean the search sweep was empty. An all-no_pr list is the
// silent-success failure mode that hid merged tabs; callers may override.
func UntrustedCloseReason(orphans []OrphanTab) error {
	if len(orphans) == 0 {
		return nil
	}
	hasUnknown := false
	hasMerged := false
	for _, tab := range orphans {
		switch tab.Bucket {
		case BucketUnknown:
			hasUnknown = true
		case BucketMerged:
			hasMerged = true
		}
	}
	if hasUnknown {
		return fmt.Errorf("%w: search classification is unknown", ErrCloseMergedUntrusted)
	}
	if !hasMerged {
		return fmt.Errorf("%w: every orphan is no_pr; pass --force to override", ErrCloseMergedUntrusted)
	}
	return nil
}

// TabCloser is the herdr surface --close-merged uses.
type TabCloser interface {
	TabList(ctx context.Context) ([]herdr.Tab, error)
	TabClose(ctx context.Context, tabID string) error
}

// CloseDeps is the injected GitHub and herdr adapters for --close-merged.
type CloseDeps struct {
	GH    OrphanGH
	Herdr TabCloser
}

// CloseDocument is the outbound `tabs --orphans --close-merged` JSON document.
type CloseDocument struct {
	GeneratedAt string      `json:"generated_at"` //nolint:tagliatelle // brief outbound contract
	DryRun      bool        `json:"dry_run"`      //nolint:tagliatelle // brief outbound contract
	Results     []CloseItem `json:"results"`
	Warnings    []string    `json:"warnings"`
}

// CloseItem is one tab's close outcome.
type CloseItem struct {
	TabID       string    `json:"tab_id"`                 //nolint:tagliatelle // brief outbound contract
	WorkspaceID string    `json:"workspace_id,omitempty"` //nolint:tagliatelle // brief outbound contract
	Ticket      string    `json:"ticket,omitempty"`
	Action      string    `json:"action"`
	Detail      string    `json:"detail,omitempty"`
	PR          *OrphanPR `json:"pr,omitempty"`
}

// CloseMerged closes tabs classified as merged orphans. Dry-run never calls
// herdr or GitHub. Live send re-verifies each candidate's PR is still
// merged/closed immediately before close so a newly opened PR cannot be
// closed by a stale classification. no_pr and unknown tabs are never
// candidates. Callers should consult UntrustedCloseReason first.
func CloseMerged(ctx context.Context, deps CloseDeps, cfg config.Config, author string, orphans []OrphanTab, now time.Time) (CloseDocument, error) {
	doc := CloseDocument{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		DryRun:      cfg.DryRun,
		Results:     []CloseItem{},
		Warnings:    []string{},
	}
	var merged []OrphanTab
	for _, tab := range orphans {
		if tab.Bucket == BucketMerged {
			merged = append(merged, tab)
		}
	}
	if cfg.DryRun {
		for _, tab := range merged {
			doc.Results = append(doc.Results, wouldCloseItem(tab))
		}
		return doc, nil
	}
	if len(merged) == 0 {
		return doc, nil
	}
	present, err := presentTabs(ctx, deps.Herdr)
	if err != nil {
		return doc, err
	}
	for _, tab := range merged {
		if err := ctx.Err(); err != nil {
			item := baseCloseItem(tab)
			item.Action = ActionFailed
			item.Detail = err.Error()
			doc.Results = append(doc.Results, item)
			return doc, fmt.Errorf("close merged: %w", err)
		}
		item, err := closeOne(ctx, deps, cfg, author, present, tab)
		doc.Results = append(doc.Results, item)
		if err != nil {
			return doc, err
		}
	}
	return doc, nil
}

func wouldCloseItem(tab OrphanTab) CloseItem {
	item := baseCloseItem(tab)
	item.Action = ActionWouldClose
	return item
}

func baseCloseItem(tab OrphanTab) CloseItem {
	return CloseItem{
		TabID:       tab.TabID,
		WorkspaceID: tab.WorkspaceID,
		Ticket:      tab.Ticket,
		PR:          tab.PR,
	}
}

func presentTabs(ctx context.Context, h TabCloser) (map[string]struct{}, error) {
	tabs, err := h.TabList(ctx)
	if err != nil {
		return nil, fmt.Errorf("tab list: %w", err)
	}
	present := make(map[string]struct{}, len(tabs))
	for _, tab := range tabs {
		present[tab.TabID] = struct{}{}
	}
	return present, nil
}

func closeOne(ctx context.Context, deps CloseDeps, cfg config.Config, author string, present map[string]struct{}, tab OrphanTab) (CloseItem, error) {
	item := baseCloseItem(tab)
	if _, ok := present[tab.TabID]; !ok {
		item.Action = ActionSkippedNotFound
		return item, nil
	}
	still, pr, err := stillMerged(ctx, deps.GH, cfg, author, tab.Ticket)
	if err != nil {
		item.Action = ActionFailed
		item.Detail = err.Error()
		return item, fmt.Errorf("%w: %w", ErrCloseFailed, err)
	}
	if !still {
		item.Action = ActionSkippedNotMerged
		if pr != nil {
			item.PR = pr
		}
		return item, nil
	}
	if pr != nil {
		item.PR = pr
	}
	if err := deps.Herdr.TabClose(ctx, tab.TabID); err != nil {
		if errors.Is(err, herdr.ErrTabNotFound) {
			item.Action = ActionSkippedNotFound
			return item, nil
		}
		item.Action = ActionFailed
		item.Detail = err.Error()
		return item, fmt.Errorf("%w: %w", ErrCloseFailed, err)
	}
	item.Action = ActionClosed
	return item, nil
}

func stillMerged(ctx context.Context, client OrphanGH, cfg config.Config, author, ticket string) (bool, *OrphanPR, error) {
	items, err := client.SearchAuthoredPRs(ctx, author, ticket)
	if err != nil {
		return false, nil, fmt.Errorf("search prs %s: %w", ticket, err)
	}
	candidates := filterByTicket(items, ticket, cfg.TitleIDPattern)
	if hasOpenPR(candidates) {
		return false, nil, nil
	}
	resolving := pickResolving(candidates)
	if resolving == nil {
		return false, nil, nil
	}
	return true, toOrphanPR(*resolving), nil
}
