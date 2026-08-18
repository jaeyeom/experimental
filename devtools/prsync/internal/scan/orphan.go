package scan

import (
	"context"
	"fmt"
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

// Orphan tab buckets. has_open_pr tabs are not orphans and are omitted.
const (
	BucketMerged = "merged"
	BucketNoPR   = "no_pr"
)

// liveAgentStatuses are the herdr statuses that mean an agent session exists.
var liveAgentStatuses = map[string]struct{}{
	"working": {},
	"idle":    {},
	"blocked": {},
}

// OrphanGH is the GitHub surface the orphan report uses.
type OrphanGH interface {
	AuthStatus(ctx context.Context) error
	UserLogin(ctx context.Context) (string, error)
	SearchAuthoredPRs(ctx context.Context, author, query string) ([]gh.PRSearchItem, error)
}

// OrphanDeps is the injected GitHub and herdr adapters for the orphan report.
type OrphanDeps struct {
	GH    OrphanGH
	Herdr Herdr
}

// OrphanDocument is the outbound `tabs --orphans` JSON document.
type OrphanDocument struct {
	GeneratedAt string      `json:"generated_at"` //nolint:tagliatelle // brief outbound contract
	Author      string      `json:"author"`
	OrphanTabs  []OrphanTab `json:"orphan_tabs"` //nolint:tagliatelle // brief outbound contract
	Warnings    []string    `json:"warnings"`
}

// OrphanTab is one live-agent tab with no open or draft PR.
type OrphanTab struct {
	TabID       string    `json:"tab_id"`       //nolint:tagliatelle // brief outbound contract
	WorkspaceID string    `json:"workspace_id"` //nolint:tagliatelle // brief outbound contract
	Label       string    `json:"label"`
	Ticket      string    `json:"ticket"`
	AgentStatus string    `json:"agent_status"` //nolint:tagliatelle // brief outbound contract
	Bucket      string    `json:"bucket"`
	PR          *OrphanPR `json:"pr"`
}

// OrphanPR is the PR that resolves a merged orphan tab. MergedAt is nil for a
// closed-but-unmerged PR (also safe to reclaim, but never merged).
type OrphanPR struct {
	Repo     string  `json:"repo"`
	Number   int     `json:"number"`
	URL      string  `json:"url"`
	State    string  `json:"state"`
	MergedAt *string `json:"merged_at"` //nolint:tagliatelle // brief outbound contract
}

// OrphansStarted reports whether the report reached the point of emitting a
// document (auth passed and the author resolved).
func OrphansStarted(doc OrphanDocument) bool {
	return doc.Author != ""
}

// Orphans walks live-agent herdr tabs and reports the ones with no open or
// draft PR. openTabs is the set of tab ids a caller-supplied scan already
// matched to an open PR; those are skipped without a GitHub search. A non-nil
// error is fatal after the caller emits the document when OrphansStarted.
func Orphans(ctx context.Context, deps OrphanDeps, cfg config.Config, openTabs map[string]struct{}, now time.Time) (OrphanDocument, error) {
	doc := OrphanDocument{
		GeneratedAt: now.UTC().Format(time.RFC3339),
		OrphanTabs:  []OrphanTab{},
		Warnings:    []string{},
	}
	if err := deps.GH.AuthStatus(ctx); err != nil {
		return doc, fmt.Errorf("gh auth: %w", err)
	}
	author, err := resolveOrphanAuthor(ctx, deps.GH, cfg.Author)
	if err != nil {
		return doc, err
	}
	doc.Author = author

	if err := deps.Herdr.RequireMin(ctx, herdrMinVersion); err != nil {
		return doc, fmt.Errorf("herdr version: %w", err)
	}
	tabs, err := deps.Herdr.TabList(ctx)
	if err != nil {
		return doc, fmt.Errorf("tab list: %w", err)
	}
	agents, err := deps.Herdr.AgentList(ctx)
	if err != nil {
		return doc, fmt.Errorf("agent list: %w", err)
	}

	live := liveAgentsByTab(agents)
	for _, tab := range tabs {
		if err := ctx.Err(); err != nil {
			return doc, fmt.Errorf("orphans: %w", err)
		}
		orphan, ok, err := classifyTab(ctx, deps.GH, cfg, author, tab, live, openTabs)
		if err != nil {
			return doc, err
		}
		if ok {
			doc.OrphanTabs = append(doc.OrphanTabs, orphan)
		}
	}
	return doc, nil
}

func resolveOrphanAuthor(ctx context.Context, client OrphanGH, configured string) (string, error) {
	if configured != "" {
		return configured, nil
	}
	login, err := client.UserLogin(ctx)
	if err != nil {
		return "", fmt.Errorf("resolve author: %w", err)
	}
	return login, nil
}

// liveAgentsByTab maps a tab id to the status of its first live agent. A tab
// absent from the map has no running agent session (an agent-less scratch tab).
func liveAgentsByTab(agents []herdr.Agent) map[string]string {
	out := make(map[string]string)
	for _, agent := range agents {
		if _, ok := liveAgentStatuses[agent.AgentStatus]; !ok {
			continue
		}
		if _, seen := out[agent.TabID]; seen {
			continue
		}
		out[agent.TabID] = agent.AgentStatus
	}
	return out
}

func classifyTab(ctx context.Context, client OrphanGH, cfg config.Config, author string, tab herdr.Tab, live map[string]string, openTabs map[string]struct{}) (OrphanTab, bool, error) {
	status, ok := live[tab.TabID]
	if !ok {
		return OrphanTab{}, false, nil
	}
	ticketPtr := ExtractID(tab.Label, cfg.TitleIDPattern)
	if ticketPtr == nil {
		return OrphanTab{}, false, nil
	}
	ticket := *ticketPtr
	if _, matched := openTabs[tab.TabID]; matched {
		return OrphanTab{}, false, nil
	}

	items, err := client.SearchAuthoredPRs(ctx, author, ticket)
	if err != nil {
		return OrphanTab{}, false, fmt.Errorf("search prs %s: %w", ticket, err)
	}
	candidates := filterByTicket(items, ticket, cfg.TitleIDPattern)
	if hasOpenPR(candidates) {
		return OrphanTab{}, false, nil
	}

	orphan := OrphanTab{
		TabID:       tab.TabID,
		WorkspaceID: tab.WorkspaceID,
		Label:       tab.Label,
		Ticket:      ticket,
		AgentStatus: status,
	}
	resolving := pickResolving(candidates)
	if resolving == nil {
		orphan.Bucket = BucketNoPR
		return orphan, true, nil
	}
	orphan.Bucket = BucketMerged
	orphan.PR = toOrphanPR(*resolving)
	return orphan, true, nil
}

// filterByTicket keeps only PRs whose title yields exactly this ticket via
// title_id_pattern, so a body-only mention never counts as the resolving PR.
func filterByTicket(items []gh.PRSearchItem, ticket string, re *regexp.Regexp) []gh.PRSearchItem {
	out := make([]gh.PRSearchItem, 0, len(items))
	for _, item := range items {
		if id := ExtractID(item.Title, re); id != nil && *id == ticket {
			out = append(out, item)
		}
	}
	return out
}

func hasOpenPR(items []gh.PRSearchItem) bool {
	for _, item := range items {
		if item.State == "open" {
			return true
		}
	}
	return false
}

// pickResolving returns the PR that best explains a reclaimable tab: a merged
// PR over a closed-unmerged one, and the most recently closed within a group.
func pickResolving(items []gh.PRSearchItem) *gh.PRSearchItem {
	var merged, closed []gh.PRSearchItem
	for _, item := range items {
		switch item.State {
		case "merged":
			merged = append(merged, item)
		case "closed":
			closed = append(closed, item)
		}
	}
	pool := merged
	if len(pool) == 0 {
		pool = closed
	}
	if len(pool) == 0 {
		return nil
	}
	best := pool[0]
	for _, item := range pool[1:] {
		if item.ClosedAt > best.ClosedAt || (item.ClosedAt == best.ClosedAt && item.Number > best.Number) {
			best = item
		}
	}
	return &best
}

func toOrphanPR(item gh.PRSearchItem) *OrphanPR {
	pr := &OrphanPR{
		Repo:   item.Repository.NameWithOwner,
		Number: item.Number,
		URL:    item.URL,
		State:  item.State,
	}
	if item.State == "merged" && !isZeroTime(item.ClosedAt) {
		at := item.ClosedAt
		pr.MergedAt = &at
	}
	return pr
}

func isZeroTime(s string) bool {
	return s == "" || strings.HasPrefix(s, "0001-01-01")
}
