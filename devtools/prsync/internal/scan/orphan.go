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
// unknown means a multi-ticket search chunk mapped no PR onto any ticket.
const (
	BucketMerged  = "merged"
	BucketNoPR    = "no_pr"
	BucketUnknown = "unknown"
)

// GitHub search rejects queries with more than five AND/OR/NOT operators
// or more than 256 characters.
const (
	maxSearchORTerms  = 6
	maxSearchQueryLen = 256
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

// OrphanTab is one live-agent tab with no open or draft PR, or whose
// search classification is unknown.
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
// draft PR. A no_pr tab whose agent is still working is omitted (in-progress,
// not reclaimable). openTabs is the set of tab ids a caller-supplied scan
// already matched to an open PR; those are skipped without a GitHub search.
// Remaining tickets are searched in OR-batched GitHub queries so a handful of
// orphan tabs cannot trip the search secondary rate limit. A chunk that maps
// no PR onto any of its tickets is unknown (not no_pr) and warned.
// A non-nil error is fatal after the caller emits the document when
// OrphansStarted.
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
	var pending []orphanCandidate
	ticketSeen := make(map[string]struct{})
	var tickets []string
	for _, tab := range tabs {
		if err := ctx.Err(); err != nil {
			return doc, fmt.Errorf("orphans: %w", err)
		}
		c, ok := candidateFromTab(tab, live, openTabs, cfg.TitleIDPattern)
		if !ok {
			continue
		}
		pending = append(pending, c)
		if _, seen := ticketSeen[c.ticket]; seen {
			continue
		}
		ticketSeen[c.ticket] = struct{}{}
		tickets = append(tickets, c.ticket)
	}

	items, unknown, warnings, err := searchAuthoredTickets(ctx, deps.GH, author, tickets, cfg.TitleIDPattern)
	if err != nil {
		return doc, err
	}
	doc.Warnings = append(doc.Warnings, warnings...)
	for _, c := range pending {
		if err := ctx.Err(); err != nil {
			return doc, fmt.Errorf("orphans: %w", err)
		}
		orphan, ok := classifyCandidate(cfg, c, items, unknown)
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

type orphanCandidate struct {
	tab    herdr.Tab
	ticket string
	status string
}

func candidateFromTab(tab herdr.Tab, live map[string]string, openTabs map[string]struct{}, re *regexp.Regexp) (orphanCandidate, bool) {
	status, ok := live[tab.TabID]
	if !ok {
		return orphanCandidate{}, false
	}
	ticketPtr := ExtractID(tab.Label, re)
	if ticketPtr == nil {
		return orphanCandidate{}, false
	}
	if _, matched := openTabs[tab.TabID]; matched {
		return orphanCandidate{}, false
	}
	return orphanCandidate{tab: tab, ticket: *ticketPtr, status: status}, true
}

// ticketSearchQueries splits tickets into GitHub search queries under the
// five-OR and 256-character limits. A single ticket is passed through as-is.
// Tokens are space-separated (`A OR B`) so SearchAuthoredPRs can pass each as
// its own argv; wrapping the whole expression in parentheses would make gh
// phrase-quote it and match nothing.
func ticketSearchQueries(tickets []string) []string {
	chunks := ticketSearchChunks(tickets)
	if chunks == nil {
		return nil
	}
	queries := make([]string, len(chunks))
	for i, chunk := range chunks {
		queries[i] = searchQuery(chunk)
	}
	return queries
}

func ticketSearchChunks(tickets []string) [][]string {
	if len(tickets) == 0 {
		return nil
	}
	var chunks [][]string
	var chunk []string
	for _, ticket := range tickets {
		trial := searchQuery(append(append([]string{}, chunk...), ticket))
		if len(chunk) > 0 && (len(chunk)+1 > maxSearchORTerms || len(trial) > maxSearchQueryLen) {
			chunks = append(chunks, chunk)
			chunk = []string{ticket}
			continue
		}
		chunk = append(chunk, ticket)
	}
	return append(chunks, chunk)
}

func searchQuery(tickets []string) string {
	return strings.Join(tickets, " OR ")
}

func searchAuthoredTickets(ctx context.Context, client OrphanGH, author string, tickets []string, re *regexp.Regexp) ([]gh.PRSearchItem, map[string]struct{}, []string, error) {
	var all []gh.PRSearchItem
	unknown := make(map[string]struct{})
	var warnings []string
	for _, chunk := range ticketSearchChunks(tickets) {
		if err := ctx.Err(); err != nil {
			return nil, nil, warnings, fmt.Errorf("search prs: %w", err)
		}
		query := searchQuery(chunk)
		items, err := client.SearchAuthoredPRs(ctx, author, query)
		if err != nil {
			return nil, nil, warnings, fmt.Errorf("search prs %s: %w", query, err)
		}
		all = append(all, items...)
		if len(chunk) > 1 && !chunkMappedAny(items, chunk, re) {
			warnings = append(warnings, fmt.Sprintf(
				"search %q returned no matching PRs for any of %d tickets; treating as unknown",
				query, len(chunk)))
			for _, ticket := range chunk {
				unknown[ticket] = struct{}{}
			}
		}
	}
	if all == nil {
		all = []gh.PRSearchItem{}
	}
	return all, unknown, warnings, nil
}

func chunkMappedAny(items []gh.PRSearchItem, tickets []string, re *regexp.Regexp) bool {
	wanted := make(map[string]struct{}, len(tickets))
	for _, ticket := range tickets {
		wanted[ticket] = struct{}{}
	}
	for _, item := range items {
		if id := ExtractID(item.Title, re); id != nil {
			if _, ok := wanted[*id]; ok {
				return true
			}
		}
	}
	return false
}

func classifyCandidate(cfg config.Config, c orphanCandidate, items []gh.PRSearchItem, unknown map[string]struct{}) (OrphanTab, bool) {
	orphan := OrphanTab{
		TabID:       c.tab.TabID,
		WorkspaceID: c.tab.WorkspaceID,
		Label:       c.tab.Label,
		Ticket:      c.ticket,
		AgentStatus: c.status,
	}
	if _, bad := unknown[c.ticket]; bad {
		orphan.Bucket = BucketUnknown
		return orphan, true
	}
	prs := filterByTicket(items, c.ticket, cfg.TitleIDPattern)
	if hasOpenPR(prs) {
		return OrphanTab{}, false
	}
	resolving := pickResolving(prs)
	if resolving == nil {
		if c.status == "working" {
			// Active work that has not opened a PR yet is not an orphan.
			return OrphanTab{}, false
		}
		orphan.Bucket = BucketNoPR
		return orphan, true
	}
	orphan.Bucket = BucketMerged
	orphan.PR = toOrphanPR(*resolving)
	return orphan, true
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
