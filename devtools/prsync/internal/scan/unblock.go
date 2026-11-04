package scan

import (
	"context"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/jira"
)

// Unblock statuses on a close document.
const (
	UnblockOK     = "ok"
	UnblockNotRun = "not_run"
	UnblockFailed = "failed"
)

// Unblock line kinds.
const (
	UnblockClear        = "clear"
	UnblockStillBlocked = "still_blocked"
)

const doneCategory = "done"

// UnblockReport is the blocks-graph result copied into the post-go follow-up.
type UnblockReport struct {
	Status string        `json:"status"`
	Text   string        `json:"text"`
	Lines  []UnblockLine `json:"lines"`
}

// UnblockLine is one ticket the closed round affected.
type UnblockLine struct {
	Kind      string   `json:"kind"`
	Ticket    string   `json:"ticket"`
	Summary   string   `json:"summary"`
	BlockedBy []string `json:"blocked_by"` //nolint:tagliatelle // brief outbound contract
}

// IssueSource reads one Jira issue. *jira.Client implements it.
type IssueSource interface {
	ViewIssue(ctx context.Context, key string) (jira.Issue, error)
}

// AttachUnblock sets doc.Unblock from the close results. A failed check does
// not invent a freed ticket. Callers still print the document when close
// itself fails.
func AttachUnblock(ctx context.Context, src IssueSource, doc *CloseDocument) {
	doc.Unblock = unblockFrom(ctx, src, doc.Results)
}

func unblockFrom(ctx context.Context, src IssueSource, results []CloseItem) UnblockReport {
	var delivered []string
	closed := 0
	seen := map[string]struct{}{}
	for _, item := range results {
		if item.Action != ActionClosed {
			continue
		}
		closed++
		if !deliveredRow(item) {
			continue
		}
		if _, ok := seen[item.Ticket]; ok {
			continue
		}
		seen[item.Ticket] = struct{}{}
		delivered = append(delivered, item.Ticket)
	}
	if closed == 0 {
		return UnblockReport{Status: UnblockNotRun, Lines: []UnblockLine{}}
	}
	if len(delivered) == 0 {
		return okNothing()
	}
	sort.Strings(delivered)
	return walkUnblock(ctx, src, delivered)
}

func deliveredRow(item CloseItem) bool {
	if item.Ticket == "" || item.PR == nil || item.PR.MergedAt == nil {
		return false
	}
	return strings.TrimSpace(*item.PR.MergedAt) != ""
}

func okNothing() UnblockReport {
	return UnblockReport{Status: UnblockOK, Text: "nothing newly unblocked", Lines: []UnblockLine{}}
}

func unblockFailed(reason string) UnblockReport {
	return UnblockReport{
		Status: UnblockFailed,
		Text:   "unblock check could not run: " + reason,
		Lines:  []UnblockLine{},
	}
}

type candidate struct {
	summary       string
	inlineStatus  *jira.Status
	fromDelivered map[string]struct{}
}

func walkUnblock(ctx context.Context, src IssueSource, delivered []string) UnblockReport {
	deliveredSet := map[string]struct{}{}
	for _, key := range delivered {
		deliveredSet[key] = struct{}{}
	}
	cache := map[string]jira.Issue{}
	found, failed := collectUnblocked(ctx, src, cache, delivered)
	if failed != nil {
		return *failed
	}
	lines, failed := unblockLines(ctx, src, cache, deliveredSet, found)
	if failed != nil {
		return *failed
	}
	if len(lines) == 0 {
		return okNothing()
	}
	sort.Slice(lines, func(i, j int) bool { return lines[i].Ticket < lines[j].Ticket })
	return UnblockReport{Status: UnblockOK, Text: renderUnblock(lines), Lines: lines}
}

func viewCached(ctx context.Context, src IssueSource, cache map[string]jira.Issue, key string) (jira.Issue, *UnblockReport) {
	if issue, ok := cache[key]; ok {
		return issue, nil
	}
	issue, err := src.ViewIssue(ctx, key)
	if err != nil {
		failed := unblockFailed(err.Error())
		return jira.Issue{}, &failed
	}
	cache[key] = issue
	return issue, nil
}

func collectUnblocked(ctx context.Context, src IssueSource, cache map[string]jira.Issue, delivered []string) (map[string]*candidate, *UnblockReport) {
	found := map[string]*candidate{}
	for _, ticket := range delivered {
		issue, failed := viewCached(ctx, src, cache, ticket)
		if failed != nil {
			return nil, failed
		}
		for _, link := range issue.Links {
			if link.Outward != "blocks" || link.OutwardKey == "" {
				continue
			}
			c := found[link.OutwardKey]
			if c == nil {
				c = &candidate{fromDelivered: map[string]struct{}{}}
				found[link.OutwardKey] = c
			}
			c.fromDelivered[ticket] = struct{}{}
			if c.summary == "" {
				c.summary = link.OutwardSummary
			}
			if c.inlineStatus == nil {
				c.inlineStatus = link.OutwardStatus
			}
		}
	}
	return found, nil
}

func unblockLines(ctx context.Context, src IssueSource, cache map[string]jira.Issue, delivered map[string]struct{}, found map[string]*candidate) ([]UnblockLine, *UnblockReport) {
	keys := make([]string, 0, len(found))
	for key := range found {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	lines := make([]UnblockLine, 0)
	for _, key := range keys {
		line, failed := oneUnblockLine(ctx, src, cache, delivered, found[key], key)
		if failed != nil {
			return nil, failed
		}
		if line != nil {
			lines = append(lines, *line)
		}
	}
	return lines, nil
}

func oneUnblockLine(ctx context.Context, src IssueSource, cache map[string]jira.Issue, delivered map[string]struct{}, c *candidate, key string) (*UnblockLine, *UnblockReport) {
	if c.inlineStatus != nil && c.inlineStatus.CategoryKey == doneCategory {
		return nil, nil
	}
	issue, failed := viewCached(ctx, src, cache, key)
	if failed != nil {
		return nil, failed
	}
	if issue.Status == nil {
		failed := unblockFailed(fmt.Sprintf("issue %s missing status", key))
		return nil, &failed
	}
	if issue.Status.CategoryKey == doneCategory {
		return nil, nil
	}
	summary := c.summary
	if summary == "" {
		summary = issue.Summary
	}
	return classifyUnblock(ctx, src, cache, delivered, c, issue, key, summary)
}

func classifyUnblock(ctx context.Context, src IssueSource, cache map[string]jira.Issue, delivered map[string]struct{}, c *candidate, issue jira.Issue, ticket, summary string) (*UnblockLine, *UnblockReport) {
	blockers := map[string]struct{}{}
	for key := range c.fromDelivered {
		blockers[key] = struct{}{}
	}
	var open []string
	for _, link := range issue.Links {
		if link.Inward != "is blocked by" || link.InwardKey == "" {
			continue
		}
		blocker := link.InwardKey
		if _, ok := delivered[blocker]; ok {
			blockers[blocker] = struct{}{}
			continue
		}
		status := link.InwardStatus
		if status == nil {
			fetched, failed := viewCached(ctx, src, cache, blocker)
			if failed != nil {
				return nil, failed
			}
			if fetched.Status == nil {
				failed := unblockFailed(fmt.Sprintf("issue %s missing status", blocker))
				return nil, &failed
			}
			status = fetched.Status
		}
		if status.CategoryKey == doneCategory {
			continue
		}
		open = append(open, blocker)
	}
	if len(open) > 0 {
		sort.Strings(open)
		return &UnblockLine{
			Kind: UnblockStillBlocked, Ticket: ticket, Summary: summary, BlockedBy: open,
		}, nil
	}
	by := mapKeys(blockers)
	return &UnblockLine{
		Kind: UnblockClear, Ticket: ticket, Summary: summary, BlockedBy: by,
	}, nil
}

func mapKeys(set map[string]struct{}) []string {
	out := make([]string, 0, len(set))
	for key := range set {
		out = append(out, key)
	}
	sort.Strings(out)
	return out
}

func renderUnblock(lines []UnblockLine) string {
	var b strings.Builder
	b.WriteString("Newly unblocked by this round")
	for _, line := range lines {
		b.WriteByte('\n')
		b.WriteString(line.Ticket)
		b.WriteByte(' ')
		b.WriteString(strconv.Quote(line.Summary))
		switch line.Kind {
		case UnblockClear:
			b.WriteString(" — was blocked only by ")
			b.WriteString(strings.Join(line.BlockedBy, ", "))
			b.WriteString(" (just merged); now clear to start")
		case UnblockStillBlocked:
			b.WriteString(" still blocked by ")
			b.WriteString(strings.Join(line.BlockedBy, ", "))
		}
	}
	return b.String()
}
