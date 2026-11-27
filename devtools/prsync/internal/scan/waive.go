package scan

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// WaiveGH is the optional GitHub surface used to fetch PR waive timelines.
type WaiveGH interface {
	WaiveTimeline(ctx context.Context, owner, repo string, number int) (gh.WaiveTimeline, error)
}

// MatchLatestWaiveComment returns the latest comment whose exact first line
// equals prefix and whose author (when authors is non-empty) is in authors.
// The returned comment preserves Body byte for byte.
func MatchLatestWaiveComment(comments []gh.WaiveComment, prefix string, authors []string) (gh.WaiveComment, bool) {
	if prefix == "" {
		return gh.WaiveComment{}, false
	}
	var (
		best  gh.WaiveComment
		found bool
	)
	for _, c := range comments {
		if commentFirstLine(c.Body) != prefix {
			continue
		}
		if len(authors) > 0 && (c.Author == "" || !containsFold(authors, c.Author)) {
			continue
		}
		if !found || !c.CreatedAt.Before(best.CreatedAt) {
			best = c
			found = true
		}
	}
	return best, found
}

func commentFirstLine(body string) string {
	line, _, _ := strings.Cut(body, "\n")
	return strings.TrimSuffix(line, "\r")
}

// ResolveWaivedHead determines the PR head commit covered by a waive comment
// posted at waiveAt, and reports whether a force-push happened after waiveAt.
func ResolveWaivedHead(_ string, waiveAt time.Time, commits []gh.PRCommit, forcePushes []gh.ForcePushEvent) (oldHead string, forcePushedAfter bool) {
	sorted := slices.Clone(forcePushes)
	slices.SortStableFunc(sorted, func(a, b gh.ForcePushEvent) int {
		return a.CreatedAt.Compare(b.CreatedAt)
	})
	var lastBefore *gh.ForcePushEvent
	for i := range sorted {
		fp := &sorted[i]
		if fp.CreatedAt.After(waiveAt) {
			return fp.BeforeOID, true
		}
		lastBefore = fp
	}
	for _, c := range commits {
		t := commitEventTime(c)
		if t.IsZero() {
			continue
		}
		if t.After(waiveAt) {
			break
		}
		oldHead = c.OID
	}
	if oldHead == "" && lastBefore != nil && lastBefore.AfterOID != "" {
		oldHead = lastBefore.AfterOID
	}
	return oldHead, false
}

func commitEventTime(c gh.PRCommit) time.Time {
	if !c.PushedAt.IsZero() {
		return c.PushedAt
	}
	return c.CommittedAt
}

// FailingWaivedChecks returns unique failing check names from checks. When
// waiveChecks is non-empty, only failing checks whose Name or Context is in
// waiveChecks are included.
func FailingWaivedChecks(checks []gh.StatusCheck, statusCheckErr string, waiveChecks []string) []string {
	out := []string{}
	if statusCheckErr != "" {
		return out
	}
	seen := make(map[string]struct{})
	for _, check := range checks {
		if !isFailingCheck(check) {
			continue
		}
		name := matchingWaivedName(check, waiveChecks)
		if name == "" {
			continue
		}
		if _, ok := seen[name]; ok {
			continue
		}
		seen[name] = struct{}{}
		out = append(out, name)
	}
	return out
}

func matchingWaivedName(check gh.StatusCheck, waiveChecks []string) string {
	if len(waiveChecks) == 0 {
		return check.DisplayName()
	}
	if check.Name != "" && slices.Contains(waiveChecks, check.Name) {
		return check.Name
	}
	if check.Context != "" && slices.Contains(waiveChecks, check.Context) {
		return check.Context
	}
	return ""
}

// RequiredCheckOutcome classifies waive_require_green checks against a PR's
// status check rollup.
type RequiredCheckOutcome struct {
	Failing []string
	Pending []string
	Missing []string
}

type requiredCheckState int

const (
	requiredCheckGreen requiredCheckState = iota
	requiredCheckPending
	requiredCheckFailing
	requiredCheckMissing
)

// EvaluateRequiredChecks evaluates each unique name in required against checks.
// A check is green only when every matching entry completed with success;
// failing beats pending when multiple entries share a name, and an absent name
// is recorded in Missing.
func EvaluateRequiredChecks(checks []gh.StatusCheck, required []string) RequiredCheckOutcome {
	var out RequiredCheckOutcome
	seen := make(map[string]struct{}, len(required))
	for _, name := range required {
		if name == "" {
			continue
		}
		if _, ok := seen[name]; ok {
			continue
		}
		seen[name] = struct{}{}
		switch evaluateOneRequiredCheck(checks, name) {
		case requiredCheckFailing:
			out.Failing = append(out.Failing, name)
		case requiredCheckPending:
			out.Pending = append(out.Pending, name)
		case requiredCheckMissing:
			out.Missing = append(out.Missing, name)
		}
	}
	return out
}

func evaluateOneRequiredCheck(checks []gh.StatusCheck, name string) requiredCheckState {
	matched := false
	pending := false
	for _, check := range checks {
		if !checkMatchesName(check, name) {
			continue
		}
		matched = true
		switch classifyRequiredCheck(check) {
		case requiredCheckFailing:
			return requiredCheckFailing
		case requiredCheckPending:
			pending = true
		}
	}
	if !matched {
		return requiredCheckMissing
	}
	if pending {
		return requiredCheckPending
	}
	return requiredCheckGreen
}

func checkMatchesName(check gh.StatusCheck, name string) bool {
	return check.Name == name || check.Context == name
}

func classifyRequiredCheck(check gh.StatusCheck) requiredCheckState {
	if check.State != "" {
		switch check.State {
		case "SUCCESS":
			return requiredCheckGreen
		case "PENDING", "EXPECTED":
			return requiredCheckPending
		default:
			return requiredCheckFailing
		}
	}
	if check.Status != "COMPLETED" {
		return requiredCheckPending
	}
	if check.Conclusion == "SUCCESS" {
		return requiredCheckGreen
	}
	return requiredCheckFailing
}

func detectStaleWaives(ctx context.Context, deps Deps, cfg config.Config, doc *Document) error {
	if cfg.WaiveCommentPrefix == "" {
		return nil
	}
	wgh, ok := deps.GH.(WaiveGH)
	if !ok {
		return nil
	}
	for i := range doc.PRs {
		pr := &doc.PRs[i]
		if pr.OutOfScope != "" {
			continue
		}
		if pr.State != "" && pr.State != "open" {
			continue
		}
		if err := detectPRStaleWaive(ctx, wgh, deps, cfg, pr, doc); err != nil {
			return err
		}
	}
	return nil
}

func detectPRStaleWaive(ctx context.Context, wgh WaiveGH, deps Deps, cfg config.Config, pr *PR, doc *Document) error {
	owner, name, err := splitRepo(pr.Repo)
	if err != nil {
		return err
	}
	timeline, err := wgh.WaiveTimeline(ctx, owner, name, pr.Number)
	if err != nil {
		return fmt.Errorf("waive timeline %s#%d: %w", pr.Repo, pr.Number, err)
	}
	waive, ok := MatchLatestWaiveComment(timeline.Comments, cfg.WaiveCommentPrefix, cfg.WaiveAuthors)
	if !ok {
		return nil
	}
	oldHead, forcePushedAfter := ResolveWaivedHead(pr.HeadSHA, waive.CreatedAt, timeline.Commits, timeline.ForcePushes)
	if oldHead == "" {
		warn(doc, fmt.Sprintf("stale waive check skipped for %s#%d: cannot resolve head at %s", pr.Repo, pr.Number, waive.CreatedAt.UTC().Format(time.RFC3339)))
		return nil
	}
	if oldHead == pr.HeadSHA {
		return nil
	}
	class, classErr := classifyStaleWaive(ctx, deps, cfg, owner, name, pr, oldHead, forcePushedAfter, timeline.Commits)
	sw := &StaleWaive{
		OldHead:       oldHead,
		NewHead:       pr.HeadSHA,
		Class:         class,
		FailingChecks: FailingWaivedChecks(pr.statusChecks, pr.statusCheckErr, cfg.WaiveChecks),
		CommentID:     waive.ID,
		CommentAuthor: waive.Author,
		CommentURL:    waive.URL,
		CommentBody:   waive.Body,
	}
	if pr.statusCheckErr == "" {
		sw.StatusChecks = slices.Clone(pr.statusChecks)
	}
	if classErr != nil {
		sw.Error = classErr.Error()
		warn(doc, fmt.Sprintf("waive classification for %s#%d defaulted to %s: %s", pr.Repo, pr.Number, WaiveClassAuthorChanges, classErr.Error()))
	}
	pr.StaleWaive = sw
	return nil
}

// FormatStaleWaiveLines renders stale waives as one human-readable line per PR.
func FormatStaleWaiveLines(doc Document) string {
	var b strings.Builder
	for _, pr := range doc.PRs {
		if pr.StaleWaive == nil {
			continue
		}
		sw := pr.StaleWaive
		failing := "none"
		if len(sw.FailingChecks) > 0 {
			failing = strings.Join(sw.FailingChecks, ",")
		}
		fmt.Fprintf(&b, "%s#%d stale waive: old=%s new=%s class=%s failing=%s",
			pr.Repo, pr.Number, sw.OldHead, sw.NewHead, sw.Class, failing)
		if sw.Error != "" {
			b.WriteString(" error=" + sw.Error)
		}
		b.WriteByte('\n')
	}
	return b.String()
}
