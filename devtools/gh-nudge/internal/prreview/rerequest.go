package prreview

import (
	"encoding/json"
	"fmt"
	"io"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// Rerequest action values.
const (
	RerequestActionWouldRerequest = "would rerequest"
	RerequestActionRerequest      = "rerequest"
	RerequestActionSkip           = "skip"
	RerequestActionError          = "error"
)

// RerequestClient is the narrow GitHub boundary used by RunRerequest.
type RerequestClient interface {
	LoadRerequestPRState(ref github.PullRef) (github.RerequestPRState, error)
	RequestReviewers(ref github.PullRef, logins []string) error
}

// RerequestOptions controls how RunRerequest executes and formats output.
type RerequestOptions struct {
	Apply bool
	JSON  bool
}

// RerequestDecision records the rerequest evaluation for one reviewer (or the
// whole PR when Reviewer is empty).
type RerequestDecision struct {
	PR               int        `json:"pr"`
	Reviewer         string     `json:"reviewer"`
	Action           string     `json:"action"`
	Reason           string     `json:"reason"`
	ThreadsTotal     int        `json:"threads_total"`      //nolint:tagliatelle // CLI JSON schema uses snake_case
	ThreadsAnswered  int        `json:"threads_answered"`   //nolint:tagliatelle // CLI JSON schema uses snake_case
	LastReviewState  string     `json:"last_review_state"`  //nolint:tagliatelle // CLI JSON schema uses snake_case
	LastReviewAt     *time.Time `json:"last_review_at"`     //nolint:tagliatelle // CLI JSON schema uses snake_case
	AuthorActivityAt *time.Time `json:"author_activity_at"` //nolint:tagliatelle // CLI JSON schema uses snake_case
}

type reviewerSummary struct {
	login               string
	lastReviewAt        time.Time
	lastReviewState     string
	hasOpinionatedState bool
	lastOpinionatedAt   time.Time
}

// EvaluateRerequest evaluates all hard rerequest rules against pr and returns
// one decision per reviewer (sorted by login), or a single PR-level skip
// decision when the PR itself is ineligible (draft, closed, or merged).
// Eligible reviewers have Action set to RerequestActionWouldRerequest.
func EvaluateRerequest(pr github.RerequestPRState) []RerequestDecision {
	if pr.IsDraft {
		return []RerequestDecision{{
			PR:     pr.Number,
			Action: RerequestActionSkip,
			Reason: "PR is draft",
		}}
	}
	switch strings.ToUpper(pr.State) {
	case "CLOSED":
		return []RerequestDecision{{
			PR:     pr.Number,
			Action: RerequestActionSkip,
			Reason: "PR is closed",
		}}
	case "MERGED":
		return []RerequestDecision{{
			PR:     pr.Number,
			Action: RerequestActionSkip,
			Reason: "PR is merged",
		}}
	}

	reviewers := summarizeReviews(pr.Reviews)
	if len(reviewers) == 0 {
		return []RerequestDecision{}
	}

	pendingUsers := make(map[string]bool, len(pr.RequestedUsers))
	for _, u := range pr.RequestedUsers {
		pendingUsers[strings.ToLower(u)] = true
	}

	lastPushAt := latestTime(pr.AuthorPushTimes)

	decisions := make([]RerequestDecision, 0, len(reviewers))
	for _, rev := range reviewers {
		decisions = append(decisions, evaluateReviewer(pr, rev, pendingUsers, lastPushAt))
	}
	return decisions
}

func summarizeReviews(reviews []github.RerequestReview) []reviewerSummary {
	byLower := make(map[string]*reviewerSummary)
	for _, r := range reviews {
		if r.Author == "" || strings.EqualFold(r.State, "PENDING") {
			continue
		}
		key := strings.ToLower(r.Author)
		s, ok := byLower[key]
		if !ok {
			s = &reviewerSummary{login: r.Author}
			byLower[key] = s
		}
		if s.lastReviewAt.IsZero() || !r.SubmittedAt.Before(s.lastReviewAt) {
			s.lastReviewAt = r.SubmittedAt
			if !s.hasOpinionatedState {
				s.lastReviewState = r.State
			}
		}
		if isOpinionatedReviewState(r.State) {
			if !s.hasOpinionatedState || !r.SubmittedAt.Before(s.lastOpinionatedAt) {
				s.hasOpinionatedState = true
				s.lastOpinionatedAt = r.SubmittedAt
				s.lastReviewState = r.State
			}
		}
	}

	keys := make([]string, 0, len(byLower))
	for k := range byLower {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	result := make([]reviewerSummary, 0, len(keys))
	for _, k := range keys {
		result = append(result, *byLower[k])
	}
	return result
}

func isOpinionatedReviewState(state string) bool {
	switch strings.ToUpper(state) {
	case "APPROVED", "CHANGES_REQUESTED", "DISMISSED":
		return true
	default:
		return false
	}
}

func evaluateReviewer(
	pr github.RerequestPRState,
	rev reviewerSummary,
	pendingUsers map[string]bool,
	lastPushAt time.Time,
) RerequestDecision {
	threadsTotal, threadsAnswered, lastReplyAt := reviewerThreadStats(pr.Threads, rev.login, pr.Author)

	var authorActivityAt time.Time
	if !lastPushAt.IsZero() {
		authorActivityAt = lastPushAt
	}
	if !lastReplyAt.IsZero() && lastReplyAt.After(authorActivityAt) {
		authorActivityAt = lastReplyAt
	}

	lastReviewAt := rev.lastReviewAt
	d := RerequestDecision{
		PR:              pr.Number,
		Reviewer:        rev.login,
		ThreadsTotal:    threadsTotal,
		ThreadsAnswered: threadsAnswered,
		LastReviewState: rev.lastReviewState,
		LastReviewAt:    &lastReviewAt,
	}
	if !authorActivityAt.IsZero() {
		act := authorActivityAt
		d.AuthorActivityAt = &act
	}

	// Rule 4: skip PR author.
	if pr.Author != "" && strings.EqualFold(rev.login, pr.Author) {
		d.Action = RerequestActionSkip
		d.Reason = "reviewer is PR author"
		return d
	}

	// Rule 1: never re-request someone whose latest review state is APPROVED.
	if strings.EqualFold(rev.lastReviewState, "APPROVED") {
		d.Action = RerequestActionSkip
		d.Reason = "approved"
		return d
	}

	// Rule 4: skip if reviewer already has a pending review request.
	if pendingUsers[strings.ToLower(rev.login)] {
		d.Action = RerequestActionSkip
		d.Reason = "review request already pending"
		return d
	}

	// Rule 2: every review thread started by this reviewer must have an author reply.
	if threadsAnswered < threadsTotal {
		unanswered := threadsTotal - threadsAnswered
		d.Action = RerequestActionSkip
		d.Reason = fmt.Sprintf("%d of %d threads have no author reply", unanswered, threadsTotal)
		return d
	}

	// Rule 3: author must have pushed a commit or replied in one of this reviewer's
	// threads after the reviewer's latest review.
	pushedAfter := !lastPushAt.IsZero() && lastPushAt.After(rev.lastReviewAt)
	repliedAfter := !lastReplyAt.IsZero() && lastReplyAt.After(rev.lastReviewAt)
	if !pushedAfter && !repliedAfter {
		d.Action = RerequestActionSkip
		d.Reason = "no author activity since last review"
		return d
	}

	d.Action = RerequestActionWouldRerequest
	d.Reason = formatRerequestReason(pushedAfter, repliedAfter, threadsTotal)
	return d
}

func reviewerThreadStats(threads []github.RerequestThread, reviewer, prAuthor string) (int, int, time.Time) {
	var total, answered int
	var lastReplyAt time.Time
	for _, thread := range threads {
		if len(thread.Comments) == 0 {
			continue
		}
		if !strings.EqualFold(thread.Comments[0].Author, reviewer) {
			continue
		}
		total++
		hasAuthorReply := false
		for _, c := range thread.Comments[1:] {
			if prAuthor != "" && strings.EqualFold(c.Author, prAuthor) {
				hasAuthorReply = true
				if c.CreatedAt.After(lastReplyAt) {
					lastReplyAt = c.CreatedAt
				}
			}
		}
		if hasAuthorReply {
			answered++
		}
	}
	return total, answered, lastReplyAt
}

func latestTime(times []time.Time) time.Time {
	var latest time.Time
	for _, t := range times {
		if t.After(latest) {
			latest = t
		}
	}
	return latest
}

func formatRerequestReason(pushedAfter, repliedAfter bool, threadsTotal int) string {
	var activity string
	switch {
	case pushedAfter && repliedAfter:
		activity = "author pushed and replied after review"
	case pushedAfter:
		activity = "author pushed after review"
	default:
		activity = "author replied after review"
	}
	if threadsTotal == 0 {
		return activity
	}
	if threadsTotal == 1 {
		return activity + ", 1 thread answered"
	}
	return fmt.Sprintf("%s, all %d threads answered", activity, threadsTotal)
}

// RunRerequest loads PR state, evaluates rerequest decisions, optionally
// applies them via client.RequestReviewers, and writes formatted output to out.
func RunRerequest(client RerequestClient, ref github.PullRef, opts RerequestOptions, out io.Writer) error {
	prState, err := client.LoadRerequestPRState(ref)
	if err != nil {
		return fmt.Errorf("load pull request state: %w", err)
	}

	decisions := EvaluateRerequest(prState)

	var applyErr error
	if opts.Apply {
		var logins []string
		for _, d := range decisions {
			if d.Action == RerequestActionWouldRerequest {
				logins = append(logins, d.Reviewer)
			}
		}
		if len(logins) > 0 {
			if err := client.RequestReviewers(ref, logins); err != nil {
				applyErr = fmt.Errorf("request reviewers: %w", err)
				for i := range decisions {
					if decisions[i].Action == RerequestActionWouldRerequest {
						decisions[i].Action = RerequestActionError
						decisions[i].Reason = fmt.Sprintf("rerequest failed: %v", err)
					}
				}
			} else {
				for i := range decisions {
					if decisions[i].Action == RerequestActionWouldRerequest {
						decisions[i].Action = RerequestActionRerequest
					}
				}
			}
		}
	}

	var formatted string
	if opts.JSON {
		formatted, err = FormatRerequestJSON(decisions)
		if err != nil {
			return err
		}
		formatted += "\n"
	} else {
		formatted = FormatRerequestText(decisions)
	}
	if _, err := io.WriteString(out, formatted); err != nil {
		return fmt.Errorf("write rerequest output: %w", err)
	}

	return applyErr
}

// FormatRerequestText formats rerequest decisions as one line per decision.
func FormatRerequestText(decisions []RerequestDecision) string {
	var b strings.Builder
	for _, d := range decisions {
		if d.Reviewer == "" {
			fmt.Fprintf(&b, "#%d %s: %s\n", d.PR, d.Action, d.Reason)
		} else {
			fmt.Fprintf(&b, "#%d %s %s: %s\n", d.PR, d.Reviewer, d.Action, d.Reason)
		}
	}
	return b.String()
}

// FormatRerequestJSON formats rerequest decisions as an indented JSON array.
func FormatRerequestJSON(decisions []RerequestDecision) (string, error) {
	if decisions == nil {
		decisions = []RerequestDecision{}
	}
	data, err := json.MarshalIndent(decisions, "", "  ")
	if err != nil {
		return "", fmt.Errorf("marshal rerequest decisions: %w", err)
	}
	return string(data), nil
}
