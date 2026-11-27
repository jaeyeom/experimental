package installcheck

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"sort"
	"strings"
	"time"
)

// FailureNoticeInterval is the minimum gap between failure messages.
const FailureNoticeInterval = 24 * time.Hour

// Input is everything one pass needs. Run and Notifier are injected so tests
// need no git, gh, or external notification binaries.
type Input struct {
	Config   Config
	State    State
	Now      time.Time
	Run      Runner
	Notifier Notifier
	GHBin    string
	DryRun   bool
}

// RepoResult describes what happened while checking one repository.
type RepoResult struct {
	Slug       string  `json:"slug"`
	URL        string  `json:"url"`
	Head       string  `json:"head,omitempty"`
	Moved      bool    `json:"moved"`
	Landed     []Issue `json:"landed,omitempty"`
	NotPlanned []Issue `json:"notPlanned,omitempty"`
	Skipped    []Issue `json:"skipped,omitempty"`
	Error      string  `json:"error,omitempty"`
}

// Result is the outcome of one pass. Next is the state to save; it keeps
// the input values for any repo that failed, and equals the input state on
// a failed send or a dry run.
type Result struct {
	Message string // the landed-issues message, sent unless DryRun
	Sent    bool
	Repos   []RepoResult
	Next    State
	Errors  []error
}

// Issue is a watched issue as reported by gh.
type Issue struct {
	Repo        string    `json:"repo"`
	Number      int       `json:"number"`
	Title       string    `json:"title"`
	State       string    `json:"state"`
	StateReason string    `json:"stateReason,omitempty"`
	ClosedAt    time.Time `json:"closedAt,omitempty"`
}

func (i Issue) notPlanned() bool { return i.StateReason == "NOT_PLANNED" }

type repoOutcome struct {
	slug    string
	head    string
	landed  []Issue
	skipped []Issue // not planned, hidden by config
	listed  []Issue // not planned, shown
	closed  []int   // every closed issue number seen, to record as reported
}

// Check runs one pass over every configured repo.
func Check(ctx context.Context, in Input) Result {
	next := cloneState(in.State)
	res := Result{
		Repos: make([]RepoResult, 0, len(in.Config.Repos)),
	}
	var outcomes []repoOutcome
	for _, repo := range in.Config.Repos {
		rr, out, moved, err := checkRepo(ctx, in, repo)
		res.Repos = append(res.Repos, rr)
		if err != nil {
			res.Errors = append(res.Errors, err)
			continue
		}
		if moved {
			outcomes = append(outcomes, out)
		}
	}
	res.Message = formatMessage(outcomes)
	if res.Message != "" && !in.DryRun {
		if err := deliver(ctx, in.Notifier, Notification{Kind: NotifyLanded, Body: res.Message}); err != nil {
			// Do not advance anything: the next run retries.
			res.Errors = append(res.Errors, err)
			res.Next = cloneState(in.State)
			return res
		}
		res.Sent = true
	}
	if !in.DryRun {
		applyOutcomes(&next, outcomes, in.Now)
		noteFailures(ctx, in, &next, &res)
		res.Next = next
		return res
	}
	res.Next = cloneState(in.State)
	return res
}

func cloneState(st State) State {
	out := st
	out.Repos = make(map[string]RepoState, len(st.Repos))
	for k, v := range st.Repos {
		v.Reported = slices.Clone(v.Reported)
		out.Repos[k] = v
	}
	return out
}

func applyOutcomes(next *State, outcomes []repoOutcome, now time.Time) {
	next.LastRun = now
	for _, o := range outcomes {
		rs := next.Repos[o.slug]
		rs.Head = o.head
		for _, n := range o.closed {
			if !rs.has(n) {
				rs.Reported = append(rs.Reported, n)
			}
		}
		sort.Ints(rs.Reported)
		next.Repos[o.slug] = rs
	}
}

// noteFailures sends at most one failure message per FailureNoticeInterval.
func noteFailures(ctx context.Context, in Input, next *State, res *Result) {
	if len(res.Errors) == 0 {
		return
	}
	if !in.State.FailureNotedAt.IsZero() && in.Now.Sub(in.State.FailureNotedAt) < FailureNoticeInterval {
		return
	}
	var b strings.Builder
	b.WriteString("install-check: a check failed; will retry on the next run:\n")
	for _, e := range res.Errors {
		b.WriteString("- ")
		b.WriteString(e.Error())
		b.WriteByte('\n')
	}
	if err := deliver(ctx, in.Notifier, Notification{Kind: NotifyFailure, Body: b.String()}); err != nil {
		res.Errors = append(res.Errors, err)
		return
	}
	next.FailureNotedAt = in.Now
}

func deliver(ctx context.Context, n Notifier, msg Notification) error {
	if n == nil {
		return errors.New("notifier is not configured")
	}
	if err := n.Notify(ctx, msg); err != nil {
		return fmt.Errorf("notify: %w", err)
	}
	return nil
}

func checkRepo(ctx context.Context, in Input, repo RepoConfig) (RepoResult, repoOutcome, bool, error) {
	rr := RepoResult{URL: repo.URL}
	slug, err := repo.Slug()
	if err != nil {
		rr.Error = err.Error()
		return rr, repoOutcome{}, false, err
	}
	rr.Slug = slug
	head, err := lsRemote(ctx, in.Run, repo.URL)
	if err != nil {
		wrapped := fmt.Errorf("%s: %w", slug, err)
		rr.Error = wrapped.Error()
		return rr, repoOutcome{}, false, wrapped
	}
	rr.Head = head
	prev, known := in.State.Repos[slug]
	if known && prev.Head == head {
		return rr, repoOutcome{}, false, nil
	}
	issues, err := fetchIssues(ctx, in.Run, in.GHBin, repo, slug, "closed")
	if err != nil {
		wrapped := fmt.Errorf("%s: %w", slug, err)
		rr.Error = wrapped.Error()
		return rr, repoOutcome{}, false, wrapped
	}
	out := classifyClosedIssues(slug, head, issues, known, prev, in.Config.SkipNotPlanned)
	rr.Moved = true
	rr.Landed = out.landed
	rr.NotPlanned = out.listed
	rr.Skipped = out.skipped
	return rr, out, true, nil
}

func classifyClosedIssues(slug, head string, issues []Issue, known bool, prev RepoState, skipNotPlanned bool) repoOutcome {
	out := repoOutcome{slug: slug, head: head}
	for _, is := range issues {
		out.closed = append(out.closed, is.Number)
		if !known || prev.has(is.Number) {
			// First run baselines silently; reported issues are never repeated.
			continue
		}
		switch {
		case !is.notPlanned():
			out.landed = append(out.landed, is)
		case skipNotPlanned:
			out.skipped = append(out.skipped, is)
		default:
			out.listed = append(out.listed, is)
		}
	}
	return out
}

func lsRemote(ctx context.Context, run Runner, url string) (string, error) {
	out, err := run(ctx, "git", "ls-remote", url, "refs/heads/main")
	if err != nil {
		return "", err
	}
	fields := strings.Fields(string(out))
	if len(fields) == 0 {
		return "", fmt.Errorf("ls-remote %s: no refs/heads/main", url)
	}
	return fields[0], nil
}

func formatMessage(outcomes []repoOutcome) string {
	var landed, notPlanned, heads []string
	for _, o := range outcomes {
		for _, is := range o.landed {
			landed = append(landed, issueLine(is, true))
		}
		for _, is := range o.listed {
			notPlanned = append(notPlanned, issueLine(is, false))
		}
		if len(o.landed)+len(o.listed) > 0 {
			heads = append(heads, fmt.Sprintf("main: %s %s", o.slug, shortSHA(o.head)))
		}
	}
	if len(landed)+len(notPlanned) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("install-check: main moved; landed since last check:\n")
	for _, l := range landed {
		b.WriteString(l + "\n")
	}
	if len(notPlanned) > 0 {
		b.WriteString("closed as not planned:\n")
		for _, l := range notPlanned {
			b.WriteString(l + "\n")
		}
	}
	b.WriteString(strings.Join(heads, "\n") + "\n")
	return b.String()
}

func issueLine(is Issue, withTime bool) string {
	line := fmt.Sprintf("- %s#%d %s", is.Repo, is.Number, is.Title)
	if withTime && !is.ClosedAt.IsZero() {
		line += fmt.Sprintf(" (closed %s)", is.ClosedAt.UTC().Format("15:04Z"))
	}
	return line
}

func shortSHA(s string) string {
	if len(s) > 7 {
		return s[:7]
	}
	return s
}
