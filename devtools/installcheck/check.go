package installcheck

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"sort"
	"strings"
	"time"
)

// FailureNoticeInterval is the minimum gap between failure messages.
const FailureNoticeInterval = 24 * time.Hour

// Input is everything one pass needs. Run is injected so tests need no git,
// gh, or agentbus.
type Input struct {
	Config Config
	State  State
	Now    time.Time
	Run    Runner
	DryRun bool
}

// Result is the outcome of one pass. Next is the state to save; it keeps
// the input values for any repo that failed, and equals the input state on
// a failed send or a dry run.
type Result struct {
	Message string // the landed-issues message, sent unless DryRun
	Sent    bool
	Next    State
	Errors  []error
}

// Issue is a watched issue as reported by gh.
type Issue struct {
	Repo        string
	Number      int
	Title       string
	State       string
	StateReason string
	ClosedAt    time.Time
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
	var res Result
	var outcomes []repoOutcome
	for _, repo := range in.Config.Repos {
		out, moved, err := checkRepo(ctx, in, repo)
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
		if err := send(ctx, in, res.Message); err != nil {
			// Do not advance anything: the next run retries.
			res.Errors = append(res.Errors, err)
			res.Next = in.State
			return res
		}
		res.Sent = true
	}
	if !in.DryRun {
		applyOutcomes(&next, outcomes, in.Now)
		noteFailures(ctx, in, &next, &res)
	}
	res.Next = next
	if in.DryRun {
		res.Next = in.State
	}
	return res
}

func cloneState(st State) State {
	out := st
	out.Repos = make(map[string]RepoState, len(st.Repos))
	for k, v := range st.Repos {
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
	if len(res.Errors) == 0 || in.Now.Sub(in.State.FailureNotedAt) < FailureNoticeInterval {
		return
	}
	body := "install-check: a check failed; will retry on the next run:\n"
	for _, e := range res.Errors {
		body += "- " + e.Error() + "\n"
	}
	if err := send(ctx, in, body); err == nil {
		next.FailureNotedAt = in.Now
	}
}

func checkRepo(ctx context.Context, in Input, repo RepoConfig) (repoOutcome, bool, error) {
	slug, err := repo.Slug()
	if err != nil {
		return repoOutcome{}, false, err
	}
	head, err := lsRemote(ctx, in.Run, repo.URL)
	if err != nil {
		return repoOutcome{}, false, fmt.Errorf("%s: %w", slug, err)
	}
	prev, known := in.State.Repos[slug]
	if known && prev.Head == head {
		return repoOutcome{}, false, nil
	}
	issues, err := fetchIssues(ctx, in.Run, repo, slug, "closed")
	if err != nil {
		return repoOutcome{}, false, fmt.Errorf("%s: %w", slug, err)
	}
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
		case in.Config.SkipNotPlanned:
			out.skipped = append(out.skipped, is)
		default:
			out.listed = append(out.listed, is)
		}
	}
	return out, true, nil
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

// send delivers body to the configured agentbus name. Exit 3 means the
// message is stored but held by the daemon, which still counts as sent.
func send(ctx context.Context, in Input, body string) error {
	_, err := in.Run(ctx, "agentbus", "send", in.Config.NotifyName, body)
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) && exitErr.ExitCode() == 3 {
		return nil
	}
	if err != nil {
		return fmt.Errorf("send to %s: %w", in.Config.NotifyName, err)
	}
	return nil
}
