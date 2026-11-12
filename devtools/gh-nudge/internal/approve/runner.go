package approve

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// Actions written to the audit log.
const (
	ActionApproved     = "approved"
	ActionWouldApprove = "would_approve"
	ActionSkipped      = "skipped"
	ActionManual       = "manual"
	ActionError        = "error"
)

// Client is the gh access gh-approve needs. *github.Client implements it.
type Client interface {
	ViewerLogin() (string, error)
	SearchUserReviewRequested() ([]github.PullRef, error)
	LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error)
	PullRequestHeadSHA(ref github.PullRef) (string, error)
	ApprovePullRequest(ref github.PullRef, commitSHA, body string) error
}

// Options are the per-run command-line choices.
type Options struct {
	// Apply approves for real. Without it, the run is a dry run.
	Apply bool
	// Explain prints every condition of every rule with the value it saw.
	Explain bool
	// PR evaluates one pull request, owner/repo#N or a URL, instead of searching.
	PR string
}

// LogEntry is one audit log line.
type LogEntry struct {
	Time    time.Time `json:"time"`
	PR      string    `json:"pr"`
	URL     string    `json:"url,omitempty"`
	Title   string    `json:"title,omitempty"`
	HeadSHA string    `json:"headSha"`
	// Rule is the matching rule, or null when none matched.
	Rule   *string      `json:"rule"`
	Rules  []RuleResult `json:"rules"`
	Action string       `json:"action"`
	Reason string       `json:"reason,omitempty"`
}

// Runner evaluates review requests and approves the ones a rule matches.
type Runner struct {
	Client Client
	Config config.ApproveConfig
	// Out receives the human-readable table.
	Out io.Writer
	// Log receives one JSON line per evaluated pull request.
	Log io.Writer
	Now func() time.Time
}

// errNotRequested marks a pull request that does not request the viewer as
// an individual. Search results like that are ignored.
var errNotRequested = errors.New("not requested as an individual reviewer")

// Run evaluates the pull requests and returns an error when any of them had
// an error. A failed pull request does not stop the others.
func (r *Runner) Run(opts Options) error {
	viewer, err := r.Client.ViewerLogin()
	if err != nil {
		return fmt.Errorf("read gh user: %w", err)
	}
	refs, single, err := r.targets(opts)
	if err != nil {
		return err
	}
	if _, err := fmt.Fprintf(r.Out, "%-32s %-14s %-20s %s\n", "PR", "ACTION", "RULE", "TITLE"); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	failed := 0
	for _, ref := range refs {
		entry, skip := r.decide(ref, viewer, opts, single)
		if skip {
			continue
		}
		if entry.Action == ActionError {
			failed++
		}
		if err := r.report(entry, opts.Explain); err != nil {
			return err
		}
	}
	if failed > 0 {
		return fmt.Errorf("%d pull request(s) failed", failed)
	}
	return nil
}

func (r *Runner) targets(opts Options) ([]github.PullRef, bool, error) {
	if opts.PR != "" {
		ref, err := github.ParsePullRef(opts.PR)
		if err != nil {
			return nil, true, fmt.Errorf("--pr: %w", err)
		}
		return []github.PullRef{ref}, true, nil
	}
	refs, err := r.Client.SearchUserReviewRequested()
	if err != nil {
		return nil, false, fmt.Errorf("search review requests: %w", err)
	}
	return refs, false, nil
}

// decide evaluates one pull request and approves it when allowed.
// skip is true for a search result that only requests a team.
func (r *Runner) decide(ref github.PullRef, viewer string, opts Options, single bool) (LogEntry, bool) {
	entry := LogEntry{
		Time: r.Now().UTC(),
		PR:   fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number),
	}
	pr, err := r.Client.LoadApprovalPullRequest(ref)
	if err != nil {
		entry.Action, entry.Reason = ActionError, err.Error()
		return entry, false
	}
	entry.URL, entry.Title, entry.HeadSHA = pr.URL, pr.Title, pr.HeadSHA
	if !containsFold(pr.RequestedUsers, viewer) {
		entry.Action, entry.Reason = ActionSkipped, errNotRequested.Error()
		return entry, !single
	}
	results, matched := Evaluate(r.Config.Rules, pr)
	entry.Rules = results
	if matched != "" {
		entry.Rule = &matched
	}
	switch {
	case pr.State != "OPEN":
		entry.Action, entry.Reason = ActionSkipped, "pull request is "+strings.ToLower(pr.State)
	case strings.EqualFold(pr.Author, viewer):
		entry.Action, entry.Reason = ActionSkipped, "the author is you; gh-approve never approves your own pull request"
	case r.Config.SkipAlreadyApproved && latestStateOnHead(pr, viewer) == "APPROVED":
		entry.Action, entry.Reason = ActionSkipped, "already approved by you at head "+pr.HeadSHA
	case matched == "":
		entry.Action = ActionManual
	case !opts.Apply:
		entry.Action = ActionWouldApprove
	default:
		entry.Action, entry.Reason = r.approve(ref, pr.HeadSHA, matched)
	}
	return entry, false
}

func (r *Runner) approve(ref github.PullRef, evaluatedSHA, rule string) (string, string) {
	head, err := r.Client.PullRequestHeadSHA(ref)
	if err != nil {
		return ActionError, err.Error()
	}
	if head != evaluatedSHA {
		return ActionSkipped, fmt.Sprintf("head moved from %s to %s since evaluation", evaluatedSHA, head)
	}
	body := fmt.Sprintf("%s\n\n<!-- gh-approve: %s -->", r.Config.Body, rule)
	if err := r.Client.ApprovePullRequest(ref, evaluatedSHA, body); err != nil {
		return ActionError, err.Error()
	}
	return ActionApproved, ""
}

func (r *Runner) report(entry LogEntry, explain bool) error {
	line, err := json.Marshal(entry)
	if err != nil {
		return fmt.Errorf("encode audit log: %w", err)
	}
	if _, err := r.Log.Write(append(line, '\n')); err != nil {
		return fmt.Errorf("write audit log: %w", err)
	}
	var out strings.Builder
	rule := "-"
	switch {
	case entry.Rule != nil:
		rule = *entry.Rule
	case entry.Action == ActionManual:
		rule = "manual"
	}
	fmt.Fprintf(&out, "%-32s %-14s %-20s %s\n", entry.PR, entry.Action, rule, entry.Title)
	if entry.Reason != "" {
		fmt.Fprintf(&out, "    %s\n", entry.Reason)
	}
	for _, result := range entry.Rules {
		switch {
		case explain:
			verdict := "no match"
			if result.Matched {
				verdict = "match"
			}
			fmt.Fprintf(&out, "    rule %s: %s\n", result.Rule, verdict)
			for _, cond := range result.Conditions {
				mark := "pass"
				if !cond.Pass {
					mark = "FAIL"
				}
				fmt.Fprintf(&out, "      %s %s: %s\n", mark, cond.Condition, cond.Saw)
			}
		case entry.Action == ActionManual:
			if first, ok := result.FirstFailure(); ok {
				fmt.Fprintf(&out, "    %s: %s failed (%s)\n", result.Rule, first.Condition, first.Saw)
			}
		}
	}
	if _, err := io.WriteString(r.Out, out.String()); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}
