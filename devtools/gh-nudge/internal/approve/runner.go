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
	ActionHandoff      = "handoff"
	ActionWouldHandoff = "would_handoff"
)

// Client is the gh access gh-approve needs. *github.Client implements it.
type Client interface {
	ViewerLogin() (string, error)
	SearchUserReviewRequested() ([]github.PullRef, error)
	LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error)
	PullRequestHeadSHA(ref github.PullRef) (string, error)
	PullRequestDiff(ref github.PullRef) (string, error)
	PullRequestFilePatches(ref github.PullRef) ([]github.PullFile, error)
	ApprovePullRequest(ref github.PullRef, commitSHA, body string) error
	CodeownersFile(ref github.PullRef, baseRef string) (string, string, error)
	TeamMembers(org, slug string) ([]string, error)
	ReviewRequestTimeline(ref github.PullRef) ([]github.ReviewRequestEvent, error)
	RequestReviewers(ref github.PullRef, logins []string) error
	RemoveRequestedReviewer(ref github.PullRef, login string) error
}

// Options are the per-run command-line choices.
type Options struct {
	// Apply approves and hands off for real. Without it, the run is a dry run.
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
	// Reviewers are the logins a hand-off requests.
	Reviewers []string `json:"reviewers,omitempty"`
	// Timeline is the loop guard's evidence: the viewer's review request
	// events, oldest first.
	Timeline []github.ReviewRequestEvent `json:"timeline,omitempty"`
	// LoopGuard is the loop guard's verdict.
	LoopGuard string `json:"loopGuard,omitempty"`
	// Steps are the hand-off writes and their results, in order.
	Steps []string `json:"steps,omitempty"`
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
	// RunCommand runs agent commands. Nil means ExecCommand.
	RunCommand CommandRunner
	// AgentCache caches agent command decisions. Nil bypasses the cache.
	AgentCache *AgentCache

	// teams caches team members for the run, keyed by lowercase org/slug.
	teams map[string]teamMembers
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

// decide evaluates one pull request and approves or hands it off when allowed.
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
	skipReason := r.skipReason(pr, viewer)
	var agent AgentFunc
	if skipReason == "" {
		agent = r.agent(pr)
		if needsOwnership(r.Config.Rules) {
			pr.Ownership = r.loadOwnership(pr, viewer)
		}
		if needsCommentPatches(r.Config.Rules) {
			r.loadCommentPatches(&pr)
		}
	}
	results, matched := Evaluate(r.Config.Rules, pr, agent)
	entry.Rules = results
	if matched != "" {
		entry.Rule = &matched
	}
	switch {
	case skipReason != "":
		entry.Action, entry.Reason = ActionSkipped, skipReason
	case matched == "":
		entry.Action = ActionManual
	case r.rule(matched).Action == config.ActionHandoff:
		r.handoff(&entry, pr, r.rule(matched), viewer, opts.Apply)
	case !opts.Apply:
		entry.Action = ActionWouldApprove
	default:
		entry.Action, entry.Reason = r.approve(ref, pr.HeadSHA, matched)
	}
	return entry, false
}

// loadCommentPatches fetches REST patches for the deletions-and-comments
// condition. A transport error is stored on pr. The condition reports a
// missing patch itself.
func (r *Runner) loadCommentPatches(pr *github.ApprovalPullRequest) {
	files, err := r.Client.PullRequestFilePatches(pr.Ref)
	if err != nil {
		pr.PatchesErr = err.Error()
		return
	}
	pr.Patches = files
}

// rule returns the rule named name.
func (r *Runner) rule(name string) config.ApproveRule {
	for _, rule := range r.Config.Rules {
		if rule.Name == name {
			return rule
		}
	}
	return config.ApproveRule{}
}

// skipReason returns why pr is never approved whatever the rules say, or "".
func (r *Runner) skipReason(pr github.ApprovalPullRequest, viewer string) string {
	switch {
	case pr.State != "OPEN":
		return "pull request is " + strings.ToLower(pr.State)
	case strings.EqualFold(pr.Author, viewer):
		return "the author is you; gh-approve never approves your own pull request"
	case r.Config.SkipAlreadyApproved && latestStateOnHead(pr, viewer) == "APPROVED":
		return "already approved by you at head " + pr.HeadSHA
	}
	return ""
}

// agent returns the AgentFunc for pr. The diff is read once, when the first
// agent command is about to run. A diff that cannot be read defers.
func (r *Runner) agent(pr github.ApprovalPullRequest) AgentFunc {
	run := r.RunCommand
	if run == nil {
		run = ExecCommand
	}
	var (
		diff    string
		diffErr error
		read    bool
	)
	return func(rule config.ApproveRule) AgentResult {
		if !read {
			diff, diffErr = r.Client.PullRequestDiff(pr.Ref)
			read = true
		}
		if diffErr != nil {
			return AgentResult{Decision: AgentDefer, Reason: diffErr.Error()}
		}
		return runAgent(run, r.AgentCache, rule, newAgentInput(pr, diff))
	}
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

// agentKeyText is the short cache key, or "none" when the cache was bypassed.
func agentKeyText(a AgentResult) string {
	if a.CacheKey == "" {
		return "none"
	}
	return a.CacheKey
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
	writeHandoff(&out, entry, explain)
	for _, result := range entry.Rules {
		switch {
		case explain:
			writeExplain(&out, result)
		case entry.Action == ActionManual && entry.Rule == nil:
			writeFirstFailure(&out, result)
		case result.Matched && result.Agent != nil:
			fmt.Fprintf(&out, "    %s\n", result.Agent.Summary())
		}
	}
	if _, err := io.WriteString(r.Out, out.String()); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

// writeHandoff writes a hand-off's reviewers, steps, and, with explain, the
// loop guard verdict.
func writeHandoff(out *strings.Builder, entry LogEntry, explain bool) {
	if len(entry.Reviewers) > 0 {
		fmt.Fprintf(out, "    reviewers: %s\n", strings.Join(entry.Reviewers, ", "))
	}
	for _, step := range entry.Steps {
		fmt.Fprintf(out, "    step: %s\n", step)
	}
	if explain && entry.LoopGuard != "" {
		fmt.Fprintf(out, "    loop guard: %s\n", entry.LoopGuard)
	}
}

// writeExplain writes every condition of result with the value it saw.
func writeExplain(out *strings.Builder, result RuleResult) {
	verdict := "no match"
	if result.Matched {
		verdict = "match"
	}
	fmt.Fprintf(out, "    rule %s: %s\n", result.Rule, verdict)
	for _, cond := range result.Conditions {
		mark := "pass"
		if !cond.Pass {
			mark = "FAIL"
		}
		fmt.Fprintf(out, "      %s %s: %s\n", mark, cond.Condition, cond.Saw)
		for _, detail := range cond.Details {
			fmt.Fprintf(out, "        %s\n", detail)
		}
		if cond.Condition == agentCondition && result.Agent != nil && result.Agent.Cache != "" {
			fmt.Fprintf(out, "        agent_cache: %s, key %s\n", result.Agent.Cache, agentKeyText(*result.Agent))
			if result.Agent.CacheError != "" {
				fmt.Fprintf(out, "        agent_cache_error: %s\n", result.Agent.CacheError)
			}
		}
	}
}

// writeFirstFailure writes why result did not match.
func writeFirstFailure(out *strings.Builder, result RuleResult) {
	first, ok := result.FirstFailure()
	switch {
	case !ok:
	case first.Condition == agentCondition && result.Agent != nil:
		fmt.Fprintf(out, "    %s: %s\n", result.Rule, result.Agent.Summary())
	default:
		fmt.Fprintf(out, "    %s: %s failed (%s)\n", result.Rule, first.Condition, first.Saw)
	}
}
