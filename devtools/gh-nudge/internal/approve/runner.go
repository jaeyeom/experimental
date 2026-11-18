package approve

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"slices"
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
	ActionMarkViewed   = "mark_viewed"
	// ActionWouldMarkViewed is the dry-run action for files that would be marked.
	ActionWouldMarkViewed = "would_mark_viewed"
)

// Client is the gh access gh-approve needs. *github.Client implements it.
type Client interface {
	ViewerLogin() (string, error)
	SearchUserReviewRequested() ([]github.PullRef, error)
	SearchReviewRequested() ([]github.PullRef, error)
	LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error)
	PullRequestHeadSHA(ref github.PullRef) (string, error)
	PullRequestDiff(ref github.PullRef) (string, error)
	PullRequestFilePatches(ref github.PullRef) ([]github.PullFile, error)
	ApprovePullRequest(ref github.PullRef, commitSHA, body string) error
	MarkFileAsViewed(ref github.PullRef, pullRequestID, path string) error
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
	// Color colors each row's and summary heading's marker and group word.
	Color bool
}

// LogEntry is one audit log line.
type LogEntry struct {
	Time  time.Time `json:"time"`
	PR    string    `json:"pr"`
	URL   string    `json:"url,omitempty"`
	Title string    `json:"title,omitempty"`
	// Author is the pull request author's login. Empty on lines written
	// before the field existed, and when the pull request could not be loaded.
	Author  string `json:"author,omitempty"`
	HeadSHA string `json:"headSha"`
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
	// Paths are the files this run marked viewed, or would mark in a dry run.
	Paths []string `json:"paths,omitempty"`
	// FileCheckPatterns maps each marked path from file_checks to the pattern that passed.
	FileCheckPatterns map[string]string `json:"fileCheckPatterns,omitempty"`
	// Exclusions are explain lines for files a selector matched and an
	// exclusion kept visible. They are not part of the audit log.
	Exclusions []string `json:"-"`
	// ExtraLogs are additional audit log entries (e.g. mark_viewed alongside
	// approved) written to the log for this pull request.
	ExtraLogs []LogEntry `json:"-"`
}

// Runner evaluates review requests and approves the ones a rule matches.
type Runner struct {
	Client Client
	Config config.ApproveConfig
	// Viewed is the mark-viewed config. It is separate from Config because
	// the pkl block is a sibling of approve.
	Viewed config.ViewedConfig
	// Out receives the human-readable table.
	Out io.Writer
	// Log receives one JSON line per evaluated pull request.
	Log io.Writer
	Now func() time.Time
	// RunCommand runs agent commands. Nil means ExecCommand.
	RunCommand CommandRunner
	// RunFileCheckCommand runs file_checks scripts. Nil means ExecFileCheckCommand.
	RunFileCheckCommand FileCheckRunner
	// AgentCache caches agent command decisions. Nil bypasses the cache.
	AgentCache *AgentCache

	// teams caches team members for the run, keyed by lowercase org/slug.
	teams map[string]teamMembers
	// marksLeft is how many files this run may still mark viewed.
	marksLeft int
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
	// The status column holds a two-column marker, a space, and the word.
	if _, err := fmt.Fprintf(r.Out, "%-*s %-32s %-18s %-20s %s\n", statusWidth+3, "STATUS", "PR", "ACTION", "RULE", "TITLE"); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	r.marksLeft = r.Viewed.MaxMarksPerRun
	failed := 0
	var reported []LogEntry
	for _, ref := range refs {
		entry, skip := r.decide(ref, viewer, opts, single)
		if skip {
			continue
		}
		if entry.Action == ActionError {
			failed++
		}
		if err := r.report(entry, opts); err != nil {
			return err
		}
		reported = append(reported, entry)
	}
	if err := writeSummary(r.Out, reported, opts.Color); err != nil {
		return err
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
	search := r.Client.SearchUserReviewRequested
	if slices.ContainsFunc(r.Config.Rules, func(rule config.ApproveRule) bool { return rule.RequestedViaTeam }) {
		search = r.Client.SearchReviewRequested
	}
	refs, err := search()
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
	entry.URL, entry.Title, entry.HeadSHA, entry.Author = pr.URL, pr.Title, pr.HeadSHA, pr.Author
	rules, viaTeam, reason := r.scope(pr, viewer)
	if reason != "" {
		entry.Action, entry.Reason = ActionSkipped, reason
		return entry, !single
	}
	skipReason := r.skipReason(pr, viewer)
	var agent AgentFunc
	if skipReason == "" {
		agent = r.agent(pr)
		if needsOwnership(rules) {
			pr.Ownership = r.loadOwnership(pr, viewer)
		}
		if needsCommentPatches(rules) {
			r.loadCommentPatches(&pr)
		}
	}
	results, matched := evaluateWithFileChecks(rules, pr, agent, r.fileChecker())
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
		entry.Action, entry.Reason = r.approve(pr, r.rule(matched))
	}
	if viaTeam != "" && entry.Action == ActionManual {
		entry.Action = ActionSkipped
		entry.Reason = "requested only through " + viaTeam + "; no requested_via_team rule matched"
		return entry, !single && !opts.Explain
	}
	r.markViewed(&entry, pr, viewer, opts.Apply)
	return entry, false
}

// scope returns the rules that may take pr and, for a team-only request,
// the team. reason is set when no rule may take it.
func (r *Runner) scope(pr github.ApprovalPullRequest, viewer string) ([]config.ApproveRule, string, string) {
	if containsFold(pr.RequestedUsers, viewer) {
		return r.Config.Rules, "", ""
	}
	team, reason := r.requestingTeam(pr, viewer)
	if team == "" {
		return nil, "", reason
	}
	return teamRules(r.Config.Rules), team, ""
}

// requestingTeam returns a requested team, as @org/slug, that includes the
// viewer, when a rule sets requested_via_team. Otherwise it returns "" and
// why the pull request is skipped.
func (r *Runner) requestingTeam(pr github.ApprovalPullRequest, viewer string) (string, string) {
	if len(teamRules(r.Config.Rules)) == 0 {
		return "", errNotRequested.Error()
	}
	var errs []string
	for _, slug := range pr.RequestedTeams {
		members, err := r.teamMembers(pr.Ref.Owner, slug)
		if err != nil {
			errs = append(errs, err.Error())
			continue
		}
		if containsFold(members, viewer) {
			return "@" + pr.Ref.Owner + "/" + slug, ""
		}
	}
	if len(errs) > 0 {
		return "", errNotRequested.Error() + "; " + strings.Join(errs, "; ")
	}
	return "", errNotRequested.Error()
}

// teamRules returns the rules that set requested_via_team.
func teamRules(rules []config.ApproveRule) []config.ApproveRule {
	var out []config.ApproveRule
	for _, rule := range rules {
		if rule.RequestedViaTeam {
			out = append(out, rule)
		}
	}
	return out
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

func (r *Runner) approve(pr github.ApprovalPullRequest, rule config.ApproveRule) (string, string) {
	ref, evaluatedSHA := pr.Ref, pr.HeadSHA
	head, err := r.Client.PullRequestHeadSHA(ref)
	if err != nil {
		return ActionError, err.Error()
	}
	if head != evaluatedSHA {
		return ActionSkipped, fmt.Sprintf("head moved from %s to %s since evaluation", evaluatedSHA, head)
	}
	if err := r.Client.ApprovePullRequest(ref, evaluatedSHA, approvalBody(r.Config.Body, rule, pr)); err != nil {
		return ActionError, err.Error()
	}
	return ActionApproved, ""
}

// approvalBody is the rule's body, or the global body when the rule sets
// none. A body without placeholders gets the rule name appended. A body
// with placeholders is used as written, with them filled in.
func approvalBody(global string, rule config.ApproveRule, pr github.ApprovalPullRequest) string {
	if len(config.BodyPlaceholders(rule.Body)) == 0 {
		body := rule.Body
		if body == "" {
			body = global
		}
		return fmt.Sprintf("%s\n\n<!-- gh-approve: %s -->", body, rule.Name)
	}
	files, owners := myFiles(pr)
	return strings.NewReplacer(
		"{"+config.PlaceholderMyFiles+"}", strings.Join(files, ", "),
		"{"+config.PlaceholderMyTeams+"}", strings.Join(owners, ", "),
		"{"+config.PlaceholderRule+"}", rule.Name,
	).Replace(rule.Body)
}

// agentKeyText is the short cache key, or "none" when the cache was bypassed.
func agentKeyText(a AgentResult) string {
	if a.CacheKey == "" {
		return "none"
	}
	return a.CacheKey
}

func (r *Runner) report(entry LogEntry, opts Options) error {
	if err := writeLogEntry(r.Log, entry); err != nil {
		return err
	}
	for _, extra := range entry.ExtraLogs {
		if err := writeLogEntry(r.Log, extra); err != nil {
			return err
		}
	}
	row := fmt.Sprintf("%s %-32s %-18s %-20s %s\n", statusCell(entry.Action, opts.Color), entry.PR, entry.Action, ruleColumn(entry), entry.Title)
	if _, err := io.WriteString(r.Out, row+details(entry, opts.Explain)); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

func writeLogEntry(w io.Writer, entry LogEntry) error {
	line, err := json.Marshal(entry)
	if err != nil {
		return fmt.Errorf("encode audit log: %w", err)
	}
	if _, err := w.Write(append(line, '\n')); err != nil {
		return fmt.Errorf("write audit log: %w", err)
	}
	return nil
}

// details are the indented lines under a row.
func details(entry LogEntry, explain bool) string {
	var out strings.Builder
	if entry.Reason != "" {
		fmt.Fprintf(&out, "    %s\n", entry.Reason)
	}
	writeHandoff(&out, entry, explain)
	writeViewed(&out, entry, explain)
	writeExclusions(&out, entry, explain)
	for _, result := range entry.Rules {
		switch {
		case explain:
			writeExplain(&out, result)
		case leftForHuman(entry):
			writeFirstFailure(&out, result)
		case result.Matched && result.Agent != nil:
			fmt.Fprintf(&out, "    %s\n", result.Agent.Summary())
		}
	}
	return out.String()
}

// ruleColumn is the RULE cell. A pull request no approve rule took says manual.
func ruleColumn(entry LogEntry) string {
	if entry.Rule != nil {
		return *entry.Rule
	}
	if leftForHuman(entry) {
		return "manual"
	}
	return "-"
}

// leftForHuman reports a pull request no approve or hand-off rule took.
func leftForHuman(entry LogEntry) bool {
	if entry.Rule != nil {
		return false
	}
	switch entry.Action {
	case ActionManual, ActionMarkViewed, ActionWouldMarkViewed:
		return true
	default:
		return false
	}
}

// writeViewed writes how many files were marked, and with explain their paths.
func writeViewed(out *strings.Builder, entry LogEntry, explain bool) {
	var verb string
	switch entry.Action {
	case ActionWouldMarkViewed, ActionWouldApprove, ActionWouldHandoff:
		if len(entry.Paths) == 0 {
			return
		}
		verb = ActionWouldMarkViewed
	case ActionMarkViewed, ActionApproved, ActionHandoff:
		if len(entry.Paths) == 0 {
			return
		}
		verb = ActionMarkViewed
	default:
		return
	}
	fmt.Fprintf(out, "    %s: %d files\n", verb, len(entry.Paths))
	if !explain {
		return
	}
	for _, path := range entry.Paths {
		fmt.Fprintf(out, "      %s\n", path)
	}
}

// writeExclusions writes, with --explain, each file an exclusion kept visible.
func writeExclusions(out *strings.Builder, entry LogEntry, explain bool) {
	if !explain {
		return
	}
	for _, line := range entry.Exclusions {
		fmt.Fprintf(out, "      %s\n", line)
	}
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
