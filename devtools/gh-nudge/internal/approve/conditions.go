// Package approve decides which review requests gh-approve may approve.
//
// A rule is a list of conditions that are ANDed. Rules are ORed. Data that
// cannot be read never counts as a passed condition.
package approve

import (
	"fmt"
	"slices"
	"strings"

	"github.com/bmatcuk/doublestar/v4"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// ConditionResult is one condition's verdict and the value it saw.
type ConditionResult struct {
	Condition string `json:"condition"`
	Pass      bool   `json:"pass"`
	Saw       string `json:"saw"`
	// Details are per-item verdicts, such as one line per changed file.
	Details []string `json:"details,omitempty"`
}

// RuleResult is one rule's verdict. Conditions are in evaluation order.
type RuleResult struct {
	Rule       string            `json:"rule"`
	Matched    bool              `json:"matched"`
	Conditions []ConditionResult `json:"conditions"`
	// Agent is set when the rule's agent command ran.
	Agent *AgentResult `json:"agent,omitempty"`
}

// FirstFailure returns the first condition that failed.
func (r RuleResult) FirstFailure() (ConditionResult, bool) {
	for _, cond := range r.Conditions {
		if !cond.Pass {
			return cond, true
		}
	}
	return ConditionResult{}, false
}

// condition is one rule condition. To add one, add its config field and an
// entry to conditions. active reports whether the rule sets the condition.
// A condition sets check, or detail when it reports per-item verdicts.
type condition struct {
	name   string
	active func(rule config.ApproveRule) bool
	check  func(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string)
	detail func(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string)
}

// conditions are evaluated in this order. The first failure is reported.
var conditions = []condition{
	{name: "repos", active: func(r config.ApproveRule) bool { return len(r.Repos) > 0 }, check: checkRepos},
	{name: "paths_all_match", active: func(r config.ApproveRule) bool { return len(r.PathsAllMatch) > 0 }, check: checkPathsAllMatch},
	{name: "paths_none_match", active: func(r config.ApproveRule) bool { return len(r.PathsNoneMatch) > 0 }, check: checkPathsNoneMatch},
	{name: "paths_any_match", active: func(r config.ApproveRule) bool { return len(r.PathsAnyMatch) > 0 }, check: checkPathsAnyMatch},
	{name: "max_changed_lines", active: func(r config.ApproveRule) bool { return r.MaxChangedLines != nil }, check: checkMaxChangedLines},
	{name: "max_added_lines", active: func(r config.ApproveRule) bool { return r.MaxAddedLines != nil }, check: checkMaxAddedLines},
	{name: "max_files", active: func(r config.ApproveRule) bool { return r.MaxFiles != nil }, check: checkMaxFiles},
	{name: "pure_deletion", active: func(r config.ApproveRule) bool { return r.PureDeletion }, check: checkPureDeletion},
	{name: "deletions_and_comments_only", active: func(r config.ApproveRule) bool { return r.DeletionsAndCommentsOnly }, check: checkDeletionsAndCommentsOnly},
	{name: "authors_allow", active: func(r config.ApproveRule) bool { return len(r.AuthorsAllow) > 0 }, check: checkAuthorsAllow},
	{name: "authors_deny", active: func(r config.ApproveRule) bool { return len(r.AuthorsDeny) > 0 }, check: checkAuthorsDeny},
	{name: "labels_required", active: func(r config.ApproveRule) bool { return len(r.LabelsRequired) > 0 }, check: checkLabelsRequired},
	{name: "labels_forbidden", active: func(r config.ApproveRule) bool { return len(r.LabelsForbidden) > 0 }, check: checkLabelsForbidden},
	{name: "required_approvals_by", active: func(r config.ApproveRule) bool { return len(r.RequiredApprovalsBy) > 0 }, check: checkRequiredApprovals},
	{name: "checks", active: func(r config.ApproveRule) bool { return r.Checks != "" }, check: checkChecks},
	{name: "not_draft", active: func(r config.ApproveRule) bool { return r.NotDraft }, check: checkNotDraft},
	{name: "max_unresolved_threads", active: func(config.ApproveRule) bool { return true }, check: checkUnresolvedThreads},
	{name: "codeowners_covered", active: func(r config.ApproveRule) bool { return r.CodeownersCovered }, detail: checkCodeownersCovered},
}

// EvaluateRule evaluates every condition the rule sets.
func EvaluateRule(rule config.ApproveRule, pr github.ApprovalPullRequest) RuleResult {
	result := RuleResult{Rule: rule.Name, Matched: true}
	for _, cond := range conditions {
		if !cond.active(rule) {
			continue
		}
		got := ConditionResult{Condition: cond.name}
		if cond.detail != nil {
			got.Pass, got.Saw, got.Details = cond.detail(rule, pr)
		} else {
			got.Pass, got.Saw = cond.check(rule, pr)
		}
		result.Conditions = append(result.Conditions, got)
		if !got.Pass {
			result.Matched = false
		}
	}
	return result
}

// AgentFunc runs a rule's agent command.
type AgentFunc func(rule config.ApproveRule) AgentResult

// Evaluate evaluates every rule and returns the name of the first matching
// rule, or "" when none matches. A rule's agent command runs last, only when
// its other conditions passed and no earlier rule matched. A nil agent runs
// no agent command, and a rule that sets one does not match.
func Evaluate(rules []config.ApproveRule, pr github.ApprovalPullRequest, agent AgentFunc) ([]RuleResult, string) {
	results := make([]RuleResult, 0, len(rules))
	matched := ""
	for _, rule := range rules {
		result := EvaluateRule(rule, pr)
		if len(rule.AgentCommand) > 0 {
			run := agent
			if matched != "" {
				run = nil
			}
			result = withAgent(result, rule, run)
		}
		if result.Matched && matched == "" {
			matched = rule.Name
		}
		results = append(results, result)
	}
	return results, matched
}

// withAgent adds the agent command's condition to result.
func withAgent(result RuleResult, rule config.ApproveRule, agent AgentFunc) RuleResult {
	cond := ConditionResult{Condition: agentCondition}
	switch {
	case !result.Matched:
		cond.Saw = "not run: another condition failed"
	case agent == nil:
		cond.Saw = "not run"
		result.Matched = false
	default:
		got := agent(rule)
		result.Agent = &got
		cond.Pass = got.Decision == AgentApprove
		cond.Saw = got.String()
		result.Matched = cond.Pass
	}
	result.Conditions = append(result.Conditions, cond)
	return result
}

func unreadable(reason string) (bool, string) {
	return false, "unreadable: " + reason
}

func matchesAny(patterns []string, value string) bool {
	for _, pattern := range patterns {
		if ok, err := doublestar.Match(pattern, value); err == nil && ok {
			return true
		}
	}
	return false
}

func containsFold(items []string, value string) bool {
	return slices.ContainsFunc(items, func(item string) bool { return strings.EqualFold(item, value) })
}

func checkRepos(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	repo := pr.Ref.Owner + "/" + pr.Ref.Repo
	return matchesAny(rule.Repos, repo), repo
}

func checkPathsAllMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.FilesErr != "" {
		return unreadable(pr.FilesErr)
	}
	var outside []string
	for _, file := range pr.Files {
		if !matchesAny(rule.PathsAllMatch, file.Path) {
			outside = append(outside, file.Path)
		}
	}
	if len(outside) > 0 {
		return false, "outside globs: " + strings.Join(outside, ", ")
	}
	return true, fmt.Sprintf("all %d files match", len(pr.Files))
}

func checkPathsNoneMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.FilesErr != "" {
		return unreadable(pr.FilesErr)
	}
	var inside []string
	for _, file := range pr.Files {
		if matchesAny(rule.PathsNoneMatch, file.Path) {
			inside = append(inside, file.Path)
		}
	}
	if len(inside) > 0 {
		return false, "matched: " + strings.Join(inside, ", ")
	}
	return true, fmt.Sprintf("none of %d files match", len(pr.Files))
}

func checkPathsAnyMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.FilesErr != "" {
		return unreadable(pr.FilesErr)
	}
	for _, file := range pr.Files {
		if matchesAny(rule.PathsAnyMatch, file.Path) {
			return true, "matched: " + file.Path
		}
	}
	return false, fmt.Sprintf("none of %d files match", len(pr.Files))
}

// checkCodeownersCovered passes when every changed file is in
// paths_any_match, or owned by neither the viewer nor a team of the viewer,
// or owned only through teams of the viewer that have other members and are
// requested reviewers. pr.Ownership holds the last two verdicts.
func checkCodeownersCovered(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string) {
	reason := ""
	switch {
	case pr.FilesErr != "":
		reason = pr.FilesErr
	case pr.Ownership == nil:
		reason = "CODEOWNERS not read"
	case pr.Ownership.Err != "":
		reason = pr.Ownership.Err
	}
	if reason != "" {
		pass, saw := unreadable(reason)
		return pass, saw, nil
	}
	var blocked, details []string
	for _, file := range pr.Ownership.Files {
		owners := strings.Join(file.Owners, " ")
		if owners == "" {
			owners = "none"
		}
		switch {
		case matchesAny(rule.PathsAnyMatch, file.Path):
			details = append(details, "pass "+file.Path+": in paths_any_match")
		case file.Blocker != "":
			blocked = append(blocked, file.Path+" ("+file.Blocker+")")
			details = append(details, "FAIL "+file.Path+": blocked by "+file.Blocker)
		case !file.Mine:
			details = append(details, "pass "+file.Path+": not owned by you or your teams (owners: "+owners+")")
		default:
			details = append(details, "pass "+file.Path+": your owning teams are requested and have other members (owners: "+owners+")")
		}
	}
	if len(blocked) > 0 {
		return false, "not covered: " + strings.Join(blocked, ", "), details
	}
	return true, fmt.Sprintf("all %d files covered", len(pr.Ownership.Files)), details
}

func checkLimit(value, limit int, unit string) (bool, string) {
	return value <= limit, fmt.Sprintf("%d %s (max %d)", value, unit, limit)
}

func checkMaxChangedLines(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	return checkLimit(pr.Additions+pr.Deletions, *rule.MaxChangedLines, "changed lines")
}

func checkMaxAddedLines(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	return checkLimit(pr.Additions, *rule.MaxAddedLines, "added lines")
}

func checkMaxFiles(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	return checkLimit(pr.ChangedFiles, *rule.MaxFiles, "files")
}

func checkPureDeletion(_ config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	return pr.Additions == 0, fmt.Sprintf("%d added lines", pr.Additions)
}

func checkDeletionsAndCommentsOnly(_ config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.Deletions <= 0 {
		return false, "0 deletions"
	}
	if pr.PatchesErr != "" {
		return unreadable(pr.PatchesErr)
	}
	if pr.Patches == nil {
		return unreadable("patches not loaded")
	}
	if pr.ChangedFiles > len(pr.Patches) {
		return false, "patch unavailable for " + missingPatchFile(pr)
	}
	adds := 0
	for _, file := range pr.Patches {
		n, reason, ok := fileCommentAdditions(file)
		if !ok {
			return false, reason
		}
		adds += n
	}
	return true, fmt.Sprintf("%d deletions, %d comment/blank additions", pr.Deletions, adds)
}

func checkAuthorsAllow(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	return pr.Author != "" && containsFold(rule.AuthorsAllow, pr.Author), "author " + pr.Author
}

func checkAuthorsDeny(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.Author == "" {
		return unreadable("author missing")
	}
	return !containsFold(rule.AuthorsDeny, pr.Author), "author " + pr.Author
}

func checkLabelsRequired(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.LabelsErr != "" {
		return unreadable(pr.LabelsErr)
	}
	var missing []string
	for _, label := range rule.LabelsRequired {
		if !slices.Contains(pr.Labels, label) {
			missing = append(missing, label)
		}
	}
	if len(missing) > 0 {
		return false, "missing: " + strings.Join(missing, ", ")
	}
	return true, "labels: " + strings.Join(pr.Labels, ", ")
}

func checkLabelsForbidden(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.LabelsErr != "" {
		return unreadable(pr.LabelsErr)
	}
	var present []string
	for _, label := range rule.LabelsForbidden {
		if slices.Contains(pr.Labels, label) {
			present = append(present, label)
		}
	}
	if len(present) > 0 {
		return false, "present: " + strings.Join(present, ", ")
	}
	return true, "labels: " + strings.Join(pr.Labels, ", ")
}

// latestStateOnHead returns login's latest APPROVED, CHANGES_REQUESTED, or
// DISMISSED review state on the head SHA. COMMENTED and PENDING reviews do
// not change it. It returns "" when there is none.
func latestStateOnHead(pr github.ApprovalPullRequest, login string) string {
	state := ""
	for _, review := range pr.Reviews {
		if !strings.EqualFold(review.Author, login) || review.CommitSHA != pr.HeadSHA {
			continue
		}
		switch review.State {
		case "APPROVED", "CHANGES_REQUESTED", "DISMISSED":
			state = review.State
		}
	}
	return state
}

func checkRequiredApprovals(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.HeadSHA == "" {
		return unreadable("head SHA missing")
	}
	pass := true
	seen := make([]string, 0, len(rule.RequiredApprovalsBy))
	for _, login := range rule.RequiredApprovalsBy {
		state := latestStateOnHead(pr, login)
		if state != "APPROVED" {
			pass = false
		}
		if state == "" {
			state = "none on head"
		}
		seen = append(seen, login+"="+state)
	}
	return pass, strings.Join(seen, ", ")
}

func checkChecks(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.ChecksErr != "" {
		return unreadable(pr.ChecksErr)
	}
	var failed, pending []string
	anyRequired := slices.ContainsFunc(pr.Checks, func(c github.ApprovalCheck) bool { return c.Required })
	for _, check := range pr.Checks {
		switch check.Outcome {
		case models.ChecksFail:
			failed = append(failed, check.Name)
		case models.ChecksPending:
			// Under green, a pending required check fails. With no required
			// check on the head, every check counts as required.
			if check.Required || !anyRequired {
				pending = append(pending, check.Name)
			}
		}
	}
	saw := fmt.Sprintf("%d checks", len(pr.Checks))
	if len(failed) > 0 {
		saw += "; failed: " + strings.Join(failed, ", ")
	}
	if len(pending) > 0 {
		saw += "; pending: " + strings.Join(pending, ", ")
	}
	if len(failed) > 0 {
		return false, saw
	}
	if rule.Checks == config.ChecksGreen && len(pending) > 0 {
		return false, saw
	}
	return true, saw
}

func checkNotDraft(_ config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.Draft {
		return false, "draft"
	}
	return true, "ready for review"
}

func checkUnresolvedThreads(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	if pr.ThreadsErr != "" {
		return unreadable(pr.ThreadsErr)
	}
	return checkLimit(pr.UnresolvedThreads, rule.MaxUnresolvedThreads, "unresolved threads")
}
