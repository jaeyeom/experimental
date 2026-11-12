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
}

// RuleResult is one rule's verdict. Conditions are in evaluation order.
type RuleResult struct {
	Rule       string            `json:"rule"`
	Matched    bool              `json:"matched"`
	Conditions []ConditionResult `json:"conditions"`
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
type condition struct {
	name   string
	active func(rule config.ApproveRule) bool
	check  func(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string)
}

// conditions are evaluated in this order. The first failure is reported.
var conditions = []condition{
	{"repos", func(r config.ApproveRule) bool { return len(r.Repos) > 0 }, checkRepos},
	{"paths_all_match", func(r config.ApproveRule) bool { return len(r.PathsAllMatch) > 0 }, checkPathsAllMatch},
	{"paths_none_match", func(r config.ApproveRule) bool { return len(r.PathsNoneMatch) > 0 }, checkPathsNoneMatch},
	{"max_changed_lines", func(r config.ApproveRule) bool { return r.MaxChangedLines != nil }, checkMaxChangedLines},
	{"max_added_lines", func(r config.ApproveRule) bool { return r.MaxAddedLines != nil }, checkMaxAddedLines},
	{"max_files", func(r config.ApproveRule) bool { return r.MaxFiles != nil }, checkMaxFiles},
	{"pure_deletion", func(r config.ApproveRule) bool { return r.PureDeletion }, checkPureDeletion},
	{"authors_allow", func(r config.ApproveRule) bool { return len(r.AuthorsAllow) > 0 }, checkAuthorsAllow},
	{"authors_deny", func(r config.ApproveRule) bool { return len(r.AuthorsDeny) > 0 }, checkAuthorsDeny},
	{"labels_required", func(r config.ApproveRule) bool { return len(r.LabelsRequired) > 0 }, checkLabelsRequired},
	{"labels_forbidden", func(r config.ApproveRule) bool { return len(r.LabelsForbidden) > 0 }, checkLabelsForbidden},
	{"required_approvals_by", func(r config.ApproveRule) bool { return len(r.RequiredApprovalsBy) > 0 }, checkRequiredApprovals},
	{"checks", func(r config.ApproveRule) bool { return r.Checks != "" }, checkChecks},
	{"not_draft", func(r config.ApproveRule) bool { return r.NotDraft }, checkNotDraft},
	{"max_unresolved_threads", func(config.ApproveRule) bool { return true }, checkUnresolvedThreads},
}

// EvaluateRule evaluates every condition the rule sets.
func EvaluateRule(rule config.ApproveRule, pr github.ApprovalPullRequest) RuleResult {
	result := RuleResult{Rule: rule.Name, Matched: true}
	for _, cond := range conditions {
		if !cond.active(rule) {
			continue
		}
		pass, saw := cond.check(rule, pr)
		result.Conditions = append(result.Conditions, ConditionResult{Condition: cond.name, Pass: pass, Saw: saw})
		if !pass {
			result.Matched = false
		}
	}
	return result
}

// Evaluate evaluates every rule and returns the name of the first matching
// rule, or "" when none matches.
func Evaluate(rules []config.ApproveRule, pr github.ApprovalPullRequest) ([]RuleResult, string) {
	results := make([]RuleResult, 0, len(rules))
	matched := ""
	for _, rule := range rules {
		result := EvaluateRule(rule, pr)
		if result.Matched && matched == "" {
			matched = rule.Name
		}
		results = append(results, result)
	}
	return results, matched
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
