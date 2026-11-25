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
	// NotEvaluated is set on an expensive condition left unread because a
	// cheaper condition of the rule already failed. Pass is false then.
	NotEvaluated bool `json:"notEvaluated,omitempty"`
}

// RuleResult is one rule's verdict. Conditions are in evaluation order.
type RuleResult struct {
	Rule       string            `json:"rule"`
	Matched    bool              `json:"matched"`
	Conditions []ConditionResult `json:"conditions"`
	// Agent is set when the rule's agent command ran.
	Agent *AgentResult `json:"agent,omitempty"`
	// FileCheckPasses holds files whose file_checks check approved.
	FileCheckPasses []FileCheckPass `json:"-"`
}

// FirstFailure returns the first condition that failed.
func (r RuleResult) FirstFailure() (ConditionResult, bool) {
	for _, cond := range r.Conditions {
		if !cond.Pass && !cond.NotEvaluated {
			return cond, true
		}
	}
	return ConditionResult{}, false
}

// condition is one rule condition. To add one, add its config field and an
// entry to conditions. active reports whether the rule sets the condition.
// A condition sets check, or detail when it reports per-item verdicts.
type condition struct {
	name      string
	fileLevel bool
	// needs is the data the condition loads on demand. A condition that
	// needs some is evaluated after the rule's other conditions passed.
	needs    loadNeeds
	needsFor func(rule config.ApproveRule) loadNeeds
	// skipLoad reports that the condition fails without reading its data.
	skipLoad func(pr github.ApprovalPullRequest) bool
	active   func(rule config.ApproveRule) bool
	check    func(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string)
	detail   func(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string)
}

// loadNeeds is a set of data a condition loads on demand.
type loadNeeds int

const (
	needOwnership loadNeeds = 1 << iota
	needPatches
	needGenerated
)

func condNeeds(cond condition, rule config.ApproveRule) loadNeeds {
	n := cond.needs
	if cond.needsFor != nil {
		n |= cond.needsFor(rule)
	}
	return n
}

func generatedNeedFor(patterns ...[]string) loadNeeds {
	for _, list := range patterns {
		if config.UsesGeneratedList(list) {
			return needGenerated
		}
	}
	return 0
}

// conditions are reported in this order. The first failure is reported.
var conditions = []condition{
	{name: "repos", active: func(r config.ApproveRule) bool { return len(r.Repos) > 0 }, check: checkRepos},
	{name: "paths_all_match", fileLevel: true, needsFor: func(r config.ApproveRule) loadNeeds { return generatedNeedFor(r.PathsAllMatch) }, active: func(r config.ApproveRule) bool { return len(r.PathsAllMatch) > 0 }, detail: checkPathsAllMatch},
	{name: "paths_none_match", fileLevel: true, needsFor: func(r config.ApproveRule) loadNeeds { return generatedNeedFor(r.PathsNoneMatch) }, active: func(r config.ApproveRule) bool { return len(r.PathsNoneMatch) > 0 }, detail: checkPathsNoneMatch},
	{name: "paths_any_match", fileLevel: true, needsFor: func(r config.ApproveRule) loadNeeds { return generatedNeedFor(r.PathsAnyMatch) }, active: func(r config.ApproveRule) bool { return len(r.PathsAnyMatch) > 0 }, detail: checkPathsAnyMatch},
	{name: "max_changed_lines", fileLevel: true, active: func(r config.ApproveRule) bool { return r.MaxChangedLines != nil }, check: checkMaxChangedLines},
	{name: "max_added_lines", fileLevel: true, active: func(r config.ApproveRule) bool { return r.MaxAddedLines != nil }, check: checkMaxAddedLines},
	{name: "max_files", fileLevel: true, active: func(r config.ApproveRule) bool { return r.MaxFiles != nil }, check: checkMaxFiles},
	{name: "pure_deletion", fileLevel: true, active: func(r config.ApproveRule) bool { return r.PureDeletion }, check: checkPureDeletion},
	{name: "deletions_and_comments_only", fileLevel: true, needs: needPatches, skipLoad: func(pr github.ApprovalPullRequest) bool { return pr.Deletions <= 0 }, active: func(r config.ApproveRule) bool { return r.DeletionsAndCommentsOnly }, check: checkDeletionsAndCommentsOnly},
	{name: "authors_allow", active: func(r config.ApproveRule) bool { return len(r.AuthorsAllow) > 0 }, check: checkAuthorsAllow},
	{name: "authors_deny", active: func(r config.ApproveRule) bool { return len(r.AuthorsDeny) > 0 }, check: checkAuthorsDeny},
	{name: "labels_required", active: func(r config.ApproveRule) bool { return len(r.LabelsRequired) > 0 }, check: checkLabelsRequired},
	{name: "labels_forbidden", active: func(r config.ApproveRule) bool { return len(r.LabelsForbidden) > 0 }, check: checkLabelsForbidden},
	{name: "required_approvals_by", active: func(r config.ApproveRule) bool { return len(r.RequiredApprovalsBy) > 0 }, check: checkRequiredApprovals},
	{name: "checks", active: func(r config.ApproveRule) bool { return r.Checks != "" }, check: checkChecks},
	{name: "not_draft", active: func(r config.ApproveRule) bool { return r.NotDraft }, check: checkNotDraft},
	{name: "max_unresolved_threads", active: func(config.ApproveRule) bool { return true }, check: checkUnresolvedThreads},
	{name: "codeowners_covered", fileLevel: true, needs: needOwnership, needsFor: func(r config.ApproveRule) loadNeeds { return generatedNeedFor(r.PathsAnyMatch) }, active: func(r config.ApproveRule) bool { return r.CodeownersCovered }, detail: checkCodeownersCovered},
	{name: "mine_comments_only", fileLevel: true, needs: needOwnership | needPatches, active: func(r config.ApproveRule) bool { return r.MineCommentsOnly }, detail: checkMineCommentsOnly},
}

// EvaluateRule evaluates every condition the rule sets. It reads only what
// pr already holds.
func EvaluateRule(rule config.ApproveRule, pr github.ApprovalPullRequest) RuleResult {
	return evaluateRule(rule, pr, nil, nil)
}

// lazyLoads loads the data of the expensive conditions the first time a
// rule needs it. Each function memoizes, so a pull request loads each at
// most once however many rules need it.
type lazyLoads struct {
	ownership func() *github.Ownership
	// patches returns the patches, or why there are none.
	patches   func() ([]github.PullFile, string)
	generated func() ([]string, string)
}

// apply fills pr with the data in need that it does not hold yet and
// reports whether it changed pr.
func (l *lazyLoads) apply(pr *github.ApprovalPullRequest, need loadNeeds) bool {
	if l == nil {
		return false
	}
	changed := false
	if need&needOwnership != 0 && pr.Ownership == nil && l.ownership != nil {
		pr.Ownership = l.ownership()
		changed = true
	}
	if need&needPatches != 0 && pr.Patches == nil && pr.PatchesErr == "" && l.patches != nil {
		pr.Patches, pr.PatchesErr = l.patches()
		changed = true
	}
	if need&needGenerated != 0 && !pr.GeneratedLoaded && pr.GeneratedErr == "" && l.generated != nil {
		pr.GeneratedPatterns, pr.GeneratedErr = l.generated()
		pr.GeneratedLoaded = true
		changed = true
	}
	return changed
}

// fileChecksNeedPatches reports whether a file_checks check on a changed
// file reads its patch.
func fileChecksNeedPatches(rule config.ApproveRule, pr github.ApprovalPullRequest) bool {
	if pr.FilesErr != "" {
		return false
	}
	if config.FileChecksUsesGeneratedList(rule.FileChecks) && (!pr.GeneratedLoaded || pr.GeneratedErr != "") {
		return false
	}
	for _, file := range pr.Files {
		if _, check, _, ok := config.MatchFileCheckWithGenerated(rule.FileChecks, pr.GeneratedPatterns, file.Path); ok && check.Builtin != config.FileCheckOnlyDeletions {
			return true
		}
	}
	return false
}

// evaluateRule evaluates the rule's cheap conditions first. The expensive
// ones, which need CODEOWNERS or patches, load their data through loads only
// when every cheap condition passed. They are reported in table order, and
// as not evaluated when a cheap one failed. With nil loads, pr already holds
// the data and every condition is evaluated.
func evaluateRule(rule config.ApproveRule, pr github.ApprovalPullRequest, fileCheck FileCheckFunc, loads *lazyLoads) RuleResult {
	ev := &ruleEval{rule: rule, pr: pr, loads: loads, hasFileChecks: len(rule.FileChecks) > 0}
	ev.refilter()
	cheap := make(map[string]ConditionResult)
	cheapFailed := ""
	for _, cond := range conditions {
		if !cond.active(rule) || condNeeds(cond, rule) != 0 {
			continue
		}
		got := ev.evaluate(cond)
		cheap[cond.name] = got
		if !got.Pass && cheapFailed == "" {
			cheapFailed = cond.name
		}
	}
	reposPassed := true
	if got, ok := cheap["repos"]; ok {
		reposPassed = got.Pass
	}
	if ev.hasFileChecks && config.FileChecksUsesGeneratedList(rule.FileChecks) && reposPassed {
		if ev.loads.apply(&ev.pr, needGenerated) {
			ev.refilter()
		}
	}
	result := RuleResult{Rule: rule.Name, Matched: true}
	for _, cond := range conditions {
		if !cond.active(rule) {
			continue
		}
		got := ev.conditionResult(rule, cond, cheap, cheapFailed)
		result.Conditions = append(result.Conditions, got)
		if !got.Pass {
			result.Matched = false
		}
	}
	if ev.hasFileChecks {
		got := ev.fileChecks(&result, reposPassed, fileCheck)
		result.Conditions = append(result.Conditions, got)
	}
	return result
}

func (e *ruleEval) conditionResult(rule config.ApproveRule, cond condition, cheap map[string]ConditionResult, cheapFailed string) ConditionResult {
	if got, ok := cheap[cond.name]; ok {
		return got
	}
	if cheapFailed != "" && cond.name == "mine_comments_only" {
		if reason := mineCommentsSizeReason(rule, e.pr); reason != "" {
			return ConditionResult{Condition: cond.name, Saw: reason}
		}
	}
	if cheapFailed != "" && e.loads != nil {
		return ConditionResult{Condition: cond.name, Saw: "not evaluated: " + cheapFailed + " failed", NotEvaluated: true}
	}
	e.load(cond)
	return e.evaluate(cond)
}

// ruleEval is one rule's evaluation of one pull request. pr gains the data
// the expensive conditions load.
type ruleEval struct {
	rule          config.ApproveRule
	pr            github.ApprovalPullRequest
	loads         *lazyLoads
	hasFileChecks bool
	// uncoveredPR is pr without the files file_checks covers.
	uncoveredPR github.ApprovalPullRequest
	allCovered  bool
}

// refilter recomputes uncoveredPR after pr changed.
func (e *ruleEval) refilter() {
	if e.hasFileChecks {
		e.uncoveredPR, e.allCovered = filterUncoveredPR(e.rule, e.pr)
	}
}

// allFilesCovered reports that file_checks covers every changed file, so a
// file-level condition has nothing left to judge.
func (e *ruleEval) allFilesCovered(cond condition) bool {
	return e.hasFileChecks && cond.fileLevel && e.allCovered && e.pr.FilesErr == ""
}

// load fills pr with the data cond needs, unless cond fails without it.
func (e *ruleEval) load(cond condition) {
	if e.allFilesCovered(cond) || (cond.skipLoad != nil && cond.skipLoad(e.pr)) {
		return
	}
	if e.loads.apply(&e.pr, condNeeds(cond, e.rule)) {
		e.refilter()
	}
}

// evaluate runs one condition on pr, or on the files file_checks does not
// cover when cond is file-level.
func (e *ruleEval) evaluate(cond condition) ConditionResult {
	got := ConditionResult{Condition: cond.name}
	targetPR := e.pr
	if e.hasFileChecks && cond.fileLevel {
		if e.allFilesCovered(cond) {
			got.Pass = true
			got.Saw = "all files covered by file_checks"
			return got
		}
		targetPR = e.uncoveredPR
	}
	if cond.detail != nil {
		got.Pass, got.Saw, got.Details = cond.detail(e.rule, targetPR)
	} else {
		got.Pass, got.Saw = cond.check(e.rule, targetPR)
	}
	return got
}

// fileChecks runs the rule's file_checks condition. It loads patches only
// when a check reads one.
func (e *ruleEval) fileChecks(result *RuleResult, reposPassed bool, fileCheck FileCheckFunc) ConditionResult {
	got := ConditionResult{Condition: fileChecksCondition}
	if !reposPassed {
		got.Saw = "not run: repos did not match"
		result.Matched = false
		return got
	}
	if fileChecksNeedPatches(e.rule, e.pr) {
		e.loads.apply(&e.pr, needPatches)
	}
	if fileCheck == nil {
		fileCheck = func(r config.ApproveRule, p github.ApprovalPullRequest) (bool, string, []string, []FileCheckPass) {
			return evaluateFileChecks(r, p, ExecFileCheckCommand, nil)
		}
	}
	got.Pass, got.Saw, got.Details, result.FileCheckPasses = fileCheck(e.rule, e.pr)
	got.Details = explainUncoveredFiles(got.Details, e.rule.UncoveredLimitedBy(), result.Conditions)
	if !got.Pass {
		result.Matched = false
	}
	return got
}

// explainUncoveredFiles reports which conditions judge files that file_checks
// does not cover, including their outcomes when those conditions were run.
func explainUncoveredFiles(details, limits []string, conditions []ConditionResult) []string {
	if len(limits) == 0 {
		return details
	}
	outcomes := make(map[string]ConditionResult, len(conditions))
	for _, cond := range conditions {
		outcomes[cond.Condition] = cond
	}
	var label strings.Builder
	for i, name := range limits {
		if i > 0 {
			label.WriteString(", ")
		}
		label.WriteString(name)
		if cond, ok := outcomes[name]; ok && !cond.NotEvaluated {
			if cond.Pass {
				label.WriteString(", which passed")
			} else {
				label.WriteString(", which failed")
			}
		}
	}
	old := "not covered (accepted by " + strings.Join(limits, ", ") + ")"
	newLabel := "not covered (left to " + label.String() + ")"
	result := make([]string, len(details))
	for i, detail := range details {
		result[i] = strings.Replace(detail, old, newLabel, 1)
	}
	return result
}

// filterUncoveredPR returns a copy of pr containing only files not covered by
// rule.FileChecks, and reports whether all files in pr were covered.
func filterUncoveredPR(rule config.ApproveRule, pr github.ApprovalPullRequest) (github.ApprovalPullRequest, bool) {
	if pr.FilesErr != "" {
		return pr, false
	}
	if config.FileChecksUsesGeneratedList(rule.FileChecks) && (!pr.GeneratedLoaded || pr.GeneratedErr != "") {
		return pr, false
	}
	uncovered := pr
	uncovered.Files = nil
	uncovered.Additions = 0
	uncovered.Deletions = 0
	uncoveredSet := make(map[string]bool)
	for _, f := range pr.Files {
		if _, _, _, covered := config.MatchFileCheckWithGenerated(rule.FileChecks, pr.GeneratedPatterns, f.Path); covered {
			continue
		}
		uncovered.Files = append(uncovered.Files, f)
		uncovered.Additions += f.Additions
		uncovered.Deletions += f.Deletions
		uncoveredSet[f.Path] = true
	}
	uncovered.ChangedFiles = len(uncovered.Files)
	if pr.Patches != nil {
		uncovered.Patches = make([]github.PullFile, 0, len(uncovered.Files))
		for _, p := range pr.Patches {
			if uncoveredSet[p.Path] {
				uncovered.Patches = append(uncovered.Patches, p)
			}
		}
	}
	if pr.Ownership != nil {
		own := *pr.Ownership
		own.Files = nil
		for _, f := range pr.Ownership.Files {
			if uncoveredSet[f.Path] {
				own.Files = append(own.Files, f)
			}
		}
		uncovered.Ownership = &own
	}
	return uncovered, len(uncovered.Files) == 0
}

// AgentFunc runs a rule's agent command.
type AgentFunc func(rule config.ApproveRule) AgentResult

// Evaluate evaluates every rule and returns the name of the first matching
// rule, or "" when none matches. A rule's agent command runs last, only when
// its other conditions passed and no earlier rule matched. A nil agent runs
// no agent command, and a rule that sets one does not match.
func Evaluate(rules []config.ApproveRule, pr github.ApprovalPullRequest, agent AgentFunc) ([]RuleResult, string) {
	return evaluateWithFileChecks(rules, pr, agent, nil)
}

func evaluateWithFileChecks(rules []config.ApproveRule, pr github.ApprovalPullRequest, agent AgentFunc, fileCheck FileCheckFunc) ([]RuleResult, string) {
	return evaluateRules(rules, pr, agent, fileCheck, nil)
}

func evaluateRules(rules []config.ApproveRule, pr github.ApprovalPullRequest, agent AgentFunc, fileCheck FileCheckFunc, loads *lazyLoads) ([]RuleResult, string) {
	results := make([]RuleResult, 0, len(rules))
	matched := ""
	for _, rule := range rules {
		result := evaluateRule(rule, pr, fileCheck, loads)
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

// unreadable fails a condition whose data could not be read. A pull request
// over the patch size cap says so without the prefix.
func unreadable(reason string) (bool, string) {
	return false, unreadableText(reason)
}

func unreadableText(reason string) string {
	if strings.HasPrefix(reason, tooLarge) {
		return reason
	}
	return "unreadable: " + reason
}

func firstGlob(patterns []string, value string) string {
	for _, pattern := range patterns {
		if ok, err := doublestar.Match(pattern, value); err == nil && ok {
			return pattern
		}
	}
	return ""
}

func matchesAny(patterns []string, value string) bool {
	return firstGlob(patterns, value) != ""
}

func containsFold(items []string, value string) bool {
	return slices.ContainsFunc(items, func(item string) bool { return strings.EqualFold(item, value) })
}

func checkRepos(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string) {
	repo := pr.Ref.Owner + "/" + pr.Ref.Repo
	return matchesAny(rule.Repos, repo), repo
}

func checkPathsAllMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string) {
	if pr.FilesErr != "" {
		pass, saw := unreadable(pr.FilesErr)
		return pass, saw, nil
	}
	if reason := generatedListError(rule.PathsAllMatch, pr); reason != "" {
		return false, reason, nil
	}
	var outside, details []string
	for _, file := range pr.Files {
		matched, fromGen, ok := matchGlobsWithGenerated(rule.PathsAllMatch, pr, file.Path)
		if !ok {
			outside = append(outside, file.Path)
			continue
		}
		if fromGen {
			details = append(details, formatGeneratedMatch(file.Path, matched))
		}
	}
	if len(outside) > 0 {
		return false, "outside globs: " + strings.Join(outside, ", "), details
	}
	return true, fmt.Sprintf("all %d files match", len(pr.Files)), details
}

func checkPathsNoneMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string) {
	if pr.FilesErr != "" {
		pass, saw := unreadable(pr.FilesErr)
		return pass, saw, nil
	}
	if reason := generatedListError(rule.PathsNoneMatch, pr); reason != "" {
		return false, reason, nil
	}
	var inside, details []string
	for _, file := range pr.Files {
		if matched, fromGen, ok := matchGlobsWithGenerated(rule.PathsNoneMatch, pr, file.Path); ok {
			inside = append(inside, file.Path)
			if fromGen {
				details = append(details, formatGeneratedMatch(file.Path, matched))
			}
		}
	}
	if len(inside) > 0 {
		return false, "matched: " + strings.Join(inside, ", "), details
	}
	return true, fmt.Sprintf("none of %d files match", len(pr.Files)), nil
}

func checkPathsAnyMatch(rule config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string) {
	if pr.FilesErr != "" {
		pass, saw := unreadable(pr.FilesErr)
		return pass, saw, nil
	}
	if reason := generatedListError(rule.PathsAnyMatch, pr); reason != "" {
		return false, reason, nil
	}
	var firstMatch string
	var details []string
	for _, file := range pr.Files {
		if matched, fromGen, ok := matchGlobsWithGenerated(rule.PathsAnyMatch, pr, file.Path); ok {
			if firstMatch == "" {
				firstMatch = file.Path
			}
			if fromGen {
				details = append(details, formatGeneratedMatch(file.Path, matched))
			}
		}
	}
	if firstMatch != "" {
		return true, "matched: " + firstMatch, details
	}
	return false, fmt.Sprintf("none of %d files match", len(pr.Files)), nil
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
	if genErr := generatedListError(rule.PathsAnyMatch, pr); genErr != "" {
		return false, genErr, nil
	}
	var blocked, details []string
	for _, file := range pr.Ownership.Files {
		owners := strings.Join(file.Owners, " ")
		if owners == "" {
			owners = "none"
		}
		matchedAny, fromGen, inAny := matchGlobsWithGenerated(rule.PathsAnyMatch, pr, file.Path)
		switch {
		case inAny:
			if fromGen {
				details = append(details, "pass "+formatGeneratedMatch(file.Path, matchedAny)+" (in paths_any_match)")
			} else {
				details = append(details, "pass "+file.Path+": in paths_any_match")
			}
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

// checkMineCommentsOnly passes when at least one changed file is owned by
// the viewer or a team of the viewer, and each such file changes only
// comment or blank lines. Any owner whose membership cannot be read fails it.
func checkMineCommentsOnly(_ config.ApproveRule, pr github.ApprovalPullRequest) (bool, string, []string) {
	if reason := mineUnreadable(pr); reason != "" {
		pass, saw := unreadable(reason)
		return pass, saw, nil
	}
	patches := make(map[string]github.PullFile, len(pr.Patches))
	for _, file := range pr.Patches {
		patches[file.Path] = file
	}
	var unknown, failed, mine, details []string
	for _, file := range pr.Ownership.Files {
		if len(file.Unknown) > 0 {
			unknown = append(unknown, file.Unknown...)
			details = append(details, "FAIL "+file.Path+": cannot tell whether you own it: "+strings.Join(file.Unknown, "; "))
			continue
		}
		if !file.Mine {
			continue
		}
		mine = append(mine, file.Path)
		label := file.Path + " (" + file.Entry + ")"
		patch, ok := patches[file.Path]
		why, pass := "patch unavailable for "+file.Path, false
		if ok {
			why, pass = commentOnlyChange(patch)
		}
		if !pass {
			failed = append(failed, why)
			details = append(details, "FAIL "+label+": "+why)
			continue
		}
		details = append(details, "pass "+label+": comment or blank lines only")
	}
	switch {
	case len(unknown) > 0:
		return false, "unreadable: team data: " + strings.Join(dedupe(unknown), "; "), details
	case len(failed) > 0:
		return false, strings.Join(failed, "; "), details
	case len(mine) == 0:
		return false, "no changed file is owned by you or your teams", details
	}
	return true, "comment-only: " + strings.Join(mine, ", "), details
}

// mineUnreadable returns why mine_comments_only cannot read its data, or "".
func mineUnreadable(pr github.ApprovalPullRequest) string {
	switch {
	case pr.FilesErr != "":
		return pr.FilesErr
	case pr.Ownership == nil:
		return "CODEOWNERS not read"
	case pr.Ownership.Err != "":
		return pr.Ownership.Err
	case pr.PatchesErr != "":
		return pr.PatchesErr
	case pr.Patches == nil:
		return "patches not loaded"
	}
	return ""
}

// myFiles returns the changed files the viewer or a team of the viewer owns,
// and those owners, in first-seen order.
func myFiles(pr github.ApprovalPullRequest) ([]string, []string) {
	if pr.Ownership == nil {
		return nil, nil
	}
	var files, owners []string
	for _, file := range pr.Ownership.Files {
		if file.Mine {
			files = append(files, file.Path)
			owners = append(owners, file.MineOwners...)
		}
	}
	return files, dedupe(owners)
}

// dedupe drops repeated items and keeps the first of each.
func dedupe(items []string) []string {
	var out []string
	for _, item := range items {
		if !slices.Contains(out, item) {
			out = append(out, item)
		}
	}
	return out
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

// openChangeRequestBy returns the login of the first reviewer (in review order)
// whose latest opinionated review (APPROVED, CHANGES_REQUESTED, or DISMISSED)
// is CHANGES_REQUESTED, across all commits on pr. COMMENTED and PENDING reviews
// do not change a reviewer's state. It returns "" when no reviewer has an open
// change request.
//
// The viewer's own earlier change request also blocks so gh-approve never
// auto-approves over a change request that has not been dismissed or resolved.
func openChangeRequestBy(pr github.ApprovalPullRequest) string {
	type reviewerState struct {
		login string
		state string
	}
	var order []string
	latest := make(map[string]reviewerState)
	for _, review := range pr.Reviews {
		if review.Author == "" {
			continue
		}
		switch review.State {
		case "APPROVED", "CHANGES_REQUESTED", "DISMISSED":
		default:
			continue
		}
		key := strings.ToLower(review.Author)
		if _, seen := latest[key]; !seen {
			order = append(order, key)
		}
		latest[key] = reviewerState{login: review.Author, state: review.State}
	}
	for _, key := range order {
		if latest[key].state == "CHANGES_REQUESTED" {
			return latest[key].login
		}
	}
	return ""
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
