// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

import "github.com/apple/pkl-go/pkl"

// One gh-approve rule. Every condition in a rule must hold (AND).
// A pull request matches when any rule matches (OR).
// A list left empty and an Int left unset add no condition.
type ApproveRule struct {
	// Rule name. It is shown in output, written to the audit log, and
	// appended to the approval body as <!-- gh-approve: <name> -->.
	Name string `pkl:"name"`

	// What a matching rule does. "approve" approves the pull request.
	// "handoff" requests `reviewers`, then removes you as an individual
	// requested reviewer. A hand-off never approves.
	Action string `pkl:"action"`

	// Logins to request on a hand-off. Required for a hand-off rule and not
	// allowed on an approve rule.
	Reviewers []string `pkl:"reviewers"`

	// Repositories as owner/name or globs such as "acme/*".
	Repos []string `pkl:"repos"`

	// Globs. Every changed file must match one of them.
	PathsAllMatch []string `pkl:"paths_all_match"`

	// Globs. No changed file may match any of them.
	PathsNoneMatch []string `pkl:"paths_none_match"`

	// Globs. At least one changed file must match one of them.
	PathsAnyMatch []string `pkl:"paths_any_match"`

	// When true, every changed file must be in paths_any_match, or be owned
	// (per the base branch's CODEOWNERS) by neither you nor a team of yours,
	// or be owned only by teams of yours that have other members and are
	// requested reviewers. Unreadable CODEOWNERS or team data fails it.
	// Team membership needs the read:org scope.
	CodeownersCovered bool `pkl:"codeowners_covered"`

	// Maximum added plus deleted lines.
	MaxChangedLines *int `pkl:"max_changed_lines"`

	// Maximum added lines.
	MaxAddedLines *int `pkl:"max_added_lines"`

	// Maximum changed files.
	MaxFiles *int `pkl:"max_files"`

	// When true, the pull request must add no lines.
	PureDeletion bool `pkl:"pure_deletion"`

	// PR author logins. The author must be one of them.
	AuthorsAllow []string `pkl:"authors_allow"`

	// PR author logins. The author must not be one of them.
	AuthorsDeny []string `pkl:"authors_deny"`

	// Labels that must all be present. Names match exactly, including case.
	LabelsRequired []string `pkl:"labels_required"`

	// Labels that must all be absent. Names match exactly, including case.
	LabelsForbidden []string `pkl:"labels_forbidden"`

	// Logins whose latest review on the current head SHA must be APPROVED.
	RequiredApprovalsBy []string `pkl:"required_approvals_by"`

	// "green": no check failed and every required check passed.
	// "allow_pending": no check failed. Pending checks are allowed.
	// Unset adds no check condition.
	Checks *string `pkl:"checks"`

	// When true, a draft pull request does not match.
	NotDraft bool `pkl:"not_draft"`

	// Maximum unresolved review threads.
	MaxUnresolvedThreads int `pkl:"max_unresolved_threads"`

	// Optional command, as argv, run only after every other condition of the
	// rule passed. It is run directly, not through a shell. Its stdin is a JSON
	// object with the PR metadata, the changed files, and the diff. It must
	// print {"decision": "approve" | "defer", "reason": "..."}. A non-zero
	// exit, a timeout, invalid JSON, or an unknown decision counts as defer.
	// Empty adds no condition.
	AgentCommand []string `pkl:"agent_command"`

	// How long agent_command may run. Its process group is killed after that.
	AgentTimeout pkl.Duration `pkl:"agent_timeout"`

	// Cache agent_command's approve and defer decisions by input hash. Set
	// false to run the command on every pass.
	AgentCache bool `pkl:"agent_cache"`
}
