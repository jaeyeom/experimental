// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// One gh-approve rule. Every condition in a rule must hold (AND).
// A pull request matches when any rule matches (OR).
// A list left empty and an Int left unset add no condition.
type ApproveRule struct {
	// Rule name. It is shown in output, written to the audit log, and
	// appended to the approval body as <!-- gh-approve: <name> -->.
	Name string `pkl:"name"`

	// Repositories as owner/name or globs such as "acme/*".
	Repos []string `pkl:"repos"`

	// Globs. Every changed file must match one of them.
	PathsAllMatch []string `pkl:"paths_all_match"`

	// Globs. No changed file may match any of them.
	PathsNoneMatch []string `pkl:"paths_none_match"`

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
}
