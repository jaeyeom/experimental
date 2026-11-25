// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// gh-pr-review rerequest settings. gh-nudge, gh-approve, and gh-merge ignore
// this section.
type RerequestConfig struct {
	// Reviewer logins or glob patterns (such as "*-bot" or "agent-*") to skip
	// when re-requesting reviewers, in addition to GitHub Bot accounts.
	ExcludeReviewers []string `pkl:"exclude_reviewers"`
}
