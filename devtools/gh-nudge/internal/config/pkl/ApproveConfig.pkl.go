// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// gh-approve settings. gh-nudge and gh-merge ignore this section.
type ApproveConfig struct {
	// Approval body. The matching rule name is appended as an HTML comment.
	Body string `pkl:"body"`

	// Skip a pull request the user already approved at the current head.
	SkipAlreadyApproved bool `pkl:"skip_already_approved"`

	// Rules. A pull request that matches none of them is left for a human.
	Rules []ApproveRule `pkl:"rules"`
}
