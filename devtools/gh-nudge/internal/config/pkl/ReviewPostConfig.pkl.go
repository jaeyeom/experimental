// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// gh-pr-review submit settings. gh-nudge, gh-approve, and gh-merge ignore
// this section.
type ReviewPostConfig struct {
	// When true, `gh-pr-review submit` hands comments to `gh-outbox` instead of
	// posting directly to GitHub. Off by default.
	ViaOutbox bool `pkl:"via_outbox"`

	// Default `--owner` agentbus name passed to `gh-outbox submit`.
	Owner string `pkl:"owner"`

	// Command, as argv, used to invoke `gh-outbox`. Empty defaults to ["gh-outbox"].
	OutboxCommand []string `pkl:"outbox_command"`
}
