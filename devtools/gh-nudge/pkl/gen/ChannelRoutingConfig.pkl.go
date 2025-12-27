// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// Channel routing rule based on file patterns
type ChannelRoutingConfig struct {
	// Regex pattern to match file paths
	Pattern string `pkl:"pattern"`

	// Slack channel to route matching PRs to
	Channel string `pkl:"channel"`
}
