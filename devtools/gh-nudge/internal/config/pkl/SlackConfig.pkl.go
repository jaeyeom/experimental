// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// Slack-related configuration
type SlackConfig struct {
	// Slack Bot OAuth token (starts with xoxb-)
	Token string `pkl:"token"`

	// Fallback channel if no routing patterns match
	DefaultChannel string `pkl:"default_channel"`

	// GitHub username to Slack user ID mapping (for @mentions)
	UserIdMapping map[string]string `pkl:"user_id_mapping"`

	// GitHub username to Slack DM channel ID mapping
	DmChannelIdMapping map[string]string `pkl:"dm_channel_id_mapping"`

	// Channel routing rules based on file patterns
	ChannelRouting []ChannelRoutingConfig `pkl:"channel_routing"`
}
