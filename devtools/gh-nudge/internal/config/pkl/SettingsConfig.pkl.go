// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// General application settings
type SettingsConfig struct {
	// Hours between reminder notifications for the same PR/reviewer
	ReminderThresholdHours int `pkl:"reminder_threshold_hours"`

	// Only send notifications during working hours
	WorkingHoursOnly bool `pkl:"working_hours_only"`

	// Template for reminder messages. Variables: {slack_id}, {title}, {hours}, {url}
	MessageTemplate string `pkl:"message_template"`

	// Send DMs to reviewers by default instead of channel messages
	DmByDefault bool `pkl:"dm_by_default"`
}
