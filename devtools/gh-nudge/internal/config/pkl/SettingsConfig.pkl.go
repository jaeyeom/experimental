// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// General application settings
type SettingsConfig struct {
	// Hours between reminder notifications for the same outstanding review request
	ReminderThresholdHours int `pkl:"reminder_threshold_hours"`

	// Only send notifications during working hours
	WorkingHoursOnly bool `pkl:"working_hours_only"`

	// Template for reminder messages. Variables: {slack_id}, {title}, {hours}, {url}
	MessageTemplate string `pkl:"message_template"`

	// Send DMs to reviewers by default instead of channel messages
	DmByDefault bool `pkl:"dm_by_default"`

	// Skip nudging unless the PR has all of these labels. Empty means no requirement.
	RequireLabels []string `pkl:"require_labels"`

	// Skip nudging if the PR has any of these labels. Empty means no skip filter.
	SkipLabels []string `pkl:"skip_labels"`

	// Skip nudging these GitHub users. Empty means no users are skipped.
	// Skipped users do not need a Slack user ID mapping.
	SkipUsers []string `pkl:"skip_users"`

	// Each entry must be present and at least this old since it was last added.
	// Empty means no label-age requirement. Every entry is required.
	RequireLabelAges []LabelAgeConfig `pkl:"require_label_ages"`

	// Minimum hours since this reviewer was first requested on the pull request.
	// Zero means no first-request age requirement. A later request does not restart the clock.
	MinFirstReviewRequestHours int `pkl:"min_first_review_request_hours"`
}
