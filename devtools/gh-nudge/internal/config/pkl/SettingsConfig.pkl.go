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

	// Labels that stop gh-merge from merging a pull request.
	// gh-nudge ignores this list. Empty means no label blocks a merge.
	// Names match exactly, including case. For example, "do-not-merge".
	BlockMergeLabels []string `pkl:"block_merge_labels"`

	// Check names that must be green before gh-merge queue enqueues.
	// Empty makes queue refuse unless --allow-no-required-checks is set.
	// Names match exactly, including case.
	RequiredChecks []string `pkl:"required_checks"`

	// Regular expressions matched against a failing merge-queue log.
	// A match is a hint, not a verdict. Empty means no hints.
	FlakeSignatures []string `pkl:"flake_signatures"`

	// Comment template that must match before a removed pull request can be
	// queued again. Placeholders are {event_id} and {class}.
	// Class is our-code, flake, or infra. Empty matches nothing.
	RcaMarker string `pkl:"rca_marker"`

	// How many earlier flake or infra requeues are allowed.
	// gh-merge queue refuses when the timeline count is greater than this.
	// Zero means no requeue is allowed.
	MaxRequeues int `pkl:"max_requeues"`
}
