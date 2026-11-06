// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// Settings gh-merge reads for one repository.
// A field set on the entry replaces the global field.
// A field left unset inherits the global field.
// An explicit empty list is set, and it does not inherit.
// gh-nudge ignores this class.
type RepoSettings struct {
	// Labels that stop gh-merge from merging a pull request.
	// Unset inherits settings.block_merge_labels. An empty list blocks nothing.
	BlockMergeLabels *[]string `pkl:"block_merge_labels"`

	// Check names that must be green before gh-merge queue enqueues.
	// Unset inherits settings.required_checks.
	// An empty list still makes queue refuse unless --allow-no-required-checks is set.
	RequiredChecks *[]string `pkl:"required_checks"`

	// Regular expressions matched against a failing merge-queue log.
	// Unset inherits settings.flake_signatures. An empty list adds no hints.
	FlakeSignatures *[]string `pkl:"flake_signatures"`

	// Comment template that must match before a removed pull request can be
	// queued again. Placeholders are {event_id} and {class}.
	// Unset inherits settings.rca_marker. An empty string matches nothing.
	RcaMarker *string `pkl:"rca_marker"`

	// How many earlier flake or infra requeues are allowed.
	// Unset inherits settings.max_requeues. Zero means no requeue is allowed.
	MaxRequeues *int `pkl:"max_requeues"`

	// Lines of a CircleCI failing step that match this regular expression.
	// Unset inherits settings.failure_log_pattern.
	// An empty string uses the default pattern.
	FailureLogPattern *string `pkl:"failure_log_pattern"`

	// How gh-merge reads CircleCI logs: auto, api, or cli.
	// Unset inherits settings.circleci_log. An empty string means auto.
	CircleciLog *string `pkl:"circleci_log"`
}
