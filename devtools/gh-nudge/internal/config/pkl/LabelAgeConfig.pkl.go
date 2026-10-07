// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// A label that must have been on the pull request for a minimum time.
type LabelAgeConfig struct {
	// Label name. Matched exactly, including case.
	Label string `pkl:"label"`

	// Minimum hours since that label was most recently added.
	MinHours int `pkl:"min_hours"`
}
