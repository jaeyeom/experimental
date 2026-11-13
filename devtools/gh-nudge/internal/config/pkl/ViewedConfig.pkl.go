// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// gh-approve "mark viewed" settings. Off by default. gh-nudge and
// gh-merge ignore this section.
type ViewedConfig struct {
	// When false, gh-approve never marks a file viewed.
	Enabled bool `pkl:"enabled"`

	// How many files one run may mark. Zero marks nothing.
	MaxMarksPerRun int `pkl:"max_marks_per_run"`

	// Globs. A changed file matching any of them is never marked, by any rule.
	// This wins over every rule's selectors.
	NeverMark []string `pkl:"never_mark"`

	// Rules. A file is marked when any rule selects it and never_mark does not.
	Rules []ViewedRule `pkl:"rules"`
}
