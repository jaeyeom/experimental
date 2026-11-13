// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// gh-approve "mark viewed" settings. Off by default. gh-nudge and
// gh-merge ignore this section.
type ViewedConfig struct {
	// When false, gh-approve never marks a file viewed.
	Enabled bool `pkl:"enabled"`

	// How many files one run may mark. Zero marks nothing.
	MaxMarksPerRun int `pkl:"max_marks_per_run"`

	// Rules. A file is marked when any rule selects it.
	Rules []ViewedRule `pkl:"rules"`
}
