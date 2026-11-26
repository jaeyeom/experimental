package config

import "fmt"

const (
	// OutputModePR creates a pull request for the work item (the default).
	OutputModePR = "pr"
	// OutputModeMerge rebases onto the base branch to keep history linear
	// and fast-forward pushes to the base branch.
	OutputModeMerge = "merge"
)

// Output is the optional [output] table: how finished worker changes land.
// It is allowed at the top level ([output]), per source ([source.output]),
// and per lane ([lane.output]). A lane overrides its source, and a source
// overrides the top level.
type Output struct {
	// Mode is OutputModePR (the default) or OutputModeMerge.
	// On a lane, empty means the source's mode.
	Mode string `toml:"mode"`
}

// Over returns base with every field set in o replacing base's.
func (o Output) Over(base Output) Output {
	if o.Mode != "" {
		base.Mode = o.Mode
	}
	return base
}

type rawOutput struct {
	Mode *string `toml:"mode"`
}

func (raw rawOutput) toOutput() Output {
	return raw.over(Output{Mode: OutputModePR})
}

func (raw rawOutput) over(base Output) Output {
	if raw.Mode != nil {
		base.Mode = *raw.Mode
	}
	return base
}

func laneOutputSet(name string, raw rawOutput) error {
	if raw.Mode != nil && *raw.Mode == "" {
		return fmt.Errorf("lane %q output.mode: %w", name, ValidOutputMode(""))
	}
	return nil
}

// ValidOutputMode checks an [output] mode value: pr or merge.
func ValidOutputMode(value string) error {
	switch value {
	case OutputModePR, OutputModeMerge:
		return nil
	case "":
		return fmt.Errorf("expected %s or %s, found empty", OutputModePR, OutputModeMerge)
	default:
		return fmt.Errorf("expected %s or %s, found %q", OutputModePR, OutputModeMerge, value)
	}
}

func validOutput(where string, o Output) error {
	if err := ValidOutputMode(o.Mode); err != nil {
		return fmt.Errorf("%s.mode: %w", where, err)
	}
	return nil
}
