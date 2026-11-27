package config

import (
	"fmt"
	"strings"
)

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
	// PushCmd is an optional command template (such as "review-and-push-loop --base {base} --json")
	// instructed to merge-mode workers instead of raw git push. Supports {base} and {id}.
	// Only valid when effective Mode is OutputModeMerge.
	PushCmd string `toml:"push_cmd"`
}

// Over returns base with every field set in o replacing base's.
func (o Output) Over(base Output) Output {
	if o.Mode != "" {
		base.Mode = o.Mode
	}
	if o.PushCmd != "" {
		base.PushCmd = o.PushCmd
	}
	return base
}

type rawOutput struct {
	Mode    *string `toml:"mode"`
	PushCmd *string `toml:"push_cmd"`
}

func (raw rawOutput) toOutput() (Output, error) {
	if raw.PushCmd != nil && strings.TrimSpace(*raw.PushCmd) == "" {
		return Output{}, fmt.Errorf("output.push_cmd: expected a non-empty command, found %q", *raw.PushCmd)
	}
	return raw.over(Output{Mode: OutputModePR}), nil
}

func (raw rawOutput) over(base Output) Output {
	if raw.Mode != nil {
		base.Mode = *raw.Mode
	}
	if raw.PushCmd != nil {
		base.PushCmd = *raw.PushCmd
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
	if o.PushCmd != "" {
		if strings.TrimSpace(o.PushCmd) == "" {
			return fmt.Errorf("%s.push_cmd: expected a non-blank command, found whitespace", where)
		}
		if o.Mode != OutputModeMerge {
			return fmt.Errorf("%s.push_cmd: expected output.mode %q when push_cmd is set, found %q", where, OutputModeMerge, o.Mode)
		}
	}
	return nil
}
