package approve

import (
	"fmt"
	"io"
	"os"
	"strings"
)

// Values of --color.
const (
	ColorAuto   = "auto"
	ColorAlways = "always"
	ColorNever  = "never"
)

// UseColor reports whether output to w is colored. mode is auto, always, or
// never. auto colors only a terminal. A non-empty noColor, the NO_COLOR
// variable, turns color off.
func UseColor(mode string, w io.Writer, noColor string) (bool, error) {
	switch mode {
	case ColorAuto:
		return noColor == "" && isTerminal(w), nil
	case ColorAlways:
		return noColor == "", nil
	case ColorNever:
		return false, nil
	default:
		return false, fmt.Errorf("--color must be auto, always, or never, not %q", mode)
	}
}

func isTerminal(w io.Writer) bool {
	file, ok := w.(*os.File)
	if !ok {
		return false
	}
	info, err := file.Stat()
	return err == nil && info.Mode()&os.ModeCharDevice != 0
}

// outcome is the group a pull request's action falls in.
type outcome struct {
	marker string
	// word follows the marker on a row. heading follows it in the summary.
	word, heading string
	color         string
}

var (
	outcomeLeft      = outcome{"👀", "left for you", "Left for you", "\x1b[33m"}
	outcomeApproved  = outcome{"✅", "approved", "Approved", "\x1b[32m"}
	outcomeHandedOff = outcome{"🤝", "handed off", "Handed off", "\x1b[36m"}
	outcomeError     = outcome{"❌", "error", "Errors", "\x1b[31m"}
	outcomeSkipped   = outcome{"⏭️", "skipped", "Skipped", "\x1b[2m"}
)

// summaryOrder is the order of the summary groups.
var summaryOrder = []outcome{outcomeLeft, outcomeApproved, outcomeHandedOff, outcomeError, outcomeSkipped}

const (
	colorReset = "\x1b[0m"
	dryRunNote = " (dry run)"
	// statusWidth fits the longest status word, "left for you (dry run)".
	statusWidth = len("left for you") + len(dryRunNote)
)

func outcomeOf(action string) outcome {
	switch action {
	case ActionApproved, ActionWouldApprove:
		return outcomeApproved
	case ActionHandoff, ActionWouldHandoff:
		return outcomeHandedOff
	case ActionManual, ActionMarkViewed, ActionWouldMarkViewed:
		return outcomeLeft
	case ActionError:
		return outcomeError
	default:
		return outcomeSkipped
	}
}

// paint wraps text in color when color is on.
func paint(text, ansi string, color bool) string {
	if !color {
		return text
	}
	return ansi + text + colorReset
}

// statusCell is a row's first column: the marker and group word, with
// "(dry run)" for a would_* action, padded to one width.
func statusCell(action string, color bool) string {
	o := outcomeOf(action)
	word := o.word
	if strings.HasPrefix(action, "would_") {
		word += dryRunNote
	}
	return paint(o.marker+" "+word, o.color, color) + strings.Repeat(" ", statusWidth-len(word))
}

// writeSummary lists the pull requests under each non-empty group, with a
// count, in summaryOrder.
func writeSummary(w io.Writer, entries []LogEntry, color bool) error {
	var out strings.Builder
	for _, o := range summaryOrder {
		var lines []string
		for _, entry := range entries {
			if outcomeOf(entry.Action) == o {
				lines = append(lines, summaryLine(entry))
			}
		}
		if len(lines) == 0 {
			continue
		}
		fmt.Fprintf(&out, "\n%s (%d)\n", paint(o.marker+" "+o.heading, o.color, color), len(lines))
		for _, line := range lines {
			out.WriteString(line)
		}
	}
	if _, err := io.WriteString(w, out.String()); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	return nil
}

// summaryLine is the pull request, its title, "(dry run)" for a would_*
// action, and the first detail line its row printed.
func summaryLine(entry LogEntry) string {
	line := "  " + entry.PR
	if entry.Title != "" {
		line += "  " + entry.Title
	}
	if strings.HasPrefix(entry.Action, "would_") {
		line += dryRunNote
	}
	first, _, _ := strings.Cut(details(entry, false), "\n")
	if reason := strings.TrimSpace(first); reason != "" {
		line += " - " + reason
	}
	return line + "\n"
}
