// Package gantt provides ASCII art generation for GANTT charts.
//
// This package creates text-based timeline visualizations suitable for
// embedding in code comments, making test cases and documentation more
// understandable.
//
// Basic usage:
//
//	chart := gantt.New().
//		WithTimeUnit(5 * time.Second).
//		WithPrefix("// ").
//		WithGrid(true, 5*time.Second)
//
//	chart.AddTask("Task_A", 0, 10*time.Second)
//	chart.AddTask("Task_B", 5*time.Second, 15*time.Second)
//
//	fmt.Print(chart.Render())
//
// Output example:
//
//	// Time:   0s    5s    10s   15s
//	//         |-----|-----|-----|
//	// Task_A  |#####|#####|     |
//	// Task_B  |     |#####|#####|
//	//         |-----|-----|-----|
package gantt

import (
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/text/canvas"
)

// Chars defines the characters used for rendering task bars.
type Chars struct {
	Full      rune // Character for fully filled unit (0 to 1)
	Empty     rune // Character for empty unit
	LeftHalf  rune // Character for right side only (0.5 to 1)
	RightHalf rune // Character for left side only (0 to 0.5)
	BothEnds  rune // Character for both ends filled but not full
}

// DefaultChars provides standard ASCII characters for GANTT charts.
var DefaultChars = Chars{
	Full:      '#',
	Empty:     ' ',
	LeftHalf:  '>',
	RightHalf: '<',
	BothEnds:  'X',
}

// Task represents a single task in the GANTT chart.
type Task struct {
	Label string
	Start time.Duration
	End   time.Duration
}

// Chart represents a GANTT chart with configurable rendering options.
type Chart struct {
	tasks         []Task
	timeUnit      time.Duration // Duration per character
	prefix        string        // Comment prefix (e.g., "// ")
	chars         Chars
	showGrid      bool
	gridInterval  time.Duration
	showSeparator bool
	labelWidth    int
}

// New creates a new GANTT chart with default settings.
func New() *Chart {
	return &Chart{
		tasks:         make([]Task, 0),
		timeUnit:      time.Second,
		prefix:        "",
		chars:         DefaultChars,
		showGrid:      false,
		gridInterval:  5 * time.Second,
		showSeparator: false,
		labelWidth:    8,
	}
}

// WithTimeUnit sets the time duration each character represents.
func (c *Chart) WithTimeUnit(unit time.Duration) *Chart {
	c.timeUnit = unit
	return c
}

// WithPrefix sets the prefix for each line (e.g., comment markers).
func (c *Chart) WithPrefix(prefix string) *Chart {
	c.prefix = prefix
	return c
}

// WithChars sets custom characters for rendering task bars.
func (c *Chart) WithChars(chars Chars) *Chart {
	c.chars = chars
	return c
}

// WithGrid enables or disables the time grid header with specified interval.
func (c *Chart) WithGrid(show bool, interval time.Duration) *Chart {
	c.showGrid = show
	c.gridInterval = interval
	return c
}

// WithSeparator enables or disables separator lines above and below tasks.
func (c *Chart) WithSeparator(show bool) *Chart {
	c.showSeparator = show
	return c
}

// WithLabelWidth sets the width reserved for task labels.
func (c *Chart) WithLabelWidth(width int) *Chart {
	c.labelWidth = width
	return c
}

// AddTask adds a task to the chart with the given label, start time, and end time.
func (c *Chart) AddTask(label string, start, end time.Duration) *Chart {
	c.tasks = append(c.tasks, Task{
		Label: label,
		Start: start,
		End:   end,
	})
	return c
}

// Duration returns the total duration covered by the chart.
func (c *Chart) Duration() time.Duration {
	if len(c.tasks) == 0 {
		return 0
	}

	var maxEnd time.Duration
	for _, task := range c.tasks {
		if task.End > maxEnd {
			maxEnd = task.End
		}
	}
	return maxEnd
}

// Render generates the ASCII art representation of the GANTT chart.
func (c *Chart) Render() string {
	if len(c.tasks) == 0 {
		return ""
	}

	duration := c.Duration()
	chartWidth := int(duration / c.timeUnit)
	if chartWidth == 0 {
		chartWidth = 1
	}

	// Calculate canvas dimensions
	totalWidth := c.labelWidth + chartWidth + 2 // +2 for spacing and border
	numRows := len(c.tasks)
	if c.showGrid {
		numRows += 2 // Header row + separator
	}
	if c.showSeparator {
		numRows += 2 // Top and bottom separator
	}

	cv := canvas.New(totalWidth, numRows+1)

	var lines []string
	row := 0

	// Render time header
	if c.showGrid {
		headerLine := c.renderTimeHeader(chartWidth)
		lines = append(lines, headerLine)
		row++

		// Separator after header
		if c.showSeparator {
			sepLine := c.renderSeparator(chartWidth)
			lines = append(lines, sepLine)
			row++
		}
	}

	// Render separator before tasks
	if c.showSeparator && !c.showGrid {
		sepLine := c.renderSeparator(chartWidth)
		lines = append(lines, sepLine)
		row++
	}

	// Render each task
	for _, task := range c.tasks {
		taskLine := c.renderTask(task, chartWidth)
		lines = append(lines, taskLine)
		row++
	}

	// Render separator after tasks
	if c.showSeparator {
		sepLine := c.renderSeparator(chartWidth)
		lines = append(lines, sepLine)
	}

	// Apply prefix and join lines
	var result strings.Builder
	for i, line := range lines {
		result.WriteString(c.prefix)
		result.WriteString(line)
		if i < len(lines)-1 {
			result.WriteByte('\n')
		}
	}

	// Clear canvas reference to allow GC
	_ = cv

	return result.String()
}

// renderTimeHeader generates the time scale header line.
func (c *Chart) renderTimeHeader(width int) string {
	var sb strings.Builder

	// Label column (Time: or empty)
	label := "Time:"
	fmt.Fprintf(&sb, "%-*s", c.labelWidth, label)

	intervalUnits := int(c.gridInterval / c.timeUnit)
	if intervalUnits == 0 {
		intervalUnits = 1
	}

	// Calculate the width needed to fit the last time label
	lastTimeStr := formatDuration(time.Duration(width) * c.timeUnit)
	headerWidth := width + len(lastTimeStr)

	// Build the time header character by character
	headerChars := make([]rune, headerWidth)
	for i := range headerChars {
		headerChars[i] = ' '
	}

	// Place time labels at interval positions
	for i := 0; i <= width; i += intervalUnits {
		timeAtPos := time.Duration(i) * c.timeUnit
		timeStr := formatDuration(timeAtPos)

		// Place each character of the time string
		for j, ch := range timeStr {
			pos := i + j
			if pos < headerWidth {
				headerChars[pos] = ch
			}
		}
	}

	sb.WriteString(string(headerChars))
	return sb.String()
}

// renderSeparator generates a separator line with vertical markers.
func (c *Chart) renderSeparator(width int) string {
	var sb strings.Builder

	// Label column padding
	sb.WriteString(strings.Repeat(" ", c.labelWidth))

	// Separator with interval markers
	intervalUnits := int(c.gridInterval / c.timeUnit)
	if intervalUnits == 0 {
		intervalUnits = 1
	}

	for i := 0; i <= width; i++ {
		if i%intervalUnits == 0 {
			sb.WriteRune('|')
		} else {
			sb.WriteRune('-')
		}
	}

	return sb.String()
}

// renderTask generates a single task row.
func (c *Chart) renderTask(task Task, width int) string {
	var sb strings.Builder

	// Task label
	label := task.Label
	if len(label) > c.labelWidth-1 {
		label = label[:c.labelWidth-1]
	}
	fmt.Fprintf(&sb, "%-*s", c.labelWidth, label)

	// Task bar
	startUnit := float64(task.Start) / float64(c.timeUnit)
	endUnit := float64(task.End) / float64(c.timeUnit)

	intervalUnits := int(c.gridInterval / c.timeUnit)
	if intervalUnits == 0 {
		intervalUnits = 1
	}

	for i := 0; i <= width; i++ {
		// Check for interval marker
		if c.showSeparator && i%intervalUnits == 0 {
			sb.WriteRune('|')
			continue
		}

		// Calculate fill for this unit
		unitStart := float64(i)
		unitEnd := float64(i + 1)

		char := c.getCharForUnit(unitStart, unitEnd, startUnit, endUnit)
		sb.WriteRune(char)
	}

	return sb.String()
}

// getCharForUnit determines the character to display for a time unit.
func (c *Chart) getCharForUnit(unitStart, unitEnd, taskStart, taskEnd float64) rune {
	// No overlap
	if taskEnd <= unitStart || taskStart >= unitEnd {
		return c.chars.Empty
	}

	// Calculate overlap
	overlapStart := max(unitStart, taskStart)
	overlapEnd := min(unitEnd, taskEnd)
	overlap := overlapEnd - overlapStart

	// Full overlap
	if overlap >= 0.99 {
		return c.chars.Full
	}

	// Partial overlap - determine position
	relStart := overlapStart - unitStart
	relEnd := overlapEnd - unitStart

	// Check for various partial fill cases
	leftFilled := relStart < 0.25
	rightFilled := relEnd > 0.75
	centerFilled := relStart < 0.5 && relEnd > 0.5

	if leftFilled && rightFilled {
		if overlap < 0.5 {
			return c.chars.BothEnds
		}
		return c.chars.Full
	}
	if leftFilled && centerFilled {
		return c.chars.LeftHalf
	}
	if rightFilled && centerFilled {
		return c.chars.RightHalf
	}
	if overlap >= 0.5 {
		return c.chars.Full
	}
	if overlap >= 0.25 {
		if relStart < 0.5 {
			return c.chars.LeftHalf
		}
		return c.chars.RightHalf
	}

	return c.chars.Empty
}

// formatDuration formats a duration for display in the time header.
func formatDuration(d time.Duration) string {
	if d == 0 {
		return "0s"
	}

	// Handle sub-second durations
	if d < time.Second {
		ms := d.Milliseconds()
		return fmt.Sprintf("%dms", ms)
	}

	// Handle seconds and minutes
	if d < time.Minute {
		return fmt.Sprintf("%ds", int(d.Seconds()))
	}

	// Format as minutes and seconds
	minutes := int(d.Minutes())
	seconds := int(d.Seconds()) % 60
	return fmt.Sprintf("%dm%ds", minutes, seconds)
}
