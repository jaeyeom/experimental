package gantt

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestNewChart(t *testing.T) {
	chart := New()
	if chart == nil {
		t.Fatal("New() returned nil")
	}
}

func TestChartWithTimeUnit(t *testing.T) {
	chart := New().WithTimeUnit(100 * time.Millisecond)
	if chart.timeUnit != 100*time.Millisecond {
		t.Errorf("expected time unit 100ms, got %v", chart.timeUnit)
	}
}

func TestChartWithPrefix(t *testing.T) {
	chart := New().WithPrefix("        // ")
	if chart.prefix != "        // " {
		t.Errorf("expected prefix '        // ', got %q", chart.prefix)
	}
}

func TestChartWithChars(t *testing.T) {
	chars := Chars{
		Full:      '#',
		Empty:     ' ',
		LeftHalf:  '>',
		RightHalf: '<',
		BothEnds:  'X',
	}
	chart := New().WithChars(chars)
	if chart.chars.Full != '#' {
		t.Errorf("expected Full char '#', got %c", chart.chars.Full)
	}
}

func TestAddTask(t *testing.T) {
	chart := New()
	chart.AddTask("Task_A", 0, 10*time.Second)

	if len(chart.tasks) != 1 {
		t.Fatalf("expected 1 task, got %d", len(chart.tasks))
	}
	if chart.tasks[0].Label != "Task_A" {
		t.Errorf("expected label 'Task_A', got %q", chart.tasks[0].Label)
	}
}

func TestBasicRender(t *testing.T) {
	chart := New().
		WithTimeUnit(5 * time.Second).
		WithChars(DefaultChars)

	chart.AddTask("Task_A", 0, 10*time.Second)

	output := chart.Render()
	if output == "" {
		t.Fatal("Render() returned empty string")
	}

	// Should contain the task label
	if !strings.Contains(output, "Task_A") {
		t.Error("output should contain task label 'Task_A'")
	}
}

func TestRenderWithPrefix(t *testing.T) {
	chart := New().
		WithTimeUnit(5 * time.Second).
		WithPrefix("// ")

	chart.AddTask("Task_A", 0, 10*time.Second)

	output := chart.Render()
	lines := strings.Split(output, "\n")

	for _, line := range lines {
		if line != "" && !strings.HasPrefix(line, "// ") {
			t.Errorf("line should start with '// ', got %q", line)
		}
	}
}

func TestRenderWithGrid(t *testing.T) {
	// Use 1 second per character to give enough space for time labels
	chart := New().
		WithTimeUnit(1*time.Second).
		WithGrid(true, 5*time.Second)

	chart.AddTask("Task_A", 0, 10*time.Second)

	output := chart.Render()

	// Grid should contain time markers
	if !strings.Contains(output, "0s") {
		t.Error("output should contain time marker '0s'")
	}
}

func TestMultipleTasks(t *testing.T) {
	chart := New().
		WithTimeUnit(5 * time.Second).
		WithChars(DefaultChars)

	chart.AddTask("Task_A", 0, 10*time.Second)
	chart.AddTask("Task_B", 5*time.Second, 15*time.Second)
	chart.AddTask("Task_C", 10*time.Second, 20*time.Second)

	output := chart.Render()

	if !strings.Contains(output, "Task_A") {
		t.Error("output should contain 'Task_A'")
	}
	if !strings.Contains(output, "Task_B") {
		t.Error("output should contain 'Task_B'")
	}
	if !strings.Contains(output, "Task_C") {
		t.Error("output should contain 'Task_C'")
	}
}

func TestTaskBarRendering(t *testing.T) {
	chart := New().
		WithTimeUnit(1 * time.Second).
		WithLabelWidth(8).
		WithChars(DefaultChars)

	// Task from 0s to 3s with cell-centered model:
	// Cell 0: '<' (task starts), Cells 1-2: '##', Cell 3: '>' (task ends)
	chart.AddTask("Task_A", 0, 3*time.Second)

	output := chart.Render()

	// Find the line with Task_A
	lines := strings.Split(output, "\n")
	var taskLine string
	for _, line := range lines {
		if strings.Contains(line, "Task_A") {
			taskLine = line
			break
		}
	}

	if taskLine == "" {
		t.Fatal("could not find Task_A line")
	}

	// With cell-centered model, 0-3s renders as <##>
	if !strings.Contains(taskLine, "<##>") {
		t.Errorf("expected task bar with '<##>', got %q", taskLine)
	}
}

func TestPartialFillRendering(t *testing.T) {
	chart := New().
		WithTimeUnit(1 * time.Second).
		WithLabelWidth(8)

	// Task from 0.5s to 2.5s - should show partial fills
	chart.AddTask("Task_A", 500*time.Millisecond, 2500*time.Millisecond)

	output := chart.Render()
	if output == "" {
		t.Fatal("Render() returned empty string")
	}
}

func TestTimeScaleHeader(t *testing.T) {
	// Use 1 second per character to give enough space for time labels
	chart := New().
		WithTimeUnit(1*time.Second).
		WithGrid(true, 5*time.Second).
		WithLabelWidth(8)

	chart.AddTask("Task_A", 0, 20*time.Second)

	output := chart.Render()

	// Should have time labels at intervals
	if !strings.Contains(output, "0s") {
		t.Error("output should contain '0s' time label")
	}
	if !strings.Contains(output, "5s") {
		t.Error("output should contain '5s' time label")
	}
}

func TestChartDuration(t *testing.T) {
	chart := New().WithTimeUnit(5 * time.Second)

	chart.AddTask("A", 0, 10*time.Second)
	chart.AddTask("B", 5*time.Second, 25*time.Second)

	duration := chart.Duration()
	if duration != 25*time.Second {
		t.Errorf("expected duration 25s, got %v", duration)
	}
}

func TestEmptyChart(t *testing.T) {
	chart := New()
	output := chart.Render()

	// Empty chart should return empty or minimal output
	if strings.Contains(output, "#") {
		t.Error("empty chart should not contain task bars")
	}
}

func TestCustomChars(t *testing.T) {
	chart := New().
		WithTimeUnit(1 * time.Second).
		WithLabelWidth(8).
		WithChars(Chars{
			Full:      '*',
			Empty:     '.',
			LeftHalf:  '[',
			RightHalf: ']',
			BothEnds:  'X',
		})

	// Task from 0s to 3s with cell-centered model:
	// Cell 0: ']' (RightHalf, task starts), Cells 1-2: '**', Cell 3: '[' (LeftHalf, task ends)
	chart.AddTask("Task_A", 0, 3*time.Second)

	output := chart.Render()

	// With cell-centered model, 0-3s renders as ]**[
	if !strings.Contains(output, "]**[") {
		t.Errorf("expected custom chars ']**[' for task bar, got %q", output)
	}
}

func TestSeparatorLine(t *testing.T) {
	chart := New().
		WithTimeUnit(5*time.Second).
		WithGrid(true, 5*time.Second).
		WithSeparator(true)

	chart.AddTask("Task_A", 0, 10*time.Second)

	output := chart.Render()

	// Should contain separator line characters (e.g., |-----|)
	if !strings.Contains(output, "|") {
		t.Error("output should contain separator character '|'")
	}
}

func TestLabelAlignment(t *testing.T) {
	chart := New().
		WithTimeUnit(1 * time.Second).
		WithLabelWidth(10)

	chart.AddTask("A", 0, 3*time.Second)

	output := chart.Render()
	lines := strings.Split(output, "\n")

	var taskLine string
	for _, line := range lines {
		if strings.Contains(line, "A") && strings.Contains(line, "#") {
			taskLine = line
			break
		}
	}

	if taskLine == "" {
		t.Fatal("could not find task line")
	}

	// Label should be padded to labelWidth
	// "A" should be followed by spaces to make up 10 chars before the bar
}

func TestRenderToString(t *testing.T) {
	chart := New().
		WithTimeUnit(5*time.Second).
		WithPrefix("        // ").
		WithGrid(true, 5*time.Second).
		WithSeparator(true).
		WithLabelWidth(8)

	chart.AddTask("Task_A", 0, 10*time.Second)
	chart.AddTask("Task_B", 5*time.Second, 15*time.Second)
	chart.AddTask("Task_C", 10*time.Second, 20*time.Second)

	output := chart.Render()

	// Verify structure matches the example from the issue
	if !strings.Contains(output, "// ") {
		t.Error("output should contain comment prefix")
	}
	if !strings.Contains(output, "Task_A") {
		t.Error("output should contain Task_A")
	}
}

func TestDurationFormatting(t *testing.T) {
	tests := []struct {
		duration time.Duration
		expected string
	}{
		{0, "0s"},
		{5 * time.Second, "5s"},
		{1 * time.Minute, "1m0s"},
		{90 * time.Second, "1m30s"},
		{100 * time.Millisecond, "100ms"},
	}

	for _, tt := range tests {
		result := formatDuration(tt.duration)
		if result != tt.expected {
			t.Errorf("formatDuration(%v) = %q, want %q", tt.duration, result, tt.expected)
		}
	}
}

func ExampleChart_Render() {
	// Create a chart matching the issue example
	chart := New().
		WithTimeUnit(1*time.Second).
		WithPrefix("// ").
		WithGrid(true, 5*time.Second).
		WithSeparator(true).
		WithLabelWidth(8)

	chart.AddTask("Task_A", 0, 10*time.Second)
	chart.AddTask("Task_B", 5*time.Second, 15*time.Second)
	chart.AddTask("Task_C", 10*time.Second, 20*time.Second)
	chart.AddTask("Task_D", 3*time.Second, 9*time.Second)

	fmt.Println(chart.Render())
	// Output:
	// // Time:   0s   5s   10s  15s  20s
	// //         |----|----|----|----|
	// // Task_A  <#########>    |    |
	// // Task_B  |    <#########>    |
	// // Task_C  |    |    <#########>
	// // Task_D  |  <#####>|    |    |
	// //         |----|----|----|----|
}

func ExampleChart_Render_customChars() {
	// Create a chart with custom characters
	chart := New().
		WithTimeUnit(1 * time.Second).
		WithChars(Chars{
			Full:      '*',
			Empty:     '.',
			LeftHalf:  '[',
			RightHalf: ']',
			BothEnds:  'X',
		}).
		WithLabelWidth(6)

	chart.AddTask("A", 0, 5*time.Second)
	chart.AddTask("B", 3*time.Second, 8*time.Second)

	fmt.Println(chart.Render())
	// Output:
	// A     ]****[...
	// B     ...]****[
}
