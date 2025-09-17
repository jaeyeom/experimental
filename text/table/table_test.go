package table

import (
	"os"
	"strings"
	"testing"
)

func TestNewTable(t *testing.T) {
	table := NewTable()
	if table == nil {
		t.Fatal("NewTable() returned nil")
		return
	}

	if table.config.Width < 40 {
		t.Errorf("Expected default width > 40, got %d", table.config.Width)
	}

	if table.config.WidthMode != WidthAuto {
		t.Errorf("Expected default width mode WidthAuto, got %d", table.config.WidthMode)
	}

	if table.config.CellMode != CellWrap {
		t.Errorf("Expected default cell mode CellWrap, got %d", table.config.CellMode)
	}
}

func TestTableWithWidth(t *testing.T) {
	table := NewTable()

	// Test fixed width
	table.WithWidth(WidthFixed, 80)
	if table.config.WidthMode != WidthFixed {
		t.Errorf("Expected WidthFixed, got %d", table.config.WidthMode)
	}
	if table.config.Width != 80 {
		t.Errorf("Expected width 80, got %d", table.config.Width)
	}

	// Test unlimited width
	table.WithWidth(WidthUnlimited)
	if table.config.WidthMode != WidthUnlimited {
		t.Errorf("Expected WidthUnlimited, got %d", table.config.WidthMode)
	}

	// Test auto width
	table.WithWidth(WidthAuto)
	if table.config.WidthMode != WidthAuto {
		t.Errorf("Expected WidthAuto, got %d", table.config.WidthMode)
	}
}

func TestTableAddColumn(t *testing.T) {
	table := NewTable()
	table.AddColumn("ID", 8, AlignLeft)
	table.AddColumn("Name", 0, AlignCenter)

	if len(table.columns) != 2 {
		t.Errorf("Expected 2 columns, got %d", len(table.columns))
	}

	if table.columns[0].Header != "ID" {
		t.Errorf("Expected first column header 'ID', got '%s'", table.columns[0].Header)
	}

	if table.columns[0].Width != 8 {
		t.Errorf("Expected first column width 8, got %d", table.columns[0].Width)
	}

	if table.columns[0].Alignment != AlignLeft {
		t.Errorf("Expected first column alignment AlignLeft, got %d", table.columns[0].Alignment)
	}
}

func TestTableAddRow(t *testing.T) {
	table := NewTable()
	table.AddColumn("ID", 8, AlignLeft)
	table.AddColumn("Name", 0, AlignCenter)

	table.AddRow("1", "John")
	table.AddRow("2", "Jane", "Extra")

	if len(table.rows) != 2 {
		t.Errorf("Expected 2 rows, got %d", len(table.rows))
	}

	if len(table.rows[0]) != 2 {
		t.Errorf("Expected first row to have 2 cells, got %d", len(table.rows[0]))
	}

	if table.rows[0][0] != "1" {
		t.Errorf("Expected first row first cell to be '1', got '%s'", table.rows[0][0])
	}
}

func TestWrapText(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		expected []string
	}{
		{
			name:     "short text",
			text:     "hello",
			width:    10,
			expected: []string{"hello"},
		},
		{
			name:     "exact width",
			text:     "hello",
			width:    5,
			expected: []string{"hello"},
		},
		{
			name:     "word wrap",
			text:     "hello world test",
			width:    10,
			expected: []string{"hello", "world test"},
		},
		{
			name:     "long word break",
			text:     "supercalifragilisticexpialidocious",
			width:    10,
			expected: []string{"supercalif", "ragilistic", "expialidoc", "ious"},
		},
		{
			name:     "zero width",
			text:     "test",
			width:    0,
			expected: []string{"test"},
		},
		{
			name:     "empty text",
			text:     "",
			width:    10,
			expected: []string{""},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := wrapText(tt.text, tt.width)
			if len(result) != len(tt.expected) {
				t.Errorf("Expected %d lines, got %d", len(tt.expected), len(result))
				return
			}
			for i, expected := range tt.expected {
				if result[i] != expected {
					t.Errorf("Line %d: expected '%s', got '%s'", i, expected, result[i])
				}
			}
		})
	}
}

func TestFormatCell(t *testing.T) {
	table := NewTable()

	tests := []struct {
		name      string
		content   string
		width     int
		alignment Alignment
		expected  string
	}{
		{
			name:      "left align",
			content:   "test",
			width:     8,
			alignment: AlignLeft,
			expected:  "test    ",
		},
		{
			name:      "right align",
			content:   "test",
			width:     8,
			alignment: AlignRight,
			expected:  "    test",
		},
		{
			name:      "center align even",
			content:   "test",
			width:     8,
			alignment: AlignCenter,
			expected:  "  test  ",
		},
		{
			name:      "center align odd",
			content:   "test",
			width:     7,
			alignment: AlignCenter,
			expected:  " test  ",
		},
		{
			name:      "exact width",
			content:   "testing",
			width:     7,
			alignment: AlignLeft,
			expected:  "testing",
		},
		{
			name:      "over width",
			content:   "testinglong",
			width:     7,
			alignment: AlignLeft,
			expected:  "testinglong",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := table.formatCell(tt.content, tt.width, tt.alignment)
			if result != tt.expected {
				t.Errorf("Expected '%s', got '%s'", tt.expected, result)
			}
		})
	}
}

func TestCalculateColumnWidths(t *testing.T) {
	tests := []struct {
		name     string
		setup    func() *Table
		expected []int
	}{
		{
			name: "fixed columns",
			setup: func() *Table {
				table := NewTable()
				table.WithWidth(WidthFixed, 30)
				table.AddColumn("ID", 8, AlignLeft)
				table.AddColumn("Name", 10, AlignLeft)
				table.AddColumn("Age", 5, AlignLeft)
				return table
			},
			expected: []int{8, 10, 5},
		},
		{
			name: "auto columns",
			setup: func() *Table {
				table := NewTable()
				table.WithWidth(WidthFixed, 30)
				table.AddColumn("ID", 0, AlignLeft)
				table.AddColumn("Name", 0, AlignLeft)
				return table
			},
			expected: []int{14, 13},
		},
		{
			name: "mixed columns",
			setup: func() *Table {
				table := NewTable()
				table.WithWidth(WidthFixed, 30)
				table.AddColumn("ID", 8, AlignLeft)
				table.AddColumn("Name", 0, AlignLeft)
				return table
			},
			expected: []int{8, 19},
		},
		{
			name: "unlimited width",
			setup: func() *Table {
				table := NewTable()
				table.WithWidth(WidthUnlimited)
				table.AddColumn("ID", 8, AlignLeft)
				table.AddColumn("Name", 0, AlignLeft)
				table.AddRow("1", "John")
				table.AddRow("2", "Jane")
				return table
			},
			expected: []int{8, 4}, // Content-based width for auto column
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			table := tt.setup()
			result := table.calculateColumnWidths()
			if len(result) != len(tt.expected) {
				t.Errorf("Expected %d column widths, got %d", len(tt.expected), len(result))
				return
			}
			for i, expected := range tt.expected {
				if result[i] != expected {
					t.Errorf("Column %d: expected width %d, got %d", i, expected, result[i])
				}
			}
		})
	}
}

func TestTableRender(t *testing.T) {
	table := NewTable()
	table.WithWidth(WidthFixed, 40)
	table.WithCellMode(CellClip)
	table.AddColumn("ID", 4, AlignLeft)
	table.AddColumn("Name", 10, AlignLeft)
	table.AddColumn("Status", 8, AlignCenter)

	table.AddRow("1", "John Smith", "Active")
	table.AddRow("2", "Jane Doe", "Inactive")

	result := table.Render()

	lines := strings.Split(strings.TrimSpace(result), "\n")
	if len(lines) < 4 {
		t.Errorf("Expected at least 4 lines in rendered output, got %d", len(lines))
	}

	// Check header
	if !strings.Contains(lines[0], "ID") || !strings.Contains(lines[0], "Name") || !strings.Contains(lines[0], "Status") {
		t.Errorf("Header line missing expected columns: %s", lines[0])
	}

	// Check separator
	if !strings.Contains(lines[1], "---") {
		t.Errorf("Expected separator line with dashes, got: %s", lines[1])
	}
}

func TestTableRenderWithWrapping(t *testing.T) {
	table := NewTable()
	table.WithWidth(WidthFixed, 25)
	table.WithCellMode(CellWrap)
	table.AddColumn("Description", 0, AlignLeft)

	table.AddRow("This is a very long description that should wrap")

	result := table.Render()
	lines := strings.Split(strings.TrimSpace(result), "\n")

	// Should have header, separator, and multiple wrapped lines
	if len(lines) < 4 {
		t.Errorf("Expected at least 4 lines for wrapped content, got %d", len(lines))
	}
}

func TestBuilder(t *testing.T) {
	builder := NewBuilder()
	table := builder.
		Width(WidthFixed, 50).
		Wrapping(CellClip).
		Column("ID", 8, AlignLeft).
		Column("Name", 0, AlignCenter).
		Row("1", "John").
		Row("2", "Jane").
		Build()

	if table.config.WidthMode != WidthFixed {
		t.Errorf("Expected WidthFixed, got %d", table.config.WidthMode)
	}

	if table.config.Width != 50 {
		t.Errorf("Expected width 50, got %d", table.config.Width)
	}

	if table.config.CellMode != CellClip {
		t.Errorf("Expected CellClip, got %d", table.config.CellMode)
	}

	if len(table.columns) != 2 {
		t.Errorf("Expected 2 columns, got %d", len(table.columns))
	}

	if len(table.rows) != 2 {
		t.Errorf("Expected 2 rows, got %d", len(table.rows))
	}
}

func TestGetTerminalWidth(t *testing.T) {
	// Test default width
	originalColumns := os.Getenv("COLUMNS")
	os.Unsetenv("COLUMNS")

	width := getTerminalWidth()
	if width < 40 {
		t.Errorf("Expected default width > 40, got %d", width)
	}

	// Test environment variable
	os.Setenv("COLUMNS", "80")
	width = getTerminalWidth()
	if width != 80 {
		t.Errorf("Expected width from env 80, got %d", width)
	}

	// Test invalid environment variable
	os.Setenv("COLUMNS", "invalid")
	width = getTerminalWidth()
	if width < 40 {
		t.Errorf("Expected default width > 40 for invalid env, got %d", width)
	}

	// Restore original value
	if originalColumns != "" {
		os.Setenv("COLUMNS", originalColumns)
	} else {
		os.Unsetenv("COLUMNS")
	}
}

func TestTableWithEmptyData(t *testing.T) {
	table := NewTable()
	result := table.Render()
	if result != "" {
		t.Errorf("Expected empty string for table without columns, got: %s", result)
	}

	table.AddColumn("Test", 10, AlignLeft)
	result = table.Render()
	if result == "" {
		t.Error("Expected non-empty string for table with columns but no rows")
	}
}
