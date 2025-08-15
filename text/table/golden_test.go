package table

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestGoldenTables tests table rendering against golden files.
func TestGoldenTables(t *testing.T) {
	tests := []struct {
		name     string
		setupFn  func() *Table
		filename string
	}{
		{
			name: "basic_fixed_width_clipped",
			setupFn: func() *Table {
				table := NewTable().
					WithWidth(WidthFixed, 50).
					WithCellMode(CellClip).
					AddColumn("ID", 4, AlignRight).
					AddColumn("Name", 15, AlignLeft).
					AddColumn("Status", 10, AlignCenter).
					AddColumn("Score", 8, AlignRight)

				table.AddRow("1", "John Smith", "Active", "95.5")
				table.AddRow("2", "Jane Doe", "Inactive", "87.2")
				table.AddRow("123", "Bob Johnson Jr.", "Pending", "92.8")
				table.AddRow("4", "Alice", "Active", "98.0")
				return table
			},
			filename: "basic_fixed_width_clipped.golden",
		},
		{
			name: "wrapped_content",
			setupFn: func() *Table {
				table := NewTable().
					WithWidth(WidthFixed, 60).
					WithCellMode(CellWrap).
					AddColumn("Service", 15, AlignLeft).
					AddColumn("Description", 0, AlignLeft).
					AddColumn("Status", 10, AlignCenter)

				table.AddRow("API Gateway", "This is a very long description that will be wrapped across multiple lines to demonstrate text wrapping functionality", "Running")
				table.AddRow("Database", "MySQL primary server", "Healthy")
				table.AddRow("Cache", "Redis cluster for session management and frequently accessed data", "Warning")
				return table
			},
			filename: "wrapped_content.golden",
		},
		{
			name: "unlimited_width",
			setupFn: func() *Table {
				table := NewTable().
					WithWidth(WidthUnlimited).
					WithCellMode(CellClip).
					AddColumn("Timestamp", 20, AlignLeft).
					AddColumn("Level", 8, AlignCenter).
					AddColumn("Component", 15, AlignLeft).
					AddColumn("Message", 0, AlignLeft)

				table.AddRow("2023-12-01 10:30:45", "INFO", "AuthService", "User login successful")
				table.AddRow("2023-12-01 10:31:02", "WARN", "DatabasePool", "Connection pool usage at 85%")
				table.AddRow("2023-12-01 10:31:15", "ERROR", "PaymentGateway", "Transaction failed: insufficient funds")
				return table
			},
			filename: "unlimited_width.golden",
		},
		{
			name: "custom_separator",
			setupFn: func() *Table {
				table := NewTable().
					WithSeparator(" │ ").
					WithWidth(WidthFixed, 45).
					WithCellMode(CellClip).
					AddColumn("Resource", 12, AlignLeft).
					AddColumn("Usage", 8, AlignRight).
					AddColumn("Limit", 8, AlignRight).
					AddColumn("Status", 10, AlignCenter)

				table.AddRow("CPU", "65%", "80%", "OK")
				table.AddRow("Memory", "4.2 GB", "8.0 GB", "OK")
				table.AddRow("Disk", "45 GB", "50 GB", "WARNING")
				return table
			},
			filename: "custom_separator.golden",
		},
		{
			name: "mixed_alignments",
			setupFn: func() *Table {
				table := NewTable().
					WithWidth(WidthFixed, 55).
					WithCellMode(CellClip).
					AddColumn("Left", 12, AlignLeft).
					AddColumn("Center", 15, AlignCenter).
					AddColumn("Right", 12, AlignRight).
					AddColumn("Auto", 0, AlignLeft)

				table.AddRow("Short", "Medium text here", "Long content", "A")
				table.AddRow("A", "B", "C", "Dynamic")
				table.AddRow("Test data", "Centered content", "Right aligned", "Sz")
				return table
			},
			filename: "mixed_alignments.golden",
		},
		{
			name: "empty_and_missing_cells",
			setupFn: func() *Table {
				table := NewTable().
					WithWidth(WidthFixed, 40).
					WithCellMode(CellClip).
					AddColumn("Col1", 8, AlignLeft).
					AddColumn("Col2", 10, AlignCenter).
					AddColumn("Col3", 8, AlignRight).
					AddColumn("Col4", 0, AlignLeft)

				table.AddRow("Data", "Present", "123", "Full")
				table.AddRow("", "Empty first", "456") // Missing last cell
				table.AddRow("Partial", "", "", "Only first and last")
				table.AddRow() // Empty row
				return table
			},
			filename: "empty_and_missing_cells.golden",
		},
		{
			name: "builder_pattern",
			setupFn: func() *Table {
				return NewBuilder().
					Width(WidthFixed, 65).
					Wrapping(CellWrap).
					Column("ID", 6, AlignRight).
					Column("File Path", 25, AlignLeft).
					Column("Size", 10, AlignRight).
					Column("Modified", 15, AlignLeft).
					Row("1", "/home/user/docs/report.pdf", "2.5 MB", "2023-12-01").
					Row("2", "/home/user/projects/myapp/src/main.go", "15.2 KB", "2023-12-02").
					Row("3", "/home/user/downloads/very-long-filename-example.zip", "150 MB", "2023-12-03").
					Build()
			},
			filename: "builder_pattern.golden",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			table := tt.setupFn()
			got := table.Render()

			goldenPath := filepath.Join("testdata", tt.filename)

			if shouldUpdateGolden() {
				updateGoldenFile(t, goldenPath, got)
				return
			}

			want := readGoldenFile(t, goldenPath)
			if got != want {
				t.Errorf("Table output mismatch for %s\nGot:\n%s\n\nWant:\n%s\n\nDiff:\n%s",
					tt.name, got, want, stringDiff(want, got))
			}
		})
	}
}

// TestGoldenTableVisual provides a test that always shows the table output
// This is useful for visual inspection during development.
func TestGoldenTableVisual(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping visual test in short mode")
	}

	table := NewBuilder().
		Width(WidthFixed, 70).
		Wrapping(CellWrap).
		Column("Feature", 15, AlignLeft).
		Column("Description", 35, AlignLeft).
		Column("Status", 12, AlignCenter).
		Row("Width Modes", "Fixed, unlimited, and auto terminal width detection", "✅ Complete").
		Row("Cell Wrapping", "Text wrapping within cells with word boundary breaking", "✅ Complete").
		Row("Alignments", "Left, center, and right text alignment options", "✅ Complete").
		Row("Separators", "Customizable column separators and padding", "✅ Complete").
		Row("Builder API", "Fluent interface for easy table construction", "✅ Complete").
		Build()

	output := table.Render()
	t.Logf("Table Visual Output:\n\n%s", output)
}

// shouldUpdateGolden checks if golden files should be updated.
func shouldUpdateGolden() bool {
	return os.Getenv("UPDATE_GOLDEN") == "1"
}

// updateGoldenFile writes the content to a golden file.
func updateGoldenFile(t *testing.T, path, content string) {
	t.Helper()

	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("Failed to create testdata directory: %v", err)
	}

	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to update golden file %s: %v", path, err)
	}

	t.Logf("Updated golden file: %s", path)
}

// readGoldenFile reads the content from a golden file.
func readGoldenFile(t *testing.T, path string) string {
	t.Helper()

	content, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("Failed to read golden file %s: %v", path, err)
	}

	return string(content)
}

// stringDiff provides a simple diff between two strings.
func stringDiff(want, got string) string {
	wantLines := strings.Split(want, "\n")
	gotLines := strings.Split(got, "\n")

	var diff strings.Builder
	maxLines := len(wantLines)
	if len(gotLines) > maxLines {
		maxLines = len(gotLines)
	}

	for i := 0; i < maxLines; i++ {
		var wantLine, gotLine string
		if i < len(wantLines) {
			wantLine = wantLines[i]
		}
		if i < len(gotLines) {
			gotLine = gotLines[i]
		}

		if wantLine != gotLine {
			diff.WriteString("Line ")
			diff.WriteString(strings.Repeat(" ", 3-len(strings.Repeat("", i))))
			diff.WriteString(":\n")
			diff.WriteString("  Want: ")
			diff.WriteString(wantLine)
			diff.WriteString("\n")
			diff.WriteString("  Got:  ")
			diff.WriteString(gotLine)
			diff.WriteString("\n")
		}
	}

	return diff.String()
}
