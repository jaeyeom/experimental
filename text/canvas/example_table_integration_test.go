package canvas

import (
	"fmt"
	"testing"

	"github.com/jaeyeom/experimental/text/table"
)

// TestExampleTableIntegration demonstrates how to combine tables with other canvas elements.
func TestExampleTableIntegration(_ *testing.T) {
	// Create a table using the existing text/table package
	userTable := table.NewTable().
		WithWidth(table.WidthFixed, 25).
		WithCellMode(table.CellClip).
		AddColumn("Name", 12, table.AlignLeft).
		AddColumn("Age", 5, table.AlignRight).
		AddColumn("City", 8, table.AlignLeft).
		AddRow("John Doe", "25", "NYC").
		AddRow("Jane Smith", "30", "LA").
		AddRow("Bob Wilson", "35", "Chicago")

	// Render table to string
	tableString := userTable.Render()

	// Create canvas for layout
	canvas := New(60, 20)
	collection := NewRenderableCollection()

	// Add title with decorative border
	title := NewTextBlock("title", "📊 User Statistics Report", Position{X: 15, Y: 2})
	collection.Add(title)

	// Add decorative top border
	titleUnderline := Line{
		ID:        "title-underline",
		Start:     Position{X: 10, Y: 3},
		Length:    35,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleDouble,
	}
	collection.Add(titleUnderline)

	// Add table as text block
	tableBlock := NewTextBlock("main-table", tableString, Position{X: 15, Y: 6})
	collection.Add(tableBlock)

	// Add summary information
	summary := NewTextBlock("summary", "Total Users: 3", Position{X: 15, Y: 12})
	collection.Add(summary)

	// Add left border decoration
	leftBorder := Line{
		ID:        "left-border",
		Start:     Position{X: 5, Y: 1},
		Length:    16,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}
	collection.Add(leftBorder)

	// Add right border decoration
	rightBorder := Line{
		ID:        "right-border",
		Start:     Position{X: 55, Y: 1},
		Length:    16,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}
	collection.Add(rightBorder)

	// Add corner accents
	topLeftCorner := NewTextBlock("top-left", "┌", Position{X: 5, Y: 1})
	topRightCorner := NewTextBlock("top-right", "┐", Position{X: 55, Y: 1})
	bottomLeftCorner := NewTextBlock("bottom-left", "└", Position{X: 5, Y: 16})
	bottomRightCorner := NewTextBlock("bottom-right", "┘", Position{X: 55, Y: 16})

	collection.Add(topLeftCorner)
	collection.Add(topRightCorner)
	collection.Add(bottomLeftCorner)
	collection.Add(bottomRightCorner)

	// Add bottom border
	bottomBorder := Line{
		ID:        "bottom-border",
		Start:     Position{X: 6, Y: 16},
		Length:    49,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}
	collection.Add(bottomBorder)

	// Add top border
	topBorder := Line{
		ID:        "top-border",
		Start:     Position{X: 6, Y: 1},
		Length:    49,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}
	collection.Add(topBorder)

	// Render everything to canvas
	collection.RenderAll(canvas)

	// Output the result
	fmt.Println(canvas.Render())

	// Output:
	// ┌─────────────────────────────────────────────────────┐
	// |                                                     |
	// |               📊 User Statistics Report             |
	// |               ═══════════════════════════════════   |
	// |                                                     |
	// |                                                     |
	// |               Name        | Age |     City          |
	// |               ------------|-----|--------           |
	// |               John Doe    |  25 | NYC               |
	// |               Jane Smith  |  30 | LA                |
	// |               Bob Wilson  |  35 | Chicago           |
	// |                                                     |
	// |               Total Users: 3                        |
	// |                                                     |
	// |                                                     |
	// |                                                     |
	// └─────────────────────────────────────────────────────┘
}

// TestRealTableIntegration tests integration with actual text/table package.
func TestRealTableIntegration(t *testing.T) {
	// Create a real table
	salesTable := table.NewTable().
		WithWidth(table.WidthFixed, 35).
		WithCellMode(table.CellWrap).
		AddColumn("Product", 15, table.AlignLeft).
		AddColumn("Sales", 8, table.AlignRight).
		AddColumn("Trend", 8, table.AlignCenter).
		AddRow("Laptops", "$45,200", "📈 UP").
		AddRow("Phones", "$32,100", "📉 DOWN").
		AddRow("Tablets", "$18,500", "➡️ FLAT")

	// Create dashboard layout
	canvas := New(50, 15)
	collection := NewRenderableCollection()

	// Dashboard title
	title := NewTextBlock("dashboard", "Sales Dashboard", Position{X: 10, Y: 1})
	collection.Add(title)

	// Main table
	tableBlock := NewTextBlock("sales-table", salesTable.Render(), Position{X: 5, Y: 4})
	collection.Add(tableBlock)

	// Add status indicators using lines
	goodPerformance := Line{
		ID:        "good-line",
		Start:     Position{X: 1, Y: 5}, // Next to Laptops row
		Length:    2,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleDouble,
	}
	collection.Add(goodPerformance)

	poorPerformance := Line{
		ID:        "poor-line",
		Start:     Position{X: 1, Y: 6}, // Next to Phones row
		Length:    2,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleDashed,
	}
	collection.Add(poorPerformance)

	// Render and verify
	collection.RenderAll(canvas)
	result := canvas.Render()

	// Verify table content is present
	if !containsAll(result, []string{"Sales Dashboard", "Laptops", "Phones", "Tablets"}) {
		t.Error("expected table content in dashboard")
	}

	// Verify decorative elements are present
	if !containsAll(result, []string{"=", "-"}) {
		t.Error("expected decorative line elements in dashboard")
	}
}

// containsAll checks if result contains all expected strings.
func containsAll(result string, expected []string) bool {
	for _, exp := range expected {
		if !contains(result, exp) {
			return false
		}
	}
	return true
}

// contains is a simple string contains check.
func contains(s, substr string) bool {
	return len(s) >= len(substr) && findIndex(s, substr) >= 0
}

// findIndex finds the first index of substr in s, or -1 if not found.
func findIndex(s, substr string) int {
	if len(substr) == 0 {
		return 0
	}
	if len(substr) > len(s) {
		return -1
	}

	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return i
		}
	}
	return -1
}
