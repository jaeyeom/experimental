package canvas

import (
	"fmt"
	"testing"
)

// ExampleGrid demonstrates grid-based layouts.
func ExampleGrid() {
	// Create a 2x3 grid for a simple data display
	grid := Grid("data", 2, 3).
		At(Position{X: 0, Y: 0}).
		WithCellSize(8, 1).
		WithGap(1, 0).
		SetCellText(0, 0, "Name").
		SetCellText(0, 1, "Age").
		SetCellText(0, 2, "City").
		SetCellText(1, 0, "Alice").
		SetCellText(1, 1, "30").
		SetCellText(1, 2, "NYC")

	canvas := New(30, 5)
	grid.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// Name     Age      City
	// Alice    30       NYC
}

// ExampleStack_vertical demonstrates vertical stacking.
func ExampleStack_vertical() {
	stack := NewStack("menu", DirectionVertical).
		At(Position{X: 0, Y: 0}).
		WithGap(0).
		AddText("1", "1. New Game").
		AddText("2", "2. Load Game").
		AddText("3", "3. Settings").
		AddText("4", "4. Exit")

	canvas := New(20, 6)
	stack.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// 1. New Game
	// 2. Load Game
	// 3. Settings
	// 4. Exit
}

// ExampleStack_horizontal demonstrates horizontal stacking.
func ExampleStack_horizontal() {
	stack := NewStack("buttons", DirectionHorizontal).
		At(Position{X: 0, Y: 0}).
		WithGap(2).
		AddText("ok", "[OK]").
		AddText("cancel", "[Cancel]").
		AddText("help", "[Help]")

	canvas := New(30, 3)
	stack.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// [OK]  [Cancel]  [Help]
}

// ExampleBorder demonstrates bordered content.
func ExampleBorder() {
	content := NewTextBlock("msg", "Hello, World!", Position{X: 0, Y: 0})
	border := NewBorder("box", content, BorderSingle)

	canvas := New(20, 5)
	border.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// ┌─────────────┐
	// │Hello, World!│
	// └─────────────┘
}

// ExampleBorder_withTitle demonstrates a border with a title.
func ExampleBorder_withTitle() {
	content := NewTextBlock("content", "Important message here", Position{X: 0, Y: 0})
	border := NewBorder("alert", content, BorderDouble).
		WithTitle("Alert", TitleTopCenter).
		WithPaddingUniform(0)

	canvas := New(30, 5)
	border.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// ╔════════Alert═════════╗
	// ║Important message here║
	// ╚══════════════════════╝
}

// ExampleAddBorder demonstrates the AddBorder convenience function.
func ExampleAddBorder() {
	border := AddBorder("note", "Quick note", BorderRounded)

	canvas := New(20, 5)
	border.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// ╭──────────╮
	// │Quick note│
	// ╰──────────╯
}

// ExampleCenter demonstrates centered text.
func ExampleCenter() {
	block := CenterAt("centered", "Hello", 15, Position{X: 0, Y: 0})

	canvas := New(20, 3)
	block.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	//      Hello
}

// ExampleTitle demonstrates a title with underline.
func ExampleTitle() {
	title := NewTitle("header", "My Application").
		At(Position{X: 0, Y: 0}).
		WithUnderline(LineStyleDouble)

	canvas := New(20, 4)
	title.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// My Application
	// ==============
}

// ExampleFlowLayout demonstrates automatic wrapping.
func ExampleFlowLayout() {
	flow := NewFlowLayout("tags", 20).
		At(Position{X: 0, Y: 0}).
		WithGap(1, 0).
		AddText("t1", "[go]", 4).
		AddText("t2", "[rust]", 6).
		AddText("t3", "[python]", 8).
		AddText("t4", "[js]", 4)

	canvas := New(30, 5)
	flow.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// [go] [rust] [python]
	// [js]
}

// ExampleBorderLayout demonstrates the five-region layout.
func ExampleBorderLayout() {
	layout := NewBorderLayout("app", 30, 5).
		At(Position{X: 0, Y: 0}).
		WithGap(1).
		SetRegionText(RegionNorth, "=== Header ===").
		SetRegionText(RegionSouth, "Status: Ready").
		SetRegionText(RegionWest, "Menu").
		SetRegionText(RegionCenter, "Main Content")

	canvas := New(35, 10)
	layout.RenderTo(canvas)
	fmt.Println(canvas.Render())
	// Output:
	// === Header ===
	//
	// Menu Main Content
	//
	// Status: Ready
}

// TestExampleDashboard demonstrates a complete dashboard layout.
func TestExampleDashboard(_ *testing.T) {
	// Create header
	header := NewTitle("header", "System Dashboard").
		At(Position{X: 0, Y: 0}).
		WithUnderline(LineStyleDouble).
		WithWidth(40)

	// Create status grid
	statusGrid := Grid("status", 2, 2).
		At(Position{X: 0, Y: 3}).
		WithCellSize(15, 1).
		WithGap(2, 0).
		SetCellText(0, 0, "CPU: 45%").
		SetCellText(0, 1, "Memory: 2.1GB").
		SetCellText(1, 0, "Disk: 120GB").
		SetCellText(1, 1, "Network: OK")

	// Create a bordered message box
	messageContent := NewTextBlock("msg", "All systems operational", Position{X: 0, Y: 0})
	messageBox := NewBorder("msgbox", messageContent, BorderRounded).
		At(Position{X: 0, Y: 6}).
		WithTitle("Status", TitleTopLeft)

	// Render everything to canvas
	canvas := New(45, 12)
	header.RenderTo(canvas)
	statusGrid.RenderTo(canvas)
	messageBox.RenderTo(canvas)

	fmt.Println(canvas.Render())

	// This test demonstrates visual output - actual verification would check specific elements
}

// TestExampleNestedLayouts demonstrates nested layout capabilities.
func TestExampleNestedLayouts(_ *testing.T) {
	// Create inner bordered boxes
	box1 := AddBorder("b1", "Box 1", BorderSingle)
	box2 := AddBorder("b2", "Box 2", BorderSingle)
	box3 := AddBorder("b3", "Box 3", BorderSingle)

	// Stack them horizontally
	stack := NewStack("boxes", DirectionHorizontal).
		At(Position{X: 0, Y: 0}).
		WithGap(1).
		Add(box1).
		Add(box2).
		Add(box3)

	canvas := New(35, 5)
	stack.RenderTo(canvas)
	fmt.Println(canvas.Render())
}

// TestExampleFormLayout demonstrates a form-like layout.
func TestExampleFormLayout(_ *testing.T) {
	// Create a form with labels and fields using grid
	form := Grid("form", 3, 2).
		At(Position{X: 0, Y: 0}).
		WithCellSize(12, 1).
		WithGap(2, 1).
		SetCellText(0, 0, "Name:").
		SetCellText(0, 1, "[__________]").
		SetCellText(1, 0, "Email:").
		SetCellText(1, 1, "[__________]").
		SetCellText(2, 0, "Password:").
		SetCellText(2, 1, "[__________]")

	// Wrap the form in a border
	formBorder := NewBorder("form-border", form, BorderDouble).
		WithTitle("Login Form", TitleTopCenter).
		WithPaddingUniform(1)

	canvas := New(40, 12)
	formBorder.RenderTo(canvas)
	fmt.Println(canvas.Render())
}
