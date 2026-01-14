package canvas

import (
	"strings"
	"testing"
)

// TestSpacing tests Spacing constructors.
func TestSpacing(t *testing.T) {
	tests := []struct {
		name     string
		spacing  Spacing
		expected Spacing
	}{
		{
			name:     "NewSpacing uniform",
			spacing:  NewSpacing(5),
			expected: Spacing{Top: 5, Right: 5, Bottom: 5, Left: 5},
		},
		{
			name:     "NewSpacingSymmetric",
			spacing:  NewSpacingSymmetric(2, 4),
			expected: Spacing{Top: 2, Right: 4, Bottom: 2, Left: 4},
		},
		{
			name:     "NewSpacingTRBL",
			spacing:  NewSpacingTRBL(1, 2, 3, 4),
			expected: Spacing{Top: 1, Right: 2, Bottom: 3, Left: 4},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.spacing != tt.expected {
				t.Errorf("got %+v, want %+v", tt.spacing, tt.expected)
			}
		})
	}
}

// TestBoxChars tests box character sets.
func TestBoxChars(t *testing.T) {
	tests := []struct {
		name     string
		style    BorderStyle
		topLeft  rune
		horizont rune
	}{
		{"single", BorderSingle, '┌', '─'},
		{"double", BorderDouble, '╔', '═'},
		{"rounded", BorderRounded, '╭', '─'},
		{"ascii", BorderASCII, '+', '-'},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			chars := GetBoxChars(tt.style)
			if chars.TopLeft != tt.topLeft {
				t.Errorf("TopLeft: got %c, want %c", chars.TopLeft, tt.topLeft)
			}
			if chars.Horizontal != tt.horizont {
				t.Errorf("Horizontal: got %c, want %c", chars.Horizontal, tt.horizont)
			}
		})
	}
}

// TestCenter tests the Center convenience function.
func TestCenter(t *testing.T) {
	block := Center("test", "Hello", 10)

	if block.ID != "test" {
		t.Errorf("ID: got %s, want test", block.ID)
	}
	if block.Align != AlignCenter {
		t.Errorf("Align: got %v, want AlignCenter", block.Align)
	}
	if block.Width != 10 {
		t.Errorf("Width: got %d, want 10", block.Width)
	}
}

// TestCenterAt tests the CenterAt convenience function.
func TestCenterAt(t *testing.T) {
	pos := Position{X: 5, Y: 3}
	block := CenterAt("test", "Hello", 10, pos)

	if block.Position != pos {
		t.Errorf("Position: got %+v, want %+v", block.Position, pos)
	}
}

// TestMarginWrapper tests the MarginWrapper type.
func TestMarginWrapper(t *testing.T) {
	content := NewTextBlock("inner", "Hi", Position{X: 0, Y: 0})
	margin := NewMargin("wrapper", content, 1, 2, 1, 2)

	if margin.GetID() != "wrapper" {
		t.Errorf("GetID: got %s, want wrapper", margin.GetID())
	}

	bounds := margin.GetBounds()
	// Content bounds are (0,0,2,1), margin adds 1 top, 2 right, 1 bottom, 2 left
	if bounds.X != -2 || bounds.Y != -1 {
		t.Errorf("Bounds position: got (%d,%d), want (-2,-1)", bounds.X, bounds.Y)
	}
}

// TestTitle tests the Title type.
func TestTitle(t *testing.T) {
	title := NewTitle("t", "Header").At(Position{X: 5, Y: 2}).WithUnderline(LineStyleDouble)

	if title.GetID() != "t" {
		t.Errorf("GetID: got %s, want t", title.GetID())
	}

	bounds := title.GetBounds()
	if bounds.Height != 2 {
		t.Errorf("Height with underline: got %d, want 2", bounds.Height)
	}

	// Render and verify
	canvas := New(20, 5)
	title.RenderTo(canvas)
	result := canvas.Render()

	if !strings.Contains(result, "Header") {
		t.Error("expected title text in output")
	}
	if !strings.Contains(result, "=") {
		t.Error("expected underline in output")
	}
}

// TestStack tests the Stack layout.
func TestStack(t *testing.T) {
	t.Run("vertical stack", func(t *testing.T) {
		stack := NewStack("vstack", DirectionVertical).
			At(Position{X: 0, Y: 0}).
			WithGap(1).
			AddText("a", "Line1").
			AddText("b", "Line2")

		canvas := New(20, 10)
		stack.RenderTo(canvas)
		result := canvas.Render()

		lines := strings.Split(result, "\n")
		if len(lines) < 3 {
			t.Errorf("expected at least 3 lines (2 text + 1 gap), got %d", len(lines))
		}
		if !strings.Contains(result, "Line1") {
			t.Error("expected Line1 in output")
		}
		if !strings.Contains(result, "Line2") {
			t.Error("expected Line2 in output")
		}
	})

	t.Run("horizontal stack", func(t *testing.T) {
		stack := NewStack("hstack", DirectionHorizontal).
			At(Position{X: 0, Y: 0}).
			WithGap(2).
			AddText("a", "A").
			AddText("b", "B")

		canvas := New(20, 5)
		stack.RenderTo(canvas)
		result := canvas.Render()

		// Should be "A  B" (with 2 char gap)
		if !strings.Contains(result, "A") || !strings.Contains(result, "B") {
			t.Error("expected both A and B in output")
		}
	})

	t.Run("stack bounds", func(t *testing.T) {
		stack := NewStack("test", DirectionVertical).
			AddText("a", "Hello").
			AddText("b", "World")

		bounds := stack.GetBounds()
		if bounds.Height != 2 {
			t.Errorf("Height: got %d, want 2", bounds.Height)
		}
	})
}

// TestGrid tests the GridLayout.
func TestGrid(t *testing.T) {
	t.Run("basic grid", func(t *testing.T) {
		grid := Grid("grid", 2, 2).
			WithCellSize(5, 1).
			SetCellText(0, 0, "A").
			SetCellText(0, 1, "B").
			SetCellText(1, 0, "C").
			SetCellText(1, 1, "D")

		canvas := New(20, 5)
		grid.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "A") {
			t.Error("expected A in grid")
		}
		if !strings.Contains(result, "D") {
			t.Error("expected D in grid")
		}
	})

	t.Run("grid bounds", func(t *testing.T) {
		grid := Grid("test", 2, 3).WithCellSize(10, 2).WithGap(1, 1)

		bounds := grid.GetBounds()
		// Width: 3*10 + 2*1 = 32
		// Height: 2*2 + 1*1 = 5
		if bounds.Width != 32 {
			t.Errorf("Width: got %d, want 32", bounds.Width)
		}
		if bounds.Height != 5 {
			t.Errorf("Height: got %d, want 5", bounds.Height)
		}
	})
}

// TestBorder tests the Border wrapper.
func TestBorder(t *testing.T) {
	t.Run("border with content", func(t *testing.T) {
		content := NewTextBlock("inner", "Hi", Position{X: 0, Y: 0})
		border := NewBorder("box", content, BorderSingle)

		canvas := New(10, 5)
		border.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "┌") {
			t.Error("expected top-left corner")
		}
		if !strings.Contains(result, "Hi") {
			t.Error("expected content")
		}
	})

	t.Run("border with title", func(t *testing.T) {
		content := NewTextBlock("inner", "Content", Position{X: 0, Y: 0})
		border := NewBorder("box", content, BorderDouble).
			WithTitle("Title", TitleTopCenter)

		canvas := New(20, 5)
		border.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "Title") {
			t.Error("expected title in border")
		}
	})

	t.Run("border bounds", func(t *testing.T) {
		content := NewTextBlock("inner", "Test", Position{X: 0, Y: 0})
		border := NewBorder("box", content, BorderSingle)

		bounds := border.GetBounds()
		// Content is 4 wide, 1 tall. Border adds 2 to each dimension.
		if bounds.Width != 6 {
			t.Errorf("Width: got %d, want 6", bounds.Width)
		}
		if bounds.Height != 3 {
			t.Errorf("Height: got %d, want 3", bounds.Height)
		}
	})

	t.Run("AddBorder convenience", func(t *testing.T) {
		border := AddBorder("test", "Hello", BorderRounded)

		if border.GetID() != "test" {
			t.Errorf("GetID: got %s, want test", border.GetID())
		}

		canvas := New(15, 5)
		border.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "╭") {
			t.Error("expected rounded corner")
		}
	})
}

// TestBorderLayout tests the BorderLayout.
func TestBorderLayout(t *testing.T) {
	t.Run("basic border layout", func(t *testing.T) {
		layout := NewBorderLayout("layout", 20, 10).
			SetRegionText(RegionNorth, "Header").
			SetRegionText(RegionSouth, "Footer").
			SetRegionText(RegionCenter, "Content")

		canvas := New(25, 15)
		layout.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "Header") {
			t.Error("expected Header in north region")
		}
		if !strings.Contains(result, "Footer") {
			t.Error("expected Footer in south region")
		}
		if !strings.Contains(result, "Content") {
			t.Error("expected Content in center region")
		}
	})

	t.Run("border layout bounds", func(t *testing.T) {
		layout := NewBorderLayout("test", 30, 20)

		bounds := layout.GetBounds()
		if bounds.Width != 30 || bounds.Height != 20 {
			t.Errorf("Bounds: got %dx%d, want 30x20", bounds.Width, bounds.Height)
		}
	})
}

// TestFlowLayout tests the FlowLayout.
func TestFlowLayout(t *testing.T) {
	t.Run("basic flow", func(t *testing.T) {
		flow := NewFlowLayout("flow", 20).
			WithGap(1, 0).
			AddText("a", "AAA", 3).
			AddText("b", "BBB", 3).
			AddText("c", "CCC", 3)

		canvas := New(25, 5)
		flow.RenderTo(canvas)
		result := canvas.Render()

		if !strings.Contains(result, "AAA") {
			t.Error("expected AAA in flow")
		}
		if !strings.Contains(result, "BBB") {
			t.Error("expected BBB in flow")
		}
	})

	t.Run("flow wrapping", func(t *testing.T) {
		// Create a flow with items that should wrap
		flow := NewFlowLayout("flow", 10).
			WithGap(1, 1).
			AddText("a", "AAAA", 4).
			AddText("b", "BBBB", 4).
			AddText("c", "CCCC", 4) // This should wrap to next line

		bounds := flow.GetBounds()
		// First row: AAAA + gap + BBBB = 4+1+4 = 9 (fits)
		// Second row: CCCC = 4
		if bounds.Height < 2 {
			t.Errorf("expected height >= 2 for wrapped flow, got %d", bounds.Height)
		}
	})

	t.Run("flow alignment", func(t *testing.T) {
		flow := NewFlowLayout("flow", 20).
			WithAlignment(AlignCenter).
			AddText("a", "Hi", 2)

		if flow.alignment != AlignCenter {
			t.Error("expected center alignment")
		}
	})
}

// TestLayoutNesting tests that layouts can be nested.
func TestLayoutNesting(t *testing.T) {
	// Create a stack with a bordered text block inside
	borderedText := AddBorder("inner", "Hello", BorderSingle)

	stack := NewStack("outer", DirectionVertical).
		AddText("title", "Title:").
		Add(borderedText)

	canvas := New(20, 10)
	stack.RenderTo(canvas)
	result := canvas.Render()

	if !strings.Contains(result, "Title:") {
		t.Error("expected Title: in output")
	}
	if !strings.Contains(result, "Hello") {
		t.Error("expected Hello in bordered box")
	}
	if !strings.Contains(result, "┌") {
		t.Error("expected border corner")
	}
}

// TestGridWithRenderables tests Grid with various Renderable types.
func TestGridWithRenderables(t *testing.T) {
	title := NewTitle("t", "T").WithUnderline(LineStyleSingle)
	text := NewTextBlock("txt", "X", Position{X: 0, Y: 0})

	grid := Grid("grid", 1, 2).
		WithCellSize(5, 3).
		SetCell(0, 0, title).
		SetCell(0, 1, text)

	canvas := New(15, 5)
	grid.RenderTo(canvas)
	result := canvas.Render()

	if !strings.Contains(result, "T") {
		t.Error("expected T in grid")
	}
	if !strings.Contains(result, "X") {
		t.Error("expected X in grid")
	}
}
