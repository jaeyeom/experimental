package canvas

import (
	"fmt"
	"strings"
	"testing"
)

func TestNew(t *testing.T) {
	tests := []struct {
		name           string
		width, height  int
		expectedWidth  int
		expectedHeight int
	}{
		{
			name:           "zero by zero",
			width:          0,
			height:         0,
			expectedWidth:  1,
			expectedHeight: 1,
		},
		{
			name:           "valid dimensions",
			width:          10,
			height:         5,
			expectedWidth:  10,
			expectedHeight: 5,
		},
		{
			name:           "zero width",
			width:          0,
			height:         5,
			expectedWidth:  1,
			expectedHeight: 1,
		},
		{
			name:           "zero height",
			width:          10,
			height:         0,
			expectedWidth:  1,
			expectedHeight: 1,
		},
		{
			name:           "negative dimensions",
			width:          -5,
			height:         -3,
			expectedWidth:  1,
			expectedHeight: 1,
		},
		{
			name:           "single cell",
			width:          1,
			height:         1,
			expectedWidth:  1,
			expectedHeight: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(tt.width, tt.height)

			if canvas.Width() != tt.expectedWidth {
				t.Errorf("expected width %d, got %d", tt.expectedWidth, canvas.Width())
			}

			if canvas.Height() != tt.expectedHeight {
				t.Errorf("expected height %d, got %d", tt.expectedHeight, canvas.Height())
			}

			// Verify all cells are initialized with background character
			for y := 0; y < canvas.Height(); y++ {
				for x := 0; x < canvas.Width(); x++ {
					if canvas.GetChar(x, y) != ' ' {
						t.Errorf("expected background character at (%d, %d), got %c", x, y, canvas.GetChar(x, y))
					}
				}
			}
		})
	}
}

func TestSetChar(t *testing.T) {
	canvas := New(5, 3)

	tests := []struct {
		name     string
		x, y     int
		char     rune
		expected bool
	}{
		{
			name:     "valid coordinates",
			x:        2,
			y:        1,
			char:     'A',
			expected: true,
		},
		{
			name:     "top-left corner",
			x:        0,
			y:        0,
			char:     'B',
			expected: true,
		},
		{
			name:     "bottom-right corner",
			x:        4,
			y:        2,
			char:     'C',
			expected: true,
		},
		{
			name:     "out of bounds x",
			x:        5,
			y:        1,
			char:     'D',
			expected: false,
		},
		{
			name:     "out of bounds y",
			x:        2,
			y:        3,
			char:     'E',
			expected: false,
		},
		{
			name:     "negative x",
			x:        -1,
			y:        1,
			char:     'F',
			expected: false,
		},
		{
			name:     "negative y",
			x:        2,
			y:        -1,
			char:     'G',
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := canvas.SetChar(tt.x, tt.y, tt.char)

			if result != tt.expected {
				t.Errorf("expected SetChar to return %t, got %t", tt.expected, result)
			}

			if tt.expected {
				// Verify the character was actually set
				if canvas.GetChar(tt.x, tt.y) != tt.char {
					t.Errorf("expected character %c at (%d, %d), got %c", tt.char, tt.x, tt.y, canvas.GetChar(tt.x, tt.y))
				}
			}
		})
	}
}

func TestGetChar(t *testing.T) {
	canvas := New(3, 3)
	canvas.SetChar(1, 1, 'X')

	tests := []struct {
		name     string
		x, y     int
		expected rune
	}{
		{
			name:     "valid coordinates with set character",
			x:        1,
			y:        1,
			expected: 'X',
		},
		{
			name:     "valid coordinates with background",
			x:        0,
			y:        0,
			expected: ' ',
		},
		{
			name:     "out of bounds x",
			x:        3,
			y:        1,
			expected: ' ',
		},
		{
			name:     "out of bounds y",
			x:        1,
			y:        3,
			expected: ' ',
		},
		{
			name:     "negative coordinates",
			x:        -1,
			y:        -1,
			expected: ' ',
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := canvas.GetChar(tt.x, tt.y)

			if result != tt.expected {
				t.Errorf("expected character %c, got %c", tt.expected, result)
			}
		})
	}
}

func TestClear(t *testing.T) {
	canvas := New(3, 3)

	// Set some characters
	canvas.SetChar(0, 0, 'A')
	canvas.SetChar(1, 1, 'B')
	canvas.SetChar(2, 2, 'C')

	// Verify characters were set
	if canvas.GetChar(0, 0) != 'A' || canvas.GetChar(1, 1) != 'B' || canvas.GetChar(2, 2) != 'C' {
		t.Error("failed to set initial characters")
	}

	// Clear the canvas
	canvas.Clear()

	// Verify all characters are background
	for y := 0; y < canvas.Height(); y++ {
		for x := 0; x < canvas.Width(); x++ {
			if canvas.GetChar(x, y) != ' ' {
				t.Errorf("expected background character at (%d, %d) after clear, got %c", x, y, canvas.GetChar(x, y))
			}
		}
	}
}

func TestRender(t *testing.T) {
	tests := []struct {
		name     string
		setup    func(*Canvas)
		expected string
	}{
		{
			name: "empty canvas",
			setup: func(_ *Canvas) {
				// No setup needed
			},
			expected: "",
		},
		{
			name: "single character",
			setup: func(c *Canvas) {
				c.SetChar(0, 0, 'X')
			},
			expected: "X",
		},
		{
			name: "single row with multiple characters",
			setup: func(c *Canvas) {
				c.SetChar(0, 0, 'H')
				c.SetChar(1, 0, 'i')
			},
			expected: "Hi",
		},
		{
			name: "multiple rows",
			setup: func(c *Canvas) {
				c.SetChar(0, 0, 'A')
				c.SetChar(0, 1, 'B')
				c.SetChar(0, 2, 'C')
			},
			expected: "A\nB\nC",
		},
		{
			name: "with trailing spaces trimmed",
			setup: func(c *Canvas) {
				c.SetChar(0, 0, 'X')
				c.SetChar(0, 1, 'Y')
				// Positions 1,0 and 1,1 remain as spaces
			},
			expected: "X\nY",
		},
		{
			name: "box pattern",
			setup: func(c *Canvas) {
				// Create a simple box pattern
				c.SetChar(0, 0, '+')
				c.SetChar(1, 0, '-')
				c.SetChar(2, 0, '+')
				c.SetChar(0, 1, '|')
				c.SetChar(2, 1, '|')
				c.SetChar(0, 2, '+')
				c.SetChar(1, 2, '-')
				c.SetChar(2, 2, '+')
			},
			expected: "+-+\n| |\n+-+",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var canvas *Canvas
			switch {
			case tt.name == "empty canvas":
				canvas = New(3, 3)
			case strings.Contains(tt.name, "single character") || strings.Contains(tt.name, "single row"):
				canvas = New(2, 1)
			case strings.Contains(tt.name, "multiple rows") || strings.Contains(tt.name, "trailing spaces"):
				canvas = New(2, 3)
			default:
				canvas = New(3, 3)
			}

			tt.setup(canvas)
			result := canvas.Render()

			if result != tt.expected {
				t.Errorf("expected:\n%q\ngot:\n%q", tt.expected, result)
			}
		})
	}
}

func TestWidthHeight(t *testing.T) {
	canvas := New(7, 4)

	if canvas.Width() != 7 {
		t.Errorf("expected width 7, got %d", canvas.Width())
	}

	if canvas.Height() != 4 {
		t.Errorf("expected height 4, got %d", canvas.Height())
	}
}

func TestPosition(t *testing.T) {
	// Test the Position struct
	pos := Position{X: 5, Y: 3}

	if pos.X != 5 {
		t.Errorf("expected X coordinate 5, got %d", pos.X)
	}

	if pos.Y != 3 {
		t.Errorf("expected Y coordinate 3, got %d", pos.Y)
	}
}

// TestUnicodeCharacters verifies that the canvas can properly store and retrieve
// various Unicode characters including Greek letters, CJK characters, emojis,
// and mathematical symbols.
//
// Limitations:
// - Only tests characters that can be represented as single runes
// - Does not test complex Unicode sequences like combining characters
// - Canvas width limits how many Unicode characters can be tested simultaneously
// - No validation of proper visual width handling for wide characters (e.g., CJK).
func TestUnicodeCharacters(t *testing.T) {
	canvas := New(3, 1)

	// Test with various Unicode characters
	unicodeChars := []rune{'α', '你', '🎨', '∑'}

	for i, char := range unicodeChars {
		if i < canvas.Width() {
			if !canvas.SetChar(i, 0, char) {
				t.Errorf("failed to set Unicode character %c", char)
			}

			if canvas.GetChar(i, 0) != char {
				t.Errorf("expected Unicode character %c, got %c", char, canvas.GetChar(i, 0))
			}
		}
	}
}

func TestLargeCanvas(t *testing.T) {
	// Test with a larger canvas for functionality
	canvas := New(100, 50)

	// Fill diagonal with 'X'
	for i := 0; i < 50 && i < 100; i++ {
		canvas.SetChar(i, i, 'X')
	}

	// Verify some diagonal positions
	if canvas.GetChar(0, 0) != 'X' {
		t.Error("expected 'X' at (0, 0)")
	}

	if canvas.GetChar(25, 25) != 'X' {
		t.Error("expected 'X' at (25, 25)")
	}

	if canvas.GetChar(49, 49) != 'X' {
		t.Error("expected 'X' at (49, 49)")
	}

	// Verify off-diagonal positions are still background
	if canvas.GetChar(1, 0) != ' ' {
		t.Error("expected background character at (1, 0)")
	}
}

// Benchmark tests for performance.
func BenchmarkNew(b *testing.B) {
	for i := 0; i < b.N; i++ {
		New(80, 24)
	}
}

func BenchmarkSetChar(b *testing.B) {
	canvas := New(80, 24)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		canvas.SetChar(i%80, (i/80)%24, 'X')
	}
}

func BenchmarkGetChar(b *testing.B) {
	canvas := New(80, 24)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		canvas.GetChar(i%80, (i/80)%24)
	}
}

func BenchmarkRender(b *testing.B) {
	canvas := New(80, 24)
	// Fill with some content
	for y := 0; y < 24; y++ {
		for x := 0; x < 80; x++ {
			canvas.SetChar(x, y, 'A')
		}
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas.Render()
	}
}

func TestTextBlockRenderTo(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		position Position
		width    int
		wrapMode WrapMode
		expected string
	}{
		{
			name:     "hello world",
			text:     "Hello World",
			position: Position{X: 2, Y: 1},
			width:    15,
			wrapMode: WrapBasic,
			expected: "Hello World",
		},
		{
			name:     "different text",
			text:     "Different Text",
			position: Position{X: 0, Y: 0},
			width:    15,
			wrapMode: WrapBasic,
			expected: "Different Text",
		},
		{
			name:     "text wrapping",
			text:     "This is a long text that needs wrapping",
			position: Position{X: 0, Y: 0},
			width:    10,
			wrapMode: WrapBasic,
			expected: "This is a\nlong text\nthat needs\nwrapping",
		},
		{
			name:     "different long text wrapping",
			text:     "Another long sentence that should wrap properly",
			position: Position{X: 0, Y: 0},
			width:    10,
			wrapMode: WrapBasic,
			expected: "Another\nlong\nsentence\nthat\nshould\nwrap\nproperly",
		},
		{
			name:     "single word longer than width",
			text:     "supercalifragilisticexpialidocious",
			position: Position{X: 0, Y: 0},
			width:    10,
			wrapMode: WrapWord,
			expected: "supercalif\nragilistic\nexpialidoc\nious",
		},
		{
			name:     "empty text",
			text:     "",
			position: Position{X: 0, Y: 0},
			width:    10,
			wrapMode: WrapBasic,
			expected: "",
		},
		{
			name:     "text with newlines",
			text:     "Line one\nLine two",
			position: Position{X: 0, Y: 0},
			width:    15,
			wrapMode: WrapBasic,
			expected: "Line one Line\ntwo",
		},
		{
			name:     "text shorter than width",
			text:     "Short",
			position: Position{X: 0, Y: 0},
			width:    20,
			wrapMode: WrapBasic,
			expected: "Short",
		},
		{
			name:     "width of 1",
			text:     "Test",
			position: Position{X: 0, Y: 0},
			width:    1,
			wrapMode: WrapWord,
			expected: "T\ne\ns\nt",
		},
		{
			name:     "text with multiple spaces",
			text:     "Text    with    spaces",
			position: Position{X: 0, Y: 0},
			width:    10,
			wrapMode: WrapBasic,
			expected: "Text with\nspaces",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(15, 7)

			block := TextBlock{
				ID:       "test",
				Text:     tt.text,
				Position: tt.position,
				Width:    tt.width,
				WrapMode: tt.wrapMode,
				Align:    AlignLeft,
			}

			block.RenderTo(canvas)

			// Verify text was placed correctly
			expectedLines := strings.Split(tt.expected, "\n")
			for lineNum, line := range expectedLines {
				for i, r := range []rune(line) {
					y := tt.position.Y + lineNum
					x := tt.position.X + i
					if canvas.GetChar(x, y) != r {
						t.Errorf("expected character %c at position (%d, %d), got %c",
							r, x, y, canvas.GetChar(x, y))
					}
				}
			}
		})
	}
}

func TestTextBlockAlignment(t *testing.T) {
	tests := []struct {
		name      string
		text      string
		width     int
		alignment Alignment
		expected  string
	}{
		{
			name:      "left align single line",
			text:      "Hello",
			width:     10,
			alignment: AlignLeft,
			expected:  "Hello",
		},
		{
			name:      "center align single line",
			text:      "Hello",
			width:     10,
			alignment: AlignCenter,
			expected:  "  Hello",
		},
		{
			name:      "right align single line",
			text:      "Hello",
			width:     10,
			alignment: AlignRight,
			expected:  "     Hello",
		},
		{
			name:      "center align even text length",
			text:      "Test",
			width:     10,
			alignment: AlignCenter,
			expected:  "   Test",
		},
		{
			name:      "center align text same as width",
			text:      "HelloWorld",
			width:     10,
			alignment: AlignCenter,
			expected:  "HelloWorld",
		},
		{
			name:      "right align with wrapping",
			text:      "Hello World Test",
			width:     10,
			alignment: AlignRight,
			expected:  "     Hello\nWorld Test",
		},
		{
			name:      "center align with wrapping",
			text:      "Hello World Test",
			width:     10,
			alignment: AlignCenter,
			expected:  "  Hello\nWorld Test",
		},
		{
			name:      "left align with wrapping",
			text:      "Hello World Test",
			width:     10,
			alignment: AlignLeft,
			expected:  "Hello\nWorld Test",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(15, 5)

			block := TextBlock{
				ID:       "test",
				Text:     tt.text,
				Position: Position{X: 0, Y: 0},
				Width:    tt.width,
				WrapMode: WrapBasic,
				Align:    tt.alignment,
			}

			block.RenderTo(canvas)

			// Verify text was placed correctly
			expectedLines := strings.Split(tt.expected, "\n")
			for lineNum, line := range expectedLines {
				for i, r := range []rune(line) {
					y := lineNum
					x := i
					if canvas.GetChar(x, y) != r {
						t.Errorf("expected character %c at position (%d, %d), got %c",
							r, x, y, canvas.GetChar(x, y))
					}
				}
			}
		})
	}
}

func TestRenderableInterface(t *testing.T) {
	// Test that TextBlock implements Renderable interface
	var _ Renderable = TextBlock{}

	block := TextBlock{
		ID:       "test-block",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	if block.GetID() != "test-block" {
		t.Errorf("expected ID 'test-block', got %s", block.GetID())
	}
}

func TestRenderableCollection(t *testing.T) {
	collection := NewRenderableCollection()

	// Test empty collection
	if collection.Count() != 0 {
		t.Errorf("expected count 0 for empty collection, got %d", collection.Count())
	}

	// Create test text blocks
	block1 := TextBlock{
		ID:       "block1",
		Text:     "First",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "Second",
		Position: Position{X: 0, Y: 1},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	// Test Add
	collection.Add(block1)
	collection.Add(block2)

	if collection.Count() != 2 {
		t.Errorf("expected count 2 after adding 2 blocks, got %d", collection.Count())
	}

	// Test Get
	retrieved, exists := collection.Get("block1")
	if !exists {
		t.Error("expected to find block1 in collection")
	}
	if retrieved.GetID() != "block1" {
		t.Errorf("expected retrieved block ID 'block1', got %s", retrieved.GetID())
	}

	// Test Get non-existent
	_, exists = collection.Get("nonexistent")
	if exists {
		t.Error("expected not to find nonexistent block")
	}

	// Test Remove
	removed := collection.Remove("block1")
	if !removed {
		t.Error("expected Remove to return true for existing block")
	}
	if collection.Count() != 1 {
		t.Errorf("expected count 1 after removing 1 block, got %d", collection.Count())
	}

	// Test Remove non-existent
	removed = collection.Remove("nonexistent")
	if removed {
		t.Error("expected Remove to return false for non-existent block")
	}

	// Test GetIDs
	ids := collection.GetIDs()
	if len(ids) != 1 {
		t.Errorf("expected 1 ID, got %d", len(ids))
	}
	if ids[0] != "block2" {
		t.Errorf("expected ID 'block2', got %s", ids[0])
	}

	// Test Clear
	collection.Clear()
	if collection.Count() != 0 {
		t.Errorf("expected count 0 after clear, got %d", collection.Count())
	}
}

func TestRectangleOverlap(t *testing.T) {
	tests := []struct {
		name     string
		r1, r2   Rectangle
		expected bool
		overlap  Rectangle
	}{
		{
			name:     "no overlap - separate",
			r1:       Rectangle{X: 0, Y: 0, Width: 2, Height: 2},
			r2:       Rectangle{X: 3, Y: 3, Width: 2, Height: 2},
			expected: false,
		},
		{
			name:     "no overlap - adjacent",
			r1:       Rectangle{X: 0, Y: 0, Width: 2, Height: 2},
			r2:       Rectangle{X: 2, Y: 0, Width: 2, Height: 2},
			expected: false,
		},
		{
			name:     "partial overlap",
			r1:       Rectangle{X: 0, Y: 0, Width: 3, Height: 3},
			r2:       Rectangle{X: 2, Y: 2, Width: 3, Height: 3},
			expected: true,
			overlap:  Rectangle{X: 2, Y: 2, Width: 1, Height: 1},
		},
		{
			name:     "complete overlap - r2 inside r1",
			r1:       Rectangle{X: 0, Y: 0, Width: 10, Height: 10},
			r2:       Rectangle{X: 2, Y: 2, Width: 3, Height: 3},
			expected: true,
			overlap:  Rectangle{X: 2, Y: 2, Width: 3, Height: 3},
		},
		{
			name:     "complete overlap - r1 inside r2",
			r1:       Rectangle{X: 2, Y: 2, Width: 3, Height: 3},
			r2:       Rectangle{X: 0, Y: 0, Width: 10, Height: 10},
			expected: true,
			overlap:  Rectangle{X: 2, Y: 2, Width: 3, Height: 3},
		},
		{
			name:     "identical rectangles",
			r1:       Rectangle{X: 1, Y: 1, Width: 5, Height: 5},
			r2:       Rectangle{X: 1, Y: 1, Width: 5, Height: 5},
			expected: true,
			overlap:  Rectangle{X: 1, Y: 1, Width: 5, Height: 5},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			overlap, hasOverlap := rectanglesOverlap(tt.r1, tt.r2)

			if hasOverlap != tt.expected {
				t.Errorf("expected overlap %t, got %t", tt.expected, hasOverlap)
			}

			if tt.expected && overlap != tt.overlap {
				t.Errorf("expected overlap region %+v, got %+v", tt.overlap, overlap)
			}
		})
	}
}

func TestTextBlockGetBounds(t *testing.T) {
	tests := []struct {
		name     string
		block    TextBlock
		expected Rectangle
	}{
		{
			name: "empty text",
			block: TextBlock{
				ID:       "empty",
				Text:     "",
				Position: Position{X: 5, Y: 5},
				Width:    10,
				WrapMode: WrapBasic,
			},
			expected: Rectangle{X: 5, Y: 5, Width: 0, Height: 0},
		},
		{
			name: "single line text",
			block: TextBlock{
				ID:       "single",
				Text:     "Hello",
				Position: Position{X: 2, Y: 3},
				Width:    10,
				WrapMode: WrapBasic,
			},
			expected: Rectangle{X: 2, Y: 3, Width: 5, Height: 1},
		},
		{
			name: "text wrapping",
			block: TextBlock{
				ID:       "wrapped",
				Text:     "Hello World Test",
				Position: Position{X: 0, Y: 0},
				Width:    10,
				WrapMode: WrapBasic,
			},
			expected: Rectangle{X: 0, Y: 0, Width: 10, Height: 2},
		},
		{
			name: "short text with wide width",
			block: TextBlock{
				ID:       "short",
				Text:     "Hi",
				Position: Position{X: 1, Y: 1},
				Width:    20,
				WrapMode: WrapBasic,
			},
			expected: Rectangle{X: 1, Y: 1, Width: 2, Height: 1},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bounds := tt.block.GetBounds()

			if bounds != tt.expected {
				t.Errorf("expected bounds %+v, got %+v", tt.expected, bounds)
			}
		})
	}
}

func TestCollisionDetection(t *testing.T) {
	collection := NewRenderableCollection()

	// Create non-overlapping text blocks
	block1 := TextBlock{
		ID:       "block1",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "World",
		Position: Position{X: 0, Y: 2},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	// Test no collisions
	collection.Add(block1)
	collection.Add(block2)
	collisions := collection.DetectCollisions()

	if len(collisions) != 0 {
		t.Errorf("expected no collisions for non-overlapping blocks, got %d", len(collisions))
	}

	// Add overlapping block
	block3 := TextBlock{
		ID:       "block3",
		Text:     "Overlap",
		Position: Position{X: 2, Y: 0}, // Overlaps with block1
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	collection.Add(block3)
	collisions = collection.DetectCollisions()

	if len(collisions) != 1 {
		t.Errorf("expected 1 collision, got %d", len(collisions))
	}

	if len(collisions) > 0 {
		collision := collisions[0]
		if collision.Object1.GetID() != "block1" && collision.Object1.GetID() != "block3" {
			t.Errorf("unexpected collision object1: %s", collision.Object1.GetID())
		}
		if collision.Object2.GetID() != "block1" && collision.Object2.GetID() != "block3" {
			t.Errorf("unexpected collision object2: %s", collision.Object2.GetID())
		}
	}
}

func TestCollisionStrategies(t *testing.T) {
	tests := []struct {
		name                string
		strategy            ResolutionStrategy
		expectError         bool
		expectedObjectCount int
	}{
		{
			name:                "overwrite strategy",
			strategy:            StrategyOverwrite,
			expectError:         false,
			expectedObjectCount: 2, // Both objects remain
		},
		{
			name:                "skip strategy",
			strategy:            StrategySkip,
			expectError:         false,
			expectedObjectCount: 1, // One object removed
		},
		{
			name:                "error strategy",
			strategy:            StrategyError,
			expectError:         true,
			expectedObjectCount: 2, // Objects remain but error returned
		},
		{
			name:                "blend strategy",
			strategy:            StrategyBlend,
			expectError:         false,
			expectedObjectCount: 2, // Both objects remain
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			collection := NewRenderableCollection()
			collection.SetCollisionStrategy(tt.strategy)

			// Add overlapping blocks
			block1 := TextBlock{
				ID:       "block1",
				Text:     "Hello",
				Position: Position{X: 0, Y: 0},
				Width:    10,
				WrapMode: WrapBasic,
			}

			block2 := TextBlock{
				ID:       "block2",
				Text:     "World",
				Position: Position{X: 2, Y: 0}, // Overlaps with block1
				Width:    10,
				WrapMode: WrapBasic,
			}

			collection.Add(block1)
			collection.Add(block2)

			err := collection.ResolveCollisions()

			if tt.expectError && err == nil {
				t.Error("expected error but got none")
			}

			if !tt.expectError && err != nil {
				t.Errorf("expected no error but got: %v", err)
			}

			if collection.Count() != tt.expectedObjectCount {
				t.Errorf("expected %d objects after resolution, got %d", tt.expectedObjectCount, collection.Count())
			}
		})
	}
}

func TestValidateLayout(t *testing.T) {
	collection := NewRenderableCollection()

	// Test valid layout (no collisions)
	block1 := TextBlock{
		ID:       "block1",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    5,
		WrapMode: WrapBasic,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "World",
		Position: Position{X: 0, Y: 2},
		Width:    5,
		WrapMode: WrapBasic,
	}

	collection.Add(block1)
	collection.Add(block2)

	err := collection.ValidateLayout()
	if err != nil {
		t.Errorf("expected no error for valid layout, got: %v", err)
	}

	// Add overlapping block
	block3 := TextBlock{
		ID:       "block3",
		Text:     "Test",
		Position: Position{X: 2, Y: 0}, // Overlaps with block1
		Width:    5,
		WrapMode: WrapBasic,
	}

	collection.Add(block3)

	err = collection.ValidateLayout()
	if err == nil {
		t.Error("expected error for invalid layout with collisions")
	}
}

func TestGetOccupiedRegions(t *testing.T) {
	collection := NewRenderableCollection()

	block1 := TextBlock{
		ID:       "block1",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "World",
		Position: Position{X: 0, Y: 2},
		Width:    10,
		WrapMode: WrapBasic,
	}

	collection.Add(block1)
	collection.Add(block2)

	regions := collection.GetOccupiedRegions()

	if len(regions) != 2 {
		t.Errorf("expected 2 occupied regions, got %d", len(regions))
	}

	// Check that regions match expected bounds
	expectedRegions := []Rectangle{
		{X: 0, Y: 0, Width: 5, Height: 1}, // block1 bounds
		{X: 0, Y: 2, Width: 5, Height: 1}, // block2 bounds
	}

	for _, expected := range expectedRegions {
		found := false
		for _, region := range regions {
			if region == expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("expected region %+v not found in occupied regions", expected)
		}
	}
}

func TestCollisionSeverity(t *testing.T) {
	tests := []struct {
		name     string
		overlap  Rectangle
		expected CollisionSeverity
	}{
		{
			name:     "low severity - small overlap",
			overlap:  Rectangle{X: 0, Y: 0, Width: 2, Height: 2}, // area = 4
			expected: CollisionLow,
		},
		{
			name:     "medium severity - moderate overlap",
			overlap:  Rectangle{X: 0, Y: 0, Width: 4, Height: 3}, // area = 12
			expected: CollisionMedium,
		},
		{
			name:     "high severity - large overlap",
			overlap:  Rectangle{X: 0, Y: 0, Width: 5, Height: 5}, // area = 25
			expected: CollisionHigh,
		},
		{
			name:     "boundary case - exactly 5",
			overlap:  Rectangle{X: 0, Y: 0, Width: 5, Height: 1}, // area = 5
			expected: CollisionLow,
		},
		{
			name:     "boundary case - exactly 20",
			overlap:  Rectangle{X: 0, Y: 0, Width: 4, Height: 5}, // area = 20
			expected: CollisionMedium,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			severity := calculateCollisionSeverity(tt.overlap)

			if severity != tt.expected {
				t.Errorf("expected severity %d, got %d", tt.expected, severity)
			}
		})
	}
}

func TestRenderableCollectionCollisionMethods(t *testing.T) {
	collection := NewRenderableCollection()

	// Test default strategy
	if collection.GetCollisionStrategy() != StrategyOverwrite {
		t.Errorf("expected default strategy StrategyOverwrite, got %d", collection.GetCollisionStrategy())
	}

	// Test setting strategy
	collection.SetCollisionStrategy(StrategyError)
	if collection.GetCollisionStrategy() != StrategyError {
		t.Errorf("expected strategy StrategyError, got %d", collection.GetCollisionStrategy())
	}
}

func TestRenderableCollectionDeterministicOrder(t *testing.T) {
	// Test that RenderAll produces deterministic results based on insertion order
	canvas := New(10, 5)
	collection := NewRenderableCollection()

	// Create overlapping blocks that will overwrite each other
	block1 := TextBlock{
		ID:       "first",
		Text:     "A",
		Position: Position{X: 0, Y: 0},
		Width:    5,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	block2 := TextBlock{
		ID:       "second",
		Text:     "B",
		Position: Position{X: 0, Y: 0},
		Width:    5,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	block3 := TextBlock{
		ID:       "third",
		Text:     "C",
		Position: Position{X: 0, Y: 0},
		Width:    5,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	// Add in specific order
	collection.Add(block1)
	collection.Add(block2)
	collection.Add(block3)

	// Render multiple times - should always produce same result
	var results []string
	for i := 0; i < 5; i++ {
		canvas.Clear()
		collection.RenderAll(canvas)
		results = append(results, canvas.Render())
	}

	// All results should be identical
	expected := results[0]
	for i, result := range results {
		if result != expected {
			t.Errorf("render %d produced different result than render 0", i)
		}
	}

	// Since block3 was added last, 'C' should be the final character at (0,0)
	// due to StrategyOverwrite behavior
	if canvas.GetChar(0, 0) != 'C' {
		t.Errorf("expected 'C' at (0,0) due to overwrite, got %c", canvas.GetChar(0, 0))
	}

	// Test that insertion order is preserved even after remove/add operations
	collection.Remove("second")
	block4 := TextBlock{
		ID:       "fourth",
		Text:     "D",
		Position: Position{X: 0, Y: 0},
		Width:    5,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}
	collection.Add(block4)

	// Should now render first, third, fourth (in that order)
	canvas.Clear()
	collection.RenderAll(canvas)

	// 'D' should be the final character since block4 was added last
	if canvas.GetChar(0, 0) != 'D' {
		t.Errorf("expected 'D' at (0,0) after remove/add, got %c", canvas.GetChar(0, 0))
	}
}

func TestLineRenderTo(t *testing.T) {
	tests := []struct {
		name     string
		line     Line
		canvasW  int
		canvasH  int
		expected string
	}{
		{
			name: "horizontal single line",
			line: Line{
				ID:        "h-line",
				Start:     Position{X: 1, Y: 1},
				Length:    5,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleSingle,
			},
			canvasW:  8,
			canvasH:  3,
			expected: "\n -----",
		},
		{
			name: "vertical single line",
			line: Line{
				ID:        "v-line",
				Start:     Position{X: 2, Y: 0},
				Length:    4,
				Direction: LineDirectionVertical,
				Style:     LineStyleSingle,
			},
			canvasW:  5,
			canvasH:  4,
			expected: "  |\n  |\n  |\n  |",
		},
		{
			name: "horizontal double line",
			line: Line{
				ID:        "h-double",
				Start:     Position{X: 0, Y: 0},
				Length:    3,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleDouble,
			},
			canvasW:  3,
			canvasH:  1,
			expected: "===",
		},
		{
			name: "vertical double line",
			line: Line{
				ID:        "v-double",
				Start:     Position{X: 0, Y: 0},
				Length:    3,
				Direction: LineDirectionVertical,
				Style:     LineStyleDouble,
			},
			canvasW:  1,
			canvasH:  3,
			expected: "‖\n‖\n‖",
		},
		{
			name: "horizontal dashed line",
			line: Line{
				ID:        "h-dash",
				Start:     Position{X: 0, Y: 0},
				Length:    7,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleDashed,
			},
			canvasW:  7,
			canvasH:  1,
			expected: "- - - -",
		},
		{
			name: "vertical dashed line",
			line: Line{
				ID:        "v-dash",
				Start:     Position{X: 0, Y: 0},
				Length:    5,
				Direction: LineDirectionVertical,
				Style:     LineStyleDashed,
			},
			canvasW:  1,
			canvasH:  5,
			expected: ":\n\n:\n\n:",
		},
		{
			name: "horizontal dotted line",
			line: Line{
				ID:        "h-dot",
				Start:     Position{X: 0, Y: 0},
				Length:    6,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleDotted,
			},
			canvasW:  6,
			canvasH:  1,
			expected: "· · ·",
		},
		{
			name: "vertical dotted line",
			line: Line{
				ID:        "v-dot",
				Start:     Position{X: 0, Y: 0},
				Length:    4,
				Direction: LineDirectionVertical,
				Style:     LineStyleDotted,
			},
			canvasW:  1,
			canvasH:  4,
			expected: "·\n\n·",
		},
		{
			name: "zero length line",
			line: Line{
				ID:        "zero",
				Start:     Position{X: 0, Y: 0},
				Length:    0,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleSingle,
			},
			canvasW:  3,
			canvasH:  3,
			expected: "",
		},
		{
			name: "single character line",
			line: Line{
				ID:        "single",
				Start:     Position{X: 1, Y: 1},
				Length:    1,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleSingle,
			},
			canvasW:  3,
			canvasH:  3,
			expected: "\n -",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(tt.canvasW, tt.canvasH)
			tt.line.RenderTo(canvas)
			result := canvas.Render()

			if result != tt.expected {
				t.Errorf("expected:\n%q\ngot:\n%q", tt.expected, result)
			}
		})
	}
}

func TestLineGetBounds(t *testing.T) {
	tests := []struct {
		name     string
		line     Line
		expected Rectangle
	}{
		{
			name: "horizontal line bounds",
			line: Line{
				ID:        "h-line",
				Start:     Position{X: 2, Y: 3},
				Length:    5,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleSingle,
			},
			expected: Rectangle{X: 2, Y: 3, Width: 5, Height: 1},
		},
		{
			name: "vertical line bounds",
			line: Line{
				ID:        "v-line",
				Start:     Position{X: 4, Y: 1},
				Length:    7,
				Direction: LineDirectionVertical,
				Style:     LineStyleSingle,
			},
			expected: Rectangle{X: 4, Y: 1, Width: 1, Height: 7},
		},
		{
			name: "zero length line bounds",
			line: Line{
				ID:        "zero",
				Start:     Position{X: 5, Y: 5},
				Length:    0,
				Direction: LineDirectionHorizontal,
				Style:     LineStyleSingle,
			},
			expected: Rectangle{X: 5, Y: 5, Width: 0, Height: 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bounds := tt.line.GetBounds()

			if bounds != tt.expected {
				t.Errorf("expected bounds %+v, got %+v", tt.expected, bounds)
			}
		})
	}
}

func TestLineGetID(t *testing.T) {
	line := Line{
		ID:        "test-line-id",
		Start:     Position{X: 0, Y: 0},
		Length:    5,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	if line.GetID() != "test-line-id" {
		t.Errorf("expected ID 'test-line-id', got %s", line.GetID())
	}
}

func TestLineInterface(t *testing.T) {
	// Test that Line implements Renderable interface
	var _ Renderable = Line{}

	line := Line{
		ID:        "interface-test",
		Start:     Position{X: 0, Y: 0},
		Length:    3,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	if line.GetID() != "interface-test" {
		t.Errorf("expected ID 'interface-test', got %s", line.GetID())
	}

	// Test bounds calculation
	bounds := line.GetBounds()
	expectedBounds := Rectangle{X: 0, Y: 0, Width: 3, Height: 1}
	if bounds != expectedBounds {
		t.Errorf("expected bounds %+v, got %+v", expectedBounds, bounds)
	}
}

func TestLineWithRenderableCollection(t *testing.T) {
	collection := NewRenderableCollection()
	canvas := New(10, 5)

	// Create horizontal and vertical lines
	hLine := Line{
		ID:        "horizontal",
		Start:     Position{X: 1, Y: 1},
		Length:    5,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	vLine := Line{
		ID:        "vertical",
		Start:     Position{X: 1, Y: 1},
		Length:    3,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}

	// Add to collection
	collection.Add(hLine)
	collection.Add(vLine)

	// Render all
	collection.RenderAll(canvas)

	// Check that both lines rendered (intersection should show vertical line due to order)
	if canvas.GetChar(1, 1) != '|' { // Vertical line overwrites horizontal at intersection
		t.Errorf("expected '|' at intersection (1,1), got %c", canvas.GetChar(1, 1))
	}

	// Check horizontal line
	if canvas.GetChar(2, 1) != '-' {
		t.Errorf("expected '-' at (2,1), got %c", canvas.GetChar(2, 1))
	}

	// Check vertical line
	if canvas.GetChar(1, 2) != '|' {
		t.Errorf("expected '|' at (1,2), got %c", canvas.GetChar(1, 2))
	}
}

func TestLineStyles(t *testing.T) {
	tests := []struct {
		name         string
		style        LineStyle
		direction    LineDirection
		expectedChar rune
	}{
		{"single horizontal", LineStyleSingle, LineDirectionHorizontal, '-'},
		{"single vertical", LineStyleSingle, LineDirectionVertical, '|'},
		{"double horizontal", LineStyleDouble, LineDirectionHorizontal, '='},
		{"double vertical", LineStyleDouble, LineDirectionVertical, '‖'},
		{"dashed horizontal", LineStyleDashed, LineDirectionHorizontal, '-'},
		{"dashed vertical", LineStyleDashed, LineDirectionVertical, ':'},
		{"dotted horizontal", LineStyleDotted, LineDirectionHorizontal, '·'},
		{"dotted vertical", LineStyleDotted, LineDirectionVertical, '·'},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			line := Line{
				ID:        "style-test",
				Start:     Position{X: 0, Y: 0},
				Length:    3,
				Direction: tt.direction,
				Style:     tt.style,
			}

			actualChar := line.getLineChar()
			if actualChar != tt.expectedChar {
				t.Errorf("expected character %c, got %c", tt.expectedChar, actualChar)
			}
		})
	}
}

func TestLineTableExample(t *testing.T) {
	// Example of using lines to create a simple table border
	canvas := New(15, 8)
	collection := NewRenderableCollection()

	// Table content
	headerBlock := TextBlock{
		ID:       "header",
		Text:     "Name   Age",
		Position: Position{X: 2, Y: 2},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	dataBlock := TextBlock{
		ID:       "data",
		Text:     "John   25\nJane   30",
		Position: Position{X: 2, Y: 4},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	// Table borders
	topBorder := Line{
		ID:        "top",
		Start:     Position{X: 1, Y: 1},
		Length:    12,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	bottomBorder := Line{
		ID:        "bottom",
		Start:     Position{X: 1, Y: 6},
		Length:    12,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	leftBorder := Line{
		ID:        "left",
		Start:     Position{X: 1, Y: 1},
		Length:    6,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}

	rightBorder := Line{
		ID:        "right",
		Start:     Position{X: 12, Y: 1},
		Length:    6,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}

	headerSeparator := Line{
		ID:        "header-sep",
		Start:     Position{X: 1, Y: 3},
		Length:    12,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	// Add all elements to collection
	collection.Add(topBorder)
	collection.Add(bottomBorder)
	collection.Add(leftBorder)
	collection.Add(rightBorder)
	collection.Add(headerSeparator)
	collection.Add(headerBlock)
	collection.Add(dataBlock)

	// Render
	collection.RenderAll(canvas)
	result := canvas.Render()

	// Verify table structure is present
	if !strings.Contains(result, "Name") {
		t.Error("expected header content in table")
	}
	if !strings.Contains(result, "John") {
		t.Error("expected data content in table")
	}

	// Simple verification that lines and text coexist
	// Just verify that we have some line characters and some text
	hasLines := strings.Contains(result, "-") || strings.Contains(result, "|")
	if !hasLines {
		t.Error("expected some line characters in rendered output")
	}
}

func TestNewTextBlock(t *testing.T) {
	block := NewTextBlock("test", "Hello World", Position{X: 5, Y: 3})

	// Test struct fields
	if block.ID != "test" {
		t.Errorf("expected ID 'test', got %s", block.ID)
	}
	if block.Text != "Hello World" {
		t.Errorf("expected text 'Hello World', got %s", block.Text)
	}
	if block.Position.X != 5 || block.Position.Y != 3 {
		t.Errorf("expected position (5,3), got (%d,%d)", block.Position.X, block.Position.Y)
	}
	if block.Width != 0 {
		t.Errorf("expected width 0 (auto-size), got %d", block.Width)
	}
	if block.WrapMode != WrapBasic {
		t.Errorf("expected WrapBasic, got %d", block.WrapMode)
	}
	if block.Align != AlignLeft {
		t.Errorf("expected AlignLeft, got %d", block.Align)
	}

	// Test that it implements Renderable
	var _ Renderable = block

	// Test rendering
	canvas := New(20, 10)
	block.RenderTo(canvas)

	// Check that text was rendered at correct position
	expectedText := "Hello World"
	for i, r := range []rune(expectedText) {
		if canvas.GetChar(5+i, 3) != r {
			t.Errorf("expected character %c at (%d,%d), got %c", r, 5+i, 3, canvas.GetChar(5+i, 3))
		}
	}
}

func TestNewTextBlockWithWidth(t *testing.T) {
	block := NewTextBlockWithWidth("test", "This is a long text that should wrap", Position{X: 2, Y: 1}, 10)

	// Test struct fields
	if block.ID != "test" {
		t.Errorf("expected ID 'test', got %s", block.ID)
	}
	if block.Width != 10 {
		t.Errorf("expected width 10, got %d", block.Width)
	}
	if block.WrapMode != WrapBasic {
		t.Errorf("expected WrapBasic, got %d", block.WrapMode)
	}

	// Test rendering with wrapping
	canvas := New(20, 10)
	block.RenderTo(canvas)

	// Check that text wrapped correctly (first line should have "This is a")
	firstLineText := "This is a"
	for i, r := range []rune(firstLineText) {
		if canvas.GetChar(2+i, 1) != r {
			t.Errorf("expected character %c at (%d,%d), got %c", r, 2+i, 1, canvas.GetChar(2+i, 1))
		}
	}

	// Check that there's text on the second line
	if canvas.GetChar(2, 2) == ' ' {
		t.Error("expected wrapped text on second line")
	}
}

func TestTableIntegrationExample(t *testing.T) {
	// Simulate using the text/table package (we'll import it in real usage)
	// For the test, we'll just create a table-like string
	tableString := `Name      | Age
----------|----
John Doe  | 25
Jane Smith| 30`

	// Create canvas with table and other elements
	canvas := New(30, 15)
	collection := NewRenderableCollection()

	// Add table as text block
	tableBlock := NewTextBlock("table", tableString, Position{X: 5, Y: 5})
	collection.Add(tableBlock)

	// Add title
	title := NewTextBlock("title", "User Report", Position{X: 5, Y: 2})
	collection.Add(title)

	// Add decorative lines
	topLine := Line{
		ID:        "top-line",
		Start:     Position{X: 0, Y: 1},
		Length:    25,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}
	collection.Add(topLine)

	bottomLine := Line{
		ID:        "bottom-line",
		Start:     Position{X: 0, Y: 12},
		Length:    25,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleDouble,
	}
	collection.Add(bottomLine)

	// Render everything
	collection.RenderAll(canvas)
	result := canvas.Render()

	// Verify components are present
	if !strings.Contains(result, "User Report") {
		t.Error("expected title in output")
	}
	if !strings.Contains(result, "John Doe") {
		t.Error("expected table content in output")
	}
	if !strings.Contains(result, "Name") {
		t.Error("expected table header in output")
	}

	// Verify lines are present
	hasHorizontalLine := strings.Contains(result, "-")
	hasDoubleHorizontalLine := strings.Contains(result, "=")
	if !hasHorizontalLine {
		t.Error("expected horizontal line in output")
	}
	if !hasDoubleHorizontalLine {
		t.Error("expected double horizontal line in output")
	}
}

func TestBlendOverlap(t *testing.T) {
	tests := []struct {
		name        string
		existing    rune
		new         rune
		overlapChar rune
		expected    rune
	}{
		{
			name:        "both non-space",
			existing:    'A',
			new:         'B',
			overlapChar: '#',
			expected:    '#',
		},
		{
			name:        "existing is space",
			existing:    ' ',
			new:         'B',
			overlapChar: '#',
			expected:    'B',
		},
		{
			name:        "new is space",
			existing:    'A',
			new:         ' ',
			overlapChar: '#',
			expected:    'A',
		},
		{
			name:        "both are space",
			existing:    ' ',
			new:         ' ',
			overlapChar: '#',
			expected:    ' ',
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			blendFunc := BlendOverlap(tt.overlapChar)
			result := blendFunc(tt.existing, tt.new)
			if result != tt.expected {
				t.Errorf("BlendOverlap(%q, %q) = %q, expected %q", tt.existing, tt.new, result, tt.expected)
			}
		})
	}
}

func TestBlendFirst(t *testing.T) {
	tests := []struct {
		name     string
		existing rune
		new      rune
		expected rune
	}{
		{
			name:     "both non-space keeps first",
			existing: 'A',
			new:      'B',
			expected: 'A',
		},
		{
			name:     "existing is space uses new",
			existing: ' ',
			new:      'B',
			expected: 'B',
		},
		{
			name:     "new is space keeps existing",
			existing: 'A',
			new:      ' ',
			expected: 'A',
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			blendFunc := BlendFirst()
			result := blendFunc(tt.existing, tt.new)
			if result != tt.expected {
				t.Errorf("BlendFirst(%q, %q) = %q, expected %q", tt.existing, tt.new, result, tt.expected)
			}
		})
	}
}

func TestBlendLast(t *testing.T) {
	tests := []struct {
		name     string
		existing rune
		new      rune
		expected rune
	}{
		{
			name:     "both non-space uses new",
			existing: 'A',
			new:      'B',
			expected: 'B',
		},
		{
			name:     "existing is space uses new",
			existing: ' ',
			new:      'B',
			expected: 'B',
		},
		{
			name:     "new is space uses space",
			existing: 'A',
			new:      ' ',
			expected: ' ',
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			blendFunc := BlendLast()
			result := blendFunc(tt.existing, tt.new)
			if result != tt.expected {
				t.Errorf("BlendLast(%q, %q) = %q, expected %q", tt.existing, tt.new, result, tt.expected)
			}
		})
	}
}

func TestBlendBoxDrawing(t *testing.T) {
	tests := []struct {
		name     string
		existing rune
		new      rune
		expected rune
	}{
		{
			name:     "ASCII horizontal + vertical",
			existing: '-',
			new:      '|',
			expected: '+',
		},
		{
			name:     "ASCII vertical + horizontal",
			existing: '|',
			new:      '-',
			expected: '+',
		},
		{
			name:     "Unicode horizontal + vertical",
			existing: '─',
			new:      '│',
			expected: '┼',
		},
		{
			name:     "Same ASCII horizontal",
			existing: '-',
			new:      '-',
			expected: '-',
		},
		{
			name:     "Non-box characters fallback",
			existing: 'A',
			new:      'B',
			expected: '#', // Default fallback
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			blendFunc := BlendBoxDrawing(BlendOverlap('#'))
			result := blendFunc(tt.existing, tt.new)
			if result != tt.expected {
				t.Errorf("BlendBoxDrawing(%q, %q) = %q, expected %q", tt.existing, tt.new, result, tt.expected)
			}
		})
	}
}

func TestRenderAllWithBlending(t *testing.T) {
	collection := NewRenderableCollection()
	collection.SetCollisionStrategy(StrategyBlend)
	collection.SetBlendFunc(BlendOverlap('#'))

	// Create two overlapping text blocks
	// "Hello" at positions 0-4, "World" at positions 3-7
	// Overlap at positions 3-4 results in '#' characters
	block1 := TextBlock{
		ID:       "block1",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "World",
		Position: Position{X: 3, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	collection.Add(block1)
	collection.Add(block2)

	canvas := New(15, 3)
	collection.RenderAllWithBlending(canvas)
	result := canvas.Render()

	expected := "Hel##rld"
	if result != expected {
		t.Errorf("RenderAllWithBlending result = %q, expected %q", result, expected)
	}
}

func TestRenderAllWithBlendingBoxDrawing(t *testing.T) {
	collection := NewRenderableCollection()
	collection.SetCollisionStrategy(StrategyBlend)
	collection.SetBlendFunc(BlendBoxDrawing(BlendOverlap('#')))

	// Create crossing lines
	horizontalLine := Line{
		ID:        "hline",
		Start:     Position{X: 0, Y: 2},
		Length:    7,
		Direction: LineDirectionHorizontal,
		Style:     LineStyleSingle,
	}

	verticalLine := Line{
		ID:        "vline",
		Start:     Position{X: 3, Y: 0},
		Length:    5,
		Direction: LineDirectionVertical,
		Style:     LineStyleSingle,
	}

	collection.Add(horizontalLine)
	collection.Add(verticalLine)

	canvas := New(10, 5)
	collection.RenderAllWithBlending(canvas)
	result := canvas.Render()

	// The intersection at (3, 2) should be '+'
	lines := strings.Split(result, "\n")

	// Check intersection character
	if len(lines) > 2 && len([]rune(lines[2])) > 3 {
		intersectionChar := []rune(lines[2])[3]
		if intersectionChar != '+' {
			t.Errorf("Expected intersection character '+', got %q", intersectionChar)
		}
	} else {
		t.Errorf("Result doesn't contain expected intersection, got: %q", result)
	}
}

func TestSetGetBlendFunc(t *testing.T) {
	collection := NewRenderableCollection()

	// Verify default blend function is set
	if collection.GetBlendFunc() == nil {
		t.Error("Expected default blend function to be set")
	}

	// Test custom blend function
	customBlend := BlendOverlap('*')
	collection.SetBlendFunc(customBlend)

	// Verify the function produces expected results
	result := collection.GetBlendFunc()('A', 'B')
	if result != '*' {
		t.Errorf("Custom blend function returned %q, expected '*'", result)
	}
}

func TestRenderAllWithBlendingFirstStrategy(t *testing.T) {
	collection := NewRenderableCollection()
	collection.SetCollisionStrategy(StrategyBlend)
	collection.SetBlendFunc(BlendFirst())

	// "ABC" at 0-2, "XYZ" at 1-3
	// BlendFirst keeps first character at overlap positions 1-2
	block1 := TextBlock{
		ID:       "block1",
		Text:     "ABC",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "XYZ",
		Position: Position{X: 1, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	collection.Add(block1)
	collection.Add(block2)

	canvas := New(10, 3)
	collection.RenderAllWithBlending(canvas)
	result := canvas.Render()

	expected := "ABCZ"
	if result != expected {
		t.Errorf("RenderAllWithBlending with BlendFirst = %q, expected %q", result, expected)
	}
}

func TestBlendingPreservesNonOverlappingContent(t *testing.T) {
	collection := NewRenderableCollection()
	collection.SetCollisionStrategy(StrategyBlend)
	collection.SetBlendFunc(BlendOverlap('#'))

	// Non-overlapping blocks
	block1 := TextBlock{
		ID:       "block1",
		Text:     "Hello",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "World",
		Position: Position{X: 0, Y: 2}, // No overlap - different Y
		Width:    10,
		WrapMode: WrapBasic,
	}

	collection.Add(block1)
	collection.Add(block2)

	canvas := New(10, 4)
	collection.RenderAllWithBlending(canvas)
	result := canvas.Render()

	// Should render both without modification
	if !strings.Contains(result, "Hello") {
		t.Error("Expected 'Hello' in output")
	}
	if !strings.Contains(result, "World") {
		t.Error("Expected 'World' in output")
	}
	if strings.Contains(result, "#") {
		t.Error("Expected no overlap characters for non-overlapping content")
	}
}

func TestResize(t *testing.T) {
	tests := []struct {
		name               string
		initialW, initialH int
		newW, newH         int
		setup              func(*Canvas)
		checkAfter         func(*Canvas) error
	}{
		{
			name:     "grow both dimensions",
			initialW: 5, initialH: 3,
			newW: 10, newH: 6,
			setup: func(c *Canvas) {
				c.SetChar(2, 1, 'X')
			},
			checkAfter: func(c *Canvas) error {
				if c.Width() != 10 || c.Height() != 6 {
					return fmt.Errorf("expected size 10x6, got %dx%d", c.Width(), c.Height())
				}
				if c.GetChar(2, 1) != 'X' {
					return fmt.Errorf("expected 'X' at (2,1), got %c", c.GetChar(2, 1))
				}
				return nil
			},
		},
		{
			name:     "shrink both dimensions",
			initialW: 10, initialH: 10,
			newW: 5, newH: 5,
			setup: func(c *Canvas) {
				c.SetChar(2, 2, 'A')
				c.SetChar(8, 8, 'B') // Will be lost after resize
			},
			checkAfter: func(c *Canvas) error {
				if c.Width() != 5 || c.Height() != 5 {
					return fmt.Errorf("expected size 5x5, got %dx%d", c.Width(), c.Height())
				}
				if c.GetChar(2, 2) != 'A' {
					return fmt.Errorf("expected 'A' at (2,2), got %c", c.GetChar(2, 2))
				}
				return nil
			},
		},
		{
			name:     "grow width shrink height",
			initialW: 5, initialH: 10,
			newW: 10, newH: 5,
			setup: func(c *Canvas) {
				c.SetChar(2, 2, 'X')
			},
			checkAfter: func(c *Canvas) error {
				if c.Width() != 10 || c.Height() != 5 {
					return fmt.Errorf("expected size 10x5, got %dx%d", c.Width(), c.Height())
				}
				if c.GetChar(2, 2) != 'X' {
					return fmt.Errorf("expected 'X' at (2,2), got %c", c.GetChar(2, 2))
				}
				return nil
			},
		},
		{
			name:     "same size is no-op",
			initialW: 5, initialH: 5,
			newW: 5, newH: 5,
			setup: func(c *Canvas) {
				c.SetChar(2, 2, 'X')
			},
			checkAfter: func(c *Canvas) error {
				if c.GetChar(2, 2) != 'X' {
					return fmt.Errorf("expected 'X' at (2,2), got %c", c.GetChar(2, 2))
				}
				return nil
			},
		},
		{
			name:     "invalid dimensions clamped to 1",
			initialW: 5, initialH: 5,
			newW: 0, newH: -1,
			setup: func(_ *Canvas) {},
			checkAfter: func(c *Canvas) error {
				if c.Width() != 1 || c.Height() != 1 {
					return fmt.Errorf("expected size 1x1 for invalid dims, got %dx%d", c.Width(), c.Height())
				}
				return nil
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(tt.initialW, tt.initialH)
			tt.setup(canvas)
			canvas.Resize(tt.newW, tt.newH)
			if err := tt.checkAfter(canvas); err != nil {
				t.Error(err)
			}
		})
	}
}

func TestClone(t *testing.T) {
	original := New(10, 5)
	original.SetChar(2, 2, 'X')
	original.SetChar(5, 3, 'Y')

	clone := original.Clone()

	// Verify clone has same dimensions
	if clone.Width() != original.Width() || clone.Height() != original.Height() {
		t.Errorf("clone dimensions mismatch: got %dx%d, want %dx%d",
			clone.Width(), clone.Height(), original.Width(), original.Height())
	}

	// Verify clone has same content
	if clone.GetChar(2, 2) != 'X' {
		t.Errorf("expected 'X' at (2,2) in clone, got %c", clone.GetChar(2, 2))
	}
	if clone.GetChar(5, 3) != 'Y' {
		t.Errorf("expected 'Y' at (5,3) in clone, got %c", clone.GetChar(5, 3))
	}

	// Verify clone is independent (modifying clone doesn't affect original)
	clone.SetChar(0, 0, 'Z')
	if original.GetChar(0, 0) == 'Z' {
		t.Error("modifying clone affected original - not a true clone")
	}

	// Verify modifying original doesn't affect clone
	original.SetChar(1, 1, 'W')
	if clone.GetChar(1, 1) == 'W' {
		t.Error("modifying original affected clone - not a true clone")
	}
}

func TestUpdate(t *testing.T) {
	canvas := New(10, 5)
	canvas.SetChar(0, 0, 'A')
	canvas.SetChar(5, 2, 'B')

	// Update a region
	updates := []struct {
		x, y int
		r    rune
	}{
		{1, 1, 'X'},
		{2, 1, 'Y'},
		{3, 1, 'Z'},
	}

	changed := canvas.Update(func(c *Canvas) {
		for _, u := range updates {
			c.SetChar(u.x, u.y, u.r)
		}
	})

	// Verify updates were applied
	if canvas.GetChar(1, 1) != 'X' || canvas.GetChar(2, 1) != 'Y' || canvas.GetChar(3, 1) != 'Z' {
		t.Error("updates not applied correctly")
	}

	// Verify original content preserved
	if canvas.GetChar(0, 0) != 'A' || canvas.GetChar(5, 2) != 'B' {
		t.Error("original content was not preserved")
	}

	// Verify changed regions are returned
	if len(changed) == 0 {
		t.Error("expected changed regions to be returned")
	}
}

func TestDiff(t *testing.T) {
	canvas1 := New(5, 3)
	canvas1.SetChar(0, 0, 'A')
	canvas1.SetChar(1, 0, 'B')
	canvas1.SetChar(2, 0, 'C')

	canvas2 := New(5, 3)
	canvas2.SetChar(0, 0, 'A') // Same
	canvas2.SetChar(1, 0, 'X') // Different
	canvas2.SetChar(2, 0, 'C') // Same
	canvas2.SetChar(3, 0, 'D') // New (was space)

	diffs := canvas1.Diff(canvas2)

	// Should have 2 differences: (1,0) B->X and (3,0) space->D
	if len(diffs) != 2 {
		t.Errorf("expected 2 diffs, got %d", len(diffs))
	}

	// Verify diff contents
	diffMap := make(map[Position]CellDiff)
	for _, d := range diffs {
		diffMap[d.Position] = d
	}

	if d, ok := diffMap[Position{X: 1, Y: 0}]; ok {
		if d.Old != 'B' || d.New != 'X' {
			t.Errorf("expected B->X at (1,0), got %c->%c", d.Old, d.New)
		}
	} else {
		t.Error("expected diff at (1,0)")
	}

	if d, ok := diffMap[Position{X: 3, Y: 0}]; ok {
		if d.Old != ' ' || d.New != 'D' {
			t.Errorf("expected space->D at (3,0), got %c->%c", d.Old, d.New)
		}
	} else {
		t.Error("expected diff at (3,0)")
	}
}

func TestDiffDifferentSizes(t *testing.T) {
	canvas1 := New(3, 3)
	canvas2 := New(5, 5)

	diffs := canvas1.Diff(canvas2)

	// Should return nil or empty for different sizes
	if diffs != nil {
		t.Errorf("expected nil for different sized canvases, got %d diffs", len(diffs))
	}
}

func TestDiffIdentical(t *testing.T) {
	canvas1 := New(5, 3)
	canvas1.SetChar(1, 1, 'X')

	canvas2 := New(5, 3)
	canvas2.SetChar(1, 1, 'X')

	diffs := canvas1.Diff(canvas2)

	if len(diffs) != 0 {
		t.Errorf("expected 0 diffs for identical canvases, got %d", len(diffs))
	}
}

// BenchmarkRenderOptimized measures render performance with varied content.
func BenchmarkRenderOptimized(b *testing.B) {
	canvas := New(80, 24)
	// Fill with varied content
	for y := 0; y < 24; y++ {
		for x := 0; x < 80; x++ {
			if (x+y)%3 == 0 {
				canvas.SetChar(x, y, 'X')
			}
		}
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas.Render()
	}
}

func BenchmarkRenderLargeCanvas(b *testing.B) {
	canvas := New(200, 100)
	for y := 0; y < 100; y++ {
		for x := 0; x < 200; x++ {
			canvas.SetChar(x, y, rune('A'+((x+y)%26)))
		}
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas.Render()
	}
}

func BenchmarkClone(b *testing.B) {
	canvas := New(80, 24)
	for y := 0; y < 24; y++ {
		for x := 0; x < 80; x++ {
			canvas.SetChar(x, y, 'A')
		}
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas.Clone()
	}
}

func BenchmarkResize(b *testing.B) {
	b.Run("grow", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			canvas := New(40, 12)
			canvas.Resize(80, 24)
		}
	})

	b.Run("shrink", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			canvas := New(80, 24)
			canvas.Resize(40, 12)
		}
	})
}

func BenchmarkDiff(b *testing.B) {
	canvas1 := New(80, 24)
	canvas2 := New(80, 24)

	// Make them slightly different
	for y := 0; y < 24; y++ {
		for x := 0; x < 80; x++ {
			canvas1.SetChar(x, y, 'A')
			if x%10 == 0 {
				canvas2.SetChar(x, y, 'B')
			} else {
				canvas2.SetChar(x, y, 'A')
			}
		}
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas1.Diff(canvas2)
	}
}

func TestCanvasPool(t *testing.T) {
	pool := NewPool(80, 24)

	// Test pool dimensions
	if pool.Width() != 80 || pool.Height() != 24 {
		t.Errorf("expected pool dimensions 80x24, got %dx%d", pool.Width(), pool.Height())
	}

	// Get a canvas from the pool
	canvas := pool.Get()
	if canvas == nil {
		t.Fatal("expected non-nil canvas from pool")
	}

	if canvas.Width() != 80 || canvas.Height() != 24 {
		t.Errorf("expected canvas dimensions 80x24, got %dx%d", canvas.Width(), canvas.Height())
	}

	// Verify canvas is cleared
	for y := 0; y < canvas.Height(); y++ {
		for x := 0; x < canvas.Width(); x++ {
			if canvas.GetChar(x, y) != ' ' {
				t.Errorf("expected cleared canvas, found %c at (%d,%d)", canvas.GetChar(x, y), x, y)
			}
		}
	}

	// Use the canvas
	canvas.SetChar(10, 5, 'X')

	// Return to pool
	pool.Put(canvas)

	// Get another canvas - should be cleared
	canvas2 := pool.Get()
	if canvas2.GetChar(10, 5) != ' ' {
		t.Error("expected canvas from pool to be cleared")
	}

	pool.Put(canvas2)
}

func TestCanvasPoolInvalidDimensions(t *testing.T) {
	pool := NewPool(0, -5)

	// Should clamp to 1x1
	if pool.Width() != 1 || pool.Height() != 1 {
		t.Errorf("expected clamped dimensions 1x1, got %dx%d", pool.Width(), pool.Height())
	}
}

func TestCanvasPoolWrongSize(t *testing.T) {
	pool := NewPool(80, 24)

	// Create a canvas with different size
	wrongSizeCanvas := New(40, 12)

	// Put should not panic, but should not accept it
	pool.Put(wrongSizeCanvas)

	// Get should still return correct size
	canvas := pool.Get()
	if canvas.Width() != 80 || canvas.Height() != 24 {
		t.Errorf("expected 80x24, got %dx%d", canvas.Width(), canvas.Height())
	}

	pool.Put(canvas)
}

func TestCanvasPoolNilPut(_ *testing.T) {
	pool := NewPool(80, 24)

	// Should not panic
	pool.Put(nil)
}

func BenchmarkCanvasPoolGetPut(b *testing.B) {
	pool := NewPool(80, 24)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		canvas := pool.Get()
		canvas.SetChar(10, 5, 'X')
		pool.Put(canvas)
	}
}

func BenchmarkCanvasPoolVsNew(b *testing.B) {
	b.Run("pool", func(b *testing.B) {
		pool := NewPool(80, 24)
		for i := 0; i < b.N; i++ {
			canvas := pool.Get()
			canvas.SetChar(10, 5, 'X')
			pool.Put(canvas)
		}
	})

	b.Run("new", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			canvas := New(80, 24)
			canvas.SetChar(10, 5, 'X')
			// No put - canvas is GC'd
			_ = canvas
		}
	})
}
