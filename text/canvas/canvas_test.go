package canvas

import (
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

func TestCanvasRenderableIntegration(t *testing.T) {
	canvas := New(15, 5)

	// Test that canvas has empty renderables initially
	if len(canvas.GetRenderableIDs()) != 0 {
		t.Error("expected no renderables in new canvas")
	}

	// Create test text blocks
	block1 := TextBlock{
		ID:       "header",
		Text:     "Header",
		Position: Position{X: 0, Y: 0},
		Width:    15,
		WrapMode: WrapBasic,
		Align:    AlignCenter,
	}

	block2 := TextBlock{
		ID:       "content",
		Text:     "Content line",
		Position: Position{X: 0, Y: 2},
		Width:    15,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	// Test AddRenderable
	canvas.AddRenderable(block1)
	canvas.AddRenderable(block2)

	ids := canvas.GetRenderableIDs()
	if len(ids) != 2 {
		t.Errorf("expected 2 renderables, got %d", len(ids))
	}

	// Test GetRenderable
	retrieved, exists := canvas.GetRenderable("header")
	if !exists {
		t.Error("expected to find header block")
	}
	if retrieved.GetID() != "header" {
		t.Errorf("expected retrieved block ID 'header', got %s", retrieved.GetID())
	}

	// Test RenderWithObjects
	result := canvas.RenderWithObjects()
	expected := "    Header\n\nContent line"
	if result != expected {
		t.Errorf("expected:\n%q\ngot:\n%q", expected, result)
	}

	// Test RemoveRenderable
	removed := canvas.RemoveRenderable("header")
	if !removed {
		t.Error("expected RemoveRenderable to return true")
	}

	ids = canvas.GetRenderableIDs()
	if len(ids) != 1 {
		t.Errorf("expected 1 renderable after removal, got %d", len(ids))
	}

	// Test ClearRenderables
	canvas.ClearRenderables()
	if len(canvas.GetRenderableIDs()) != 0 {
		t.Error("expected no renderables after clear")
	}
}
