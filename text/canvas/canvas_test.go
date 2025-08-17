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
	// Test with a larger canvas to ensure performance is reasonable
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

func TestAddTextBlock(t *testing.T) {
	canvas := New(20, 10)

	block := TextBlock{
		ID:       "test1",
		Text:     "Hello World",
		Position: Position{X: 2, Y: 1},
		Width:    15,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block)

	// Verify text was placed correctly
	expected := "Hello World"
	for i, r := range []rune(expected) {
		if canvas.GetChar(2+i, 1) != r {
			t.Errorf("expected character %c at position (%d, %d), got %c",
				r, 2+i, 1, canvas.GetChar(2+i, 1))
		}
	}

	// Verify the text block was stored
	blocks := canvas.GetTextBlocks()
	if len(blocks) != 1 {
		t.Errorf("expected 1 text block, got %d", len(blocks))
	}
	if blocks[0].ID != "test1" {
		t.Errorf("expected block ID 'test1', got %s", blocks[0].ID)
	}
}

func TestAddTextBlockWithWrapping(t *testing.T) {
	canvas := New(15, 5)

	block := TextBlock{
		ID:       "wrap1",
		Text:     "This is a long text that needs wrapping",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block)

	// Check first line
	firstLine := "This is a"
	for i, r := range []rune(firstLine) {
		if canvas.GetChar(i, 0) != r {
			t.Errorf("line 1: expected character %c at position (%d, %d), got %c",
				r, i, 0, canvas.GetChar(i, 0))
		}
	}

	// Check second line
	secondLine := "long text"
	for i, r := range []rune(secondLine) {
		if canvas.GetChar(i, 1) != r {
			t.Errorf("line 2: expected character %c at position (%d, %d), got %c",
				r, i, 1, canvas.GetChar(i, 1))
		}
	}
}

func TestTextBlockAlignment(t *testing.T) {
	tests := []struct {
		name     string
		align    Alignment
		text     string
		width    int
		expected string
	}{
		{
			name:     "left alignment",
			align:    AlignLeft,
			text:     "Hello",
			width:    10,
			expected: "Hello     ",
		},
		{
			name:     "center alignment",
			align:    AlignCenter,
			text:     "Hello",
			width:    10,
			expected: "  Hello   ",
		},
		{
			name:     "right alignment",
			align:    AlignRight,
			text:     "Hello",
			width:    10,
			expected: "     Hello",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(15, 3)

			block := TextBlock{
				ID:       "align_test",
				Text:     tt.text,
				Position: Position{X: 0, Y: 0},
				Width:    tt.width,
				WrapMode: WrapBasic,
				Align:    tt.align,
			}

			canvas.AddTextBlock(block)

			// Check the alignment
			for i, expected := range []rune(tt.expected) {
				if i >= canvas.Width() {
					break
				}
				got := canvas.GetChar(i, 0)
				if got != expected {
					t.Errorf("expected character %c at position %d, got %c", expected, i, got)
				}
			}
		})
	}
}

func TestTextBlockWrapModes(t *testing.T) {
	tests := []struct {
		name     string
		wrapMode WrapMode
		text     string
		width    int
		indent   string
	}{
		{
			name:     "basic wrap",
			wrapMode: WrapBasic,
			text:     "Hello world this is a test",
			width:    10,
		},
		{
			name:     "word wrap",
			wrapMode: WrapWord,
			text:     "Hello world superlongwordthatexceedswidth test",
			width:    10,
		},
		{
			name:     "soft wrap",
			wrapMode: WrapSoft,
			text:     "Hello world this is soft wrapping",
			width:    10,
		},
		{
			name:     "indent wrap",
			wrapMode: WrapIndent,
			text:     "First line and second line content",
			width:    15,
			indent:   "  ",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			canvas := New(20, 10)

			block := TextBlock{
				ID:       "wrap_test",
				Text:     tt.text,
				Position: Position{X: 0, Y: 0},
				Width:    tt.width,
				WrapMode: tt.wrapMode,
				Align:    AlignLeft,
				Indent:   tt.indent,
			}

			canvas.AddTextBlock(block)

			// Verify at least some content was placed
			hasContent := false
			for y := 0; y < 5; y++ {
				for x := 0; x < tt.width; x++ {
					if canvas.GetChar(x, y) != ' ' {
						hasContent = true
						break
					}
				}
				if hasContent {
					break
				}
			}

			if !hasContent {
				t.Error("expected wrapped text to be rendered on canvas")
			}
		})
	}
}

func TestRemoveTextBlock(t *testing.T) {
	canvas := New(15, 5)

	// Add two text blocks
	block1 := TextBlock{
		ID:       "block1",
		Text:     "First block",
		Position: Position{X: 0, Y: 0},
		Width:    11,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	block2 := TextBlock{
		ID:       "block2",
		Text:     "Second",
		Position: Position{X: 0, Y: 1},
		Width:    6,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block1)
	canvas.AddTextBlock(block2)

	// Verify both blocks exist
	blocks := canvas.GetTextBlocks()
	if len(blocks) != 2 {
		t.Errorf("expected 2 text blocks, got %d", len(blocks))
	}

	// Remove first block
	canvas.RemoveTextBlock("block1")

	// Verify only second block remains
	blocks = canvas.GetTextBlocks()
	if len(blocks) != 1 {
		t.Errorf("expected 1 text block after removal, got %d", len(blocks))
	}
	if blocks[0].ID != "block2" {
		t.Errorf("expected remaining block to be 'block2', got %s", blocks[0].ID)
	}

	// Verify first line is cleared (background characters)
	for x := 0; x < 11; x++ {
		if canvas.GetChar(x, 0) != ' ' {
			t.Errorf("expected background character at (%d, 0) after removal, got %c",
				x, canvas.GetChar(x, 0))
		}
	}

	// Verify second line still has content
	secondText := "Second"
	for i, r := range []rune(secondText) {
		if canvas.GetChar(i, 1) != r {
			t.Errorf("expected character %c at (%d, 1), got %c",
				r, i, canvas.GetChar(i, 1))
		}
	}
}

func TestReplaceTextBlock(t *testing.T) {
	canvas := New(15, 3)

	// Add initial text block
	block := TextBlock{
		ID:       "replaceable",
		Text:     "Original",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block)

	// Verify original text
	originalText := "Original"
	for i, r := range []rune(originalText) {
		if canvas.GetChar(i, 0) != r {
			t.Errorf("expected original character %c at position %d, got %c",
				r, i, canvas.GetChar(i, 0))
		}
	}

	// Replace with new text block (same ID)
	newBlock := TextBlock{
		ID:       "replaceable",
		Text:     "New Text",
		Position: Position{X: 0, Y: 0},
		Width:    10,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(newBlock)

	// Verify only one block exists
	blocks := canvas.GetTextBlocks()
	if len(blocks) != 1 {
		t.Errorf("expected 1 text block after replacement, got %d", len(blocks))
	}

	// Verify new text
	newText := "New Text"
	for i, r := range []rune(newText) {
		if canvas.GetChar(i, 0) != r {
			t.Errorf("expected new character %c at position %d, got %c",
				r, i, canvas.GetChar(i, 0))
		}
	}

	// Verify original text area is cleared beyond new text
	if canvas.GetChar(8, 0) != ' ' {
		t.Errorf("expected background character at position 8, got %c", canvas.GetChar(8, 0))
	}
}

func TestGetTextBlocks(t *testing.T) {
	canvas := New(10, 5)

	// Add multiple text blocks
	blocks := []TextBlock{
		{
			ID:       "block1",
			Text:     "Text 1",
			Position: Position{X: 0, Y: 0},
			Width:    6,
			WrapMode: WrapBasic,
			Align:    AlignLeft,
		},
		{
			ID:       "block2",
			Text:     "Text 2",
			Position: Position{X: 0, Y: 1},
			Width:    6,
			WrapMode: WrapWord,
			Align:    AlignCenter,
		},
	}

	for _, block := range blocks {
		canvas.AddTextBlock(block)
	}

	// Get text blocks and verify they match
	retrievedBlocks := canvas.GetTextBlocks()
	if len(retrievedBlocks) != len(blocks) {
		t.Errorf("expected %d text blocks, got %d", len(blocks), len(retrievedBlocks))
	}

	// Verify it's a copy by modifying elements directly (not just appending)
	if len(retrievedBlocks) > 0 {
		originalID := retrievedBlocks[0].ID
		retrievedBlocks[0].ID = "modified_id"

		// Get blocks again to verify the canvas wasn't affected
		newRetrievedBlocks := canvas.GetTextBlocks()
		if len(newRetrievedBlocks) > 0 && newRetrievedBlocks[0].ID != originalID {
			t.Error("GetTextBlocks should return a copy - modifying returned slice affected the canvas")
		}
	}
}

func TestTextBlockOutOfBounds(t *testing.T) {
	canvas := New(5, 3)

	// Text block that goes beyond canvas bounds
	block := TextBlock{
		ID:       "oob",
		Text:     "This text is much longer than the canvas width",
		Position: Position{X: 3, Y: 1},
		Width:    20,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block)

	// Verify only text within bounds is rendered
	// Characters at x >= 5 should not be set
	for y := 0; y < canvas.Height(); y++ {
		for x := 5; x < 10; x++ { // Check beyond canvas width
			// This should not panic, GetChar should handle out of bounds
			char := canvas.GetChar(x, y)
			if char != ' ' {
				t.Errorf("expected background character for out-of-bounds position (%d, %d), got %c", x, y, char)
			}
		}
	}
}

func TestEmptyTextBlock(t *testing.T) {
	canvas := New(10, 3)

	block := TextBlock{
		ID:       "empty",
		Text:     "",
		Position: Position{X: 2, Y: 1},
		Width:    5,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}

	canvas.AddTextBlock(block)

	// Verify empty text doesn't affect canvas
	for y := 0; y < canvas.Height(); y++ {
		for x := 0; x < canvas.Width(); x++ {
			if canvas.GetChar(x, y) != ' ' {
				t.Errorf("expected background character at (%d, %d) for empty text block, got %c",
					x, y, canvas.GetChar(x, y))
			}
		}
	}

	// Verify the block is still stored
	blocks := canvas.GetTextBlocks()
	if len(blocks) != 1 {
		t.Errorf("expected 1 text block, got %d", len(blocks))
	}
}
