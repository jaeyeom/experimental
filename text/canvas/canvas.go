// Package canvas provides a 2D text canvas for arbitrary text positioning.
//
// This package allows you to create a 2D grid of runes that can be manipulated
// and rendered as text. It supports setting and getting individual characters
// at specific coordinates, clearing the canvas, and rendering the entire canvas
// as a string.
//
// Basic usage:
//
//	canvas := canvas.New(10, 5)
//	canvas.SetChar(0, 0, 'H')
//	canvas.SetChar(1, 0, 'i')
//	fmt.Print(canvas.Render())
package canvas

import (
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/text/wrap"
)

// Position represents a coordinate position on the canvas.
type Position struct {
	X, Y int
}

// WrapMode specifies the text wrapping mode to use for a TextBlock.
type WrapMode int

const (
	// WrapBasic uses the basic Text function for wrapping text to fit within specified widths.
	// Preserves paragraph breaks (double newlines) and wraps at word boundaries.
	// See: github.com/jaeyeom/experimental/text/wrap.Text.
	WrapBasic WrapMode = iota

	// WrapWord uses the WordWrap function for wrapping with long word handling.
	// Similar to WrapBasic but breaks long words that exceed the width by splitting them.
	// See: github.com/jaeyeom/experimental/text/wrap.WordWrap.
	WrapWord

	// WrapSoft uses the SoftWrap function for soft wrapping at word boundaries.
	// Prefers to break at natural word boundaries and avoids breaking words when possible.
	// See: github.com/jaeyeom/experimental/text/wrap.SoftWrap.
	WrapSoft

	// WrapIndent uses the TextIndent function for wrapping with indentation.
	// First line starts without indent, continuation lines use the specified indent string.
	// See: github.com/jaeyeom/experimental/text/wrap.TextIndent.
	WrapIndent
)

// Alignment specifies the text alignment within a TextBlock.
type Alignment int

const (
	// AlignLeft aligns text to the left side of the text block.
	AlignLeft Alignment = iota
	// AlignCenter centers text within the text block.
	AlignCenter
	// AlignRight aligns text to the right side of the text block.
	AlignRight
)

// TextBlock represents a positioned text block on the canvas with wrapping and alignment.
type TextBlock struct {
	// ID uniquely identifies the text block for removal and updates.
	ID string
	// Text is the content to be displayed.
	Text string
	// Position is the top-left corner where the text block starts.
	Position Position
	// Width is the maximum width of the text block.
	Width int
	// WrapMode specifies how text should be wrapped.
	WrapMode WrapMode
	// Align specifies how text should be aligned within the block.
	Align Alignment
	// Indent is used for WrapIndent mode to specify the indent string.
	Indent string
}

// Canvas represents a 2D text canvas with arbitrary positioning capabilities.
// It maintains a grid of runes that can be manipulated and rendered as text.
type Canvas struct {
	width, height int
	cells         [][]rune
	background    rune
	textBlocks    []TextBlock
}

// New creates a new Canvas with the specified width and height.
// The canvas is initialized with spaces as the background character.
// Width and height must be positive values.
func New(width, height int) *Canvas {
	if width <= 0 || height <= 0 {
		width = 1
		height = 1
	}

	c := &Canvas{
		width:      width,
		height:     height,
		background: ' ',
	}

	c.initializeCells()
	return c
}

// initializeCells initializes the 2D rune grid with background characters.
func (c *Canvas) initializeCells() {
	c.cells = make([][]rune, c.height)
	for y := 0; y < c.height; y++ {
		c.cells[y] = make([]rune, c.width)
		for x := 0; x < c.width; x++ {
			c.cells[y][x] = c.background
		}
	}
}

// SetChar sets the character at the specified coordinates.
// Returns true if the coordinates are valid and the character was set,
// false if the coordinates are out of bounds.
func (c *Canvas) SetChar(x, y int, r rune) bool {
	if !c.isValidCoordinate(x, y) {
		return false
	}
	c.cells[y][x] = r
	return true
}

// GetChar returns the character at the specified coordinates.
// Returns the background character if coordinates are out of bounds.
func (c *Canvas) GetChar(x, y int) rune {
	if !c.isValidCoordinate(x, y) {
		return c.background
	}
	return c.cells[y][x]
}

// Clear resets the canvas by filling all cells with the background character.
func (c *Canvas) Clear() {
	for y := 0; y < c.height; y++ {
		for x := 0; x < c.width; x++ {
			c.cells[y][x] = c.background
		}
	}
}

// Render converts the canvas to a string representation.
// Each row is separated by a newline character.
// Trailing spaces are trimmed from each line.
func (c *Canvas) Render() string {
	var lines []string

	for y := 0; y < c.height; y++ {
		line := string(c.cells[y])
		// Trim trailing spaces for cleaner output
		line = strings.TrimRight(line, " ")
		lines = append(lines, line)
	}

	// Remove trailing empty lines
	for len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}

	return strings.Join(lines, "\n")
}

// Width returns the width of the canvas.
func (c *Canvas) Width() int {
	return c.width
}

// Height returns the height of the canvas.
func (c *Canvas) Height() int {
	return c.height
}

// isValidCoordinate checks if the given coordinates are within canvas bounds.
func (c *Canvas) isValidCoordinate(x, y int) bool {
	return x >= 0 && x < c.width && y >= 0 && y < c.height
}

// AddTextBlock adds a text block to the canvas. If a text block with the same ID
// already exists, it will be replaced. The text block is rendered immediately
// onto the canvas using the specified wrapping and alignment settings.
func (c *Canvas) AddTextBlock(block TextBlock) {
	c.RemoveTextBlock(block.ID)
	c.textBlocks = append(c.textBlocks, block)
	c.renderTextBlock(block)
}

// RemoveTextBlock removes a text block with the specified ID from the canvas.
// The canvas is re-rendered after removal to reflect the changes.
func (c *Canvas) RemoveTextBlock(id string) {
	for i, block := range c.textBlocks {
		if block.ID == id {
			c.textBlocks = slices.Delete(c.textBlocks, i, i+1)
			c.rerender()
			return
		}
	}
}

// GetTextBlocks returns a copy of all text blocks currently on the canvas.
// The returned slice is a shallow copy, preventing external modification of the
// canvas's internal state while allowing safe inspection of text block properties.
//
// This method is typically used for:
//   - Inspecting current text blocks without risk of modification
//   - Iterating over text blocks for display or analysis
//   - Debugging canvas state
//
// Example usage:
//
//	canvas := canvas.New(20, 10)
//	canvas.AddTextBlock(TextBlock{ID: "header", Text: "Title", ...})
//
//	// Safe to iterate and inspect
//	for _, block := range canvas.GetTextBlocks() {
//		fmt.Printf("Block %s: %s\n", block.ID, block.Text)
//	}
//
//	// Modifying returned slice won't affect canvas
//	blocks := canvas.GetTextBlocks()
//	blocks[0].Text = "Modified" // Canvas remains unchanged
func (c *Canvas) GetTextBlocks() []TextBlock {
	return slices.Clone(c.textBlocks)
}

// rerender clears the canvas and re-renders all text blocks.
func (c *Canvas) rerender() {
	c.Clear()
	for _, block := range c.textBlocks {
		c.renderTextBlock(block)
	}
}

// renderTextBlock renders a single text block onto the canvas.
func (c *Canvas) renderTextBlock(block TextBlock) {
	if block.Width <= 0 {
		return
	}

	var wrappedText string
	var lines int

	switch block.WrapMode {
	case WrapBasic:
		wrappedText, lines = wrap.Text(block.Text, block.Width)
	case WrapWord:
		wrappedText, lines = wrap.WordWrap(block.Text, block.Width)
	case WrapSoft:
		wrappedText, lines = wrap.SoftWrap(block.Text, block.Width)
	case WrapIndent:
		wrappedText, lines = wrap.TextIndent(block.Text, block.Width, block.Indent)
	default:
		wrappedText, lines = wrap.Text(block.Text, block.Width)
	}

	if lines == 0 {
		return
	}

	textLines := strings.Split(wrappedText, "\n")
	for i, line := range textLines {
		y := block.Position.Y + i
		if y >= c.height {
			break
		}

		alignedLine := c.alignText(line, block.Width, block.Align)
		for j, r := range []rune(alignedLine) {
			x := block.Position.X + j
			if x >= c.width {
				break
			}
			c.SetChar(x, y, r)
		}
	}
}

// alignText aligns text within the specified width according to the alignment setting.
func (c *Canvas) alignText(text string, width int, align Alignment) string {
	textRunes := []rune(text)
	textLen := len(textRunes)

	if textLen >= width {
		return text
	}

	padding := width - textLen

	switch align {
	case AlignLeft:
		return text
	case AlignCenter:
		leftPad := padding / 2
		rightPad := padding - leftPad
		return strings.Repeat(" ", leftPad) + text + strings.Repeat(" ", rightPad)
	case AlignRight:
		return strings.Repeat(" ", padding) + text
	default:
		return text
	}
}
