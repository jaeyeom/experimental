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
	"strings"

	"github.com/jaeyeom/experimental/text/wrap"
)

// CharSetter defines an interface for setting characters at specific coordinates.
// Any type implementing this interface can be used as a target for TextBlock rendering.
type CharSetter interface {
	SetChar(x, y int, r rune) bool
}

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

// RenderTo renders the text block to any CharSetter implementation.
func (tb TextBlock) RenderTo(cs CharSetter) {
	if tb.Text == "" {
		return
	}

	var wrappedText string

	switch tb.WrapMode {
	case WrapWord:
		wrappedText, _ = wrap.WordWrap(tb.Text, tb.Width)
	case WrapSoft:
		wrappedText, _ = wrap.SoftWrap(tb.Text, tb.Width)
	case WrapIndent:
		wrappedText, _ = wrap.TextIndent(tb.Text, tb.Width, tb.Indent)
	default: // WrapBasic
		wrappedText, _ = wrap.Text(tb.Text, tb.Width)
	}

	lines := strings.Split(wrappedText, "\n")

	for i, line := range lines {
		alignedLine := tb.alignLine(line)
		for j, r := range []rune(alignedLine) {
			cs.SetChar(tb.Position.X+j, tb.Position.Y+i, r)
		}
	}
}

// alignLine applies the specified alignment to a single line of text.
func (tb TextBlock) alignLine(line string) string {
	lineRunes := []rune(strings.TrimRight(line, " "))
	lineLen := len(lineRunes)

	if lineLen >= tb.Width || tb.Align == AlignLeft {
		return line
	}

	padding := tb.Width - lineLen

	switch tb.Align {
	case AlignCenter:
		leftPad := padding / 2
		return strings.Repeat(" ", leftPad) + string(lineRunes)
	case AlignRight:
		return strings.Repeat(" ", padding) + string(lineRunes)
	default: // AlignLeft
		return line
	}
}

// Canvas represents a 2D text canvas with arbitrary positioning capabilities.
// It maintains a grid of runes that can be manipulated and rendered as text.
type Canvas struct {
	width, height int
	cells         [][]rune
	background    rune
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
