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

// Renderable defines an interface for objects that can be rendered to a CharSetter.
// This allows different types of objects (text blocks, lines, bars, etc.) to be
// rendered consistently to any canvas or drawing surface.
type Renderable interface {
	// RenderTo renders the object to the provided CharSetter implementation.
	RenderTo(cs CharSetter)

	// GetID returns a unique identifier for this renderable object.
	GetID() string
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

// GetID returns the unique identifier for this text block.
func (tb TextBlock) GetID() string {
	return tb.ID
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
	renderables   *RenderableCollection
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
		width:       width,
		height:      height,
		background:  ' ',
		renderables: NewRenderableCollection(),
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

// RenderableCollection manages a collection of renderable objects.
// It provides methods to add, remove, and render multiple objects to a canvas.
type RenderableCollection struct {
	objects map[string]Renderable
}

// NewRenderableCollection creates a new empty collection of renderable objects.
func NewRenderableCollection() *RenderableCollection {
	return &RenderableCollection{
		objects: make(map[string]Renderable),
	}
}

// Add adds a renderable object to the collection.
// If an object with the same ID already exists, it will be replaced.
func (rc *RenderableCollection) Add(r Renderable) {
	rc.objects[r.GetID()] = r
}

// Remove removes a renderable object from the collection by its ID.
// Returns true if the object was found and removed, false otherwise.
func (rc *RenderableCollection) Remove(id string) bool {
	_, exists := rc.objects[id]
	if exists {
		delete(rc.objects, id)
	}
	return exists
}

// Get retrieves a renderable object by its ID.
// Returns the object and true if found, nil and false otherwise.
func (rc *RenderableCollection) Get(id string) (Renderable, bool) {
	obj, exists := rc.objects[id]
	return obj, exists
}

// Clear removes all renderable objects from the collection.
func (rc *RenderableCollection) Clear() {
	rc.objects = make(map[string]Renderable)
}

// Count returns the number of renderable objects in the collection.
func (rc *RenderableCollection) Count() int {
	return len(rc.objects)
}

// RenderAll renders all objects in the collection to the provided CharSetter.
// Objects are rendered in an undefined order since they're stored in a map.
func (rc *RenderableCollection) RenderAll(cs CharSetter) {
	for _, obj := range rc.objects {
		obj.RenderTo(cs)
	}
}

// GetIDs returns a slice of all object IDs in the collection.
func (rc *RenderableCollection) GetIDs() []string {
	ids := make([]string, 0, len(rc.objects))
	for id := range rc.objects {
		ids = append(ids, id)
	}
	return ids
}

// AddRenderable adds a renderable object to the canvas.
func (c *Canvas) AddRenderable(r Renderable) {
	c.renderables.Add(r)
}

// RemoveRenderable removes a renderable object from the canvas by its ID.
func (c *Canvas) RemoveRenderable(id string) bool {
	return c.renderables.Remove(id)
}

// GetRenderable retrieves a renderable object by its ID.
func (c *Canvas) GetRenderable(id string) (Renderable, bool) {
	return c.renderables.Get(id)
}

// ClearRenderables removes all renderable objects from the canvas.
func (c *Canvas) ClearRenderables() {
	c.renderables.Clear()
}

// GetRenderableIDs returns a slice of all renderable object IDs on the canvas.
func (c *Canvas) GetRenderableIDs() []string {
	return c.renderables.GetIDs()
}

// RenderWithObjects clears the canvas and renders all renderable objects to it.
func (c *Canvas) RenderWithObjects() string {
	c.Clear()
	c.renderables.RenderAll(c)
	return c.Render()
}
