// Package canvas provides a 2D text canvas for arbitrary text positioning.
//
// This package allows you to create a 2D grid of runes that can be manipulated
// and rendered as text. It supports setting and getting individual characters
// at specific coordinates, clearing the canvas, and rendering the entire canvas
// as a string.
//
// The package also provides TextBlock functionality that integrates with the
// text/wrap package to support various text wrapping and positioning modes.
//
// Basic usage:
//
//	canvas := canvas.New(10, 5)
//	canvas.SetChar(0, 0, 'H')
//	canvas.SetChar(1, 0, 'i')
//	fmt.Print(canvas.Render())
//
// TextBlock integration patterns with text/wrap package:
//
// Basic text wrapping:
//
//	canvas := canvas.New(20, 10)
//	block := canvas.TextBlock{
//		ID:       "main",
//		Text:     "This is a long text that will wrap at word boundaries",
//		Position: canvas.Position{X: 2, Y: 1},
//		Width:    15,
//		WrapMode: canvas.WrapBasic,
//		Align:    canvas.AlignLeft,
//	}
//	canvas.AddTextBlock(block)
//	fmt.Print(canvas.Render())
//
// Word wrapping with long word handling:
//
//	block := canvas.TextBlock{
//		ID:       "wordwrap",
//		Text:     "Normal words superlongwordthatexceedswidth more text",
//		Position: canvas.Position{X: 0, Y: 0},
//		Width:    10,
//		WrapMode: canvas.WrapWord,  // Breaks long words
//		Align:    canvas.AlignCenter,
//	}
//	canvas.AddTextBlock(block)
//
// Soft wrapping (prefers natural word boundaries):
//
//	block := canvas.TextBlock{
//		ID:       "softwrap",
//		Text:     "This text prefers to break at natural boundaries",
//		Position: canvas.Position{X: 0, Y: 0},
//		Width:    12,
//		WrapMode: canvas.WrapSoft,
//		Align:    canvas.AlignRight,
//	}
//	canvas.AddTextBlock(block)
//
// Indented wrapping (continuation lines are indented):
//
//	block := canvas.TextBlock{
//		ID:       "indent",
//		Text:     "First line starts without indent, continuation lines are indented",
//		Position: canvas.Position{X: 0, Y: 0},
//		Width:    20,
//		WrapMode: canvas.WrapIndent,
//		Align:    canvas.AlignLeft,
//		Indent:   "    ",  // 4-space indent for continuation lines
//	}
//	canvas.AddTextBlock(block)
//
// Dynamic text block management:
//
//	// Add multiple blocks
//	canvas.AddTextBlock(headerBlock)
//	canvas.AddTextBlock(contentBlock)
//
//	// Update existing block (same ID replaces)
//	updatedHeader := headerBlock
//	updatedHeader.Text = "Updated Header"
//	canvas.AddTextBlock(updatedHeader)
//
//	// Remove specific block
//	canvas.RemoveTextBlock("content")
//
//	// Inspect all blocks
//	for _, block := range canvas.GetTextBlocks() {
//		fmt.Printf("Block %s: %q\n", block.ID, block.Text)
//	}
package canvas

import (
	"fmt"
	"strings"
	"sync"

	"github.com/jaeyeom/experimental/text/wrap"
)

// CharSetter defines an interface for setting characters at specific coordinates.
// Any type implementing this interface can be used as a target for TextBlock rendering.
type CharSetter interface {
	SetChar(x, y int, r rune) bool
}

// Rectangle represents a rectangular region with position and dimensions.
type Rectangle struct {
	X, Y, Width, Height int
}

// CollisionSeverity indicates the severity level of a collision.
type CollisionSeverity int

const (
	// CollisionLow indicates a minor overlap that may be acceptable.
	CollisionLow CollisionSeverity = iota
	// CollisionMedium indicates a moderate overlap that should be addressed.
	CollisionMedium
	// CollisionHigh indicates a severe overlap that must be resolved.
	CollisionHigh
)

// Collision represents an overlap between two renderable objects.
type Collision struct {
	Object1, Object2 Renderable
	OverlapRegion    Rectangle
	Severity         CollisionSeverity
}

// ResolutionStrategy defines how collision conflicts should be resolved.
type ResolutionStrategy int

const (
	// StrategyOverwrite allows later objects to overwrite earlier ones at collision points.
	StrategyOverwrite ResolutionStrategy = iota
	// StrategySkip skips rendering objects that would collide with existing ones.
	StrategySkip
	// StrategyError returns an error when collisions are detected.
	StrategyError
	// StrategyBlend attempts to blend overlapping content (implementation specific).
	// When this strategy is used, call RenderAllWithBlending instead of RenderAll
	// to apply the configured blend function at collision points.
	StrategyBlend
)

// BlendFunc defines a function type for blending two overlapping characters.
// The first argument is the existing character, the second is the incoming character
// being written, and the function returns the blended result.
type BlendFunc func(existing, incoming rune) rune

// BlendOverlap returns a BlendFunc that uses the specified character for all overlaps.
// This is useful for visually indicating collision regions.
func BlendOverlap(overlapChar rune) BlendFunc {
	return func(existing, incoming rune) rune {
		// Don't blend if one of them is a space (background)
		if existing == ' ' {
			return incoming
		}
		if incoming == ' ' {
			return existing
		}
		return overlapChar
	}
}

// BlendFirst returns a BlendFunc that keeps the first (existing) character.
func BlendFirst() BlendFunc {
	return func(existing, incoming rune) rune {
		if existing == ' ' {
			return incoming
		}
		return existing
	}
}

// BlendLast returns a BlendFunc that keeps the last (incoming) character.
// This is equivalent to the default overwrite behavior.
func BlendLast() BlendFunc {
	return func(_, incoming rune) rune {
		return incoming
	}
}

// boxDrawingMergeMap defines how box-drawing characters should be merged.
// The key is a pair of characters (existing, new), and the value is the merged result.
var boxDrawingMergeMap = map[[2]rune]rune{
	// Horizontal + Vertical intersections
	{'-', '|'}: '+',
	{'|', '-'}: '+',
	{'─', '│'}: '┼',
	{'│', '─'}: '┼',

	// Single line corners and intersections
	{'─', '┌'}: '┬',
	{'─', '┐'}: '┬',
	{'─', '└'}: '┴',
	{'─', '┘'}: '┴',
	{'│', '┌'}: '├',
	{'│', '┐'}: '┤',
	{'│', '└'}: '├',
	{'│', '┘'}: '┤',

	// T-junctions to crosses
	{'┬', '│'}: '┼',
	{'┴', '│'}: '┼',
	{'├', '─'}: '┼',
	{'┤', '─'}: '┼',
	{'│', '┬'}: '┼',
	{'│', '┴'}: '┼',
	{'─', '├'}: '┼',
	{'─', '┤'}: '┼',
}

// isBoxDrawingChar returns true if the rune is a box-drawing character.
func isBoxDrawingChar(r rune) bool {
	// Box Drawing block: U+2500 to U+257F
	return r >= 0x2500 && r <= 0x257F
}

// BlendBoxDrawing returns a BlendFunc that intelligently merges box-drawing characters.
// For non-box-drawing characters, it uses the fallback BlendFunc.
func BlendBoxDrawing(fallback BlendFunc) BlendFunc {
	return func(existing, incoming rune) rune {
		// Check if we have a predefined merge for this pair
		if merged, ok := boxDrawingMergeMap[[2]rune{existing, incoming}]; ok {
			return merged
		}

		// Handle simple ASCII line characters
		isASCIILine := func(r rune) bool { return r == '-' || r == '|' }
		if isASCIILine(existing) && isASCIILine(incoming) {
			if existing != incoming {
				return '+'
			}
			return existing
		}

		// If both are box-drawing characters but no specific merge rule,
		// use the incoming character
		if isBoxDrawingChar(existing) && isBoxDrawingChar(incoming) {
			return incoming
		}

		// Fall back to the provided fallback function
		if fallback != nil {
			return fallback(existing, incoming)
		}
		return incoming
	}
}

// DefaultBlendFunc is the default blend function used when StrategyBlend is selected.
// It uses '#' to indicate overlapping regions.
var DefaultBlendFunc = BlendOverlap('#')

// Renderable defines an interface for objects that can be rendered to a CharSetter.
// This allows different types of objects (text blocks, lines, bars, etc.) to be
// rendered consistently to any canvas or drawing surface.
type Renderable interface {
	// RenderTo renders the object to the provided CharSetter implementation.
	RenderTo(cs CharSetter)

	// GetID returns a unique identifier for this renderable object.
	GetID() string

	// GetBounds returns the rectangular bounds that this object occupies when rendered.
	GetBounds() Rectangle
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

// wrapText applies the configured wrapping mode to the text and returns the wrapped lines.
func (tb TextBlock) wrapText() []string {
	if tb.Text == "" {
		return []string{}
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

	return strings.Split(wrappedText, "\n")
}

// GetBounds returns the rectangular bounds that this text block occupies when rendered.
func (tb TextBlock) GetBounds() Rectangle {
	if tb.Text == "" {
		return Rectangle{X: tb.Position.X, Y: tb.Position.Y, Width: 0, Height: 0}
	}

	lines := tb.wrapText()

	height := len(lines)
	maxWidth := 0

	for _, line := range lines {
		lineWidth := len([]rune(strings.TrimRight(line, " ")))
		if lineWidth > maxWidth {
			maxWidth = lineWidth
		}
	}

	// Determine width: use actual text width for auto-size (Width=0),
	// otherwise use minimum of actual text width and configured width
	width := maxWidth
	if tb.Width > 0 && maxWidth > tb.Width {
		width = tb.Width
	}

	return Rectangle{
		X:      tb.Position.X,
		Y:      tb.Position.Y,
		Width:  width,
		Height: height,
	}
}

// RenderTo renders the text block to any CharSetter implementation.
func (tb TextBlock) RenderTo(cs CharSetter) {
	if tb.Text == "" {
		return
	}

	lines := tb.wrapText()

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
	// Find the last non-empty row to avoid trailing newlines
	lastNonEmptyRow := -1
	for y := c.height - 1; y >= 0; y-- {
		for x := c.width - 1; x >= 0; x-- {
			if c.cells[y][x] != c.background {
				lastNonEmptyRow = y
				break
			}
		}
		if lastNonEmptyRow >= 0 {
			break
		}
	}

	// Empty canvas
	if lastNonEmptyRow < 0 {
		return ""
	}

	// Pre-allocate builder with estimated capacity
	// Estimate: (width + 1 for newline) * number of rows
	var sb strings.Builder
	sb.Grow((c.width + 1) * (lastNonEmptyRow + 1))

	for y := 0; y <= lastNonEmptyRow; y++ {
		// Find last non-space character in this row
		lastNonSpace := -1
		for x := c.width - 1; x >= 0; x-- {
			if c.cells[y][x] != c.background {
				lastNonSpace = x
				break
			}
		}

		// Write the row up to the last non-space character
		for x := 0; x <= lastNonSpace; x++ {
			sb.WriteRune(c.cells[y][x])
		}

		// Add newline if not the last row
		if y < lastNonEmptyRow {
			sb.WriteByte('\n')
		}
	}

	return sb.String()
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
	objects           map[string]Renderable
	insertionOrder    []string
	collisionStrategy ResolutionStrategy
	blendFunc         BlendFunc
}

// NewRenderableCollection creates a new empty collection of renderable objects.
func NewRenderableCollection() *RenderableCollection {
	return &RenderableCollection{
		objects:           make(map[string]Renderable),
		insertionOrder:    make([]string, 0),
		collisionStrategy: StrategyOverwrite,
		blendFunc:         DefaultBlendFunc,
	}
}

// Add adds a renderable object to the collection.
// If an object with the same ID already exists, it will be replaced.
func (rc *RenderableCollection) Add(r Renderable) {
	id := r.GetID()
	if _, exists := rc.objects[id]; !exists {
		rc.insertionOrder = append(rc.insertionOrder, id)
	}
	rc.objects[id] = r
}

// Remove removes a renderable object from the collection by its ID.
// Returns true if the object was found and removed, false otherwise.
func (rc *RenderableCollection) Remove(id string) bool {
	_, exists := rc.objects[id]
	if exists {
		delete(rc.objects, id)
		// Remove from insertion order slice
		for i, oid := range rc.insertionOrder {
			if oid == id {
				rc.insertionOrder = append(rc.insertionOrder[:i], rc.insertionOrder[i+1:]...)
				break
			}
		}
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
	rc.insertionOrder = make([]string, 0)
}

// Count returns the number of renderable objects in the collection.
func (rc *RenderableCollection) Count() int {
	return len(rc.objects)
}

// RenderAll renders all objects in the collection to the provided CharSetter.
// Objects are rendered in insertion order, ensuring deterministic behavior
// for collision resolution strategies like StrategyOverwrite.
func (rc *RenderableCollection) RenderAll(cs CharSetter) {
	for _, id := range rc.insertionOrder {
		if obj, exists := rc.objects[id]; exists {
			obj.RenderTo(cs)
		}
	}
}

// blendingCharSetter wraps a CharSetter to track written cells and apply blending.
type blendingCharSetter struct {
	target    CharSetter
	written   map[[2]int]rune // tracks which cells have been written and their values
	blendFunc BlendFunc
}

// newBlendingCharSetter creates a new blending wrapper for a CharSetter.
func newBlendingCharSetter(target CharSetter, blendFunc BlendFunc) *blendingCharSetter {
	return &blendingCharSetter{
		target:    target,
		written:   make(map[[2]int]rune),
		blendFunc: blendFunc,
	}
}

// SetChar implements CharSetter with blending support.
func (bcs *blendingCharSetter) SetChar(x, y int, r rune) bool {
	key := [2]int{x, y}
	if existing, hasExisting := bcs.written[key]; hasExisting {
		// Apply blend function when cell was already written
		r = bcs.blendFunc(existing, r)
	}
	bcs.written[key] = r
	return bcs.target.SetChar(x, y, r)
}

// RenderAllWithBlending renders all objects with blending support.
// This method should be used when StrategyBlend is the collision strategy.
// It tracks which cells have been written and applies the configured BlendFunc
// when objects overlap.
func (rc *RenderableCollection) RenderAllWithBlending(cs CharSetter) {
	bcs := newBlendingCharSetter(cs, rc.blendFunc)
	for _, id := range rc.insertionOrder {
		if obj, exists := rc.objects[id]; exists {
			obj.RenderTo(bcs)
		}
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

// SetCollisionStrategy sets the collision resolution strategy for the collection.
func (rc *RenderableCollection) SetCollisionStrategy(strategy ResolutionStrategy) {
	rc.collisionStrategy = strategy
}

// GetCollisionStrategy returns the current collision resolution strategy.
func (rc *RenderableCollection) GetCollisionStrategy() ResolutionStrategy {
	return rc.collisionStrategy
}

// SetBlendFunc sets the blend function used when StrategyBlend is selected.
// The blend function determines how overlapping characters are combined.
func (rc *RenderableCollection) SetBlendFunc(fn BlendFunc) {
	rc.blendFunc = fn
}

// GetBlendFunc returns the current blend function.
func (rc *RenderableCollection) GetBlendFunc() BlendFunc {
	return rc.blendFunc
}

// rectanglesOverlap checks if two rectangles overlap and returns the overlap region.
func rectanglesOverlap(r1, r2 Rectangle) (Rectangle, bool) {
	// Check if rectangles don't overlap
	if r1.X >= r2.X+r2.Width || r2.X >= r1.X+r1.Width ||
		r1.Y >= r2.Y+r2.Height || r2.Y >= r1.Y+r1.Height {
		return Rectangle{}, false
	}

	// Calculate overlap region
	x := intMax(r1.X, r2.X)
	y := intMax(r1.Y, r2.Y)
	right := intMin(r1.X+r1.Width, r2.X+r2.Width)
	bottom := intMin(r1.Y+r1.Height, r2.Y+r2.Height)

	return Rectangle{
		X:      x,
		Y:      y,
		Width:  right - x,
		Height: bottom - y,
	}, true
}

// intMax returns the maximum of two integers.
func intMax(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// intMin returns the minimum of two integers.
func intMin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// calculateCollisionSeverity determines the severity of a collision based on overlap area.
func calculateCollisionSeverity(overlap Rectangle) CollisionSeverity {
	area := overlap.Width * overlap.Height

	if area <= 5 {
		return CollisionLow
	} else if area <= 20 {
		return CollisionMedium
	}
	return CollisionHigh
}

// DetectCollisions detects all collisions between objects in the collection.
func (rc *RenderableCollection) DetectCollisions() []Collision {
	var collisions []Collision

	// Get all objects as a slice for easier iteration
	objects := make([]Renderable, 0, len(rc.objects))
	for _, obj := range rc.objects {
		objects = append(objects, obj)
	}

	// Check each pair of objects for collisions
	for i := 0; i < len(objects); i++ {
		for j := i + 1; j < len(objects); j++ {
			obj1, obj2 := objects[i], objects[j]
			bounds1, bounds2 := obj1.GetBounds(), obj2.GetBounds()

			if overlap, hasOverlap := rectanglesOverlap(bounds1, bounds2); hasOverlap {
				collision := Collision{
					Object1:       obj1,
					Object2:       obj2,
					OverlapRegion: overlap,
					Severity:      calculateCollisionSeverity(overlap),
				}
				collisions = append(collisions, collision)
			}
		}
	}

	return collisions
}

// ValidateLayout checks if the current layout has any collisions and returns an error if found.
func (rc *RenderableCollection) ValidateLayout() error {
	collisions := rc.DetectCollisions()
	if len(collisions) > 0 {
		return fmt.Errorf("layout validation failed: found %d collision(s)", len(collisions))
	}
	return nil
}

// GetOccupiedRegions returns a slice of all rectangular regions occupied by objects.
func (rc *RenderableCollection) GetOccupiedRegions() []Rectangle {
	regions := make([]Rectangle, 0, len(rc.objects))
	for _, obj := range rc.objects {
		regions = append(regions, obj.GetBounds())
	}
	return regions
}

// ResolveCollisions applies the configured collision resolution strategy.
func (rc *RenderableCollection) ResolveCollisions() error {
	collisions := rc.DetectCollisions()

	if len(collisions) == 0 {
		return nil
	}

	switch rc.collisionStrategy {
	case StrategyError:
		return fmt.Errorf("collision resolution failed: found %d collision(s)", len(collisions))

	case StrategySkip:
		// Remove objects that collide with earlier objects
		// We keep track of which objects to remove
		toRemove := make(map[string]bool)
		processedIDs := make(map[string]bool)

		for _, collision := range collisions {
			id1, id2 := collision.Object1.GetID(), collision.Object2.GetID()

			// If object1 was processed first, remove object2
			// If object2 was processed first, remove object1
			// If neither was processed, keep object1 and remove object2
			switch {
			case processedIDs[id1] && !processedIDs[id2]:
				toRemove[id2] = true
			case processedIDs[id2] && !processedIDs[id1]:
				toRemove[id1] = true
			case !processedIDs[id1] && !processedIDs[id2]:
				// Neither processed, keep first object, remove second
				processedIDs[id1] = true
				toRemove[id2] = true
			}
		}

		// Remove the conflicting objects
		for id := range toRemove {
			rc.Remove(id)
		}

	case StrategyOverwrite:
		// TODO(#57): Fix non-deterministic overwrite order due to map
		// iteration Currently RenderAll iterates over map in undefined
		// order, making "later objects overwrite earlier ones" behavior
		// unpredictable Default behavior - later objects overwrite
		// earlier ones No action needed as RenderAll already handles
		// this

	case StrategyBlend:
		// Blending is handled during rendering via RenderAllWithBlending.
		// This strategy keeps all objects and applies the configured BlendFunc
		// at collision points during rendering. Use SetBlendFunc to customize
		// how overlapping characters are combined.

	default:
		return fmt.Errorf("unknown collision resolution strategy: %d", rc.collisionStrategy)
	}

	return nil
}

// LineStyle specifies the visual style of a line.
type LineStyle int

const (
	// LineStyleSingle uses single-character lines (- for horizontal, | for vertical).
	LineStyleSingle LineStyle = iota
	// LineStyleDouble uses double-character lines (= for horizontal, ‖ for vertical).
	LineStyleDouble
	// LineStyleDashed uses dashed lines (- - for horizontal, : for vertical).
	LineStyleDashed
	// LineStyleDotted uses dotted lines (. . for horizontal, · for vertical).
	LineStyleDotted
)

// LineDirection specifies the direction of a line.
type LineDirection int

const (
	// LineDirectionHorizontal creates horizontal lines.
	LineDirectionHorizontal LineDirection = iota
	// LineDirectionVertical creates vertical lines.
	LineDirectionVertical
)

// Line represents a line element that can be rendered to a canvas.
// Lines can be horizontal or vertical and support different visual styles.
type Line struct {
	// ID uniquely identifies the line for removal and updates.
	ID string
	// Start is the starting position of the line.
	Start Position
	// Length is the length of the line in characters.
	Length int
	// Direction specifies whether the line is horizontal or vertical.
	Direction LineDirection
	// Style specifies the visual appearance of the line.
	Style LineStyle
}

// GetID returns the unique identifier for this line.
func (l Line) GetID() string {
	return l.ID
}

// GetBounds returns the rectangular bounds that this line occupies when rendered.
func (l Line) GetBounds() Rectangle {
	if l.Length <= 0 {
		return Rectangle{X: l.Start.X, Y: l.Start.Y, Width: 0, Height: 0}
	}

	switch l.Direction {
	case LineDirectionHorizontal:
		return Rectangle{
			X:      l.Start.X,
			Y:      l.Start.Y,
			Width:  l.Length,
			Height: 1,
		}
	case LineDirectionVertical:
		return Rectangle{
			X:      l.Start.X,
			Y:      l.Start.Y,
			Width:  1,
			Height: l.Length,
		}
	default:
		return Rectangle{X: l.Start.X, Y: l.Start.Y, Width: 0, Height: 0}
	}
}

// getLineChar returns the character to use for the line based on style and direction.
func (l Line) getLineChar() rune {
	switch l.Style {
	case LineStyleSingle:
		if l.Direction == LineDirectionHorizontal {
			return '-'
		}
		return '|'
	case LineStyleDouble:
		if l.Direction == LineDirectionHorizontal {
			return '='
		}
		return '‖'
	case LineStyleDashed:
		if l.Direction == LineDirectionHorizontal {
			return '-'
		}
		return ':'
	case LineStyleDotted:
		if l.Direction == LineDirectionHorizontal {
			return '·'
		}
		return '·'
	default:
		if l.Direction == LineDirectionHorizontal {
			return '-'
		}
		return '|'
	}
}

// shouldRenderCharAt determines if a character should be rendered at the given offset
// for dashed and dotted line styles.
func (l Line) shouldRenderCharAt(offset int) bool {
	switch l.Style {
	case LineStyleSingle, LineStyleDouble:
		return true
	case LineStyleDashed:
		return offset%2 == 0 // Render every other character for dashed effect
	case LineStyleDotted:
		return offset%2 == 0 // Render every other character for dotted effect
	default:
		return true
	}
}

// RenderTo renders the line to any CharSetter implementation.
func (l Line) RenderTo(cs CharSetter) {
	if l.Length <= 0 {
		return
	}

	lineChar := l.getLineChar()

	switch l.Direction {
	case LineDirectionHorizontal:
		for i := 0; i < l.Length; i++ {
			if l.shouldRenderCharAt(i) {
				cs.SetChar(l.Start.X+i, l.Start.Y, lineChar)
			}
		}
	case LineDirectionVertical:
		for i := 0; i < l.Length; i++ {
			if l.shouldRenderCharAt(i) {
				cs.SetChar(l.Start.X, l.Start.Y+i, lineChar)
			}
		}
	}
}

// NewTextBlock creates a new TextBlock with common defaults for simple text positioning.
// This is a convenience function for creating text blocks from strings.
func NewTextBlock(id string, text string, position Position) TextBlock {
	return TextBlock{
		ID:       id,
		Text:     text,
		Position: position,
		Width:    0, // Auto-size width
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}
}

// NewTextBlockWithWidth creates a new TextBlock with specified width and wrapping.
func NewTextBlockWithWidth(id string, text string, position Position, width int) TextBlock {
	return TextBlock{
		ID:       id,
		Text:     text,
		Position: position,
		Width:    width,
		WrapMode: WrapBasic,
		Align:    AlignLeft,
	}
}

// CellDiff represents a difference between two canvas cells at a specific position.
type CellDiff struct {
	Position Position
	Old      rune
	New      rune
}

// Resize changes the canvas dimensions while preserving existing content
// that fits within the new bounds. Content outside the new bounds is lost.
// Invalid dimensions (<=0) are clamped to 1.
func (c *Canvas) Resize(width, height int) {
	if width <= 0 {
		width = 1
	}
	if height <= 0 {
		height = 1
	}

	// No-op if size unchanged
	if width == c.width && height == c.height {
		return
	}

	// Create new cells
	newCells := make([][]rune, height)
	for y := 0; y < height; y++ {
		newCells[y] = make([]rune, width)
		for x := 0; x < width; x++ {
			// Copy from old cells if within old bounds, otherwise use background
			if y < c.height && x < c.width {
				newCells[y][x] = c.cells[y][x]
			} else {
				newCells[y][x] = c.background
			}
		}
	}

	c.cells = newCells
	c.width = width
	c.height = height
}

// Clone creates a deep copy of the canvas.
// The returned canvas is completely independent of the original.
func (c *Canvas) Clone() *Canvas {
	clone := &Canvas{
		width:      c.width,
		height:     c.height,
		background: c.background,
		cells:      make([][]rune, c.height),
	}

	for y := 0; y < c.height; y++ {
		clone.cells[y] = make([]rune, c.width)
		copy(clone.cells[y], c.cells[y])
	}

	return clone
}

// Update applies a batch of modifications to the canvas and returns
// the list of cells that were changed. This is useful for tracking
// which parts of the canvas need to be redrawn.
//
// The function takes a snapshot before applying changes, then computes
// the diff after the function completes.
func (c *Canvas) Update(fn func(*Canvas)) []CellDiff {
	// Take snapshot before changes
	snapshot := c.Clone()

	// Apply the changes
	fn(c)

	// Compute and return the diff
	return snapshot.Diff(c)
}

// Diff compares this canvas with another and returns a list of cell differences.
// Returns nil if the canvases have different dimensions.
// This is useful for implementing efficient terminal updates where only
// changed cells need to be redrawn.
func (c *Canvas) Diff(other *Canvas) []CellDiff {
	if c.width != other.width || c.height != other.height {
		return nil
	}

	var diffs []CellDiff

	for y := 0; y < c.height; y++ {
		for x := 0; x < c.width; x++ {
			oldChar := c.cells[y][x]
			newChar := other.cells[y][x]
			if oldChar != newChar {
				diffs = append(diffs, CellDiff{
					Position: Position{X: x, Y: y},
					Old:      oldChar,
					New:      newChar,
				})
			}
		}
	}

	return diffs
}

// Pool provides memory pooling for Canvas allocation and reuse.
// This is useful for high-throughput scenarios where canvases are frequently
// created and destroyed, reducing GC pressure.
type Pool struct {
	pool   sync.Pool
	width  int
	height int
}

// NewPool creates a new pool for canvases of the specified dimensions.
// All canvases obtained from this pool will have the same width and height.
func NewPool(width, height int) *Pool {
	if width <= 0 {
		width = 1
	}
	if height <= 0 {
		height = 1
	}

	return &Pool{
		width:  width,
		height: height,
		pool: sync.Pool{
			New: func() interface{} {
				return New(width, height)
			},
		},
	}
}

// Get retrieves a canvas from the pool, or creates a new one if the pool is empty.
// The returned canvas is cleared and ready for use.
func (p *Pool) Get() *Canvas {
	canvas := p.pool.Get().(*Canvas)

	// Ensure dimensions match (in case pool was used with different sizes somehow)
	if canvas.width != p.width || canvas.height != p.height {
		canvas.Resize(p.width, p.height)
	}

	// Clear the canvas for fresh use
	canvas.Clear()

	return canvas
}

// Put returns a canvas to the pool for reuse.
// The canvas should not be used after calling Put.
func (p *Pool) Put(canvas *Canvas) {
	if canvas == nil {
		return
	}

	// Only accept canvases of the correct size
	if canvas.width != p.width || canvas.height != p.height {
		return
	}

	p.pool.Put(canvas)
}

// Width returns the width of canvases in this pool.
func (p *Pool) Width() int {
	return p.width
}

// Height returns the height of canvases in this pool.
func (p *Pool) Height() int {
	return p.height
}
