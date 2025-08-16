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
)

// Position represents a coordinate position on the canvas.
type Position struct {
	X, Y int
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
