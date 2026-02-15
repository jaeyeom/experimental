// Package box provides ASCII art box rendering with sections and connectors.
//
// It uses box-drawing characters from the canvas package and supports
// multiple border styles, configurable padding, and comment prefixes.
//
// Basic usage:
//
//	output := box.New().
//		AddSection("Title", "Description").
//		AddSection("Another Title", "Another Description").
//		Render()
//
// Output example:
//
//	┌──────────────────────────────┐
//	│  Title                       │
//	│  Description                 │
//	│              │               │
//	│              ▼               │
//	│  Another Title               │
//	│  Another Description         │
//	└──────────────────────────────┘
package box

import (
	"strings"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/text/canvas"
)

// Box builds an ASCII art box with sections separated by connectors.
type Box struct {
	sections  [][]string
	connector []string
	style     canvas.BorderStyle
	padding   int
	width     int
	prefix    string
}

// New creates a new Box with default settings.
func New() *Box {
	return &Box{
		connector: []string{"│", "▼"},
		style:     canvas.BorderSingle,
		padding:   2,
	}
}

// AddSection adds a text section to the box.
func (b *Box) AddSection(lines ...string) *Box {
	b.sections = append(b.sections, lines)
	return b
}

// WithConnector sets the connector lines drawn between sections.
func (b *Box) WithConnector(lines ...string) *Box {
	b.connector = lines
	return b
}

// WithStyle sets the border style.
func (b *Box) WithStyle(style canvas.BorderStyle) *Box {
	b.style = style
	return b
}

// WithPadding sets the left/right inner padding.
func (b *Box) WithPadding(padding int) *Box {
	b.padding = padding
	return b
}

// WithWidth sets the total box width. 0 means auto-calculate.
func (b *Box) WithWidth(width int) *Box {
	b.width = width
	return b
}

// WithPrefix sets a comment prefix prepended to every output line.
func (b *Box) WithPrefix(prefix string) *Box {
	b.prefix = prefix
	return b
}

// Render produces the final ASCII art string.
func (b *Box) Render() string {
	chars := canvas.GetBoxChars(b.style)
	innerWidth := b.calcInnerWidth()

	var sb strings.Builder

	// Top border
	sb.WriteString(b.prefix)
	sb.WriteRune(chars.TopLeft)
	sb.WriteString(strings.Repeat(string(chars.Horizontal), innerWidth))
	sb.WriteRune(chars.TopRight)

	for i, section := range b.sections {
		// Connector between sections (not before the first)
		if i > 0 {
			for _, cl := range b.connector {
				sb.WriteByte('\n')
				sb.WriteString(b.prefix)
				sb.WriteRune(chars.Vertical)
				sb.WriteString(b.centerPad(cl, innerWidth))
				sb.WriteRune(chars.Vertical)
			}
		}

		// Section lines
		for _, line := range section {
			sb.WriteByte('\n')
			sb.WriteString(b.prefix)
			sb.WriteRune(chars.Vertical)
			sb.WriteString(b.leftPad(line, innerWidth))
			sb.WriteRune(chars.Vertical)
		}
	}

	// Bottom border
	sb.WriteByte('\n')
	sb.WriteString(b.prefix)
	sb.WriteRune(chars.BottomLeft)
	sb.WriteString(strings.Repeat(string(chars.Horizontal), innerWidth))
	sb.WriteRune(chars.BottomRight)

	return sb.String()
}

// calcInnerWidth returns the width between the two vertical border characters.
func (b *Box) calcInnerWidth() int {
	if b.width > 0 {
		return b.width - 2 // minus left and right border chars
	}

	maxLen := 0
	for _, section := range b.sections {
		for _, line := range section {
			if n := utf8.RuneCountInString(line); n > maxLen {
				maxLen = n
			}
		}
	}
	for _, cl := range b.connector {
		if n := utf8.RuneCountInString(cl); n > maxLen {
			maxLen = n
		}
	}

	return maxLen + 2*b.padding
}

// leftPad returns a string of exactly innerWidth runes with the content
// left-aligned after padding spaces.
func (b *Box) leftPad(text string, innerWidth int) string {
	textLen := utf8.RuneCountInString(text)
	right := innerWidth - b.padding - textLen
	if right < 0 {
		right = 0
	}
	return strings.Repeat(" ", b.padding) + text + strings.Repeat(" ", right)
}

// centerPad returns a string of exactly innerWidth runes with the content centered.
func (b *Box) centerPad(text string, innerWidth int) string {
	textLen := utf8.RuneCountInString(text)
	total := innerWidth - textLen
	left := total / 2
	right := total - left
	if left < 0 {
		left = 0
	}
	if right < 0 {
		right = 0
	}
	return strings.Repeat(" ", left) + text + strings.Repeat(" ", right)
}
