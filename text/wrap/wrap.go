// Package wrap provides text wrapping utilities for formatting text to fit
// within specified widths. It supports various wrapping modes including basic
// text wrapping, indented wrapping, word wrapping with long word handling, and
// soft wrapping.
package wrap

import (
	"strings"
)

// Text wraps the given text to the specified width, preserving paragraph breaks
// (double newlines) and returning both the wrapped text and the total number of lines.
// If width is <= 0, returns the original text unchanged. Empty text returns empty
// string with 0 lines.
func Text(text string, width int) (wrapped string, lines int) {
	if text == "" {
		return "", 0
	}
	if width <= 0 {
		return text, countLines(text)
	}

	var result []string
	paragraphs := strings.Split(text, "\n\n")

	for i, paragraph := range paragraphs {
		if i > 0 {
			result = append(result, "")
		}
		wrappedParagraph := wrapParagraph(paragraph, width)
		result = append(result, wrappedParagraph...)
	}

	wrapped = strings.Join(result, "\n")
	lines = len(result)
	return wrapped, lines
}

// wrapParagraph wraps a single paragraph to the specified width by breaking
// at word boundaries. Returns a slice of lines, each no longer than width
// characters (unless a single word exceeds the width).
func wrapParagraph(paragraph string, width int) []string {
	paragraph = strings.TrimSpace(paragraph)
	if paragraph == "" {
		return []string{""}
	}

	words := strings.Fields(paragraph)
	if len(words) == 0 {
		return []string{""}
	}

	var lines []string
	var currentLine strings.Builder

	for _, word := range words {
		switch {
		case currentLine.Len() == 0:
			currentLine.WriteString(word)
		case currentLine.Len()+1+len(word) <= width:
			currentLine.WriteString(" ")
			currentLine.WriteString(word)
		default:
			lines = append(lines, currentLine.String())
			currentLine.Reset()
			currentLine.WriteString(word)
		}
	}

	if currentLine.Len() > 0 {
		lines = append(lines, currentLine.String())
	}

	return lines
}

// countLines counts the number of lines in the given text by counting
// newline characters plus one. Returns 0 for empty text.
func countLines(text string) int {
	if text == "" {
		return 0
	}
	return strings.Count(text, "\n") + 1
}

// TextIndent wraps text to the specified width while applying the given indent
// string to all continuation lines (lines after the first line of each paragraph).
// The effective wrapping width is reduced by the length of the indent string.
// If the effective width becomes <= 0, returns the original text unchanged.
func TextIndent(text string, width int, indent string) (wrapped string, lines int) {
	if width <= 0 {
		return text, countLines(text)
	}

	effectiveWidth := width - len(indent)
	if effectiveWidth <= 0 {
		return text, countLines(text)
	}

	var result []string
	paragraphs := strings.Split(text, "\n\n")

	for i, paragraph := range paragraphs {
		if i > 0 {
			result = append(result, "")
		}
		wrappedLines := wrapParagraph(paragraph, effectiveWidth)
		for j, line := range wrappedLines {
			if j == 0 {
				result = append(result, line)
			} else {
				result = append(result, indent+line)
			}
		}
	}

	wrapped = strings.Join(result, "\n")
	lines = len(result)
	return wrapped, lines
}

// WordWrap wraps text to the specified width while preserving word boundaries.
// Unlike Text, this function handles words longer than the width by breaking them
// across multiple lines. Paragraph breaks (double newlines) are preserved.
func WordWrap(text string, width int) (wrapped string, lines int) {
	if text == "" {
		return "", 0
	}
	if width <= 0 {
		return text, countLines(text)
	}

	var result []string
	paragraphs := strings.Split(text, "\n\n")

	for i, paragraph := range paragraphs {
		if i > 0 {
			result = append(result, "")
		}
		wrappedParagraph := wrapParagraphWithLongWords(paragraph, width)
		result = append(result, wrappedParagraph...)
	}

	wrapped = strings.Join(result, "\n")
	lines = len(result)
	return wrapped, lines
}

// wrapParagraphWithLongWords wraps a paragraph to the specified width,
// breaking long words that exceed the width into multiple lines.
// Uses breakLongWord to split oversized words at character boundaries.
func wrapParagraphWithLongWords(paragraph string, width int) []string {
	paragraph = strings.TrimSpace(paragraph)
	if paragraph == "" {
		return []string{""}
	}

	words := strings.Fields(paragraph)
	if len(words) == 0 {
		return []string{""}
	}

	var lines []string
	var currentLine strings.Builder

	for _, word := range words {
		switch {
		case len(word) > width:
			if currentLine.Len() > 0 {
				lines = append(lines, currentLine.String())
				currentLine.Reset()
			}
			lines = append(lines, breakLongWord(word, width)...)
		case currentLine.Len() == 0:
			currentLine.WriteString(word)
		case currentLine.Len()+1+len(word) <= width:
			currentLine.WriteString(" ")
			currentLine.WriteString(word)
		default:
			lines = append(lines, currentLine.String())
			currentLine.Reset()
			currentLine.WriteString(word)
		}
	}

	if currentLine.Len() > 0 {
		lines = append(lines, currentLine.String())
	}

	return lines
}

// breakLongWord breaks a word that exceeds the specified width into multiple
// lines by splitting at character boundaries (rune-aware). Each resulting line
// will be at most width characters long, except possibly the last line.
func breakLongWord(word string, width int) []string {
	var lines []string
	runes := []rune(word)

	for i := 0; i < len(runes); i += width {
		end := i + width
		if end > len(runes) {
			end = len(runes)
		}
		lines = append(lines, string(runes[i:end]))
	}

	return lines
}

// SoftWrap wraps text to the specified width using soft wrapping, which breaks
// lines only at word boundaries and never breaks words. This is identical to
// the Text function but the name emphasizes the soft-wrapping behavior.
// Paragraph breaks (double newlines) are preserved.
func SoftWrap(text string, width int) (wrapped string, lines int) {
	if text == "" {
		return "", 0
	}
	if width <= 0 {
		return text, countLines(text)
	}

	var result []string
	paragraphs := strings.Split(text, "\n\n")

	for i, paragraph := range paragraphs {
		if i > 0 {
			result = append(result, "")
		}
		wrappedParagraph := softWrapParagraph(paragraph, width)
		result = append(result, wrappedParagraph...)
	}

	wrapped = strings.Join(result, "\n")
	lines = len(result)
	return wrapped, lines
}

// softWrapParagraph wraps a single paragraph using soft wrapping, breaking
// only at word boundaries and never splitting words. Lines will not exceed
// the specified width unless a single word is longer than the width.
func softWrapParagraph(paragraph string, width int) []string {
	paragraph = strings.TrimSpace(paragraph)
	if paragraph == "" {
		return []string{""}
	}

	words := strings.Fields(paragraph)
	if len(words) == 0 {
		return []string{""}
	}

	var lines []string
	var currentLine strings.Builder

	for _, word := range words {
		switch {
		case currentLine.Len() == 0:
			currentLine.WriteString(word)
		case currentLine.Len()+1+len(word) <= width:
			currentLine.WriteString(" ")
			currentLine.WriteString(word)
		default:
			lines = append(lines, currentLine.String())
			currentLine.Reset()
			currentLine.WriteString(word)
		}
	}

	if currentLine.Len() > 0 {
		lines = append(lines, currentLine.String())
	}

	return lines
}
