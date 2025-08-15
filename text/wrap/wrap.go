// Package wrap provides text wrapping utilities for formatting text to fit within specified widths.
// It supports various wrapping modes including basic text wrapping, indented wrapping,
// word wrapping with long word handling, and soft wrapping.
package wrap

import (
	"strings"
)

// Text wraps the given text to the specified width and returns the wrapped text
// and the number of lines after wrapping.
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

// wrapParagraph wraps a single paragraph to the specified width.
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

// countLines counts the number of lines in the given text.
func countLines(text string) int {
	if text == "" {
		return 0
	}
	return strings.Count(text, "\n") + 1
}

// TextIndent wraps text with a specific indent for continuation lines.
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

// WordWrap wraps text preserving word boundaries and handles long words.
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

// wrapParagraphWithLongWords wraps a paragraph and handles words longer than width.
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

// breakLongWord breaks a word that's longer than the width into multiple lines.
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

// SoftWrap wraps text at word boundaries, preferring to break at whitespace.
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

// softWrapParagraph wraps a paragraph preferring to break at natural boundaries.
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
