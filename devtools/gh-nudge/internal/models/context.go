package models

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// LineContext represents code lines around a comment for display purposes.
type LineContext struct {
	Range LineRange `json:"range"`
	Lines []string  `json:"lines"`
}

// CommentWithLineContext wraps a Comment with its line context for display.
type CommentWithLineContext struct {
	Comment
	Context *LineContext `json:"context,omitempty"`
}

// GetLineContext reads a file and extracts lines around the specified line number.
// It returns contextLines before and after the target line.
func GetLineContext(filePath string, lineNumber int, contextLines int) (*LineContext, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to open file %s: %w", filePath, err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	currentLine := 0
	var allLines []string

	// Read all lines from the file
	for scanner.Scan() {
		currentLine++
		allLines = append(allLines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("failed to read file %s: %w", filePath, err)
	}

	// Check if line number is valid
	if lineNumber < 1 || lineNumber > len(allLines) {
		return nil, fmt.Errorf("line number %d out of range (file has %d lines)", lineNumber, len(allLines))
	}

	// Calculate start and end lines (1-indexed)
	startLine := maxInt(1, lineNumber-contextLines)
	endLine := minInt(len(allLines), lineNumber+contextLines)

	// Extract the context lines (convert to 0-indexed for slice)
	contextLineSlice := allLines[startLine-1 : endLine]

	return &LineContext{
		Range: NewLineRange(startLine, endLine),
		Lines: contextLineSlice,
	}, nil
}

// GetLineContextForComment retrieves line context for a specific comment.
// For multi-line comments, it shows context around the entire range.
func GetLineContextForComment(filePath string, comment Comment, contextLines int) (*LineContext, error) {
	lineRange := comment.GetLineRange()

	// For multi-line comments, show context around the entire range
	targetLine := (lineRange.StartLine + lineRange.EndLine) / 2

	return GetLineContext(filePath, targetLine, contextLines)
}

// FormatLineContext formats line context for display with line numbers.
func FormatLineContext(context *LineContext, highlightLine int) string {
	if context == nil {
		return ""
	}

	var result strings.Builder
	// TODO: Consider if LineRange can provide an iterator.
	for i, line := range context.Lines {
		lineNum := context.Range.StartLine + i
		prefix := "  "
		if lineNum == highlightLine {
			prefix = "> "
		}
		result.WriteString(fmt.Sprintf("%s%4d | %s\n", prefix, lineNum, line))
	}
	return result.String()
}
