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

// GetLineContext reads a file and extracts lines around the specified line range.
// It returns contextLines before and after the target range.
func GetLineContext(filePath string, lineRange LineRange, contextLines int) (*LineContext, error) {
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

	// Check if line range is valid
	if lineRange.StartLine < 1 || lineRange.StartLine > len(allLines) {
		return nil, fmt.Errorf("start line %d out of range (file has %d lines)", lineRange.StartLine, len(allLines))
	}
	if lineRange.EndLine < 1 || lineRange.EndLine > len(allLines) {
		return nil, fmt.Errorf("end line %d out of range (file has %d lines)", lineRange.EndLine, len(allLines))
	}
	if lineRange.StartLine > lineRange.EndLine {
		return nil, fmt.Errorf("invalid line range: start line %d is after end line %d", lineRange.StartLine, lineRange.EndLine)
	}

	// Calculate start and end lines with context (1-indexed)
	startLine := maxInt(1, lineRange.StartLine-contextLines)
	endLine := minInt(len(allLines), lineRange.EndLine+contextLines)

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
	return GetLineContext(filePath, lineRange, contextLines)
}

// FormatLineContext formats line context for display with line numbers.
func FormatLineContext(context *LineContext, highlightLine int) string {
	if context == nil {
		return ""
	}

	var result strings.Builder
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
