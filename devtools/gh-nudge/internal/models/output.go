package models

import (
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// SubmitResult contains the result data from a submit operation.
type SubmitResult struct {
	Status           string    `json:"status"`
	PRNumber         int       `json:"prNumber"`
	Owner            string    `json:"owner"`
	Repo             string    `json:"repo"`
	Comments         int       `json:"comments"`
	SubmittedAt      time.Time `json:"submittedAt"`
	PostSubmitAction string    `json:"postSubmitAction"`
	File             string    `json:"file,omitempty"` // File path if file-specific submission
}

// JSONFormatter formats output as JSON.
type JSONFormatter struct{}

// NewJSONFormatter creates a new JSON formatter.
func NewJSONFormatter() *JSONFormatter {
	return &JSONFormatter{}
}

// FormatSubmitResult formats submit result as JSON.
func (f *JSONFormatter) FormatSubmitResult(result SubmitResult) (string, error) {
	jsonData, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return "", fmt.Errorf("failed to marshal result: %w", err)
	}
	return string(jsonData), nil
}

// FormatComments formats comments as JSON.
func (f *JSONFormatter) FormatComments(comments []Comment) (string, error) {
	jsonData, err := json.MarshalIndent(comments, "", "  ")
	if err != nil {
		return "", fmt.Errorf("failed to marshal comments: %w", err)
	}
	return string(jsonData), nil
}

// TextFormatter formats output as human-readable text.
type TextFormatter struct{}

// NewTextFormatter creates a new text formatter.
func NewTextFormatter() *TextFormatter {
	return &TextFormatter{}
}

// FormatSubmitResult formats submit result as text.
func (f *TextFormatter) FormatSubmitResult(result SubmitResult) (string, error) {
	if result.File != "" {
		return fmt.Sprintf("Submitted review for PR %s/%s#%d with %d comments from file %s",
			result.Owner, result.Repo, result.PRNumber, result.Comments, result.File), nil
	}
	return fmt.Sprintf("Submitted review for PR %s/%s#%d with %d comments",
		result.Owner, result.Repo, result.PRNumber, result.Comments), nil
}

// FormatComments formats comments as a table.
func (f *TextFormatter) FormatComments(comments []Comment) (string, error) {
	if len(comments) == 0 {
		return "No comments found", nil
	}

	// Get terminal width from environment or default to 120
	termWidth := 120
	if os.Getenv("COLUMNS") != "" {
		if width, err := strconv.Atoi(os.Getenv("COLUMNS")); err == nil && width > 0 {
			termWidth = width
		}
	}

	// Calculate column widths dynamically
	idWidth := 8
	lineWidth := 8
	sideWidth := 8
	createdWidth := 16

	// Reserve space for separators and padding
	reservedWidth := idWidth + lineWidth + sideWidth + createdWidth + 8 // 8 for separators

	// Remaining width for file and comment columns
	remainingWidth := termWidth - reservedWidth
	if remainingWidth < 40 {
		remainingWidth = 40 // Minimum usable width
	}

	// Allocate remaining width: prioritize comment over file, but ensure file gets reasonable space
	fileWidth := minInt(40, remainingWidth/3)  // File gets 1/3 or max 40
	commentWidth := remainingWidth - fileWidth // Comment gets the rest

	var result strings.Builder
	fmt.Fprintf(&result, "%-*s %-*s %-*s %-*s %-*s %-*s\n",
		idWidth, "ID",
		fileWidth, "File",
		lineWidth, "Line",
		sideWidth, "Side",
		commentWidth, "Comment",
		createdWidth, "Created")
	result.WriteString(strings.Repeat("-", minInt(termWidth, idWidth+fileWidth+lineWidth+sideWidth+commentWidth+createdWidth+12)))
	result.WriteString("\n")

	for _, comment := range comments {
		lineStr := strconv.Itoa(comment.Line)
		if comment.IsMultiLine() {
			lineStr = fmt.Sprintf("%d-%d", *comment.StartLine, comment.Line)
		}

		fmt.Fprintf(&result, "%-*s %-*s %-*s %-*s %-*s %-*s\n",
			idWidth, comment.FormatIDShort(),
			fileWidth, smartTruncateFilePath(comment.Path, fileWidth),
			lineWidth, lineStr,
			sideWidth, comment.Side,
			commentWidth, TruncateString(comment.Body, commentWidth),
			createdWidth, comment.CreatedAt.Format("2006-01-02 15:04"))
	}

	return result.String(), nil
}

// TruncateString truncates a string to maxLen characters, adding "..." if truncated.
func TruncateString(s string, maxLen int) string {
	if maxLen <= 3 {
		if len(s) <= maxLen {
			return s
		}
		return s[:maxLen]
	}
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}

// smartTruncateFilePath truncates a file path by preserving the most important part (end of path).
func smartTruncateFilePath(path string, maxLen int) string {
	if len(path) <= maxLen {
		return path
	}

	if maxLen <= 3 {
		return path[:maxLen]
	}

	// For file paths, the end is usually more important than the beginning
	// Try to preserve the filename and some directory context
	parts := strings.Split(path, "/")
	if len(parts) == 1 {
		// No directory separators, just truncate normally
		return TruncateString(path, maxLen)
	}

	// Start with the filename and work backwards adding directories
	result := parts[len(parts)-1] // filename
	for i := len(parts) - 2; i >= 0; i-- {
		candidate := parts[i] + "/" + result
		if len(candidate) > maxLen-3 { // leave room for "..."
			if result == parts[len(parts)-1] {
				// Even just the filename is too long
				return TruncateString(result, maxLen)
			}
			return "..." + result
		}
		result = candidate
	}

	return result
}

// minInt returns the smaller of two integers.
func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}
