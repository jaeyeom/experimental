package models

import (
	"encoding/json"
	"fmt"
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
	return fmt.Sprintf("Submitted review for PR %s/%s#%d with %d comments",
		result.Owner, result.Repo, result.PRNumber, result.Comments), nil
}

// FormatComments formats comments as a table.
func (f *TextFormatter) FormatComments(comments []Comment) (string, error) {
	if len(comments) == 0 {
		return "No comments found", nil
	}

	var result strings.Builder
	fmt.Fprintf(&result, "%-8s %-30s %-8s %-8s %-50s %-20s\n", "ID", "File", "Line", "Side", "Comment", "Created")
	result.WriteString(strings.Repeat("-", 124))
	result.WriteString("\n")

	for _, comment := range comments {
		lineStr := strconv.Itoa(comment.Line)
		if comment.IsMultiLine() {
			lineStr = fmt.Sprintf("%d-%d", *comment.StartLine, comment.Line)
		}

		fmt.Fprintf(&result, "%-8s %-30s %-8s %-8s %-50s %-20s\n",
			comment.FormatIDShort(),
			TruncateString(comment.Path, 30),
			lineStr,
			comment.Side,
			TruncateString(comment.Body, 50),
			comment.CreatedAt.Format("2006-01-02 15:04"))
	}

	return result.String(), nil
}

// TruncateString truncates a string to maxLen characters, adding "..." if truncated.
func TruncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
