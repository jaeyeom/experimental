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
//
// A submit operation refers to submitting a code review to GitHub, which includes:
//   - Posting review comments on specific lines of a pull request
//   - Optionally submitting a review body with an overall assessment
//   - Specifying a review event (e.g., COMMENT, APPROVE, REQUEST_CHANGES)
//   - Executing post-submit actions (e.g., clearing local comments, archiving)
//
// This result is typically returned after successfully submitting a review via the GitHub API,
// and contains metadata about what was submitted and when.
type SubmitResult struct {
	Status           string     `json:"status"`           // Success/failure status of the submission
	PRNumber         int        `json:"prNumber"`         // The PR number that was reviewed
	Repository       Repository `json:"repository"`       // The repository (owner/name) containing the PR
	Comments         int        `json:"comments"`         // Number of comments submitted in the review
	SubmittedAt      time.Time  `json:"submittedAt"`      // Timestamp when the review was submitted
	PostSubmitAction string     `json:"postSubmitAction"` // Name of the post-submit action that was executed (e.g., "clear", "archive", "none")
	File             string     `json:"file,omitempty"`   // File path if this was a file-specific submission (only comments for this file were submitted)
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

// FormatCommentsWithContext formats comments with line context as JSON.
func (f *JSONFormatter) FormatCommentsWithContext(comments []CommentWithLineContext) (string, error) {
	jsonData, err := json.MarshalIndent(comments, "", "  ")
	if err != nil {
		return "", fmt.Errorf("failed to marshal comments with context: %w", err)
	}
	return string(jsonData), nil
}

// FormatSingleComment formats a single comment as JSON.
func (f *JSONFormatter) FormatSingleComment(comment Comment) (string, error) {
	jsonData, err := json.MarshalIndent(comment, "", "  ")
	if err != nil {
		return "", fmt.Errorf("failed to marshal comment: %w", err)
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
		return fmt.Sprintf("Submitted review for PR %s#%d with %d comments from file %s",
			result.Repository, result.PRNumber, result.Comments, result.File), nil
	}
	return fmt.Sprintf("Submitted review for PR %s#%d with %d comments",
		result.Repository, result.PRNumber, result.Comments), nil
}

// FormatComments formats comments as a table.
func (f *TextFormatter) FormatComments(comments []Comment) (string, error) {
	if len(comments) == 0 {
		return "No comments found\n\nTotal: 0 items", nil
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

	// Reserve space for separators and padding (6 separators * 3 chars each = 18)
	reservedWidth := idWidth + lineWidth + sideWidth + createdWidth + 18

	// Remaining width for file and comment columns
	remainingWidth := termWidth - reservedWidth
	if remainingWidth < 40 {
		remainingWidth = 40 // Minimum usable width
	}

	// Allocate remaining width: prioritize comment over file
	fileWidth := minInt(50, remainingWidth/3)  // File gets 1/3 or max 50
	commentWidth := remainingWidth - fileWidth // Comment gets the rest

	var result strings.Builder

	// Print header
	fmt.Fprintf(&result, "%-*s | %-*s | %-*s | %-*s | %-*s | %-*s\n",
		idWidth, "ID",
		fileWidth, "File",
		lineWidth, "Line",
		sideWidth, "Side",
		commentWidth, "Comment",
		createdWidth, "Created")
	result.WriteString(strings.Repeat("-", termWidth))
	result.WriteString("\n")

	for _, comment := range comments {
		// Wrap long content into multiple lines
		fileLines := wrapText(comment.Path, fileWidth)
		commentLines := wrapText(comment.Body, commentWidth)

		// Determine max number of lines needed for this row
		maxLines := maxInt(len(fileLines), len(commentLines))

		// Print each line of the multiline row
		for i := 0; i < maxLines; i++ {
			fileLine := ""
			if i < len(fileLines) {
				fileLine = fileLines[i]
			}

			commentLine := ""
			if i < len(commentLines) {
				commentLine = commentLines[i]
			}

			if i == 0 {
				// First line includes all fields
				fmt.Fprintf(&result, "%-*s | %-*s | %-*s | %-*s | %-*s | %-*s\n",
					idWidth, comment.FormatIDShort(),
					fileWidth, fileLine,
					lineWidth, comment.Line.String(),
					sideWidth, comment.Side,
					commentWidth, commentLine,
					createdWidth, comment.CreatedAt.Format("2006-01-02 15:04"))
			} else {
				// Continuation lines only show file and comment
				fmt.Fprintf(&result, "%-*s | %-*s | %-*s | %-*s | %-*s | %-*s\n",
					idWidth, "",
					fileWidth, fileLine,
					lineWidth, "",
					sideWidth, "",
					commentWidth, commentLine,
					createdWidth, "")
			}
		}

		// Add separator between rows
		result.WriteString(strings.Repeat("-", termWidth))
		result.WriteString("\n")
	}

	// Add total count
	fmt.Fprintf(&result, "\nTotal: %d items\n", len(comments))

	return result.String(), nil
}

// FormatCommentsWithContext formats comments with line context as a table.
func (f *TextFormatter) FormatCommentsWithContext(comments []CommentWithLineContext) (string, error) {
	if len(comments) == 0 {
		return "No comments found\n\nTotal: 0 items", nil
	}

	var result strings.Builder

	for idx, cwc := range comments {
		if idx > 0 {
			result.WriteString("\n")
		}

		comment := cwc.Comment

		// Print comment header
		result.WriteString(strings.Repeat("=", 80))
		result.WriteString("\n")
		fmt.Fprintf(&result, "Comment ID: %s\n", comment.FormatIDShort())
		fmt.Fprintf(&result, "File: %s\n", comment.Path)

		fmt.Fprintf(&result, "Line: %v | Side: %s\n", comment.Line, comment.Side)
		fmt.Fprintf(&result, "Created: %s\n", comment.CreatedAt.Format("2006-01-02 15:04"))
		result.WriteString(strings.Repeat("-", 80))
		result.WriteString("\n")

		// Print code context if available
		if cwc.Context != nil {
			result.WriteString("Code Context:\n")
			contextStr := FormatLineContext(cwc.Context, comment.Line.EndLine)
			result.WriteString(contextStr)
			result.WriteString(strings.Repeat("-", 80))
			result.WriteString("\n")
		}

		// Print comment body
		fmt.Fprintf(&result, "Comment:\n%s\n", comment.Body)
		result.WriteString(strings.Repeat("=", 80))
		result.WriteString("\n")
	}

	// Add total count
	fmt.Fprintf(&result, "\nTotal: %d items\n", len(comments))

	return result.String(), nil
}

// FormatSingleComment formats a single comment in a clean, LLM-friendly format.
func (f *TextFormatter) FormatSingleComment(comment Comment) (string, error) {
	var result strings.Builder

	fmt.Fprintf(&result, "File: %s\n", comment.Path)
	fmt.Fprintf(&result, "Line: %v\n", comment.Line)
	fmt.Fprintf(&result, "Side: %s\n", comment.Side)
	fmt.Fprintf(&result, "ID: %s\n", comment.FormatIDShort())
	fmt.Fprintf(&result, "Created: %s\n", comment.CreatedAt.Format("2006-01-02 15:04"))
	fmt.Fprintf(&result, "\nComment:\n%s\n", comment.Body)

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

// minInt returns the smaller of two integers.
func minInt(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// maxInt returns the larger of two integers.
func maxInt(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// wrapText wraps text to fit within the specified width, breaking at word boundaries when possible.
func wrapText(text string, width int) []string {
	if width <= 0 {
		return []string{text}
	}

	var lines []string
	words := strings.Fields(text)

	if len(words) == 0 {
		return []string{""}
	}

	currentLine := ""
	for _, word := range words {
		// If the word itself is longer than width, we need to break it
		switch {
		case len(word) > width:
			// Add current line if it exists
			if currentLine != "" {
				lines = append(lines, currentLine)
				currentLine = ""
			}

			// Break the long word into chunks
			for len(word) > width {
				lines = append(lines, word[:width])
				word = word[width:]
			}

			// Set remaining part as current line
			if word != "" {
				currentLine = word
			}
		case currentLine == "":
			currentLine = word
		case len(currentLine)+1+len(word) <= width:
			currentLine += " " + word
		default:
			lines = append(lines, currentLine)
			currentLine = word
		}
	}

	if currentLine != "" {
		lines = append(lines, currentLine)
	}

	if len(lines) == 0 {
		return []string{""}
	}

	return lines
}
