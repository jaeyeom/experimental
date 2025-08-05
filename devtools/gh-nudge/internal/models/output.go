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
		lineStr := strconv.Itoa(comment.Line)
		if comment.IsMultiLine() {
			lineStr = fmt.Sprintf("%d-%d", *comment.StartLine, comment.Line)
		}

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
					lineWidth, lineStr,
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
