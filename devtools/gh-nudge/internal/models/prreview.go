package models

import (
	"fmt"
	"strconv"
	"strings"
	"time"
)

// DiffHunk represents a diff hunk in a pull request.
type DiffHunk struct {
	File      string `json:"file"`
	Side      string `json:"side"`       // "LEFT" or "RIGHT"
	StartLine int    `json:"start_line"` // Starting line number
	EndLine   int    `json:"end_line"`   // Ending line number
	Content   string `json:"content"`    // The diff content
	SHA       string `json:"sha"`        // Commit SHA
}

// Comment represents a line-specific comment in a pull request.
type Comment struct {
	Path      string    `json:"path"`       // File path
	Line      int       `json:"line"`       // Line number (for single line comments)
	StartLine *int      `json:"start_line"` // Starting line for multi-line comments
	Body      string    `json:"body"`       // Comment text
	Side      string    `json:"side"`       // "LEFT" or "RIGHT"
	SHA       string    `json:"sha"`        // Commit SHA
	CreatedAt time.Time `json:"created_at"` // When comment was created
}

// PRDiffHunks represents the diff hunks for a pull request.
type PRDiffHunks struct {
	PRNumber    int        `json:"pr_number"`
	Owner       string     `json:"owner"`
	Repo        string     `json:"repo"`
	CapturedAt  time.Time  `json:"captured_at"`
	DiffHunks   []DiffHunk `json:"diff_hunks"`
	CommitSHA   string     `json:"commit_sha"`
	BaseSHA     string     `json:"base_sha"`
	Description string     `json:"description,omitempty"`
}

// PRComments represents the comments for a pull request.
type PRComments struct {
	PRNumber  int       `json:"pr_number"`
	Owner     string    `json:"owner"`
	Repo      string    `json:"repo"`
	Comments  []Comment `json:"comments"`
	UpdatedAt time.Time `json:"updated_at"`
}

// PRReview represents a review to be submitted to GitHub.
type PRReview struct {
	Body     string    `json:"body"`
	Event    string    `json:"event,omitempty"` // "COMMENT", "APPROVE", "REQUEST_CHANGES"
	Comments []Comment `json:"comments"`
}

// CommentFilter represents filters for listing comments.
type CommentFilter struct {
	File      string `json:"file,omitempty"`
	Line      *int   `json:"line,omitempty"`
	StartLine *int   `json:"start_line,omitempty"`
	EndLine   *int   `json:"end_line,omitempty"`
	Side      string `json:"side,omitempty"`
}

// CommentMatch represents a comment match for deletion.
type CommentMatch struct {
	Index   int     `json:"index"`
	Comment Comment `json:"comment"`
}

// LineRange represents a range of lines.
type LineRange struct {
	StartLine int `json:"start_line"`
	EndLine   int `json:"end_line"`
}

// ParseLineSpec parses a line specification (e.g., "15" or "15-20").
func ParseLineSpec(spec string) (*LineRange, error) {
	if strings.Contains(spec, "-") {
		parts := strings.Split(spec, "-")
		if len(parts) != 2 {
			return nil, fmt.Errorf("invalid range format")
		}

		startLine, err := strconv.Atoi(strings.TrimSpace(parts[0]))
		if err != nil {
			return nil, fmt.Errorf("invalid start line: %w", err)
		}

		endLine, err := strconv.Atoi(strings.TrimSpace(parts[1]))
		if err != nil {
			return nil, fmt.Errorf("invalid end line: %w", err)
		}

		if startLine > endLine {
			return nil, fmt.Errorf("start line cannot be greater than end line")
		}

		return &LineRange{StartLine: startLine, EndLine: endLine}, nil
	}

	line, err := strconv.Atoi(strings.TrimSpace(spec))
	if err != nil {
		return nil, fmt.Errorf("invalid line number: %w", err)
	}

	return &LineRange{StartLine: line, EndLine: line}, nil
}

// IsInRange checks if a line number is within the diff hunks.
func (h DiffHunk) IsInRange(line int) bool {
	return line >= h.StartLine && line <= h.EndLine
}

// MatchesFilter checks if a comment matches the given filter.
func (c Comment) MatchesFilter(filter CommentFilter) bool {
	if filter.File != "" && c.Path != filter.File {
		return false
	}
	if filter.Side != "" && c.Side != filter.Side {
		return false
	}
	if filter.Line != nil && c.Line != *filter.Line {
		return false
	}
	if filter.StartLine != nil && c.StartLine != nil && *c.StartLine != *filter.StartLine {
		return false
	}
	if filter.EndLine != nil && c.StartLine != nil && c.Line != *filter.EndLine {
		return false
	}
	return true
}

// IsMultiLine checks if the comment spans multiple lines.
func (c Comment) IsMultiLine() bool {
	return c.StartLine != nil && *c.StartLine != c.Line
}

// GetLineRange returns the line range for the comment.
func (c Comment) GetLineRange() LineRange {
	if c.IsMultiLine() {
		return LineRange{StartLine: *c.StartLine, EndLine: c.Line}
	}
	return LineRange{StartLine: c.Line, EndLine: c.Line}
}

// IsDuplicate checks if two comments are duplicates.
func (c Comment) IsDuplicate(other Comment) bool {
	// Normalize body for comparison (trim whitespace, etc.)
	return c.Path == other.Path &&
		c.Line == other.Line &&
		c.Side == other.Side &&
		c.Body == other.Body
}
