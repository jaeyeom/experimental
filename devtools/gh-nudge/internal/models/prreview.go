package models

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"log/slog"
	"strconv"
	"strings"
	"time"
)

// OperationType represents the type of diff operation as a single character.
type OperationType string

const (
	OperationDelete OperationType = "d" // Delete operation
	OperationInsert OperationType = "a" // Insert (add) operation
	OperationChange OperationType = "c" // Change operation
)

// String returns the string representation of the operation type.
func (op OperationType) String() string {
	return string(op)
}

// ParseOperationType parses a single character into an OperationType.
func ParseOperationType(s string) (OperationType, error) {
	switch s {
	case "d":
		return OperationDelete, nil
	case "a":
		return OperationInsert, nil
	case "c":
		return OperationChange, nil
	default:
		return "", fmt.Errorf("unknown operation type: %s", s)
	}
}

// DiffHunk represents a diff hunk in a pull request.
type DiffHunk struct {
	File      string `json:"file"`
	Side      string `json:"side"`      // "LEFT" or "RIGHT"
	StartLine int    `json:"startLine"` // Starting line number
	EndLine   int    `json:"endLine"`   // Ending line number
	Content   string `json:"content"`   // The diff content
	SHA       string `json:"sha"`       // Commit SHA
}

// LineAdjustment represents a line number adjustment operation.
type LineAdjustment struct {
	Operation   OperationType `json:"operation"`   // Type of operation (delete, insert, change)
	OldStart    int           `json:"oldStart"`    // Start line in original file
	OldEnd      int           `json:"oldEnd"`      // End line in original file
	NewStart    int           `json:"newStart"`    // Start line in new file
	NewEnd      int           `json:"newEnd"`      // End line in new file
	AppliedAt   time.Time     `json:"appliedAt"`   // When adjustment was applied
	Description string        `json:"description"` // Human-readable description
}

// Comment represents a line-specific comment in a pull request.
type Comment struct {
	ID                string           `json:"id"`                          // Unique comment ID (40-char hex string)
	Path              string           `json:"path"`                        // File path
	Line              int              `json:"line"`                        // Line number (for single line comments)
	StartLine         *int             `json:"startLine"`                   // Starting line for multi-line comments
	Body              string           `json:"body"`                        // Comment text
	Side              string           `json:"side"`                        // "LEFT" or "RIGHT"
	SHA               string           `json:"sha"`                         // Commit SHA
	CreatedAt         time.Time        `json:"createdAt"`                   // When comment was created
	OriginalLine      int              `json:"originalLine,omitempty"`      // Original line number before adjustments
	OriginalStartLine *int             `json:"originalStartLine,omitempty"` // Original start line for multi-line comments
	AdjustmentHistory []LineAdjustment `json:"adjustmentHistory,omitempty"` // History of line adjustments
}

// PRDiffHunks represents the diff hunks for a pull request.
type PRDiffHunks struct {
	PRNumber    int        `json:"prNumber"`
	Owner       string     `json:"owner"`
	Repo        string     `json:"repo"`
	CapturedAt  time.Time  `json:"capturedAt"`
	DiffHunks   []DiffHunk `json:"diffHunks"`
	CommitSHA   string     `json:"commitSha"`
	BaseSHA     string     `json:"baseSha"`
	Description string     `json:"description,omitempty"`
}

// PRComments represents the comments for a pull request.
type PRComments struct {
	PRNumber  int       `json:"prNumber"`
	Owner     string    `json:"owner"`
	Repo      string    `json:"repo"`
	Comments  []Comment `json:"comments"`
	UpdatedAt time.Time `json:"updatedAt"`
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
	StartLine *int   `json:"startLine,omitempty"`
	EndLine   *int   `json:"endLine,omitempty"`
	Side      string `json:"side,omitempty"`
}

// LineRange represents a range of lines.
type LineRange struct {
	StartLine int `json:"startLine"`
	EndLine   int `json:"endLine"`
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

// Executor defines a generic action that can be executed with context.
type Executor interface {
	// Execute performs the action
	Execute(storage CommentClearer, owner, repo string, prNumber int) error
	// Name returns the name of the action
	Name() string
}

// CommentClearer defines the ability to clear comments.
type CommentClearer interface {
	ClearComments(owner, repo string, prNumber int) error
}

// ClearAction removes all local comments after successful submission.
type ClearAction struct{}

func (a ClearAction) Execute(storage CommentClearer, owner, repo string, prNumber int) error {
	if err := storage.ClearComments(owner, repo, prNumber); err != nil {
		// Don't fail the entire operation if clearing fails - just warn
		slog.Warn("failed to clear local comments after submission",
			"owner", owner,
			"repo", repo,
			"pr", prNumber,
			"error", err)
		return nil // Return nil to not fail the submission
	}
	slog.Info("cleared local comments after successful submission",
		"owner", owner,
		"repo", repo,
		"pr", prNumber)
	return nil
}

func (a ClearAction) Name() string {
	return "clear"
}

// KeepAction preserves all local comments after successful submission.
type KeepAction struct{}

func (a KeepAction) Execute(_ CommentClearer, owner, repo string, prNumber int) error {
	slog.Info("local comments preserved after submission",
		"owner", owner,
		"repo", repo,
		"pr", prNumber)
	return nil
}

func (a KeepAction) Name() string {
	return "keep"
}

// ArchiveAction moves comments to an archive/history (future enhancement).
type ArchiveAction struct{}

func (a ArchiveAction) Execute(_ CommentClearer, owner, repo string, prNumber int) error {
	// Future enhancement: implement archiving
	slog.Info("archive feature not yet implemented, comments preserved",
		"owner", owner,
		"repo", repo,
		"pr", prNumber)
	return nil
}

func (a ArchiveAction) Name() string {
	return "archive"
}

// CreatePostSubmitExecutor creates a post-submit action Executor from a string.
func CreatePostSubmitExecutor(s string) (Executor, error) {
	switch strings.ToLower(s) {
	case "clear", "":
		return ClearAction{}, nil
	case "keep":
		return KeepAction{}, nil
	case "archive":
		return ArchiveAction{}, nil
	default:
		return nil, fmt.Errorf("invalid post-submit action: %q (valid: clear, keep, archive)", s)
	}
}

// IdentifierType represents the type of identifier (PR or branch).
type IdentifierType int

const (
	IdentifierPR IdentifierType = iota
	IdentifierBranch
)

// ParsedIdentifier represents a parsed identifier that can be either a PR number or branch name.
type ParsedIdentifier struct {
	Type       IdentifierType
	PRNumber   int
	BranchName string
}

// ParseIdentifier parses an identifier string to determine if it's a PR number or branch name.
// Pure numeric strings are treated as PR numbers, all others as branch names.
func ParseIdentifier(identifier string) (*ParsedIdentifier, error) {
	if identifier == "" {
		return nil, fmt.Errorf("identifier cannot be empty")
	}

	// Try to parse as integer (PR number)
	if num, err := strconv.Atoi(identifier); err == nil {
		if num <= 0 {
			return nil, fmt.Errorf("PR number must be positive, got: %d", num)
		}
		return &ParsedIdentifier{
			Type:     IdentifierPR,
			PRNumber: num,
		}, nil
	}

	// Otherwise treat as branch name
	return &ParsedIdentifier{
		Type:       IdentifierBranch,
		BranchName: identifier,
	}, nil
}

// IsPR returns true if the identifier represents a PR.
func (p *ParsedIdentifier) IsPR() bool {
	return p.Type == IdentifierPR
}

// IsBranch returns true if the identifier represents a branch.
func (p *ParsedIdentifier) IsBranch() bool {
	return p.Type == IdentifierBranch
}

// String returns a string representation of the identifier.
func (p *ParsedIdentifier) String() string {
	if p.IsPR() {
		return strconv.Itoa(p.PRNumber)
	}
	return p.BranchName
}

// DiffSpec generates a diff specification string from the adjustment fields.
// Returns format like "15,17d14" for deletions, "14a15,17" for insertions, "15,17c14,16" for changes.
func (adj LineAdjustment) DiffSpec() string {
	// Validate operation type
	switch adj.Operation {
	case OperationDelete, OperationInsert, OperationChange:
		// Valid operations, continue
	default:
		return ""
	}

	// Format old range
	oldRange := ""
	if adj.OldStart == adj.OldEnd {
		oldRange = fmt.Sprintf("%d", adj.OldStart)
	} else {
		oldRange = fmt.Sprintf("%d,%d", adj.OldStart, adj.OldEnd)
	}

	// Format new range
	newRange := ""
	if adj.NewStart == adj.NewEnd {
		newRange = fmt.Sprintf("%d", adj.NewStart)
	} else {
		newRange = fmt.Sprintf("%d,%d", adj.NewStart, adj.NewEnd)
	}

	// Combine with operation character
	return fmt.Sprintf("%s%s%s", oldRange, adj.Operation, newRange)
}

// GenerateCommentID generates a random 40-character hex string for comment IDs.
// This is similar to git commit hashes but uses pure randomness.
func GenerateCommentID() string {
	randomBytes := make([]byte, 20) // 20 bytes = 40 hex chars
	if _, err := rand.Read(randomBytes); err != nil {
		// This should never happen with crypto/rand
		panic(fmt.Sprintf("failed to generate random bytes: %v", err))
	}
	return hex.EncodeToString(randomBytes)
}

// MatchesIDPrefix checks if the comment ID matches the given prefix.
func (c Comment) MatchesIDPrefix(prefix string) bool {
	if prefix == "" {
		return false
	}
	return strings.HasPrefix(c.ID, prefix)
}

// FormatIDShort returns a shortened version of the comment ID (first 8 characters).
func (c Comment) FormatIDShort() string {
	if len(c.ID) >= 8 {
		return c.ID[:8]
	}
	return c.ID
}

// BranchDiffHunks represents the diff hunks for a local branch.
type BranchDiffHunks struct {
	BranchName  string     `json:"branchName"`
	Owner       string     `json:"owner"`
	Repo        string     `json:"repo"`
	CapturedAt  time.Time  `json:"capturedAt"`
	DiffHunks   []DiffHunk `json:"diffHunks"`
	CommitSHA   string     `json:"commitSha"`
	BaseSHA     string     `json:"baseSha"`
	BaseBranch  string     `json:"baseBranch,omitempty"`
	Description string     `json:"description,omitempty"`
}

// BranchComments represents the comments for a local branch.
type BranchComments struct {
	BranchName string    `json:"branchName"`
	Owner      string    `json:"owner"`
	Repo       string    `json:"repo"`
	Comments   []Comment `json:"comments"`
	UpdatedAt  time.Time `json:"updatedAt"`
}
