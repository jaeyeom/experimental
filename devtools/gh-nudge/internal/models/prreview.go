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

// Side represents the side of a diff (LEFT or RIGHT).
type Side string

const (
	SideUnspecified Side = ""      // Unspecified side
	SideLeft        Side = "LEFT"  // Left side of diff (old version)
	SideRight       Side = "RIGHT" // Right side of diff (new version)
)

// String returns the string representation of the side.
func (s Side) String() string {
	return string(s)
}

// ParseSide parses a string into a Side type.
func ParseSide(s string) (Side, error) {
	switch strings.ToUpper(s) {
	case "":
		return SideUnspecified, nil
	case "LEFT":
		return SideLeft, nil
	case "RIGHT":
		return SideRight, nil
	default:
		return SideUnspecified, fmt.Errorf("unknown side: %s (expected LEFT, RIGHT, or empty)", s)
	}
}

// DiffHunk represents a diff hunk in a pull request.
type DiffHunk struct {
	Location FileLocation `json:"location"` // File location (path + line range)
	Side     Side         `json:"side"`     // Side of the diff (LEFT or RIGHT)
	Content  string       `json:"content"`  // The diff content
	SHA      string       `json:"sha"`      // Commit SHA
}

// IsInRange checks if a line number is within the diff hunk.
func (dh DiffHunk) IsInRange(line int) bool {
	return dh.Location.Lines.StartLine <= line && line <= dh.Location.Lines.EndLine
}

// CommentStatus represents the status of a comment.
type CommentStatus string

const (
	StatusUnresolved CommentStatus = "unresolved"
	StatusResolved   CommentStatus = "resolved"
	StatusArchived   CommentStatus = "archived"
)

// CommentPriority represents the priority level of a comment.
type CommentPriority string

const (
	PriorityHigh   CommentPriority = "high"
	PriorityMedium CommentPriority = "medium"
	PriorityLow    CommentPriority = "low"
)

// Comment represents a line-specific comment in a pull request.
type Comment struct {
	ID                string           `json:"id"`                          // Unique comment ID (40-char hex string)
	Path              string           `json:"path"`                        // File path
	Line              LineRange        `json:"line"`                        // Line range for this comment
	Body              string           `json:"body"`                        // Comment text
	Side              Side             `json:"side"`                        // Side of the diff (LEFT or RIGHT)
	SHA               string           `json:"sha"`                         // Commit SHA
	CreatedAt         time.Time        `json:"createdAt"`                   // When comment was created
	OriginalRange     *LineRange       `json:"originalRange,omitempty"`     // Original line range before adjustments
	AdjustmentHistory []LineAdjustment `json:"adjustmentHistory,omitempty"` // History of line adjustments
	Status            CommentStatus    `json:"status,omitempty"`            // Comment status (unresolved, resolved, archived)
	ResolvedAt        *time.Time       `json:"resolvedAt,omitempty"`        // When comment was resolved
	ResolutionReason  string           `json:"resolutionReason,omitempty"`  // Reason for resolution
	Priority          CommentPriority  `json:"priority,omitempty"`          // Priority level
	Source            string           `json:"source,omitempty"`            // "local" or "github"
	GitHubID          *int64           `json:"githubId,omitempty"`          // Original GitHub comment ID
	LastSynced        *time.Time       `json:"lastSynced,omitempty"`        // When last synced with GitHub
	SyncStatus        string           `json:"syncStatus,omitempty"`        // "synced", "modified", "conflict"
}

// PRDiffHunks represents the diff hunks for a pull request.
type PRDiffHunks struct {
	PRNumber    int        `json:"prNumber"`
	Repository  Repository `json:"repository"`
	CapturedAt  time.Time  `json:"capturedAt"`
	DiffHunks   []DiffHunk `json:"diffHunks"`
	CommitSHA   string     `json:"commitSha"`
	BaseSHA     string     `json:"baseSha"`
	Description string     `json:"description,omitempty"`
}

// PRComments represents the comments for a pull request.
type PRComments struct {
	PRNumber   int        `json:"prNumber"`
	Repository Repository `json:"repository"`
	Comments   []Comment  `json:"comments"`
	UpdatedAt  time.Time  `json:"updatedAt"`
}

// PRReview represents a review to be submitted to GitHub.
type PRReview struct {
	Body     string    `json:"body"`
	Event    string    `json:"event,omitempty"` // "COMMENT", "APPROVE", "REQUEST_CHANGES"
	Comments []Comment `json:"comments"`
}

// CommentFilter represents filters for listing comments.
type CommentFilter struct {
	File         string     `json:"file,omitempty"`
	Side         Side       `json:"side,omitempty"`
	LineRange    *LineRange `json:"lineRange,omitempty"`    // For filtering by line range (supports single line or range)
	ShowArchived bool       `json:"showArchived,omitempty"` // Whether to include archived comments
}

// LineRange represents a range of lines.
type LineRange struct {
	StartLine int `json:"startLine"`
	EndLine   int `json:"endLine"`
}

// NewLineRange creates a new LineRange from start and end line numbers.
func NewLineRange(startLine, endLine int) LineRange {
	return LineRange{StartLine: startLine, EndLine: endLine}
}

// NewSingleLine creates a LineRange for a single line.
func NewSingleLine(line int) LineRange {
	return LineRange{StartLine: line, EndLine: line}
}

// String returns a string representation of the line range.
func (lr LineRange) String() string {
	if lr.IsMultiLine() {
		return fmt.Sprintf("%d-%d", lr.StartLine, lr.EndLine)
	}
	return fmt.Sprintf("%d", lr.StartLine)
}

// IsMultiLine checks if the range spans multiple lines.
func (lr LineRange) IsMultiLine() bool {
	return lr.StartLine != lr.EndLine
}

// Overlaps checks if this range overlaps with another range.
func (lr LineRange) Overlaps(other LineRange) bool {
	return lr.StartLine <= other.EndLine && lr.EndLine >= other.StartLine
}

// Union returns a new LineRange that encompasses both this range and another range.
// The resulting range spans from the minimum start line to the maximum end line.
// If the ranges don't overlap, the result will include any lines between them.
//
// Example:
//
//	LineRange{10, 15}.Union(LineRange{12, 20}) -> LineRange{10, 20}  // Overlapping
//	LineRange{10, 15}.Union(LineRange{20, 25}) -> LineRange{10, 25}  // Non-overlapping (includes gap)
func (lr LineRange) Union(other LineRange) LineRange {
	startLine := lr.StartLine
	if other.StartLine < startLine {
		startLine = other.StartLine
	}
	endLine := lr.EndLine
	if other.EndLine > endLine {
		endLine = other.EndLine
	}
	return LineRange{StartLine: startLine, EndLine: endLine}
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

// FileLocation represents a location in a file with line range information.
// This provides a unified way to handle file+line references throughout the codebase.
type FileLocation struct {
	Path  string    `json:"path"`
	Lines LineRange `json:"lines"`
}

// NewFileLocation creates a new FileLocation with the given path and line range.
func NewFileLocation(path string, lines LineRange) FileLocation {
	return FileLocation{Path: path, Lines: lines}
}

// NewFileLocationSingleLine creates a new FileLocation for a single line.
func NewFileLocationSingleLine(path string, line int) FileLocation {
	return FileLocation{Path: path, Lines: NewSingleLine(line)}
}

// Key returns a canonical string representation "path:line" or "path:startLine-endLine".
// This is used for grouping and map keys.
func (fl FileLocation) Key() string {
	return fmt.Sprintf("%s:%s", fl.Path, fl.Lines.String())
}

// Equals checks if two file locations are the same.
func (fl FileLocation) Equals(other FileLocation) bool {
	return fl.Path == other.Path && fl.Lines == other.Lines
}

// String returns a human-readable representation.
func (fl FileLocation) String() string {
	return fl.Key()
}

// ParseFileLocation parses a "path:line" or "path:startLine-endLine" string.
// Returns an error if the format is invalid.
func ParseFileLocation(key string) (FileLocation, error) {
	// Find the last colon to split path from line spec
	lastColon := strings.LastIndex(key, ":")
	if lastColon == -1 {
		return FileLocation{}, fmt.Errorf("invalid file location format: missing colon")
	}

	path := key[:lastColon]
	lineSpec := key[lastColon+1:]

	lines, err := ParseLineSpec(lineSpec)
	if err != nil {
		return FileLocation{}, fmt.Errorf("invalid line specification: %w", err)
	}

	return FileLocation{Path: path, Lines: *lines}, nil
}

// MatchesFilter checks if a comment matches the given filter.
func (c Comment) MatchesFilter(filter CommentFilter) bool {
	if !filter.ShowArchived && c.IsArchived() {
		return false
	}
	if filter.File != "" && c.Path != filter.File {
		return false
	}
	if filter.Side != "" && c.Side != filter.Side {
		return false
	}
	if filter.LineRange != nil {
		if !c.Line.Overlaps(*filter.LineRange) {
			return false
		}
	}
	return true
}

// Apply filters a list of comments and returns only those that match the filter.
func (f CommentFilter) Apply(comments []Comment) []Comment {
	var filtered []Comment
	for _, comment := range comments {
		if comment.MatchesFilter(f) {
			filtered = append(filtered, comment)
		}
	}
	return filtered
}

// IsMultiLine checks if the comment spans multiple lines.
func (c Comment) IsMultiLine() bool {
	return c.Line.IsMultiLine()
}

// GetLineRange returns the line range for the comment.
func (c Comment) GetLineRange() LineRange {
	return c.Line
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
	Execute(storage CommentClearer, repository Repository, prNumber int, file string) error
	// Name returns the name of the action
	Name() string
}

// CommentClearer defines the ability to clear comments.
type CommentClearer interface {
	ClearPRComments(repository Repository, prNumber int) error
	ClearPRCommentsForFile(repository Repository, prNumber int, file string) error
}

// CommentArchiver defines the ability to archive comments.
type CommentArchiver interface {
	CommentClearer
	ArchiveComments(repository Repository, prNumber int, reviewBody, reviewEvent string) (*ArchivedSubmission, error)
	ListArchivedSubmissions(repository Repository, prNumber int) (*ArchiveMetadata, error)
	GetArchivedSubmission(repository Repository, prNumber int, submissionID string) (*ArchivedSubmission, error)
	CleanupOldArchives(repository Repository, prNumber int, olderThan time.Duration) error
}

// ClearAction removes all local comments after successful submission.
type ClearAction struct{}

func (a ClearAction) Execute(storage CommentClearer, repository Repository, prNumber int, file string) error {
	if file != "" {
		// Clear comments for specific file only
		if err := storage.ClearPRCommentsForFile(repository, prNumber, file); err != nil {
			// Don't fail the entire operation if clearing fails - just warn
			slog.Warn("failed to clear local comments for file after submission",
				"owner", repository.Owner,
				"repo", repository.Name,
				"pr", prNumber,
				"file", file,
				"error", err)
			return nil // Return nil to not fail the submission
		}
		slog.Info("cleared local comments for file after successful submission",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber,
			"file", file)
	} else {
		// Clear all comments
		if err := storage.ClearPRComments(repository, prNumber); err != nil {
			// Don't fail the entire operation if clearing fails - just warn
			slog.Warn("failed to clear local comments after submission",
				"owner", repository.Owner,
				"repo", repository.Name,
				"pr", prNumber,
				"error", err)
			return nil // Return nil to not fail the submission
		}
		slog.Info("cleared local comments after successful submission",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber)
	}
	return nil
}

func (a ClearAction) Name() string {
	return "clear"
}

// KeepAction preserves all local comments after successful submission.
type KeepAction struct{}

func (a KeepAction) Execute(_ CommentClearer, repository Repository, prNumber int, file string) error {
	if file != "" {
		slog.Info("local comments for file preserved after submission",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber,
			"file", file)
	} else {
		slog.Info("local comments preserved after submission",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber)
	}
	return nil
}

func (a KeepAction) Name() string {
	return "keep"
}

// ArchiveAction moves comments to an archive/history (future enhancement).
type ArchiveAction struct{}

func (a ArchiveAction) Execute(storage CommentClearer, repository Repository, prNumber int, file string) error {
	// Cast to access archive functionality
	archiveStorage, ok := storage.(CommentArchiver)
	if !ok {
		slog.Warn("storage does not support archiving, preserving comments instead",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber)
		return nil
	}

	if file != "" {
		// File-specific archiving is not supported yet - just preserve
		slog.Info("file-specific archiving not supported yet, comments for file preserved",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber,
			"file", file)
		return nil
	}

	// Archive all comments for the PR
	archivedSubmission, err := archiveStorage.ArchiveComments(repository, prNumber, "", "COMMENT")
	if err != nil {
		slog.Warn("failed to archive comments, preserving instead",
			"owner", repository.Owner,
			"repo", repository.Name,
			"pr", prNumber,
			"error", err)
		return nil // Don't fail the submission if archiving fails
	}

	slog.Info("archived review comments for historical reference",
		"owner", repository.Owner,
		"repo", repository.Name,
		"pr", prNumber,
		"submission_id", archivedSubmission.SubmissionID,
		"comment_count", archivedSubmission.CommentCount)

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

// IsUnresolved returns true if the comment is unresolved.
func (c Comment) IsUnresolved() bool {
	return c.Status == "" || c.Status == StatusUnresolved
}

// IsResolved returns true if the comment is resolved.
func (c Comment) IsResolved() bool {
	return c.Status == StatusResolved
}

// IsArchived returns true if the comment is archived.
func (c Comment) IsArchived() bool {
	return c.Status == StatusArchived
}

// Resolve marks the comment as resolved with an optional reason.
// It sets the status to StatusResolved, records the current time as ResolvedAt,
// and stores the reason if provided.
func (c *Comment) Resolve(reason string) {
	now := time.Now()
	c.Status = StatusResolved
	c.ResolvedAt = &now
	c.ResolutionReason = reason
}

// Archive marks the comment as archived with an optional reason.
// It sets the status to StatusArchived, records the current time as ResolvedAt,
// and stores the reason if provided.
// Archiving is similar to resolving but indicates the comment is stored for historical reference.
func (c *Comment) Archive(reason string) {
	now := time.Now()
	c.Status = StatusArchived
	c.ResolvedAt = &now
	c.ResolutionReason = reason
}

// Reopen marks the comment as unresolved, clearing resolution metadata.
// This is typically used when a previously resolved comment needs to be revisited.
func (c *Comment) Reopen() {
	c.Status = StatusUnresolved
	c.ResolvedAt = nil
	c.ResolutionReason = ""
}

// BranchDiffHunks represents the diff hunks for a local branch.
type BranchDiffHunks struct {
	BranchName  string     `json:"branchName"`
	Repository  Repository `json:"repository"`
	CapturedAt  time.Time  `json:"capturedAt"`
	DiffHunks   []DiffHunk `json:"diffHunks"`
	CommitSHA   string     `json:"commitSha"`
	BaseSHA     string     `json:"baseSha"`
	BaseBranch  string     `json:"baseBranch,omitempty"`
	Description string     `json:"description,omitempty"`
}

// BranchComments represents the comments for a local branch.
type BranchComments struct {
	BranchName string     `json:"branchName"`
	Repository Repository `json:"repository"`
	Comments   []Comment  `json:"comments"`
	UpdatedAt  time.Time  `json:"updatedAt"`
}

// ArchivedSubmission represents a complete archived review submission.
type ArchivedSubmission struct {
	SubmissionID string                 `json:"submissionId"` // Unique identifier
	ArchivedAt   time.Time              `json:"archivedAt"`   // When archived
	SubmittedAt  time.Time              `json:"submittedAt"`  // When originally submitted
	PRNumber     int                    `json:"prNumber"`
	Owner        string                 `json:"owner"`
	Repo         string                 `json:"repo"`
	ReviewBody   string                 `json:"reviewBody"`  // The PR review body
	ReviewEvent  string                 `json:"reviewEvent"` // COMMENT/APPROVE/REQUEST_CHANGES
	Comments     []Comment              `json:"comments"`    // The archived comments
	CommentCount int                    `json:"commentCount"`
	Metadata     map[string]interface{} `json:"metadata,omitempty"`
}

// ArchiveMetadata maintains an index of all archives for a PR.
type ArchiveMetadata struct {
	PRNumber      int                  `json:"prNumber"`
	Owner         string               `json:"owner"`
	Repo          string               `json:"repo"`
	Archives      []ArchivedSubmission `json:"archives"`
	LastUpdated   time.Time            `json:"lastUpdated"`
	TotalArchives int                  `json:"totalArchives"`
}

// MergeStrategy represents different strategies for handling conflicting comments.
type MergeStrategy string

const (
	MergeStrategyOverwrite MergeStrategy = "overwrite"
	MergeStrategyMerge     MergeStrategy = "merge"
	MergeStrategySkip      MergeStrategy = "skip"
)

// PullOptions contains options for the pull command.
type PullOptions struct {
	File          string        `json:"file,omitempty"`
	Author        string        `json:"author,omitempty"`
	MergeStrategy MergeStrategy `json:"mergeStrategy,omitempty"`
	DryRun        bool          `json:"dryRun,omitempty"`
}

// MergeResult contains the result of merging GitHub comments with local comments.
type MergeResult struct {
	PulledComments     []Comment `json:"pulledComments"`
	ConflictedComments []Comment `json:"conflictedComments"`
	SkippedComments    []Comment `json:"skippedComments"`
	TotalProcessed     int       `json:"totalProcessed"`
}

// ReviewComments is a unified container for comments that works with any ReviewTarget.
// This replaces the separate PRComments and BranchComments types for new code.
type ReviewComments struct {
	Target     string     `json:"target"` // String representation of target (from ReviewTarget.String())
	Repository Repository `json:"repository"`
	Comments   []Comment  `json:"comments"`
	UpdatedAt  time.Time  `json:"updatedAt"`
}

// ReviewDiffHunks is a unified container for diff hunks that works with any ReviewTarget.
// This replaces the separate PRDiffHunks and BranchDiffHunks types for new code.
type ReviewDiffHunks struct {
	Target      string     `json:"target"` // String representation of target (from ReviewTarget.String())
	Repository  Repository `json:"repository"`
	CapturedAt  time.Time  `json:"capturedAt"`
	DiffHunks   []DiffHunk `json:"diffHunks"`
	CommitSHA   string     `json:"commitSha"`
	BaseSHA     string     `json:"baseSha"`
	BaseBranch  string     `json:"baseBranch,omitempty"` // For branches
	Description string     `json:"description,omitempty"`
}
