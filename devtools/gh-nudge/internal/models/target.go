package models

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
)

// ReviewTarget represents either a PR or a branch for review operations.
// It provides only identity and path information for storage operations.
type ReviewTarget interface {
	// String returns a human-readable representation (e.g., "#123" or "branch:feature/foo")
	String() string

	// BuildPath constructs the storage path for this target relative to the repository.
	// The path is used for storing comments, diff hunks, and metadata.
	BuildPath(repository Repository) string

	// IsPR returns true if this is a PR target, false for branch targets.
	IsPR() bool
}

// PRTarget represents a pull request review target.
type PRTarget struct {
	Number int
}

// String returns a string representation of the PR target.
func (t PRTarget) String() string {
	return fmt.Sprintf("#%d", t.Number)
}

// BuildPath constructs the storage path for a pull request.
func (t PRTarget) BuildPath(repo Repository) string {
	return filepath.Join("repos", repo.Owner, repo.Name, "pull", strconv.Itoa(t.Number))
}

// IsPR returns true since this is a PR target.
func (t PRTarget) IsPR() bool {
	return true
}

// BranchTarget represents a branch review target.
type BranchTarget struct {
	Name string
}

// String returns a string representation of the branch target.
func (t BranchTarget) String() string {
	return fmt.Sprintf("branch:%s", t.Name)
}

// BuildPath constructs the storage path for a branch.
// Branch names with slashes are sanitized by replacing "/" with "_".
func (t BranchTarget) BuildPath(repo Repository) string {
	sanitizedBranch := strings.ReplaceAll(t.Name, "/", "_")
	return filepath.Join("repos", repo.Owner, repo.Name, "branch", sanitizedBranch)
}

// IsPR returns false since this is a branch target.
func (t BranchTarget) IsPR() bool {
	return false
}

// NewReviewTarget creates a ReviewTarget from a ParsedIdentifier.
// This is the factory function that converts the parsed identifier into the appropriate target type.
func NewReviewTarget(parsed *ParsedIdentifier) ReviewTarget {
	if parsed.IsPR() {
		return PRTarget{Number: parsed.PRNumber}
	}
	return BranchTarget{Name: parsed.BranchName}
}

// NewPRTarget creates a new PR review target.
func NewPRTarget(prNumber int) PRTarget {
	return PRTarget{Number: prNumber}
}

// NewBranchTarget creates a new branch review target.
func NewBranchTarget(branchName string) BranchTarget {
	return BranchTarget{Name: branchName}
}
