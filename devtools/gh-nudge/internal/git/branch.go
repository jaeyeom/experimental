package git

import (
	"fmt"
	"regexp"
	"strings"
)

// Branch represents a validated git branch name.
// Once created, a Branch is guaranteed to have a valid name that is safe
// to use in git commands without risk of command injection.
type Branch struct {
	name string
}

// NewBranch creates a new Branch instance after validating the branch name.
// Returns an error if the branch name is invalid or potentially unsafe.
func NewBranch(name string) (Branch, error) {
	if err := validateBranchName(name); err != nil {
		return Branch{}, err
	}
	return Branch{name: name}, nil
}

// String returns the branch name as a string.
func (b Branch) String() string {
	return b.name
}

// validBranchNamePattern defines what constitutes a valid git branch name.
// This follows git's branch naming rules to prevent command injection.
var validBranchNamePattern = regexp.MustCompile(`^[a-zA-Z0-9][a-zA-Z0-9._/-]*[a-zA-Z0-9]$|^[a-zA-Z0-9]$`)

// validateBranchName ensures a branch name is safe to use in git commands.
func validateBranchName(branchName string) error {
	if branchName == "" {
		return fmt.Errorf("branch name cannot be empty")
	}

	// Check for dangerous characters and patterns
	if strings.Contains(branchName, "..") {
		return fmt.Errorf("branch name cannot contain '..'")
	}
	if strings.HasPrefix(branchName, "-") {
		return fmt.Errorf("branch name cannot start with '-'")
	}
	if strings.HasSuffix(branchName, ".") {
		return fmt.Errorf("branch name cannot end with '.'")
	}
	if strings.Contains(branchName, " ") {
		return fmt.Errorf("branch name cannot contain spaces")
	}
	if strings.Contains(branchName, "\t") || strings.Contains(branchName, "\n") || strings.Contains(branchName, "\r") {
		return fmt.Errorf("branch name cannot contain whitespace characters")
	}

	// Validate against the pattern
	if !validBranchNamePattern.MatchString(branchName) {
		return fmt.Errorf("invalid branch name: %q", branchName)
	}

	return nil
}
