// Package models contains data structures used throughout the application.
package models

// PullRequest represents a GitHub pull request with review information.
type PullRequest struct {
	Title          string          `json:"title"`
	URL            string          `json:"url"`
	Files          []File          `json:"files"`
	ReviewRequests []ReviewRequest `json:"reviewRequests"`
	Mergeable      string          `json:"mergeable,omitempty"`
	HeadRefName    string          `json:"headRefName,omitempty"`
}

// File represents a file changed in a pull request.
type File struct {
	Path      string `json:"path"`
	Additions int    `json:"additions"`
	Deletions int    `json:"deletions"`
}

// ReviewRequest represents a user or team requested to review a PR.
type ReviewRequest struct {
	Type  string `json:"__typename"`      // "User" or "Team"
	Login string `json:"login,omitempty"` // For users
	Name  string `json:"name,omitempty"`  // For teams
	Slug  string `json:"slug,omitempty"`  // For teams
}
