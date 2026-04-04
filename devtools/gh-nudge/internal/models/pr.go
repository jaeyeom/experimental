// Package models contains data structures used throughout the application.
package models

// Constants for CI check status classification.
const (
	ChecksPass    = "PASS"
	ChecksFail    = "FAIL"
	ChecksPending = "PENDING"
	ChecksNone    = "NONE"
)

// PullRequest represents a GitHub pull request with review information.
type PullRequest struct {
	Title             string          `json:"title"`
	URL               string          `json:"url"`
	Files             []File          `json:"files"`
	ReviewRequests    []ReviewRequest `json:"reviewRequests"`
	Mergeable         string          `json:"mergeable,omitempty"`
	HeadRefName       string          `json:"headRefName,omitempty"`
	StatusCheckRollup []StatusCheck   `json:"statusCheckRollup,omitempty"`
}

// StatusCheck represents a single CI check or status from GitHub's statusCheckRollup.
type StatusCheck struct {
	Name        string `json:"name"`
	Status      string `json:"status"`     // e.g., "COMPLETED", "IN_PROGRESS", "QUEUED"
	Conclusion  string `json:"conclusion"` // e.g., "SUCCESS", "FAILURE", "NEUTRAL", "SKIPPED"
	StartedAt   string `json:"startedAt,omitempty"`
	CompletedAt string `json:"completedAt,omitempty"`
}

// ChecksStatus returns the overall CI check status for the pull request.
// It returns one of ChecksPass, ChecksFail, ChecksPending, or ChecksNone.
func (pr *PullRequest) ChecksStatus() string {
	if len(pr.StatusCheckRollup) == 0 {
		return ChecksNone
	}
	hasPending := false
	for _, check := range pr.StatusCheckRollup {
		switch check.Conclusion {
		case "FAILURE", "CANCELLED", "ACTION_REQUIRED":
			return ChecksFail
		}
		if check.Status != "COMPLETED" {
			hasPending = true
		}
	}
	if hasPending {
		return ChecksPending
	}
	return ChecksPass
}

// FailedChecks returns checks with a failing conclusion.
func (pr *PullRequest) FailedChecks() []StatusCheck {
	var failed []StatusCheck
	for _, check := range pr.StatusCheckRollup {
		switch check.Conclusion {
		case "FAILURE", "CANCELLED", "ACTION_REQUIRED":
			failed = append(failed, check)
		}
	}
	return failed
}

// PendingChecks returns checks that have not completed yet.
func (pr *PullRequest) PendingChecks() []StatusCheck {
	var pending []StatusCheck
	for _, check := range pr.StatusCheckRollup {
		if check.Status != "COMPLETED" {
			pending = append(pending, check)
		}
	}
	return pending
}

// File represents a file changed in a pull request.
type File struct {
	Path      string `json:"path"`
	Additions int    `json:"additions"`
	Deletions int    `json:"deletions"`
}

// ReviewRequest represents a user or team requested to review a PR.
type ReviewRequest struct {
	//nolint: tagliatelle
	Type  string `json:"__typename"`      // "User" or "Team"
	Login string `json:"login,omitempty"` // For users
	Name  string `json:"name,omitempty"`  // For teams
	Slug  string `json:"slug,omitempty"`  // For teams
}
