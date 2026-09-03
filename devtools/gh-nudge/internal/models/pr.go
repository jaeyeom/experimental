// Package models contains data structures used throughout the application.
package models

import "time"

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
	LatestReviews     []Review        `json:"latestReviews,omitempty"`
	Mergeable         string          `json:"mergeable,omitempty"`
	HeadRefName       string          `json:"headRefName,omitempty"`
	StatusCheckRollup []StatusCheck   `json:"statusCheckRollup,omitempty"`
	IsDraft           bool            `json:"isDraft,omitempty"`
	Labels            []Label         `json:"labels,omitempty"`
}

// Label represents a GitHub label attached to a pull request.
type Label struct {
	Name string `json:"name"`
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

// HasLabel reports whether the pull request has a label with the given name.
func (pr *PullRequest) HasLabel(name string) bool {
	for _, label := range pr.Labels {
		if label.Name == name {
			return true
		}
	}
	return false
}

// AllowsNudge reports whether a nudge should be sent for this pull request
// given the configured label filters. Empty slices mean no constraint.
// requireLabels must all be present; skipLabels must all be absent.
func (pr *PullRequest) AllowsNudge(requireLabels, skipLabels []string) bool {
	for _, required := range requireLabels {
		if !pr.HasLabel(required) {
			return false
		}
	}
	for _, skip := range skipLabels {
		if pr.HasLabel(skip) {
			return false
		}
	}
	return true
}

// File represents a file changed in a pull request.
type File struct {
	Path      string `json:"path"`
	Additions int    `json:"additions"`
	Deletions int    `json:"deletions"`
}

// Review is a submitted pull request review from GitHub's latestReviews field.
type Review struct {
	Author      ReviewAuthor `json:"author"`
	SubmittedAt time.Time    `json:"submittedAt"`
	State       string       `json:"state"`
}

// ReviewAuthor is the GitHub user who submitted a review.
type ReviewAuthor struct {
	Login string `json:"login"`
}

// LatestReviewSubmittedAt returns the most recent submittedAt for the given
// reviewer login, or the zero time if they have no submitted review.
func (pr *PullRequest) LatestReviewSubmittedAt(login string) time.Time {
	var latest time.Time
	for _, review := range pr.LatestReviews {
		if review.Author.Login != login || review.SubmittedAt.IsZero() {
			continue
		}
		if review.SubmittedAt.After(latest) {
			latest = review.SubmittedAt
		}
	}
	return latest
}

// ReviewRequest represents a user or team requested to review a PR.
type ReviewRequest struct {
	//nolint: tagliatelle
	Type  string `json:"__typename"`      // "User" or "Team"
	Login string `json:"login,omitempty"` // For users
	Name  string `json:"name,omitempty"`  // For teams
	Slug  string `json:"slug,omitempty"`  // For teams
}
