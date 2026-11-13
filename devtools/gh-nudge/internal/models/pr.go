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
	// State is a commit status context result: SUCCESS, FAILURE, ERROR,
	// PENDING, or EXPECTED. A non-empty State means this entry is a status
	// context. Check runs leave it empty and use Status and Conclusion.
	State string `json:"state,omitempty"`
	// Context is the status context name. It is used when Name is empty.
	Context string `json:"context,omitempty"`
}

// statusContext reports whether this rollup entry is a commit status context.
func (c StatusCheck) statusContext() bool {
	return c.State != ""
}

func (c StatusCheck) failed() bool {
	if c.statusContext() {
		return c.State == "FAILURE" || c.State == "ERROR"
	}
	switch c.Conclusion {
	case "FAILURE", "CANCELLED", "ACTION_REQUIRED":
		return true
	default:
		return false
	}
}

func (c StatusCheck) incomplete() bool {
	if c.statusContext() {
		return c.State == "PENDING" || c.State == "EXPECTED"
	}
	return c.Status != "COMPLETED"
}

// withContextName copies the check and uses Context as the name when Name is empty.
func (c StatusCheck) withContextName() StatusCheck {
	if c.Name == "" && c.statusContext() {
		c.Name = c.Context
	}
	return c
}

// ChecksStatus returns the overall CI check status for the pull request.
// It returns one of ChecksPass, ChecksFail, ChecksPending, or ChecksNone.
func (pr *PullRequest) ChecksStatus() string {
	if len(pr.StatusCheckRollup) == 0 {
		return ChecksNone
	}
	hasPending := false
	for _, check := range pr.StatusCheckRollup {
		if check.failed() {
			return ChecksFail
		}
		if check.incomplete() {
			hasPending = true
		}
	}
	if hasPending {
		return ChecksPending
	}
	return ChecksPass
}

// FailedChecks returns check runs with a failing conclusion and status
// contexts whose state is FAILURE or ERROR.
func (pr *PullRequest) FailedChecks() []StatusCheck {
	var failed []StatusCheck
	for _, check := range pr.StatusCheckRollup {
		if check.failed() {
			failed = append(failed, check.withContextName())
		}
	}
	return failed
}

// PendingChecks returns check runs that have not completed and status
// contexts whose state is PENDING or EXPECTED.
func (pr *PullRequest) PendingChecks() []StatusCheck {
	var pending []StatusCheck
	for _, check := range pr.StatusCheckRollup {
		if check.incomplete() {
			pending = append(pending, check.withContextName())
		}
	}
	return pending
}

// ListCI is the gh-merge list verdict for a required_checks list.
// Status is PASS, FAIL, PENDING, or NONE.
// Failed counts required names that failed or are missing.
// Pending counts required names that are still running.
// Others lists failing rollup entries that are not required, in rollup order.
type ListCI struct {
	Status  string
	Failed  int
	Pending int
	Others  []string
}

// ListCIStatus scores only required check names.
// A rollup entry matches by check-run name, or by context when name is empty.
// An empty required list is NONE. A missing or failed required name is FAIL.
// Pending required names are PENDING when none failed or are missing.
// Every required name that is neither failed nor pending is PASS.
func (pr *PullRequest) ListCIStatus(required []string) ListCI {
	others := otherFailures(pr.StatusCheckRollup, required)
	if len(required) == 0 {
		return ListCI{Status: ChecksNone, Others: others}
	}
	byName := rollupByName(pr.StatusCheckRollup)
	result := ListCI{Status: ChecksPass, Others: others}
	for _, name := range required {
		check, ok := byName[name]
		if !ok || check.failed() {
			result.Failed++
			result.Status = ChecksFail
			continue
		}
		if check.incomplete() {
			result.Pending++
		}
	}
	if result.Status == ChecksFail {
		result.Pending = 0
		return result
	}
	if result.Pending > 0 {
		result.Status = ChecksPending
	}
	return result
}

func rollupByName(checks []StatusCheck) map[string]StatusCheck {
	byName := make(map[string]StatusCheck, len(checks))
	for _, check := range checks {
		name := check.rollupName()
		if name == "" {
			continue
		}
		byName[name] = check
	}
	return byName
}

func otherFailures(checks []StatusCheck, required []string) []string {
	requiredNames := make(map[string]struct{}, len(required))
	for _, name := range required {
		requiredNames[name] = struct{}{}
	}
	var names []string
	for _, check := range checks {
		name := check.rollupName()
		if name == "" || !check.failed() {
			continue
		}
		if _, ok := requiredNames[name]; ok {
			continue
		}
		names = append(names, name)
	}
	return names
}

func (c StatusCheck) rollupName() string {
	if c.Name != "" {
		return c.Name
	}
	return c.Context
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

// LabelMinAge requires a label to have been on the pull request for MinHours
// since it was most recently added.
type LabelMinAge struct {
	Label    string
	MinHours int
}

// MeetsLabelMinAges reports whether every rule is satisfied at now.
// An empty rule list has no age requirement.
// A rule is satisfied when the pull request currently has that label and
// addedAt has an add time at least MinHours before now.
// A missing or zero add time does not satisfy the rule.
func (pr *PullRequest) MeetsLabelMinAges(rules []LabelMinAge, addedAt map[string]time.Time, now time.Time) bool {
	for _, rule := range rules {
		added, ok := addedAt[rule.Label]
		if !pr.HasLabel(rule.Label) || !ok || added.IsZero() {
			return false
		}
		required := time.Duration(rule.MinHours) * time.Hour
		if now.Sub(added) < required {
			return false
		}
	}
	return true
}

// BlocksMerge reports whether any blockLabels entry is on the pull request.
// It returns the first matching label. An empty list does not block.
// Label names match exactly, including case.
func (pr *PullRequest) BlocksMerge(blockLabels []string) (string, bool) {
	for _, name := range blockLabels {
		if pr.HasLabel(name) {
			return name, true
		}
	}
	return "", false
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

// Viewer viewed states from PullRequest.files.nodes.viewerViewedState.
const (
	ViewerViewed    = "VIEWED"
	ViewerUnviewed  = "UNVIEWED"
	ViewerDismissed = "DISMISSED"
)

// File represents a file changed in a pull request.
type File struct {
	Path      string `json:"path"`
	Additions int    `json:"additions"`
	Deletions int    `json:"deletions"`
	// ViewerViewedState is VIEWED, UNVIEWED, or DISMISSED. Empty means
	// the pull request was loaded without that field.
	ViewerViewedState string `json:"viewerViewedState,omitempty"`
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

// MeetsFirstReviewRequestAge reports whether a reviewer was first requested
// at least minHours before now. A non-positive minHours has no requirement.
// A missing or zero request time does not satisfy a positive minimum.
func MeetsFirstReviewRequestAge(requestedAt time.Time, minHours int, now time.Time) bool {
	if minHours <= 0 {
		return true
	}
	if requestedAt.IsZero() {
		return false
	}
	required := time.Duration(minHours) * time.Hour
	return now.Sub(requestedAt) >= required
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
