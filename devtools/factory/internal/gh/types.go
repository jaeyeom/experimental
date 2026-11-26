// Package gh finds the current user's pull requests through the gh CLI.
// Commands run through an injected go-cmdexec Executor.
package gh

import "time"

// PR is one pull request returned by a gh search.
type PR struct {
	Repo          string    // Repository, as owner/repo.
	Number        int       // Pull request number.
	Title         string    // Pull request title.
	URL           string    // Pull request URL.
	Draft         bool      // True when the pull request is a draft.
	Merged        bool      // True when the pull request is merged.
	MergedAt      time.Time // Merge time. Zero when the pull request is not merged.
	CreatedAt     time.Time // Open time. Required in every search result.
	ClosedAt      time.Time // Close time, merged or not. Zero while open.
	Head          string    // Head branch name.
	ClosingIssues []string  // Issues this pull request closes, as owner/repo#N.
}

// PRView is one pull request from `gh pr view`.
type PRView struct {
	Repo      string
	Number    int
	Title     string
	Body      string
	URL       string
	Draft     bool
	Base      string
	Labels    []string
	Reviewers []string
}
