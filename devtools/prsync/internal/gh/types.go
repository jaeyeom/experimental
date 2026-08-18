// Package gh talks to GitHub through the gh CLI.
package gh

import (
	"errors"
	"fmt"
)

// ErrUnauthenticated is returned when `gh auth status` exits non-zero.
var ErrUnauthenticated = errors.New("gh is not authenticated")

// ErrInaccessible is returned only for 404 / not-found / archived repositories.
var ErrInaccessible = errors.New("repository inaccessible")

// ProcError is a non-zero process exit from gh.
type ProcError struct {
	ExitCode int
	Stdout   string
	Stderr   string
}

func (e *ProcError) Error() string {
	if e == nil {
		return ""
	}
	if e.Stderr != "" {
		return e.Stderr
	}
	if e.Stdout != "" {
		return e.Stdout
	}
	return fmt.Sprintf("exit %d", e.ExitCode)
}

// PRListItem is one row from `gh pr list --json`.
type PRListItem struct {
	Number            int            `json:"number"`
	Title             string         `json:"title"`
	URL               string         `json:"url"`
	BaseRefName       string         `json:"baseRefName"`
	HeadRefName       string         `json:"headRefName"`
	HeadRefOid        string         `json:"headRefOid"`
	Mergeable         string         `json:"mergeable"`
	IsDraft           bool           `json:"isDraft"`
	ReviewDecision    string         `json:"reviewDecision"`
	ReviewRequests    []ReviewReq    `json:"reviewRequests"`
	LatestReviews     []LatestReview `json:"latestReviews"`
	StatusCheckRollup []StatusCheck  `json:"statusCheckRollup"`
}

// ReviewReq is a requested reviewer (user or team) from gh JSON.
type ReviewReq struct {
	Type  string `json:"__typename"` //nolint:tagliatelle // gh JSON
	Login string `json:"login,omitempty"`
	Name  string `json:"name,omitempty"`
	Slug  string `json:"slug,omitempty"`
}

// LatestReview is an unused-by-v1 bucket field kept so we do not refetch.
type LatestReview struct {
	Author      ReviewAuthor `json:"author"`
	State       string       `json:"state"`
	SubmittedAt string       `json:"submittedAt"`
}

// ReviewAuthor is the author of a latest review.
type ReviewAuthor struct {
	Login string `json:"login"`
}

// StatusCheck is one statusCheckRollup entry.
type StatusCheck struct {
	Name       string `json:"name"`
	Status     string `json:"status"`
	Conclusion string `json:"conclusion"`
}

// Thread is one review thread after pagination flattening.
type Thread struct {
	ID         string
	IsResolved bool
	Comments   []ThreadComment
}

// ThreadComment is the last comment on a review thread.
type ThreadComment struct {
	ID     string
	Author *ThreadAuthor
	Path   string
	Line   *int
	URL    string
	Body   string
}

// ThreadAuthor is a GitHub user on a review comment. Nil author means deleted.
type ThreadAuthor struct {
	Login string
}
