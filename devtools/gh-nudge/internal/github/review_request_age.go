package github

import "time"

// EarliestReviewRequestedAt returns the earliest time each user was requested
// to review the pull request. A later request does not replace an earlier one.
// Users with no review_requested event are absent from the map.
// prURL is the pull request HTML URL from gh, such as
// https://github.com/owner/repo/pull/123.
func (c *Client) EarliestReviewRequestedAt(prURL string) (map[string]time.Time, error) {
	events, err := c.listIssueEvents(prURL)
	if err != nil {
		return nil, err
	}

	requested := make(map[string]time.Time)
	for _, event := range events {
		if event.Event != "review_requested" || event.RequestedReviewer == nil || event.CreatedAt.IsZero() {
			continue
		}
		login := event.RequestedReviewer.Login
		if login == "" {
			continue
		}
		if prev, ok := requested[login]; ok && !event.CreatedAt.Before(prev) {
			continue
		}
		requested[login] = event.CreatedAt
	}
	return requested, nil
}
