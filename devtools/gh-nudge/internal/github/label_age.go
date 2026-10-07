package github

import (
	"encoding/json"
	"fmt"
	"net/url"
	"strconv"
	"strings"
	"time"
)

// issueEvent is one GitHub issue event. Pull requests share issue numbers,
// and a "labeled" event records when a label was added.
type issueEvent struct {
	Event     string    `json:"event"`
	CreatedAt time.Time `json:"created_at"` //nolint:tagliatelle // GitHub API uses snake_case
	Label     *struct {
		Name string `json:"name"`
	} `json:"label"`
}

// LatestLabelAddedAt returns the newest time each requested label was added
// to the pull request. Labels with no labeled event are absent from the map.
// prURL is the pull request HTML URL from gh, such as
// https://github.com/owner/repo/pull/123.
func (c *Client) LatestLabelAddedAt(prURL string, labels []string) (map[string]time.Time, error) {
	wanted := make(map[string]struct{}, len(labels))
	for _, label := range labels {
		if label == "" {
			continue
		}
		wanted[label] = struct{}{}
	}
	if len(wanted) == 0 {
		return map[string]time.Time{}, nil
	}

	owner, repo, number, err := parsePullRequestURL(prURL)
	if err != nil {
		return nil, err
	}

	path := fmt.Sprintf("repos/%s/%s/issues/%d/events?per_page=100", owner, repo, number)
	output, err := c.executor.Execute("gh", "api", "--paginate", path)
	if err != nil {
		return nil, fmt.Errorf("failed to list issue events for %s: %w", prURL, err)
	}

	var events []issueEvent
	if err := json.Unmarshal([]byte(output), &events); err != nil {
		return nil, fmt.Errorf("failed to parse issue events for %s: %w", prURL, err)
	}

	added := make(map[string]time.Time)
	for _, event := range events {
		if event.Event != "labeled" || event.Label == nil || event.CreatedAt.IsZero() {
			continue
		}
		name := event.Label.Name
		if _, ok := wanted[name]; !ok {
			continue
		}
		if prev, ok := added[name]; ok && !event.CreatedAt.After(prev) {
			continue
		}
		added[name] = event.CreatedAt
	}
	return added, nil
}

func parsePullRequestURL(prURL string) (owner, repo string, number int, err error) {
	parsed, err := url.Parse(prURL)
	if err != nil {
		return "", "", 0, fmt.Errorf("invalid pull request URL %q: %w", prURL, err)
	}
	if parsed.Host != "github.com" {
		return "", "", 0, fmt.Errorf("invalid pull request URL %q", prURL)
	}
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	if len(parts) < 4 || parts[2] != "pull" {
		return "", "", 0, fmt.Errorf("invalid pull request URL %q", prURL)
	}
	number, err = strconv.Atoi(parts[3])
	if err != nil || number <= 0 {
		return "", "", 0, fmt.Errorf("invalid pull request URL %q", prURL)
	}
	return parts[0], parts[1], number, nil
}
