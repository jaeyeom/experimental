package gh

import (
	"context"
	"encoding/json"
	"fmt"
)

// Repository visibilities, from `gh repo view --json visibility`.
const (
	VisibilityPublic   = "PUBLIC"
	VisibilityPrivate  = "PRIVATE"
	VisibilityInternal = "INTERNAL"
)

// Visibility returns repo's visibility: PUBLIC, PRIVATE, or INTERNAL.
func (c *Client) Visibility(ctx context.Context, repo string) (string, error) {
	if !validRepo(repo) {
		return "", fmt.Errorf("gh repo view: expected repository owner/repo, found %q", repo)
	}
	result, err := c.execute(ctx, "repo", "view", repo, "--json", "visibility")
	if err != nil {
		return "", err
	}
	if result.ExitCode != 0 {
		return "", ghFailure(result)
	}
	var out struct {
		Visibility *string `json:"visibility"`
	}
	if err := json.Unmarshal([]byte(result.Output), &out); err != nil || out.Visibility == nil {
		return "", fmt.Errorf("gh repo view: expected {\"visibility\": ...}, found %s", quoteSnippet([]byte(result.Output)))
	}
	switch *out.Visibility {
	case VisibilityPublic, VisibilityPrivate, VisibilityInternal:
		return *out.Visibility, nil
	default:
		return "", fmt.Errorf("gh repo view: expected visibility PUBLIC, PRIVATE, or INTERNAL, found %q", *out.Visibility)
	}
}
