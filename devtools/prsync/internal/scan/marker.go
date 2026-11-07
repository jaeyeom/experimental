package scan

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// renderRCAMarker renders the gh-merge rca_marker template.
func renderRCAMarker(marker, eventID, class string) string {
	return rcamarker.Render(marker, eventID, class)
}

// parseRCAMarker parses comments with the gh-merge rca_marker template.
func parseRCAMarker(marker, eventID string, comments []string) (string, bool, error) {
	class, matched, err := rcamarker.Parse(marker, eventID, comments)
	if err != nil {
		return "", false, fmt.Errorf("rca marker: %w", err)
	}
	return class, matched, nil
}

func mergeQueueActions(ctx context.Context, client GH, marker func(string, string) (string, error), owner, repo string, number int) ([]Action, error) {
	if marker == nil {
		return nil, nil
	}
	template, err := marker(owner, repo)
	if err != nil {
		return nil, fmt.Errorf("rca marker: %w", err)
	}
	if template == "" {
		return nil, nil
	}
	evidence, err := client.MarkerComments(ctx, owner, repo, number)
	if err != nil {
		return nil, fmt.Errorf("rca comments: %w", err)
	}
	action, ok, err := newestOurCodeFix(template, evidence)
	if err != nil || !ok {
		return nil, err
	}
	return []Action{action}, nil
}

func newestOurCodeFix(marker string, evidence gh.MarkerComments) (Action, bool, error) {
	var (
		found   bool
		best    gh.MarkerComment
		eventID string
		class   string
	)
	for _, comment := range evidence.Comments {
		id, got, matched, err := rcamarker.Match(marker, comment.Body)
		if err != nil {
			return Action{}, false, fmt.Errorf("match rca marker: %w", err)
		}
		if !matched {
			continue
		}
		if !found || comment.CreatedAt.After(best.CreatedAt) {
			found = true
			best = comment
			eventID = id
			class = got
		}
	}
	if !found || class != "our-code" {
		return Action{}, false, nil
	}
	if evidence.HeadCommittedAt.IsZero() || !evidence.HeadCommittedAt.Before(best.CreatedAt) {
		return Action{}, false, nil
	}
	return Action{
		Name:       ActionMergeQueueFix,
		EventID:    eventID,
		Class:      class,
		CommentURL: best.URL,
	}, true, nil
}
