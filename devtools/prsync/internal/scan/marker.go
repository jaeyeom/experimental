package scan

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// genericFailureMarkerRE matches machine-readable HTML comment failure markers
// such as:
//
//	<!-- prsync-failure kind=our-code head=abc123 queue_run=run-42 -->
//	<!-- merge-failure kind=infra-flake head_sha=abc123 run_id=run-42 -->
var genericFailureMarkerRE = regexp.MustCompile(`<!--\s*(?:prsync-failure|merge-failure|queue-failure)\s+([^>]+?)\s*-->`)

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
	actions, _, err := mergeQueueSignals(ctx, client, marker, config.Defaults(), owner, repo, number)
	return actions, err
}

func mergeQueueSignals(ctx context.Context, client GH, marker func(string, string) (string, error), cfg config.Config, owner, repo string, number int) ([]Action, *FailureMarker, error) {
	template := cfg.Policy.FailureMarkerTemplate
	if marker != nil {
		t, err := marker(owner, repo)
		if err != nil {
			return nil, nil, fmt.Errorf("rca marker: %w", err)
		}
		if t != "" {
			template = t
		}
	}
	if template == "" && cfg.Policy.QueueFix == config.PolicyOff {
		return nil, nil, nil
	}
	evidence, err := client.MarkerComments(ctx, owner, repo, number)
	if err != nil {
		return nil, nil, fmt.Errorf("rca comments: %w", err)
	}
	var actions []Action
	if template != "" {
		action, ok, err := newestOurCodeFix(template, evidence)
		if err != nil {
			return nil, nil, err
		}
		if ok {
			actions = []Action{action}
		}
	}
	fm, err := ParseNewestFailureMarker(evidence.Comments, template)
	if err != nil {
		return nil, nil, err
	}
	return actions, fm, nil
}

// ParseNewestFailureMarker inspects PR comments for machine-readable merge-tool
// failure markers (generic HTML comment markers and optional gh-merge
// rca_marker templates) and returns the newest one by CreatedAt.
func ParseNewestFailureMarker(comments []gh.MarkerComment, rcaTemplate string) (*FailureMarker, error) {
	var best *FailureMarker
	for _, c := range comments {
		m, ok, err := parseFailureMarkerFromComment(c, rcaTemplate)
		if err != nil {
			return nil, err
		}
		if !ok {
			continue
		}
		if best == nil || m.CreatedAt.After(best.CreatedAt) {
			hit := m
			best = &hit
		}
	}
	return best, nil
}

func parseFailureMarkerFromComment(c gh.MarkerComment, rcaTemplate string) (FailureMarker, bool, error) {
	if m, ok := parseGenericFailureMarker(c); ok {
		return m, true, nil
	}
	if rcaTemplate != "" {
		rm, matched, err := rcamarker.Match(rcaTemplate, c.Body)
		if err != nil {
			return FailureMarker{}, false, fmt.Errorf("match rca marker: %w", err)
		}
		if matched {
			return FailureMarker{
				Kind:       mapRCAClass(rm.Class),
				HeadSHA:    rm.Keys[rcamarker.KeyCommit],
				QueueRunID: rm.EventID,
				CommentURL: c.URL,
				CreatedAt:  c.CreatedAt,
			}, true, nil
		}
	}
	return FailureMarker{}, false, nil
}

func parseGenericFailureMarker(c gh.MarkerComment) (FailureMarker, bool) {
	sub := genericFailureMarkerRE.FindStringSubmatch(c.Body)
	if len(sub) < 2 {
		return FailureMarker{}, false
	}
	fm := FailureMarker{
		CommentURL: c.URL,
		CreatedAt:  c.CreatedAt,
	}
	for _, tok := range strings.Fields(sub[1]) {
		k, v, ok := strings.Cut(tok, "=")
		if !ok || v == "" {
			continue
		}
		switch strings.ToLower(k) {
		case "kind", "class":
			fm.Kind = normalizeFailureKind(v)
		case "head", "head_sha", "commit":
			fm.HeadSHA = v
		case "queue_run", "queue_run_id", "run_id", "run", "event_id":
			fm.QueueRunID = v
		}
	}
	if fm.Kind == "" {
		return FailureMarker{}, false
	}
	return fm, true
}

func normalizeFailureKind(raw string) string {
	switch strings.ToLower(strings.TrimSpace(raw)) {
	case FailureKindOurCode:
		return FailureKindOurCode
	case FailureKindInfraFlake, "flake", "infra":
		return FailureKindInfraFlake
	default:
		return FailureKindUnknown
	}
}

func mapRCAClass(class string) string {
	switch class {
	case "our-code":
		return FailureKindOurCode
	case "flake", "infra":
		return FailureKindInfraFlake
	default:
		return FailureKindUnknown
	}
}

func newestOurCodeFix(marker string, evidence gh.MarkerComments) (Action, bool, error) {
	var (
		found   bool
		best    gh.MarkerComment
		eventID string
		class   string
	)
	for _, comment := range evidence.Comments {
		m, matched, err := rcamarker.Match(marker, comment.Body)
		if err != nil {
			return Action{}, false, fmt.Errorf("match rca marker: %w", err)
		}
		if !matched {
			continue
		}
		if !found || comment.CreatedAt.After(best.CreatedAt) {
			found = true
			best = comment
			eventID = m.EventID
			class = m.Class
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
