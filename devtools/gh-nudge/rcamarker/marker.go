// Package rcamarker renders and parses the gh-merge rca_marker template.
package rcamarker

import (
	"fmt"
	"regexp"
	"strings"
)

// Render substitutes {event_id} and {class} in marker.
func Render(marker, eventID, class string) string {
	line := strings.ReplaceAll(marker, "{event_id}", eventID)
	return strings.ReplaceAll(line, "{class}", class)
}

// Parse reports the RCA class for eventID in comments.
// our-code wins over flake and infra. Otherwise the last matching class is used.
// matched is false when marker is empty, lacks a placeholder, or no comment matches.
func Parse(marker, eventID string, comments []string) (string, bool, error) {
	if marker == "" || !strings.Contains(marker, "{event_id}") || !strings.Contains(marker, "{class}") {
		return "", false, nil
	}
	pattern := regexp.QuoteMeta(marker)
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{event_id}"), regexp.QuoteMeta(eventID))
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{class}"), `(our-code|flake|infra)`)
	re, err := regexp.Compile(pattern)
	if err != nil {
		return "", false, fmt.Errorf("compile rca marker: %w", err)
	}
	found := ""
	for _, comment := range comments {
		match := re.FindStringSubmatch(comment)
		if match == nil {
			continue
		}
		if match[1] == "our-code" {
			return "our-code", true, nil
		}
		found = match[1]
	}
	if found == "" {
		return "", false, nil
	}
	return found, true, nil
}
