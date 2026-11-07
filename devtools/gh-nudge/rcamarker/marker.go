// Package rcamarker renders and parses the gh-merge rca_marker template.
package rcamarker

import (
	"fmt"
	"regexp"
	"strings"
)

const rcaClassGroup = `(our-code|flake|infra)`

// Render substitutes {event_id} and {class} in marker.
func Render(marker, eventID, class string) string {
	line := strings.ReplaceAll(marker, "{event_id}", eventID)
	return strings.ReplaceAll(line, "{class}", class)
}

// Parse reports the RCA class for eventID in comments.
// our-code wins over flake and infra. Otherwise the last matching class is used.
// matched is false when marker is empty, lacks a placeholder, or no comment matches.
func Parse(marker, eventID string, comments []string) (string, bool, error) {
	if !markerReady(marker) {
		return "", false, nil
	}
	pattern := regexp.QuoteMeta(marker)
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{event_id}"), regexp.QuoteMeta(eventID))
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{class}"), rcaClassGroup)
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

// Match reports the event id and class when comment matches marker.
// The event id is the text captured for {event_id}. Class is our-code, flake, or infra.
func Match(marker, comment string) (string, string, bool, error) {
	if !markerReady(marker) {
		return "", "", false, nil
	}
	eventAt := strings.Index(marker, "{event_id}")
	classAt := strings.Index(marker, "{class}")
	pattern := regexp.QuoteMeta(marker)
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{event_id}"), `(.+?)`)
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{class}"), rcaClassGroup)
	re, err := regexp.Compile(pattern)
	if err != nil {
		return "", "", false, fmt.Errorf("compile rca marker: %w", err)
	}
	found := re.FindStringSubmatch(comment)
	if found == nil {
		return "", "", false, nil
	}
	eventID, class := found[1], found[2]
	if classAt < eventAt {
		eventID, class = found[2], found[1]
	}
	return eventID, class, true, nil
}

func markerReady(marker string) bool {
	return marker != "" && strings.Contains(marker, "{event_id}") && strings.Contains(marker, "{class}")
}
