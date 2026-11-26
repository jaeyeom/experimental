package source

import (
	"strings"
	"time"
)

// LabelNotBefore reads the deferral date from labels named prefix +
// YYYY-MM-DD, such as not-before:2026-12-01. It returns the latest valid
// date, or nil when none, and every label with the prefix whose date does
// not parse. An empty prefix reads nothing.
func LabelNotBefore(labels []string, prefix string) (*time.Time, []string) {
	if prefix == "" {
		return nil, nil
	}
	var latest *time.Time
	var bad []string
	for _, label := range labels {
		value, ok := strings.CutPrefix(label, prefix)
		if !ok {
			continue
		}
		parsed, err := time.Parse(time.DateOnly, strings.TrimSpace(value))
		if err != nil {
			bad = append(bad, label)
			continue
		}
		date := time.Date(parsed.Year(), parsed.Month(), parsed.Day(), 0, 0, 0, 0, time.UTC)
		if latest == nil || date.After(*latest) {
			latest = &date
		}
	}
	return latest, bad
}
