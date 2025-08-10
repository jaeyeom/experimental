package models

import (
	"fmt"
	"strings"
)

// ConfidenceLevel represents the confidence level of auto-detection suggestions.
type ConfidenceLevel string

const (
	ConfidenceLow    ConfidenceLevel = "low"
	ConfidenceMedium ConfidenceLevel = "medium"
	ConfidenceHigh   ConfidenceLevel = "high"
)

// LineChangeType represents the type of line change detected.
type LineChangeType string

const (
	LineAdded   LineChangeType = "added"
	LineDeleted LineChangeType = "deleted"
	LineChanged LineChangeType = "changed"
)

// LineChange represents a detected change in a file.
type LineChange struct {
	Type         LineChangeType `json:"type"`
	OriginalLine int            `json:"originalLine"`
	NewLine      int            `json:"newLine"`
	Content      string         `json:"content,omitempty"`
}

// MappingSuggestion represents an automatic mapping suggestion.
type MappingSuggestion struct {
	OriginalLine int             `json:"originalLine"`
	Offset       int             `json:"offset"`
	Confidence   ConfidenceLevel `json:"confidence"`
	Reason       string          `json:"reason"`
}

// AutoDetectResult contains the results of automatic change detection.
type AutoDetectResult struct {
	File        string              `json:"file"`
	Changes     []LineChange        `json:"changes"`
	Suggestions []MappingSuggestion `json:"suggestions"`
	Confidence  ConfidenceLevel     `json:"confidence"`
}

// ConvertToSimpleMappingSpec converts auto-detection suggestions to a simple mapping diff spec.
func (r *AutoDetectResult) ConvertToSimpleMappingSpec() string {
	if len(r.Suggestions) == 0 {
		return ""
	}

	var parts []string
	for _, suggestion := range r.Suggestions {
		if suggestion.Offset >= 0 {
			parts = append(parts, fmt.Sprintf("%d:+%d", suggestion.OriginalLine, suggestion.Offset))
		} else {
			parts = append(parts, fmt.Sprintf("%d:%d", suggestion.OriginalLine, suggestion.Offset))
		}
	}
	return strings.Join(parts, ";")
}

// GetHighConfidenceSuggestions returns only high confidence suggestions.
func (r *AutoDetectResult) GetHighConfidenceSuggestions() []MappingSuggestion {
	var highConf []MappingSuggestion
	for _, suggestion := range r.Suggestions {
		if suggestion.Confidence == ConfidenceHigh {
			highConf = append(highConf, suggestion)
		}
	}
	return highConf
}
