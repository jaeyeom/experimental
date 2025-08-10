package models

import (
	"testing"
)

func TestConvertToSimpleMappingSpec(t *testing.T) {
	tests := []struct {
		name     string
		result   AutoDetectResult
		expected string
	}{
		{
			name: "empty suggestions",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{},
			},
			expected: "",
		},
		{
			name: "single positive offset",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 15, Offset: 2},
				},
			},
			expected: "15:+2",
		},
		{
			name: "single negative offset",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 30, Offset: -3},
				},
			},
			expected: "30:-3",
		},
		{
			name: "multiple suggestions",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 15, Offset: 2},
					{OriginalLine: 30, Offset: -1},
					{OriginalLine: 45, Offset: 1},
				},
			},
			expected: "15:+2;30:-1;45:+1",
		},
		{
			name: "zero offset",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 20, Offset: 0},
				},
			},
			expected: "20:+0",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.result.ConvertToSimpleMappingSpec()
			if result != tt.expected {
				t.Errorf("ConvertToSimpleMappingSpec() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestGetHighConfidenceSuggestions(t *testing.T) {
	tests := []struct {
		name     string
		result   AutoDetectResult
		expected int
	}{
		{
			name: "no suggestions",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{},
			},
			expected: 0,
		},
		{
			name: "all high confidence",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 10, Confidence: ConfidenceHigh},
					{OriginalLine: 20, Confidence: ConfidenceHigh},
				},
			},
			expected: 2,
		},
		{
			name: "mixed confidence levels",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 10, Confidence: ConfidenceHigh},
					{OriginalLine: 20, Confidence: ConfidenceMedium},
					{OriginalLine: 30, Confidence: ConfidenceLow},
					{OriginalLine: 40, Confidence: ConfidenceHigh},
				},
			},
			expected: 2,
		},
		{
			name: "no high confidence",
			result: AutoDetectResult{
				Suggestions: []MappingSuggestion{
					{OriginalLine: 10, Confidence: ConfidenceMedium},
					{OriginalLine: 20, Confidence: ConfidenceLow},
				},
			},
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.result.GetHighConfidenceSuggestions()
			if len(result) != tt.expected {
				t.Errorf("GetHighConfidenceSuggestions() returned %d suggestions, want %d", len(result), tt.expected)
			}

			// Verify all returned suggestions are high confidence
			for _, suggestion := range result {
				if suggestion.Confidence != ConfidenceHigh {
					t.Errorf("GetHighConfidenceSuggestions() returned non-high confidence suggestion: %v", suggestion)
				}
			}
		})
	}
}
