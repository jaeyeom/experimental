package models

import "testing"

// TestFormatDetection tests the auto-detection of diff format types.
func TestFormatDetection(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected FormatType
	}{
		// Simple mapping format tests
		{
			name:     "Simple mapping single deletion",
			input:    "15:-2",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping single insertion",
			input:    "30:+3",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping positive offset without plus",
			input:    "30:3",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping negative offset",
			input:    "15:-2",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping multiple operations",
			input:    "15:-2;30:+3",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping complex chain",
			input:    "10:-1;20:+2;30:-3",
			expected: FormatSimpleMapping,
		},
		{
			name:     "Simple mapping with spaces",
			input:    "15 : -2 ; 30 : +3",
			expected: FormatSimpleMapping,
		},

		// Classic diff format tests
		{
			name:     "Classic diff delete single line",
			input:    "15d14",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff delete range",
			input:    "15,17d14",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff insert single line",
			input:    "30a31",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff insert range",
			input:    "30a31,33",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff change single line",
			input:    "5c6",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff change range",
			input:    "5,7c6,8",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff multiple operations",
			input:    "15,16d14;29a30,32",
			expected: FormatClassicDiff,
		},
		{
			name:     "Classic diff complex chain",
			input:    "10d9;20,21a21,23;30c32",
			expected: FormatClassicDiff,
		},

		// Unknown/invalid format tests
		{
			name:     "Empty string",
			input:    "",
			expected: FormatUnknown,
		},
		{
			name:     "Invalid format",
			input:    "invalid",
			expected: FormatUnknown,
		},
		{
			name:     "Mixed format attempt",
			input:    "15:-2;30a31",
			expected: FormatUnknown,
		},
		{
			name:     "Invalid classic diff",
			input:    "15x14",
			expected: FormatUnknown,
		},
		{
			name:     "Invalid simple mapping",
			input:    "15:abc",
			expected: FormatUnknown,
		},
		{
			name:     "Missing colon in simple mapping",
			input:    "15-2",
			expected: FormatUnknown,
		},
		{
			name:     "Random text",
			input:    "hello world",
			expected: FormatUnknown,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := DetectFormat(tt.input)
			if result != tt.expected {
				t.Errorf("DetectFormat(%q) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

// TestSimpleMappingToClassicDiffConversion tests the conversion of simple mapping format to classic diff format.
func TestSimpleMappingToClassicDiffConversion(t *testing.T) {
	tests := []struct {
		name         string
		mapping      string
		expected     string
		expectError  bool
		errorMessage string
	}{
		// Single operation conversions
		{
			name:     "Single deletion mapping",
			mapping:  "15:-2",
			expected: "15,16d14",
		},
		{
			name:     "Single insertion mapping",
			mapping:  "30:+3",
			expected: "29a30,32",
		},
		{
			name:     "Single insertion without plus sign",
			mapping:  "30:3",
			expected: "29a30,32",
		},
		{
			name:     "Deletion of single line",
			mapping:  "15:-1",
			expected: "15d14",
		},
		{
			name:     "Insertion of single line",
			mapping:  "30:+1",
			expected: "29a30",
		},

		// Multiple operation conversions
		{
			name:     "Delete then insert",
			mapping:  "15:-2;30:+3",
			expected: "15,16d14;29a28,30", // Delete lines 15-16, then insert before original line 30
		},
		{
			name:     "Insert then delete",
			mapping:  "10:+2;20:-1",
			expected: "9a10,11;20d21", // Insert 2 lines before 10 → "9a10,11", then delete original line 20 → "20d21"
		},
		{
			name:     "Multiple deletions",
			mapping:  "10:-1;20:-2",
			expected: "10d9;20,21d18", // Delete line 10 → "10d9", then delete lines 20-21 (original) → "20,21d18"
		},
		{
			name:     "Multiple insertions",
			mapping:  "10:+1;20:+2",
			expected: "9a10;19a21,22", // Insert 1 line before 10 → "9a10", then insert 2 lines before original 20 → "19a21,22"
		},
		{
			name:     "Complex chain",
			mapping:  "5:-1;15:+2;25:-3",
			expected: "5d4;14a14,15;25,27d25", // Delete line 5 → "5d4", insert before original 15 → "14a14,15", delete at original 25 → "25,27d25"
		},

		// Edge cases
		{
			name:     "Zero offset (no change)",
			mapping:  "15:0",
			expected: "",
		},
		{
			name:     "Delete entire content (hypothetical)",
			mapping:  "1:-100",
			expected: "1,100d0",
		},
		{
			name:     "Large insertion",
			mapping:  "50:+10",
			expected: "49a50,59",
		},

		// Error cases
		{
			name:         "Invalid format",
			mapping:      "15-2",
			expectError:  true,
			errorMessage: "invalid simple mapping format",
		},
		{
			name:         "Non-numeric line",
			mapping:      "abc:-2",
			expectError:  true,
			errorMessage: "invalid line number",
		},
		{
			name:         "Non-numeric offset",
			mapping:      "15:abc",
			expectError:  true,
			errorMessage: "invalid offset",
		},
		{
			name:         "Negative line number",
			mapping:      "-5:+2",
			expectError:  true,
			errorMessage: "line number must be positive",
		},
		{
			name:         "Empty mapping",
			mapping:      "",
			expectError:  true,
			errorMessage: "empty mapping",
		},
		{
			name:         "Line zero",
			mapping:      "0:+2",
			expectError:  true,
			errorMessage: "line number must be positive",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := ConvertSimpleMappingToClassicDiff(tt.mapping)

			if tt.expectError {
				if err == nil {
					t.Errorf("ConvertSimpleMappingToClassicDiff(%q) expected error but got none", tt.mapping)
					return
				}
				if tt.errorMessage != "" && !contains(err.Error(), tt.errorMessage) {
					t.Errorf("ConvertSimpleMappingToClassicDiff(%q) error = %q, want to contain %q", tt.mapping, err.Error(), tt.errorMessage)
				}
				return
			}

			if err != nil {
				t.Errorf("ConvertSimpleMappingToClassicDiff(%q) unexpected error: %v", tt.mapping, err)
				return
			}

			if result != tt.expected {
				t.Errorf("ConvertSimpleMappingToClassicDiff(%q) = %q, want %q", tt.mapping, result, tt.expected)
			}
		})
	}
}

// TestIntegrationDiffFormats tests that both formats produce identical results.
func TestIntegrationDiffFormats(t *testing.T) {
	integrationTests := []struct {
		name          string
		simpleMapping string
		classicDiff   string
		description   string
	}{
		{
			name:          "Simple deletion",
			simpleMapping: "15:-2",
			classicDiff:   "15,16d14",
			description:   "Delete 2 lines starting at line 15",
		},
		{
			name:          "Simple insertion",
			simpleMapping: "30:+3",
			classicDiff:   "29a30,32",
			description:   "Insert 3 lines before line 30",
		},
		{
			name:          "Multiple operations",
			simpleMapping: "15:-2;30:+3",
			classicDiff:   "15,16d14;29a28,30",
			description:   "Delete 2 lines at 15, then insert 3 lines before original 30",
		},
		{
			name:          "Single line operations",
			simpleMapping: "10:-1;20:+1",
			classicDiff:   "10d9;19a20",
			description:   "Delete 1 line at 10, insert 1 line before original 20",
		},
	}

	for _, tt := range integrationTests {
		t.Run(tt.name, func(t *testing.T) {
			// Convert simple mapping to classic diff
			converted, err := ConvertSimpleMappingToClassicDiff(tt.simpleMapping)
			if err != nil {
				t.Errorf("Failed to convert simple mapping: %v", err)
				return
			}

			// Verify the conversion matches expected classic diff
			if converted != tt.classicDiff {
				t.Errorf("Conversion mismatch: got %q, want %q", converted, tt.classicDiff)
				return
			}

			// Parse both formats to LineAdjustment arrays
			simpleParsed, err := ParseSimpleMappingSpec(tt.simpleMapping)
			if err != nil {
				t.Errorf("Failed to parse simple mapping: %v", err)
				return
			}

			classicParsed, err := ParseDiffSpec(tt.classicDiff)
			if err != nil {
				t.Errorf("Failed to parse classic diff: %v", err)
				return
			}

			// Both should produce identical LineAdjustment arrays
			if !equalLineAdjustments(simpleParsed, classicParsed) {
				t.Errorf("Parsed results differ:\nSimple: %+v\nClassic: %+v", simpleParsed, classicParsed)
			}
		})
	}
}

// Helper functions for tests.
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(substr) == 0 ||
		(len(s) > 0 && len(substr) > 0 && findSubstring(s, substr)))
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

func equalLineAdjustments(a, b []LineAdjustment) bool {
	if len(a) != len(b) {
		return false
	}

	for i, adj1 := range a {
		adj2 := b[i]
		if adj1.Operation != adj2.Operation ||
			adj1.OldStart != adj2.OldStart ||
			adj1.OldEnd != adj2.OldEnd ||
			adj1.NewStart != adj2.NewStart ||
			adj1.NewEnd != adj2.NewEnd {
			return false
		}
	}

	return true
}
