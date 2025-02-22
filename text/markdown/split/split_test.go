// Package split provides test cases for the markdown splitter functionality.
//
// Author: Claude 3.5 Sonnet (Codeium AI Assistant)
// Created: February 13, 2025
package split

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCountHeaderLevel(t *testing.T) {
	tests := []struct {
		name     string
		line     string
		expected int
	}{
		{
			name:     "level 1 header",
			line:     "# Header 1",
			expected: 1,
		},
		{
			name:     "level 2 header",
			line:     "## Header 2",
			expected: 2,
		},
		{
			name:     "level 3 header",
			line:     "### Header 3",
			expected: 3,
		},
		{
			name:     "regular text",
			line:     "Regular text",
			expected: 0,
		},
		{
			name:     "empty line",
			line:     "",
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := countHeaderLevel(tt.line); got != tt.expected {
				t.Errorf("countHeaderLevel(%q) = %v, want %v", tt.line, got, tt.expected)
			}
		})
	}
}

func TestGenerateFilename(t *testing.T) {
	tests := []struct {
		name            string
		title           string
		sectionCounters []int
		level           int
		expected        string
	}{
		{
			name:            "level 1 header",
			title:           "Header 1",
			sectionCounters: []int{0, 0, 0},
			level:           1,
			expected:        "100_header_1.md",
		},
		{
			name:            "level 2 header with number",
			title:           "1.2 Sub Header",
			sectionCounters: []int{1, 9, 0},
			level:           2,
			expected:        "1A0_sub_header.md",
		},
		{
			name:            "level 3 header with number",
			title:           "1.2.3 Sub Sub Header",
			sectionCounters: []int{1, 10, 35},
			level:           3,
			expected:        "1AZ_sub_sub_header.md",
		},
		{
			name:            "overflow test",
			title:           "Overflow Test",
			sectionCounters: []int{35, 35, 35},
			level:           1,
			expected:        "Z00_overflow_test.md",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := generateFilename(tt.title, tt.sectionCounters, tt.level); got != tt.expected {
				t.Errorf("generateFilename(%q, %v, %v) = %v, want %v",
					tt.title, tt.sectionCounters, tt.level, got, tt.expected)
			}
		})
	}
}

func TestSplitMarkdown(t *testing.T) {
	// Create a temporary directory for test files
	tmpDir, err := os.MkdirTemp("", "split-markdown-test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// Create test input file
	inputFile := filepath.Join(tmpDir, "test.md")
	content := `# Header 1
Content 1

## Header 2
Content 2

### Header 3
Content 3
`
	if err := os.WriteFile(inputFile, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}

	// Create output directory
	outputDir := filepath.Join(tmpDir, "output")

	// Run split_markdown
	if err := SplitMarkdown(inputFile, outputDir); err != nil {
		t.Fatal(err)
	}

	// Check if files were created
	expectedFiles := []string{
		"000_table_of_contents.md",
		"100_header_1.md",
		"110_header_2.md",
		"111_header_3.md",
	}

	for _, file := range expectedFiles {
		path := filepath.Join(outputDir, file)
		if _, err := os.Stat(path); os.IsNotExist(err) {
			t.Errorf("Expected file %s does not exist", file)
		}
	}

	// Check content of files
	checkFileContent := func(filename, expectedContent string) {
		content, err := os.ReadFile(filepath.Join(outputDir, filename))
		if err != nil {
			t.Errorf("Failed to read file %s: %v", filename, err)
			return
		}
		if !strings.Contains(string(content), expectedContent) {
			t.Errorf("File %s does not contain expected content %q", filename, expectedContent)
		}
	}

	checkFileContent("100_header_1.md", "Content 1")
	checkFileContent("110_header_2.md", "Content 2")
	checkFileContent("111_header_3.md", "Content 3")
}
