// Package split provides functionality to split large markdown files into
// smaller parts based on header levels, using a base36 naming scheme for better
// organization.
//
// Author: Claude 3.5 Sonnet (Codeium AI Assistant)
// Created: February 13, 2025
package split

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// countHeaderLevel returns the header level (number of '#' at start) of a line.
// Returns 0 if the line is not a header.
func countHeaderLevel(line string) int {
	line = strings.TrimSpace(line)
	for i, c := range line {
		if c != '#' {
			return i
		}
	}
	return 0
}

// extractSectionNumber extracts the section number from a title if it exists.
// Returns the section number and the clean title.
func extractSectionNumber(title string) (string, string) {
	re := regexp.MustCompile(`^(\d+(?:\.\d+)*\s+)?(.+)$`)
	matches := re.FindStringSubmatch(title)
	if len(matches) < 3 {
		return "", title
	}
	return strings.TrimSpace(matches[1]), strings.TrimSpace(matches[2])
}

// toBase36 converts a number to base36 (0-9A-Z).
func toBase36(n int) string {
	if n < 10 {
		return fmt.Sprintf("%d", n)
	}
	return string('A' + rune(n-10))
}

// generateFilename creates a filename based on section level and counters.
// Format: [0-9A-Z][0-9A-Z][0-9A-Z]_ where each position can be:
//
// - A digit (0-9) or letter (A-Z), allowing 36 items per level.
func generateFilename(title string, sectionCounters []int, level int) string {
	// Update counters first
	if level <= len(sectionCounters) {
		// Only increment the counter for the current level
		sectionCounters[level-1]++
		// Reset all lower levels
		for i := level; i < len(sectionCounters); i++ {
			sectionCounters[i] = 0
		}
	} else {
		for len(sectionCounters) < level {
			if len(sectionCounters)+1 == level {
				sectionCounters = append(sectionCounters, 1)
			} else {
				sectionCounters = append(sectionCounters, 0)
			}
		}
	}

	// Create the numeric prefix using base36 for all levels
	first := toBase36(min(sectionCounters[0], 35))
	second := toBase36(min(sectionCounters[1], 35))
	third := toBase36(min(sectionCounters[2], 35))

	numericPrefix := fmt.Sprintf("%s%s%s", first, second, third)

	// Sanitize the title part
	_, cleanTitle := extractSectionNumber(title)
	sanitizedTitle := regexp.MustCompile(`[^\w\s-]`).ReplaceAllString(strings.ToLower(cleanTitle), "")
	sanitizedTitle = regexp.MustCompile(`[-\s]+`).ReplaceAllString(sanitizedTitle, "_")
	sanitizedTitle = strings.Trim(sanitizedTitle, "-_")

	return fmt.Sprintf("%s_%s.md", numericPrefix, sanitizedTitle)
}

// SplitMarkdown splits a markdown file into multiple files based on headers.
func SplitMarkdown(inputFile, outputDir string) error {
	// Create output directory
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		return fmt.Errorf("failed to create output directory: %w", err)
	}

	// Open input file
	file, err := os.Open(inputFile)
	if err != nil {
		return fmt.Errorf("failed to open input file: %w", err)
	}
	defer file.Close()

	var (
		currentSection  []string
		currentHeader   string
		currentFilename string
		sectionCounters = []int{0, 0, 0}
		toc             = []string{"# Table of Contents\n\n"}
	)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		headerLevel := countHeaderLevel(line)

		if headerLevel > 0 {
			// Save previous section if it exists
			if currentHeader != "" && currentFilename != "" {
				if err := os.WriteFile(
					filepath.Join(outputDir, currentFilename),
					[]byte(strings.Join(currentSection, "\n")),
					0o600,
				); err != nil {
					return fmt.Errorf("failed to write section file: %w", err)
				}
			}

			// Start new section
			currentHeader = strings.TrimSpace(strings.TrimLeft(line, "#"))
			currentFilename = generateFilename(currentHeader, sectionCounters, headerLevel)
			currentSection = []string{line}

			// Add to table of contents
			indent := strings.Repeat("  ", headerLevel-1)
			toc = append(toc, fmt.Sprintf("%s- [%s](%s)\n", indent, currentHeader, currentFilename))
		} else if len(currentSection) > 0 {
			currentSection = append(currentSection, line)
		}
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading input file: %w", err)
	}

	// Save the last section
	if currentHeader != "" && currentFilename != "" {
		if err := os.WriteFile(
			filepath.Join(outputDir, currentFilename),
			[]byte(strings.Join(currentSection, "\n")),
			0o600,
		); err != nil {
			return fmt.Errorf("failed to write last section file: %w", err)
		}
	}

	// Save table of contents
	if err := os.WriteFile(
		filepath.Join(outputDir, "000_table_of_contents.md"),
		[]byte(strings.Join(toc, "")),
		0o600,
	); err != nil {
		return fmt.Errorf("failed to write table of contents: %w", err)
	}

	return nil
}
