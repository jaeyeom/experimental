// Package gherkin provides functionality for parsing Gherkin feature files.
package gherkin

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Feature represents a parsed Gherkin feature file.
type Feature struct {
	ID       string // Extracted from filename (e.g., "login" from "login.feature")
	Name     string // Feature name from file content
	FilePath string // Absolute path to the .feature file
	Content  string // Raw content of the feature file
}

// Parser parses Gherkin feature files.
type Parser interface {
	Parse(filePath string) (*Feature, error)
	ParseAll(filePaths []string) ([]*Feature, error)
}

// DefaultParser implements Parser using file reading.
type DefaultParser struct{}

// Compile-time check that DefaultParser implements Parser.
var _ Parser = (*DefaultParser)(nil)

// NewParser creates a new DefaultParser.
func NewParser() *DefaultParser {
	return &DefaultParser{}
}

// Parse reads a feature file and extracts metadata.
func (p *DefaultParser) Parse(filePath string) (*Feature, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("reading feature file %s: %w", filePath, err)
	}

	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return nil, fmt.Errorf("getting absolute path for %s: %w", filePath, err)
	}

	id := ExtractFeatureID(filePath)
	name := extractFeatureName(string(content))

	return &Feature{
		ID:       id,
		Name:     name,
		FilePath: absPath,
		Content:  string(content),
	}, nil
}

// ParseAll parses multiple feature files.
func (p *DefaultParser) ParseAll(filePaths []string) ([]*Feature, error) {
	features := make([]*Feature, 0, len(filePaths))
	for _, path := range filePaths {
		f, err := p.Parse(path)
		if err != nil {
			return nil, err
		}
		features = append(features, f)
	}
	return features, nil
}

// ExtractFeatureID extracts the feature ID from a file path.
// Example: "/path/to/login.feature" -> "login".
func ExtractFeatureID(filePath string) string {
	base := filepath.Base(filePath)
	return strings.TrimSuffix(base, ".feature")
}

// extractFeatureName extracts the feature name from file content.
// It looks for a line starting with "Feature:" and returns the rest.
func extractFeatureName(content string) string {
	scanner := bufio.NewScanner(strings.NewReader(content))
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "Feature:") {
			name := strings.TrimPrefix(line, "Feature:")
			return strings.TrimSpace(name)
		}
	}
	return ""
}
