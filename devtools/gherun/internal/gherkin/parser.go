// Package gherkin provides functionality for parsing Gherkin feature files.
package gherkin

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/vars"
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
type DefaultParser struct {
	vars vars.Vars
}

// Compile-time check that DefaultParser implements Parser.
var _ Parser = (*DefaultParser)(nil)

// NewParser creates a new DefaultParser.
func NewParser() *DefaultParser {
	return &DefaultParser{}
}

// NewParserWithVars creates a parser that substitutes template variables.
func NewParserWithVars(v vars.Vars) *DefaultParser {
	return &DefaultParser{vars: v}
}

// Parse reads a feature file and extracts metadata.
// If variables are configured, they are substituted in the content.
func (p *DefaultParser) Parse(filePath string) (*Feature, error) {
	rawContent, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("reading feature file %s: %w", filePath, err)
	}

	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return nil, fmt.Errorf("getting absolute path for %s: %w", filePath, err)
	}

	// Apply variable substitution if vars are configured
	content := string(rawContent)
	if p.vars != nil {
		content, err = p.vars.Substitute(content)
		if err != nil {
			return nil, fmt.Errorf("substituting variables in %s: %w", filePath, err)
		}
	}

	// Always check for unreplaced variable placeholders
	// This catches the case where variables are used but no --var or --env-file was provided
	if unreplacedVars := vars.FindVariables(content); len(unreplacedVars) > 0 {
		return nil, fmt.Errorf("feature file %s contains variables that were not provided: %v (use --var or --env-file to provide values)", filePath, unreplacedVars)
	}

	id := ExtractFeatureID(filePath)
	name := extractFeatureName(content)

	return &Feature{
		ID:       id,
		Name:     name,
		FilePath: absPath,
		Content:  content,
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
