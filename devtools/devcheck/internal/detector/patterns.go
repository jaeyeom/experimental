// Package detector provides file pattern matching for project detection.
package detector

import (
	"path/filepath"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// FilePattern represents a file pattern for detection.
type FilePattern struct {
	// Pattern is the glob pattern to match
	Pattern string
	// Language is the associated language (if any)
	Language config.Language
	// BuildSystem is the associated build system (if any)
	BuildSystem config.BuildSystem
	// Required indicates if this pattern must be present
	Required bool
}

// PatternMatcher handles file pattern matching for project detection.
type PatternMatcher struct {
	// languagePatterns maps languages to their file patterns
	languagePatterns map[config.Language][]FilePattern
	// buildSystemPatterns maps build systems to their file patterns
	buildSystemPatterns map[config.BuildSystem][]FilePattern
	// configFilePatterns maps tools to their configuration files
	configFilePatterns map[string][]FilePattern
}

// NewPatternMatcher creates a new pattern matcher with default patterns.
func NewPatternMatcher() *PatternMatcher {
	pm := &PatternMatcher{
		languagePatterns:    make(map[config.Language][]FilePattern),
		buildSystemPatterns: make(map[config.BuildSystem][]FilePattern),
		configFilePatterns:  make(map[string][]FilePattern),
	}
	pm.initializeDefaultPatterns()
	return pm
}

// initializeDefaultPatterns sets up the default file patterns for detection.
func (pm *PatternMatcher) initializeDefaultPatterns() {
	// Go language patterns
	pm.languagePatterns[config.LanguageGo] = []FilePattern{
		{Pattern: "go.mod", Language: config.LanguageGo, Required: true},
		{Pattern: "*.go", Language: config.LanguageGo, Required: false},
		{Pattern: "go.sum", Language: config.LanguageGo, Required: false},
	}

	// Python language patterns
	pm.languagePatterns[config.LanguagePython] = []FilePattern{
		{Pattern: "pyproject.toml", Language: config.LanguagePython, Required: false},
		{Pattern: "requirements.txt", Language: config.LanguagePython, Required: false},
		{Pattern: "setup.py", Language: config.LanguagePython, Required: false},
		{Pattern: "*.py", Language: config.LanguagePython, Required: false},
	}

	// TypeScript/JavaScript patterns
	pm.languagePatterns[config.LanguageTypeScript] = []FilePattern{
		{Pattern: "tsconfig.json", Language: config.LanguageTypeScript, Required: true},
		{Pattern: "*.ts", Language: config.LanguageTypeScript, Required: false},
		{Pattern: "*.tsx", Language: config.LanguageTypeScript, Required: false},
	}

	pm.languagePatterns[config.LanguageJavaScript] = []FilePattern{
		{Pattern: "package.json", Language: config.LanguageJavaScript, Required: false},
		{Pattern: "*.js", Language: config.LanguageJavaScript, Required: false},
		{Pattern: "*.jsx", Language: config.LanguageJavaScript, Required: false},
	}

	// Build system patterns
	pm.buildSystemPatterns[config.BuildSystemBazel] = []FilePattern{
		{Pattern: "BUILD.bazel", BuildSystem: config.BuildSystemBazel, Required: false},
		{Pattern: "BUILD", BuildSystem: config.BuildSystemBazel, Required: false},
		{Pattern: "MODULE.bazel", BuildSystem: config.BuildSystemBazel, Required: true},
		{Pattern: "WORKSPACE", BuildSystem: config.BuildSystemBazel, Required: false},
		{Pattern: "WORKSPACE.bazel", BuildSystem: config.BuildSystemBazel, Required: false},
	}

	pm.buildSystemPatterns[config.BuildSystemMake] = []FilePattern{
		{Pattern: "Makefile", BuildSystem: config.BuildSystemMake, Required: true},
		{Pattern: "makefile", BuildSystem: config.BuildSystemMake, Required: false},
		{Pattern: "*.mk", BuildSystem: config.BuildSystemMake, Required: false},
	}

	// Configuration file patterns
	pm.configFilePatterns["golangci-lint"] = []FilePattern{
		{Pattern: ".golangci.yml"},
		{Pattern: ".golangci.yaml"},
		{Pattern: "golangci.yml"},
		{Pattern: "golangci.yaml"},
	}

	pm.configFilePatterns["ruff"] = []FilePattern{
		{Pattern: "ruff.toml"},
		{Pattern: ".ruff.toml"},
		{Pattern: "pyproject.toml"}, // ruff config can be in pyproject.toml
	}

	pm.configFilePatterns["eslint"] = []FilePattern{
		{Pattern: ".eslintrc.json"},
		{Pattern: ".eslintrc.js"},
		{Pattern: ".eslintrc.yml"},
		{Pattern: ".eslintrc.yaml"},
		{Pattern: "eslint.config.js"},
	}
}

// MatchLanguages checks which languages are present in the given files.
func (pm *PatternMatcher) MatchLanguages(files []string) []config.Language {
	var detected []config.Language

	for language, patterns := range pm.languagePatterns {
		if pm.matchPatterns(files, patterns) {
			detected = append(detected, language)
		}
	}

	return detected
}

// MatchBuildSystem detects the primary build system from the given files.
func (pm *PatternMatcher) MatchBuildSystem(files []string) config.BuildSystem {
	// Priority order: Bazel first, then Make, then none
	buildSystems := []config.BuildSystem{
		config.BuildSystemBazel,
		config.BuildSystemMake,
	}

	for _, buildSystem := range buildSystems {
		if patterns, exists := pm.buildSystemPatterns[buildSystem]; exists {
			if pm.matchPatterns(files, patterns) {
				return buildSystem
			}
		}
	}

	return config.BuildSystemNone
}

// MatchConfigFiles finds configuration files for the given tool.
func (pm *PatternMatcher) MatchConfigFiles(files []string, tool string) []string {
	var matches []string

	if patterns, exists := pm.configFilePatterns[tool]; exists {
		for _, file := range files {
			for _, pattern := range patterns {
				if matched, _ := filepath.Match(pattern.Pattern, filepath.Base(file)); matched {
					matches = append(matches, file)
				}
			}
		}
	}

	return matches
}

// matchPatterns checks if the required patterns are satisfied by the file list.
func (pm *PatternMatcher) matchPatterns(files []string, patterns []FilePattern) bool {
	requiredMatches := 0
	totalRequired := 0
	optionalMatches := 0

	// Count required patterns
	for _, pattern := range patterns {
		if pattern.Required {
			totalRequired++
		}
	}

	// Check each pattern against files
	for _, pattern := range patterns {
		matched := false
		for _, file := range files {
			if pm.matchPattern(file, pattern.Pattern) {
				matched = true
				break
			}
		}

		if matched {
			if pattern.Required {
				requiredMatches++
			} else {
				optionalMatches++
			}
		}
	}

	// Language/build system is detected if:
	// 1. All required patterns are matched, OR
	// 2. No required patterns exist but at least one optional pattern matches
	if totalRequired > 0 {
		return requiredMatches == totalRequired
	}

	return optionalMatches > 0
}

// matchPattern checks if a file matches a pattern (supports glob patterns).
func (pm *PatternMatcher) matchPattern(file, pattern string) bool {
	// Handle exact matches first
	if filepath.Base(file) == pattern {
		return true
	}

	// Handle glob patterns
	if matched, err := filepath.Match(pattern, filepath.Base(file)); err == nil && matched {
		return true
	}

	// Handle regex patterns (if pattern contains regex characters)
	if strings.Contains(pattern, "^") || strings.Contains(pattern, "$") || strings.Contains(pattern, "[") {
		if re, err := regexp.Compile(pattern); err == nil {
			return re.MatchString(filepath.Base(file))
		}
	}

	return false
}

// AddLanguagePattern adds a custom language pattern.
func (pm *PatternMatcher) AddLanguagePattern(language config.Language, pattern FilePattern) {
	if pm.languagePatterns[language] == nil {
		pm.languagePatterns[language] = []FilePattern{}
	}
	pm.languagePatterns[language] = append(pm.languagePatterns[language], pattern)
}

// AddBuildSystemPattern adds a custom build system pattern.
func (pm *PatternMatcher) AddBuildSystemPattern(buildSystem config.BuildSystem, pattern FilePattern) {
	if pm.buildSystemPatterns[buildSystem] == nil {
		pm.buildSystemPatterns[buildSystem] = []FilePattern{}
	}
	pm.buildSystemPatterns[buildSystem] = append(pm.buildSystemPatterns[buildSystem], pattern)
}

// AddConfigFilePattern adds a custom configuration file pattern.
func (pm *PatternMatcher) AddConfigFilePattern(tool string, pattern FilePattern) {
	if pm.configFilePatterns[tool] == nil {
		pm.configFilePatterns[tool] = []FilePattern{}
	}
	pm.configFilePatterns[tool] = append(pm.configFilePatterns[tool], pattern)
}
