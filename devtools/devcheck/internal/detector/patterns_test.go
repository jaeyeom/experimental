package detector

import (
	"reflect"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestNewPatternMatcher(t *testing.T) {
	pm := NewPatternMatcher()

	if pm == nil {
		t.Fatal("NewPatternMatcher returned nil")
		return
	}

	// Test that default patterns are initialized
	if len(pm.languagePatterns) == 0 {
		t.Error("Expected language patterns to be initialized")
	}

	if len(pm.buildSystemPatterns) == 0 {
		t.Error("Expected build system patterns to be initialized")
	}

	if len(pm.configFilePatterns) == 0 {
		t.Error("Expected config file patterns to be initialized")
	}
}

func TestPatternMatcher_MatchLanguages(t *testing.T) {
	pm := NewPatternMatcher()

	tests := []struct {
		name     string
		files    []string
		expected []config.Language
	}{
		{
			name:     "go project",
			files:    []string{"go.mod", "main.go", "README.md"},
			expected: []config.Language{config.LanguageGo},
		},
		{
			name:     "python project",
			files:    []string{"pyproject.toml", "main.py", "requirements.txt"},
			expected: []config.Language{config.LanguagePython},
		},
		{
			name:     "typescript project",
			files:    []string{"tsconfig.json", "src/main.ts", "package.json"},
			expected: []config.Language{config.LanguageTypeScript, config.LanguageJavaScript},
		},
		{
			name:     "mixed project",
			files:    []string{"go.mod", "main.go", "pyproject.toml", "script.py"},
			expected: []config.Language{config.LanguageGo, config.LanguagePython},
		},
		{
			name:     "no recognized files",
			files:    []string{"README.md", "LICENSE", "doc.txt"},
			expected: []config.Language{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := pm.MatchLanguages(tt.files)

			// Sort both slices for comparison since order might vary
			if len(result) != len(tt.expected) {
				t.Errorf("Expected %d languages, got %d: %v", len(tt.expected), len(result), result)
				return
			}

			// Check that all expected languages are present
			expectedMap := make(map[config.Language]bool)
			for _, lang := range tt.expected {
				expectedMap[lang] = true
			}

			for _, lang := range result {
				if !expectedMap[lang] {
					t.Errorf("Unexpected language detected: %v", lang)
				}
				delete(expectedMap, lang)
			}

			if len(expectedMap) > 0 {
				t.Errorf("Missing expected languages: %v", expectedMap)
			}
		})
	}
}

func TestPatternMatcher_MatchBuildSystem(t *testing.T) {
	pm := NewPatternMatcher()

	tests := []struct {
		name     string
		files    []string
		expected config.BuildSystem
	}{
		{
			name:     "bazel project",
			files:    []string{"MODULE.bazel", "BUILD.bazel", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "make project",
			files:    []string{"Makefile", "main.c", "README.md"},
			expected: config.BuildSystemMake,
		},
		{
			name:     "bazel priority over make",
			files:    []string{"MODULE.bazel", "Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "bazel with WORKSPACE (legacy)",
			files:    []string{"WORKSPACE", "BUILD.bazel", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "bazel with WORKSPACE.bazel (legacy)",
			files:    []string{"WORKSPACE.bazel", "BUILD", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "bazel workspace priority over make",
			files:    []string{"WORKSPACE", "Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "no build system",
			files:    []string{"main.go", "README.md"},
			expected: config.BuildSystemNone,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := pm.MatchBuildSystem(tt.files)
			if result != tt.expected {
				t.Errorf("Expected build system %v, got %v", tt.expected, result)
			}
		})
	}
}

func TestPatternMatcher_MatchBuildSystemWithLocation(t *testing.T) {
	pm := NewPatternMatcher()

	tests := []struct {
		name     string
		files    []string
		expected config.BuildSystem
	}{
		{
			name:     "bazel in root, make in subdirectory",
			files:    []string{"MODULE.bazel", "src/Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "make in root, bazel in subdirectory",
			files:    []string{"Makefile", "third_party/MODULE.bazel", "main.go"},
			expected: config.BuildSystemMake,
		},
		{
			name:     "make in root, bazel workspace in subdirectory",
			files:    []string{"Makefile", "third_party/WORKSPACE", "main.go"},
			expected: config.BuildSystemMake,
		},
		{
			name:     "bazel workspace in root, make in subdirectory",
			files:    []string{"WORKSPACE", "src/Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "both in subdirectories - bazel priority",
			files:    []string{"src/MODULE.bazel", "third_party/Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "bazel deeper than make",
			files:    []string{"src/Makefile", "src/third_party/vendor/MODULE.bazel", "main.go"},
			expected: config.BuildSystemMake,
		},
		{
			name:     "multiple build files in root - bazel priority",
			files:    []string{"MODULE.bazel", "Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
		{
			name:     "only subdirectory build files",
			files:    []string{"examples/MODULE.bazel", "tests/Makefile", "main.go"},
			expected: config.BuildSystemBazel,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := pm.MatchBuildSystemWithLocation(tt.files)
			if result != tt.expected {
				t.Errorf("Expected build system %v, got %v", tt.expected, result)
			}
		})
	}
}

func TestPatternMatcher_MatchConfigFiles(t *testing.T) {
	pm := NewPatternMatcher()

	tests := []struct {
		name     string
		files    []string
		tool     string
		expected []string
	}{
		{
			name:     "golangci-lint config",
			files:    []string{".golangci.yml", "main.go", "README.md"},
			tool:     "golangci-lint",
			expected: []string{".golangci.yml"},
		},
		{
			name:     "ruff config in pyproject",
			files:    []string{"pyproject.toml", "main.py"},
			tool:     "ruff",
			expected: []string{"pyproject.toml"},
		},
		{
			name:     "eslint config",
			files:    []string{".eslintrc.json", "package.json", "src/main.ts"},
			tool:     "eslint",
			expected: []string{".eslintrc.json"},
		},
		{
			name:     "no config files",
			files:    []string{"main.go", "README.md"},
			tool:     "golangci-lint",
			expected: []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := pm.MatchConfigFiles(tt.files, tt.tool)

			// Handle empty slice comparison
			if len(tt.expected) == 0 && len(result) == 0 {
				return // Both are empty, test passes
			}

			if !reflect.DeepEqual(result, tt.expected) {
				t.Errorf("Expected config files %v, got %v", tt.expected, result)
			}
		})
	}
}

func TestPatternMatcher_matchPattern(t *testing.T) {
	pm := NewPatternMatcher()

	tests := []struct {
		name     string
		file     string
		pattern  string
		expected bool
	}{
		{"exact match", "go.mod", "go.mod", true},
		{"glob match", "main.go", "*.go", true},
		{"no match", "main.py", "*.go", false},
		{"path with directory", "src/main.go", "*.go", true},
		{"case sensitive", "Makefile", "makefile", false},
		{"case sensitive match", "Makefile", "Makefile", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := pm.matchPattern(tt.file, tt.pattern)
			if result != tt.expected {
				t.Errorf("matchPattern(%q, %q) = %v, expected %v", tt.file, tt.pattern, result, tt.expected)
			}
		})
	}
}

func TestPatternMatcher_AddCustomPatterns(t *testing.T) {
	pm := NewPatternMatcher()

	// Test adding custom language pattern
	customLang := config.Language("rust")
	customPattern := FilePattern{
		Pattern:  "Cargo.toml",
		Language: customLang,
		Required: true,
	}

	pm.AddLanguagePattern(customLang, customPattern)

	// Test that the custom pattern is detected
	result := pm.MatchLanguages([]string{"Cargo.toml", "src/main.rs"})
	found := false
	for _, lang := range result {
		if lang == customLang {
			found = true
			break
		}
	}

	if !found {
		t.Error("Custom language pattern not detected")
	}
}
