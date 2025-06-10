// Package detector provides language-specific detection functionality.
package detector

import (
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// GoDetector implements language detection for Go projects.
type GoDetector struct {
	patternMatcher *PatternMatcher
}

// NewGoDetector creates a new Go language detector.
func NewGoDetector() *GoDetector {
	return &GoDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectLanguage checks if the given path contains a Go project.
func (d *GoDetector) DetectLanguage(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	languages := d.patternMatcher.MatchLanguages(result.Files)
	for _, lang := range languages {
		if lang == config.LanguageGo {
			return true, nil
		}
	}

	return false, nil
}

// GetLanguage returns the language this detector handles.
func (d *GoDetector) GetLanguage() config.Language {
	return config.LanguageGo
}

// GetConfigFiles returns the configuration files for Go tools.
func (d *GoDetector) GetConfigFiles(rootPath string) map[string]string {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return map[string]string{}
	}

	configFiles := make(map[string]string)

	// Check for golangci-lint config
	matches := d.patternMatcher.MatchConfigFiles(result.Files, "golangci-lint")
	if len(matches) > 0 {
		configFiles["golangci-lint"] = matches[0]
	}

	return configFiles
}

// GetTools returns the available tools for Go development.
func (d *GoDetector) GetTools(rootPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Format tools
	tools[config.ToolTypeFormat] = []string{"gofumpt", "gofmt"}

	// Lint tools
	tools[config.ToolTypeLint] = []string{"golangci-lint"}

	// Test tools
	tools[config.ToolTypeTest] = []string{"go test"}

	return tools
}

// PythonDetector implements language detection for Python projects.
type PythonDetector struct {
	patternMatcher *PatternMatcher
}

// NewPythonDetector creates a new Python language detector.
func NewPythonDetector() *PythonDetector {
	return &PythonDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectLanguage checks if the given path contains a Python project.
func (d *PythonDetector) DetectLanguage(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	languages := d.patternMatcher.MatchLanguages(result.Files)
	for _, lang := range languages {
		if lang == config.LanguagePython {
			return true, nil
		}
	}

	return false, nil
}

// GetLanguage returns the language this detector handles.
func (d *PythonDetector) GetLanguage() config.Language {
	return config.LanguagePython
}

// GetConfigFiles returns the configuration files for Python tools.
func (d *PythonDetector) GetConfigFiles(rootPath string) map[string]string {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return map[string]string{}
	}

	configFiles := make(map[string]string)

	// Check for ruff config
	matches := d.patternMatcher.MatchConfigFiles(result.Files, "ruff")
	if len(matches) > 0 {
		configFiles["ruff"] = matches[0]
	}

	return configFiles
}

// GetTools returns the available tools for Python development.
func (d *PythonDetector) GetTools(rootPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Format tools
	tools[config.ToolTypeFormat] = []string{"ruff format", "black"}

	// Lint tools
	tools[config.ToolTypeLint] = []string{"ruff check", "flake8"}

	// Test tools
	tools[config.ToolTypeTest] = []string{"pytest", "python -m unittest"}

	return tools
}

// TypeScriptDetector implements language detection for TypeScript projects.
type TypeScriptDetector struct {
	patternMatcher *PatternMatcher
}

// NewTypeScriptDetector creates a new TypeScript language detector.
func NewTypeScriptDetector() *TypeScriptDetector {
	return &TypeScriptDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectLanguage checks if the given path contains a TypeScript project.
func (d *TypeScriptDetector) DetectLanguage(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	languages := d.patternMatcher.MatchLanguages(result.Files)
	for _, lang := range languages {
		if lang == config.LanguageTypeScript {
			return true, nil
		}
	}

	return false, nil
}

// GetLanguage returns the language this detector handles.
func (d *TypeScriptDetector) GetLanguage() config.Language {
	return config.LanguageTypeScript
}

// GetConfigFiles returns the configuration files for TypeScript tools.
func (d *TypeScriptDetector) GetConfigFiles(rootPath string) map[string]string {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return map[string]string{}
	}

	configFiles := make(map[string]string)

	// Check for ESLint config
	matches := d.patternMatcher.MatchConfigFiles(result.Files, "eslint")
	if len(matches) > 0 {
		configFiles["eslint"] = matches[0]
	}

	// Check for TypeScript config
	if result.HasFile("tsconfig.json") {
		configFiles["typescript"] = "tsconfig.json"
	}

	return configFiles
}

// GetTools returns the available tools for TypeScript development.
func (d *TypeScriptDetector) GetTools(rootPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Check if package.json exists to determine npm vs other tools
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err == nil && result.HasFile("package.json") {
		// Use npm scripts if package.json exists
		tools[config.ToolTypeFormat] = []string{"npm run format", "prettier"}
		tools[config.ToolTypeLint] = []string{"npm run lint", "eslint"}
		tools[config.ToolTypeTest] = []string{"npm test", "npm run test"}
	} else {
		// Fallback to direct tool usage
		tools[config.ToolTypeFormat] = []string{"prettier"}
		tools[config.ToolTypeLint] = []string{"eslint"}
		tools[config.ToolTypeTest] = []string{"jest", "mocha"}
	}

	return tools
}

// JavaScriptDetector implements language detection for JavaScript projects.
type JavaScriptDetector struct {
	patternMatcher *PatternMatcher
}

// NewJavaScriptDetector creates a new JavaScript language detector.
func NewJavaScriptDetector() *JavaScriptDetector {
	return &JavaScriptDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectLanguage checks if the given path contains a JavaScript project.
func (d *JavaScriptDetector) DetectLanguage(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	languages := d.patternMatcher.MatchLanguages(result.Files)
	for _, lang := range languages {
		if lang == config.LanguageJavaScript {
			return true, nil
		}
	}

	return false, nil
}

// GetLanguage returns the language this detector handles.
func (d *JavaScriptDetector) GetLanguage() config.Language {
	return config.LanguageJavaScript
}

// GetConfigFiles returns the configuration files for JavaScript tools.
func (d *JavaScriptDetector) GetConfigFiles(rootPath string) map[string]string {
	return (&TypeScriptDetector{patternMatcher: d.patternMatcher}).GetConfigFiles(rootPath)
}

// GetTools returns the available tools for JavaScript development.
func (d *JavaScriptDetector) GetTools(rootPath string) map[config.ToolType][]string {
	return (&TypeScriptDetector{patternMatcher: d.patternMatcher}).GetTools(rootPath)
}
