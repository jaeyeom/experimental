// Package detector provides the main project detection functionality.
package detector

import (
	"fmt"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// languageDetector defines the interface for language-specific detection.
type languageDetector interface {
	// DetectLanguage checks if the given path contains the specific language
	DetectLanguage(rootPath string) (bool, error)

	// GetLanguage returns the language this detector handles
	GetLanguage() config.Language

	// GetConfigFiles returns the configuration files for this language
	GetConfigFiles(rootPath string) map[string]string

	// GetTools returns the available tools for this language
	GetTools(rootPath string) map[config.ToolType][]string
}

// buildSystemDetector defines the interface for build system detection.
type buildSystemDetector interface {
	// DetectBuildSystem checks if the given path uses this build system
	DetectBuildSystem(rootPath string) (bool, error)

	// GetBuildSystem returns the build system this detector handles
	GetBuildSystem() config.BuildSystem

	// GetTools returns the available tools for this build system
	GetTools(rootPath string) map[config.ToolType][]string
}

// ProjectDetector implements the main project detection logic.
type ProjectDetector struct {
	patternMatcher       *PatternMatcher
	languageDetectors    map[config.Language]languageDetector
	buildSystemDetectors map[config.BuildSystem]buildSystemDetector
}

// NewProjectDetector creates a new project detector with all language and build system detectors.
func NewProjectDetector() *ProjectDetector {
	detector := &ProjectDetector{
		patternMatcher:       NewPatternMatcher(),
		languageDetectors:    make(map[config.Language]languageDetector),
		buildSystemDetectors: make(map[config.BuildSystem]buildSystemDetector),
	}

	// Register language detectors
	goDetector := NewGoDetector()
	pythonDetector := NewPythonDetector()
	tsDetector := NewTypeScriptDetector()
	jsDetector := NewJavaScriptDetector()

	detector.languageDetectors[config.LanguageGo] = goDetector
	detector.languageDetectors[config.LanguagePython] = pythonDetector
	detector.languageDetectors[config.LanguageTypeScript] = tsDetector
	detector.languageDetectors[config.LanguageJavaScript] = jsDetector

	// Register build system detectors
	bazelDetector := NewBazelDetector()
	makeDetector := NewMakeDetector()

	detector.buildSystemDetectors[config.BuildSystemBazel] = bazelDetector
	detector.buildSystemDetectors[config.BuildSystemMake] = makeDetector

	return detector
}

// Detect analyzes the given path and returns project configuration.
func (d *ProjectDetector) Detect(rootPath string) (*config.ProjectConfig, error) {
	// Resolve to absolute path
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve absolute path: %w", err)
	}

	// Scan the directory
	scanner := NewScanner(DefaultScanOptions())
	scanResult, err := scanner.Scan(absPath)
	if err != nil {
		return nil, err
	}

	// Detect languages
	languages := d.patternMatcher.MatchLanguages(scanResult.Files)

	// Detect build system
	buildSystem := d.patternMatcher.MatchBuildSystem(scanResult.Files)

	// Check for git repository (scan with different options to include .git)
	gitScanOptions := DefaultScanOptions()
	gitScanOptions.IncludeHidden = true
	gitScanOptions.MaxDepth = 2 // Scan one level deeper to find .git/HEAD
	// Remove .git from ignore patterns temporarily
	newIgnorePatterns := make([]string, 0, len(gitScanOptions.IgnorePatterns))
	for _, pattern := range gitScanOptions.IgnorePatterns {
		if pattern != ".git" {
			newIgnorePatterns = append(newIgnorePatterns, pattern)
		}
	}
	gitScanOptions.IgnorePatterns = newIgnorePatterns

	gitScanner := NewScanner(gitScanOptions)
	gitScanResult, _ := gitScanner.Scan(absPath)
	hasGit := false
	if gitScanResult != nil {
		// Check for .git directory or .git file (in case of worktrees)
		for _, dir := range gitScanResult.Directories {
			if dir == ".git" {
				hasGit = true
				break
			}
		}
		// Also check for .git/HEAD or other git files
		if !hasGit {
			hasGit = gitScanResult.HasPattern(".git/*") || gitScanResult.HasFile(".git")
		}
	}

	// Aggregate tools from all detected components
	tools := make(map[config.ToolType][]string)

	// Add language-specific tools
	for _, lang := range languages {
		if detector, exists := d.languageDetectors[lang]; exists {
			langTools := detector.GetTools(absPath)
			for toolType, toolList := range langTools {
				tools[toolType] = append(tools[toolType], toolList...)
			}
		}
	}

	// Add build system tools (these take priority)
	if detector, exists := d.buildSystemDetectors[buildSystem]; exists {
		buildTools := detector.GetTools(absPath)
		for toolType, toolList := range buildTools {
			// Prepend build system tools to give them priority
			tools[toolType] = append(toolList, tools[toolType]...)
		}
	}

	// Collect configuration files
	configFiles := make(map[string]string)

	// Add language-specific config files
	for _, lang := range languages {
		if detector, exists := d.languageDetectors[lang]; exists {
			langConfigs := detector.GetConfigFiles(absPath)
			for tool, file := range langConfigs {
				configFiles[tool] = file
			}
		}
	}

	return &config.ProjectConfig{
		RootPath:      absPath,
		BuildSystem:   buildSystem,
		Languages:     languages,
		Tools:         tools,
		ConfigFiles:   configFiles,
		HasGit:        hasGit,
		DetectionTime: time.Now(),
	}, nil
}

// SupportedLanguages returns the list of languages this detector supports.
func (d *ProjectDetector) SupportedLanguages() []config.Language {
	languages := make([]config.Language, 0, len(d.languageDetectors))
	for lang := range d.languageDetectors {
		languages = append(languages, lang)
	}
	return languages
}

// SupportedBuildSystems returns the list of build systems this detector supports.
func (d *ProjectDetector) SupportedBuildSystems() []config.BuildSystem {
	buildSystems := make([]config.BuildSystem, 0, len(d.buildSystemDetectors))
	for bs := range d.buildSystemDetectors {
		buildSystems = append(buildSystems, bs)
	}
	return buildSystems
}

// BazelDetector implements build system detection for Bazel.
type BazelDetector struct {
	patternMatcher *PatternMatcher
}

// NewBazelDetector creates a new Bazel build system detector.
func NewBazelDetector() *BazelDetector {
	return &BazelDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectBuildSystem checks if the given path uses Bazel.
func (d *BazelDetector) DetectBuildSystem(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	buildSystem := d.patternMatcher.MatchBuildSystem(result.Files)
	return buildSystem == config.BuildSystemBazel, nil
}

// GetBuildSystem returns the build system this detector handles.
func (d *BazelDetector) GetBuildSystem() config.BuildSystem {
	return config.BuildSystemBazel
}

// GetTools returns the available tools for Bazel.
func (d *BazelDetector) GetTools(rootPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Bazel provides unified commands for all operations
	tools[config.ToolTypeFormat] = []string{"bazel run //tools:format", "bazel run //:format"}
	tools[config.ToolTypeLint] = []string{"bazel run //tools:lint", "bazel run //:lint"}
	tools[config.ToolTypeTest] = []string{"bazel test //..."}

	return tools
}

// MakeDetector implements build system detection for Make.
type MakeDetector struct {
	patternMatcher *PatternMatcher
}

// NewMakeDetector creates a new Make build system detector.
func NewMakeDetector() *MakeDetector {
	return &MakeDetector{
		patternMatcher: NewPatternMatcher(),
	}
}

// DetectBuildSystem checks if the given path uses Make.
func (d *MakeDetector) DetectBuildSystem(rootPath string) (bool, error) {
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return false, err
	}

	buildSystem := d.patternMatcher.MatchBuildSystem(result.Files)
	return buildSystem == config.BuildSystemMake, nil
}

// GetBuildSystem returns the build system this detector handles.
func (d *MakeDetector) GetBuildSystem() config.BuildSystem {
	return config.BuildSystemMake
}

// GetTools returns the available tools for Make.
func (d *MakeDetector) GetTools(rootPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Make provides targets for different operations
	tools[config.ToolTypeFormat] = []string{"make format"}
	tools[config.ToolTypeLint] = []string{"make lint"}
	tools[config.ToolTypeTest] = []string{"make test"}

	return tools
}
