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
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve absolute path: %w", err)
	}

	scanResult, err := d.performInitialScan(absPath)
	if err != nil {
		return nil, err
	}

	languages := d.patternMatcher.MatchLanguages(scanResult.Files)
	buildSystem := d.patternMatcher.MatchBuildSystemWithLocation(scanResult.Files)
	hasGit := d.detectGitRepository(absPath)

	tools := d.aggregateTools(languages, buildSystem, absPath)
	configFiles := d.collectConfigFiles(languages, absPath)

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

func (d *ProjectDetector) performInitialScan(absPath string) (*ScanResult, error) {
	scanner := NewScanner(DefaultScanOptions())
	return scanner.Scan(absPath)
}

func (d *ProjectDetector) detectGitRepository(absPath string) bool {
	gitScanOptions := d.createGitScanOptions()
	gitScanner := NewScanner(gitScanOptions)
	gitScanResult, _ := gitScanner.Scan(absPath)

	if gitScanResult == nil {
		return false
	}

	return d.checkForGitIndicators(gitScanResult)
}

func (d *ProjectDetector) createGitScanOptions() ScanOptions {
	gitScanOptions := DefaultScanOptions()
	gitScanOptions.IncludeHidden = true
	gitScanOptions.MaxDepth = 2

	// Remove .git from ignore patterns
	newIgnorePatterns := make([]string, 0, len(gitScanOptions.IgnorePatterns))
	for _, pattern := range gitScanOptions.IgnorePatterns {
		if pattern != ".git" {
			newIgnorePatterns = append(newIgnorePatterns, pattern)
		}
	}
	gitScanOptions.IgnorePatterns = newIgnorePatterns

	return gitScanOptions
}

func (d *ProjectDetector) checkForGitIndicators(gitScanResult *ScanResult) bool {
	// Check for .git directory
	for _, dir := range gitScanResult.Directories {
		if dir == ".git" {
			return true
		}
	}

	// Check for .git files (worktrees)
	return gitScanResult.HasPattern(".git/*") || gitScanResult.HasFile(".git")
}

func (d *ProjectDetector) aggregateTools(languages []config.Language, buildSystem config.BuildSystem, absPath string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Add language-specific tools
	d.addLanguageTools(tools, languages, absPath)

	// Add build system tools (with priority)
	d.addBuildSystemTools(tools, buildSystem, absPath)

	return tools
}

func (d *ProjectDetector) addLanguageTools(tools map[config.ToolType][]string, languages []config.Language, absPath string) {
	for _, lang := range languages {
		if detector, exists := d.languageDetectors[lang]; exists {
			langTools := detector.GetTools(absPath)
			for toolType, toolList := range langTools {
				tools[toolType] = append(tools[toolType], toolList...)
			}
		}
	}
}

func (d *ProjectDetector) addBuildSystemTools(tools map[config.ToolType][]string, buildSystem config.BuildSystem, absPath string) {
	if detector, exists := d.buildSystemDetectors[buildSystem]; exists {
		buildTools := detector.GetTools(absPath)
		for toolType, toolList := range buildTools {
			// Prepend build system tools to give them priority
			tools[toolType] = append(toolList, tools[toolType]...)
		}
	}
}

func (d *ProjectDetector) collectConfigFiles(languages []config.Language, absPath string) map[string]string {
	configFiles := make(map[string]string)

	for _, lang := range languages {
		if detector, exists := d.languageDetectors[lang]; exists {
			langConfigs := detector.GetConfigFiles(absPath)
			for tool, file := range langConfigs {
				configFiles[tool] = file
			}
		}
	}

	return configFiles
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
func (d *BazelDetector) GetTools(_ string) map[config.ToolType][]string {
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
func (d *MakeDetector) GetTools(_ string) map[config.ToolType][]string {
	tools := make(map[config.ToolType][]string)

	// Make provides targets for different operations
	tools[config.ToolTypeFormat] = []string{"make format"}
	tools[config.ToolTypeLint] = []string{"make lint"}
	tools[config.ToolTypeTest] = []string{"make test"}

	return tools
}
