// Package detector provides the main project detection functionality.
package detector

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// directoryScanner walks a project tree once per Detect call.
type directoryScanner interface {
	Scan(rootPath string) (*ScanResult, error)
}

// ProjectDetector implements the main project detection logic.
type ProjectDetector struct {
	scanner        directoryScanner
	patternMatcher *PatternMatcher
	languages      []languageProfile
	buildTools     map[config.BuildSystem]func(string) map[config.ToolType][]config.Tool
}

// NewProjectDetector creates a project detector with the default language table and scanners.
func NewProjectDetector() *ProjectDetector {
	return &ProjectDetector{
		scanner:        NewScanner(DefaultScanOptions()),
		patternMatcher: NewPatternMatcher(),
		languages:      languageProfiles,
		buildTools: map[config.BuildSystem]func(string) map[config.ToolType][]config.Tool{
			config.BuildSystemBazel: bazelTools,
			config.BuildSystemMake:  makeTools,
		},
	}
}

// Detect analyzes the given path and returns project configuration.
func (d *ProjectDetector) Detect(rootPath string) (*config.ProjectConfig, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve absolute path: %w", err)
	}

	scanResult, err := d.scanner.Scan(absPath)
	if err != nil {
		return nil, fmt.Errorf("scan project: %w", err)
	}
	if err := scanErrors(scanResult); err != nil {
		return nil, err
	}

	languages := d.patternMatcher.MatchLanguages(scanResult.Files)
	buildSystem := selectBuildSystem(absPath, d.patternMatcher.MatchBuildSystemsAtChosenDepth(scanResult.Files))
	hasGit, err := detectGitRepository(absPath)
	if err != nil {
		return nil, err
	}

	cfg := &config.ProjectConfig{
		RootPath:      absPath,
		BuildSystem:   buildSystem,
		Languages:     languages,
		Tools:         d.aggregateTools(languages, buildSystem, absPath, scanResult),
		ConfigFiles:   d.collectConfigFiles(languages, scanResult),
		HasGit:        hasGit,
		DetectionTime: time.Now(),
	}
	if err := cfg.Validate(); err != nil {
		return nil, fmt.Errorf("invalid project config: %w", err)
	}
	return cfg, nil
}

func scanErrors(result *ScanResult) error {
	if result == nil || len(result.Errors) == 0 {
		return nil
	}
	return fmt.Errorf("scan errors: %w", errors.Join(result.Errors...))
}

func detectGitRepository(absPath string) (bool, error) {
	_, err := os.Stat(filepath.Join(absPath, ".git"))
	if err == nil {
		return true, nil
	}
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	return false, fmt.Errorf("inspect git metadata: %w", err)
}

func (d *ProjectDetector) aggregateTools(languages []config.Language, buildSystem config.BuildSystem, absPath string, scan *ScanResult) map[config.ToolType][]config.Tool {
	tools := make(map[config.ToolType][]config.Tool)
	d.addLanguageTools(tools, languages, absPath, scan)
	d.addBuildSystemTools(tools, buildSystem, absPath)
	return tools
}

func (d *ProjectDetector) addLanguageTools(tools map[config.ToolType][]config.Tool, languages []config.Language, absPath string, scan *ScanResult) {
	for _, profile := range d.languages {
		if !hasLanguage(languages, profile.language) {
			continue
		}
		for toolType, toolList := range profile.tools(absPath, scan) {
			tools[toolType] = append(tools[toolType], toolList...)
		}
	}
}

func (d *ProjectDetector) addBuildSystemTools(tools map[config.ToolType][]config.Tool, buildSystem config.BuildSystem, absPath string) {
	lookup, exists := d.buildTools[buildSystem]
	if !exists {
		return
	}
	for toolType, toolList := range lookup(absPath) {
		tools[toolType] = append(toolList, tools[toolType]...)
	}
}

func (d *ProjectDetector) collectConfigFiles(languages []config.Language, scan *ScanResult) map[string]string {
	configFiles := make(map[string]string)
	for _, profile := range d.languages {
		if !hasLanguage(languages, profile.language) {
			continue
		}
		for _, key := range profile.configKeys {
			matches := d.patternMatcher.MatchConfigFiles(scan.Files, key)
			if len(matches) > 0 {
				configFiles[key] = matches[0]
			}
		}
		if profile.extraConfig != nil {
			profile.extraConfig(scan, configFiles)
		}
	}
	return configFiles
}

// SupportedLanguages returns the list of languages this detector supports.
func (d *ProjectDetector) SupportedLanguages() []config.Language {
	languages := make([]config.Language, 0, len(d.languages))
	for _, profile := range d.languages {
		languages = append(languages, profile.language)
	}
	return languages
}

// SupportedBuildSystems returns the list of build systems this detector supports.
func (d *ProjectDetector) SupportedBuildSystems() []config.BuildSystem {
	buildSystems := make([]config.BuildSystem, 0, len(d.buildTools))
	for bs := range d.buildTools {
		buildSystems = append(buildSystems, bs)
	}
	return buildSystems
}

func bazelTools(rootPath string) map[config.ToolType][]config.Tool {
	var format, lint []config.Tool
	for _, label := range bazelFormatLabels {
		if bazelLabelExists(rootPath, label) {
			format = append(format, config.Tool{Command: "bazel", Args: []string{"run", label}})
		}
	}
	for _, label := range bazelLintLabels {
		if bazelLabelExists(rootPath, label) {
			lint = append(lint, config.Tool{Command: "bazel", Args: []string{"run", label}})
		}
	}
	return map[config.ToolType][]config.Tool{
		config.ToolTypeFormat: availableTools(format...),
		config.ToolTypeLint:   availableTools(lint...),
		config.ToolTypeTest:   availableTools(config.Tool{Command: "bazel", Args: []string{"test", "//..."}}),
	}
}

func makeTools(rootPath string) map[config.ToolType][]config.Tool {
	tools := make(map[config.ToolType][]config.Tool)
	targets := []struct {
		toolType config.ToolType
		target   string
	}{
		{config.ToolTypeFormat, "format"},
		{config.ToolTypeLint, "lint"},
		{config.ToolTypeTest, "test"},
	}
	for _, item := range targets {
		if makefileHasTarget(rootPath, item.target) {
			tools[item.toolType] = availableTools(config.Tool{Command: "make", Args: []string{item.target}})
		}
	}
	return tools
}
