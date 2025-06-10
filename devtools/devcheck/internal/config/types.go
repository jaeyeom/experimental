// Package config defines the core types and interfaces for the DevCheck tool.
package config

import "time"

// BuildSystem represents the detected build system type.
type BuildSystem string

const (
	BuildSystemBazel BuildSystem = "bazel"
	BuildSystemMake  BuildSystem = "make"
	BuildSystemNone  BuildSystem = "none"
)

// Language represents a detected programming language.
type Language string

const (
	LanguageGo         Language = "go"
	LanguagePython     Language = "python"
	LanguageTypeScript Language = "typescript"
	LanguageJavaScript Language = "javascript"
)

// ToolType represents the type of development tool.
type ToolType string

const (
	ToolTypeFormat ToolType = "format"
	ToolTypeLint   ToolType = "lint"
	ToolTypeTest   ToolType = "test"
)

// ProjectConfig contains the detected repository metadata and tools.
type ProjectConfig struct {
	// RootPath is the absolute path to the repository root
	RootPath string

	// BuildSystem is the primary build system detected
	BuildSystem BuildSystem

	// Languages contains all detected programming languages
	Languages []Language

	// Tools maps tool types to their detected commands
	Tools map[ToolType][]string

	// ConfigFiles maps languages/tools to their configuration files
	ConfigFiles map[string]string

	// HasGit indicates if the repository is a git repository
	HasGit bool

	// DetectionTime records when the detection was performed
	DetectionTime time.Time
}
