// Package config defines the core types and interfaces for the DevCheck tool.
package config

import (
	"fmt"
	"path/filepath"
	"time"
)

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

// Severity represents the severity level of an issue.
type Severity string

const (
	SeverityError   Severity = "error"
	SeverityWarning Severity = "warning"
	SeverityInfo    Severity = "info"
	SeverityHint    Severity = "hint"
)

// ProjectConfig contains the detected repository metadata and tools.
type ProjectConfig struct {
	// RootPath is the absolute path to the repository root
	RootPath string `json:"rootPath"`

	// BuildSystem is the primary build system detected
	BuildSystem BuildSystem `json:"buildSystem"`

	// Languages contains all detected programming languages
	Languages []Language `json:"languages"`

	// Tools maps tool types to their detected commands
	Tools map[ToolType][]string `json:"tools"`

	// ConfigFiles maps languages/tools to their configuration files
	ConfigFiles map[string]string `json:"configFiles"`

	// HasGit indicates if the repository is a git repository
	HasGit bool `json:"hasGit"`

	// DetectionTime records when the detection was performed
	DetectionTime time.Time `json:"detectionTime"`
}

// Validate ensures the ProjectConfig has valid data.
func (pc *ProjectConfig) Validate() error {
	if pc.RootPath == "" {
		return fmt.Errorf("rootPath cannot be empty")
	}

	// Ensure the path is absolute
	if !filepath.IsAbs(pc.RootPath) {
		return fmt.Errorf("rootPath must be an absolute path: %s", pc.RootPath)
	}

	// Validate build system
	switch pc.BuildSystem {
	case BuildSystemBazel, BuildSystemMake, BuildSystemNone, "":
		// Valid values
	default:
		return fmt.Errorf("invalid build system: %s", pc.BuildSystem)
	}

	// Validate languages
	for _, lang := range pc.Languages {
		switch lang {
		case LanguageGo, LanguagePython, LanguageTypeScript, LanguageJavaScript:
			// Valid values
		default:
			return fmt.Errorf("invalid language: %s", lang)
		}
	}

	// Validate tool types
	for toolType := range pc.Tools {
		switch toolType {
		case ToolTypeFormat, ToolTypeLint, ToolTypeTest:
			// Valid values
		default:
			return fmt.Errorf("invalid tool type: %s", toolType)
		}
	}

	return nil
}

// Issue represents a linting or formatting problem found in the code.
type Issue struct {
	// FilePath is the path to the file containing the issue
	FilePath string `json:"filePath"`

	// Line is the line number where the issue occurs (1-based, 0 means unknown)
	Line int `json:"line"`

	// Column is the column number where the issue occurs (1-based, 0 means unknown)
	Column int `json:"column"`

	// EndLine is the ending line number for multi-line issues (optional)
	EndLine int `json:"endLine,omitempty"`

	// EndColumn is the ending column number (optional)
	EndColumn int `json:"endColumn,omitempty"`

	// Severity is the severity level of the issue
	Severity Severity `json:"severity"`

	// Message is the description of the issue
	Message string `json:"message"`

	// Code is the error/warning code (if applicable)
	Code string `json:"code,omitempty"`

	// SuggestedFix is a suggested fix for the issue (optional)
	SuggestedFix string `json:"suggestedFix,omitempty"`

	// ToolName is the name of the tool that reported this issue
	ToolName string `json:"toolName"`

	// Category is the category of the issue (e.g., "style", "bug", "performance")
	Category string `json:"category,omitempty"`
}

// Validate ensures the Issue has valid data.
func (i *Issue) Validate() error {
	if err := i.validatePositions(); err != nil {
		return err
	}

	if err := i.validateRequiredFields(); err != nil {
		return err
	}

	return i.validateSeverity()
}

func (i *Issue) validateRequiredFields() error {
	if i.FilePath == "" {
		return fmt.Errorf("filePath cannot be empty")
	}

	if i.Message == "" {
		return fmt.Errorf("message cannot be empty")
	}

	if i.ToolName == "" {
		return fmt.Errorf("toolName cannot be empty")
	}

	return nil
}

func (i *Issue) validatePositions() error {
	if err := i.validateNonNegativePositions(); err != nil {
		return err
	}

	return i.validateEndPositions()
}

func (i *Issue) validateNonNegativePositions() error {
	if i.Line < 0 {
		return fmt.Errorf("line cannot be negative")
	}

	if i.Column < 0 {
		return fmt.Errorf("column cannot be negative")
	}

	if i.EndLine < 0 {
		return fmt.Errorf("endLine cannot be negative")
	}

	if i.EndColumn < 0 {
		return fmt.Errorf("endColumn cannot be negative")
	}

	return nil
}

func (i *Issue) validateEndPositions() error {
	if i.EndLine > 0 && i.EndLine < i.Line {
		return fmt.Errorf("endLine cannot be before line")
	}

	if i.EndLine == i.Line && i.EndColumn > 0 && i.EndColumn < i.Column {
		return fmt.Errorf("endColumn cannot be before column on the same line")
	}

	return nil
}

func (i *Issue) validateSeverity() error {
	switch i.Severity {
	case SeverityError, SeverityWarning, SeverityInfo, SeverityHint:
		return nil
	case "":
		return fmt.Errorf("severity cannot be empty")
	default:
		return fmt.Errorf("invalid severity: %s", i.Severity)
	}
}

// String returns a human-readable representation of the issue.
func (i *Issue) String() string {
	location := i.FilePath
	if i.Line > 0 {
		location = fmt.Sprintf("%s:%d", location, i.Line)
		if i.Column > 0 {
			location = fmt.Sprintf("%s:%d", location, i.Column)
		}
	}

	return fmt.Sprintf("%s: %s: %s [%s]", location, i.Severity, i.Message, i.ToolName)
}
