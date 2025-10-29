package config

import (
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestProjectConfig_Initialization(t *testing.T) {
	tests := []struct {
		name string
		pc   ProjectConfig
		want bool
	}{
		{
			name: "empty config",
			pc:   ProjectConfig{},
			want: true,
		},
		{
			name: "valid config",
			pc: ProjectConfig{
				RootPath:      "/test/path",
				BuildSystem:   BuildSystemBazel,
				Languages:     []Language{LanguageGo, LanguagePython},
				Tools:         map[ToolType][]string{ToolTypeFormat: {"gofumpt"}},
				ConfigFiles:   map[string]string{"golangci-lint": ".golangci.yml"},
				HasGit:        true,
				DetectionTime: time.Now(),
			},
			want: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Test that ProjectConfig can be created and basic fields accessed
			if tt.pc.BuildSystem == "" && tt.name == "empty config" {
				// Empty config should have default values
				if tt.pc.BuildSystem != BuildSystemNone && tt.pc.BuildSystem != "" {
					t.Errorf("Expected empty BuildSystem, got %v", tt.pc.BuildSystem)
				}
			}

			if tt.name == "valid config" {
				if tt.pc.RootPath != "/test/path" {
					t.Errorf("Expected RootPath /test/path, got %v", tt.pc.RootPath)
				}
				if tt.pc.BuildSystem != BuildSystemBazel {
					t.Errorf("Expected BuildSystem bazel, got %v", tt.pc.BuildSystem)
				}
				if len(tt.pc.Languages) != 2 {
					t.Errorf("Expected 2 languages, got %d", len(tt.pc.Languages))
				}
			}
		})
	}
}

func TestProjectConfig_Validate(t *testing.T) {
	tests := []struct {
		name    string
		pc      ProjectConfig
		wantErr bool
		errMsg  string
	}{
		{
			name: "valid config",
			pc: ProjectConfig{
				RootPath:    "/test/path",
				BuildSystem: BuildSystemBazel,
				Languages:   []Language{LanguageGo},
				Tools:       map[ToolType][]string{ToolTypeLint: {"golangci-lint"}},
			},
			wantErr: false,
		},
		{
			name:    "empty rootPath",
			pc:      ProjectConfig{},
			wantErr: true,
			errMsg:  "rootPath cannot be empty",
		},
		{
			name: "relative rootPath",
			pc: ProjectConfig{
				RootPath: "relative/path",
			},
			wantErr: true,
			errMsg:  "rootPath must be an absolute path",
		},
		{
			name: "invalid build system",
			pc: ProjectConfig{
				RootPath:    "/test/path",
				BuildSystem: "invalid",
			},
			wantErr: true,
			errMsg:  "invalid build system",
		},
		{
			name: "invalid language",
			pc: ProjectConfig{
				RootPath:  "/test/path",
				Languages: []Language{"invalid"},
			},
			wantErr: true,
			errMsg:  "invalid language",
		},
		{
			name: "invalid tool type",
			pc: ProjectConfig{
				RootPath: "/test/path",
				Tools:    map[ToolType][]string{"invalid": {"tool"}},
			},
			wantErr: true,
			errMsg:  "invalid tool type",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.pc.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("Validate() error = %v, want error containing %v", err, tt.errMsg)
			}
		})
	}
}

func TestProjectConfig_JSON(t *testing.T) {
	pc := ProjectConfig{
		RootPath:      "/test/path",
		BuildSystem:   BuildSystemBazel,
		Languages:     []Language{LanguageGo, LanguagePython},
		Tools:         map[ToolType][]string{ToolTypeFormat: {"gofumpt"}},
		ConfigFiles:   map[string]string{"golangci-lint": ".golangci.yml"},
		HasGit:        true,
		DetectionTime: time.Date(2024, 1, 1, 12, 0, 0, 0, time.UTC),
	}

	// Test marshaling
	data, err := json.Marshal(pc)
	if err != nil {
		t.Fatalf("Failed to marshal ProjectConfig: %v", err)
	}

	// Test unmarshaling
	var pc2 ProjectConfig
	if err := json.Unmarshal(data, &pc2); err != nil {
		t.Fatalf("Failed to unmarshal ProjectConfig: %v", err)
	}

	// Verify fields
	if pc2.RootPath != pc.RootPath {
		t.Errorf("RootPath mismatch: got %v, want %v", pc2.RootPath, pc.RootPath)
	}
	if pc2.BuildSystem != pc.BuildSystem {
		t.Errorf("BuildSystem mismatch: got %v, want %v", pc2.BuildSystem, pc.BuildSystem)
	}
	if len(pc2.Languages) != len(pc.Languages) {
		t.Errorf("Languages length mismatch: got %d, want %d", len(pc2.Languages), len(pc.Languages))
	}
}

func TestBuildSystemConstants(t *testing.T) {
	tests := []struct {
		name   string
		bs     BuildSystem
		expect string
	}{
		{"bazel", BuildSystemBazel, "bazel"},
		{"make", BuildSystemMake, "make"},
		{"none", BuildSystemNone, "none"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.bs) != tt.expect {
				t.Errorf("Expected %s, got %s", tt.expect, string(tt.bs))
			}
		})
	}
}

func TestLanguageConstants(t *testing.T) {
	tests := []struct {
		name   string
		lang   Language
		expect string
	}{
		{"go", LanguageGo, "go"},
		{"python", LanguagePython, "python"},
		{"typescript", LanguageTypeScript, "typescript"},
		{"javascript", LanguageJavaScript, "javascript"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.lang) != tt.expect {
				t.Errorf("Expected %s, got %s", tt.expect, string(tt.lang))
			}
		})
	}
}

func TestToolTypeConstants(t *testing.T) {
	tests := []struct {
		name   string
		tool   ToolType
		expect string
	}{
		{"format", ToolTypeFormat, "format"},
		{"lint", ToolTypeLint, "lint"},
		{"test", ToolTypeTest, "test"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.tool) != tt.expect {
				t.Errorf("Expected %s, got %s", tt.expect, string(tt.tool))
			}
		})
	}
}

func TestSeverityConstants(t *testing.T) {
	tests := []struct {
		name   string
		sev    Severity
		expect string
	}{
		{"error", SeverityError, "error"},
		{"warning", SeverityWarning, "warning"},
		{"info", SeverityInfo, "info"},
		{"hint", SeverityHint, "hint"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.sev) != tt.expect {
				t.Errorf("Expected %s, got %s", tt.expect, string(tt.sev))
			}
		})
	}
}

func TestIssue_Validate(t *testing.T) {
	tests := []struct {
		name    string
		issue   Issue
		wantErr bool
		errMsg  string
	}{
		{
			name: "valid issue",
			issue: Issue{
				FilePath: "/test/file.go",
				Line:     10,
				Column:   5,
				Severity: SeverityError,
				Message:  "syntax error",
				ToolName: "golangci-lint",
			},
			wantErr: false,
		},
		{
			name:    "empty filePath",
			issue:   Issue{},
			wantErr: true,
			errMsg:  "filePath cannot be empty",
		},
		{
			name: "negative line",
			issue: Issue{
				FilePath: "/test/file.go",
				Line:     -1,
			},
			wantErr: true,
			errMsg:  "line cannot be negative",
		},
		{
			name: "negative column",
			issue: Issue{
				FilePath: "/test/file.go",
				Column:   -1,
			},
			wantErr: true,
			errMsg:  "column cannot be negative",
		},
		{
			name: "endLine before line",
			issue: Issue{
				FilePath: "/test/file.go",
				Line:     10,
				EndLine:  5,
				Severity: SeverityError,
				Message:  "test",
				ToolName: "test",
			},
			wantErr: true,
			errMsg:  "endLine cannot be before line",
		},
		{
			name: "endColumn before column on same line",
			issue: Issue{
				FilePath:  "/test/file.go",
				Line:      10,
				Column:    10,
				EndLine:   10,
				EndColumn: 5,
				Severity:  SeverityError,
				Message:   "test",
				ToolName:  "test",
			},
			wantErr: true,
			errMsg:  "endColumn cannot be before column on the same line",
		},
		{
			name: "empty severity",
			issue: Issue{
				FilePath: "/test/file.go",
				Message:  "test",
				ToolName: "test",
			},
			wantErr: true,
			errMsg:  "severity cannot be empty",
		},
		{
			name: "invalid severity",
			issue: Issue{
				FilePath: "/test/file.go",
				Severity: "invalid",
				Message:  "test",
				ToolName: "test",
			},
			wantErr: true,
			errMsg:  "invalid severity",
		},
		{
			name: "empty message",
			issue: Issue{
				FilePath: "/test/file.go",
				Severity: SeverityError,
				ToolName: "test",
			},
			wantErr: true,
			errMsg:  "message cannot be empty",
		},
		{
			name: "empty toolName",
			issue: Issue{
				FilePath: "/test/file.go",
				Severity: SeverityError,
				Message:  "test",
			},
			wantErr: true,
			errMsg:  "toolName cannot be empty",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.issue.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("Validate() error = %v, want error containing %v", err, tt.errMsg)
			}
		})
	}
}

func TestIssue_String(t *testing.T) {
	tests := []struct {
		name  string
		issue Issue
		want  string
	}{
		{
			name: "issue with line and column",
			issue: Issue{
				FilePath: "/test/file.go",
				Line:     10,
				Column:   5,
				Severity: SeverityError,
				Message:  "syntax error",
				ToolName: "golangci-lint",
			},
			want: "/test/file.go:10:5: error: syntax error [golangci-lint]",
		},
		{
			name: "issue with line only",
			issue: Issue{
				FilePath: "/test/file.go",
				Line:     10,
				Severity: SeverityWarning,
				Message:  "unused variable",
				ToolName: "go vet",
			},
			want: "/test/file.go:10: warning: unused variable [go vet]",
		},
		{
			name: "issue without line",
			issue: Issue{
				FilePath: "/test/file.go",
				Severity: SeverityInfo,
				Message:  "file too long",
				ToolName: "custom-linter",
			},
			want: "/test/file.go: info: file too long [custom-linter]",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.issue.String()
			if got != tt.want {
				t.Errorf("String() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestIssue_JSON(t *testing.T) {
	issue := Issue{
		FilePath:     "/test/file.go",
		Line:         10,
		Column:       5,
		EndLine:      12,
		EndColumn:    8,
		Severity:     SeverityError,
		Message:      "syntax error",
		Code:         "E001",
		SuggestedFix: "Add semicolon",
		ToolName:     "golangci-lint",
		Category:     "syntax",
	}

	// Test marshaling
	data, err := json.Marshal(issue)
	if err != nil {
		t.Fatalf("Failed to marshal Issue: %v", err)
	}

	// Test unmarshaling
	var issue2 Issue
	if err := json.Unmarshal(data, &issue2); err != nil {
		t.Fatalf("Failed to unmarshal Issue: %v", err)
	}

	// Verify fields
	if issue2.FilePath != issue.FilePath {
		t.Errorf("FilePath mismatch: got %v, want %v", issue2.FilePath, issue.FilePath)
	}
	if issue2.Line != issue.Line {
		t.Errorf("Line mismatch: got %v, want %v", issue2.Line, issue.Line)
	}
	if issue2.Severity != issue.Severity {
		t.Errorf("Severity mismatch: got %v, want %v", issue2.Severity, issue.Severity)
	}
	if issue2.Message != issue.Message {
		t.Errorf("Message mismatch: got %v, want %v", issue2.Message, issue.Message)
	}
}
