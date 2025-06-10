package config

import (
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
