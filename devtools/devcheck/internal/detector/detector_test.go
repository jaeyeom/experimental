package detector

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestProjectDetector_Detect(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "project_detector_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name              string
		files             []string
		expectedLanguages []config.Language
		expectedBuild     config.BuildSystem
		expectedHasGit    bool
	}{
		{
			name:              "go project with bazel",
			files:             []string{"go.mod", "main.go", "MODULE.bazel", "BUILD.bazel"},
			expectedLanguages: []config.Language{config.LanguageGo},
			expectedBuild:     config.BuildSystemBazel,
			expectedHasGit:    false,
		},
		{
			name:              "python project with make",
			files:             []string{"pyproject.toml", "main.py", "Makefile"},
			expectedLanguages: []config.Language{config.LanguagePython},
			expectedBuild:     config.BuildSystemMake,
			expectedHasGit:    false,
		},
		{
			name:              "mixed project with git",
			files:             []string{"go.mod", "main.go", "pyproject.toml", "script.py", ".git/HEAD"},
			expectedLanguages: []config.Language{config.LanguageGo, config.LanguagePython},
			expectedBuild:     config.BuildSystemNone,
			expectedHasGit:    true,
		},
		{
			name:              "typescript project",
			files:             []string{"tsconfig.json", "package.json", "src/main.ts"},
			expectedLanguages: []config.Language{config.LanguageTypeScript, config.LanguageJavaScript},
			expectedBuild:     config.BuildSystemNone,
			expectedHasGit:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test directory
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create test files and directories
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				dir := filepath.Dir(filePath)
				if dir != testDir {
					err := os.MkdirAll(dir, 0o755)
					if err != nil {
						t.Fatal(err)
					}
				}

				if filepath.Base(file) == "HEAD" {
					// Create git HEAD file
					err := os.WriteFile(filePath, []byte("ref: refs/heads/main"), 0o600)
					if err != nil {
						t.Fatal(err)
					}
				} else {
					err := os.WriteFile(filePath, []byte("test content"), 0o600)
					if err != nil {
						t.Fatal(err)
					}
				}
			}

			detector := NewProjectDetector()
			result, err := detector.Detect(testDir)
			if err != nil {
				t.Errorf("Detect() error = %v", err)
				return
			}

			// Check root path
			if result.RootPath != testDir {
				t.Errorf("Expected RootPath %s, got %s", testDir, result.RootPath)
			}

			// Check build system
			if result.BuildSystem != tt.expectedBuild {
				t.Errorf("Expected BuildSystem %v, got %v", tt.expectedBuild, result.BuildSystem)
			}

			// Check languages
			if len(result.Languages) != len(tt.expectedLanguages) {
				t.Errorf("Expected %d languages, got %d: %v", len(tt.expectedLanguages), len(result.Languages), result.Languages)
			} else {
				expectedMap := make(map[config.Language]bool)
				for _, lang := range tt.expectedLanguages {
					expectedMap[lang] = true
				}

				for _, lang := range result.Languages {
					if !expectedMap[lang] {
						t.Errorf("Unexpected language detected: %v", lang)
					}
					delete(expectedMap, lang)
				}

				if len(expectedMap) > 0 {
					t.Errorf("Missing expected languages: %v", expectedMap)
				}
			}

			// Check git detection
			if result.HasGit != tt.expectedHasGit {
				t.Errorf("Expected HasGit %v, got %v", tt.expectedHasGit, result.HasGit)
			}

			// Check that tools are populated
			if len(result.Tools) == 0 {
				t.Error("Expected tools to be populated")
			}

			// Check that detection time is set
			if result.DetectionTime.IsZero() {
				t.Error("Expected DetectionTime to be set")
			}
		})
	}
}

func TestProjectDetector_SupportedLanguages(t *testing.T) {
	detector := NewProjectDetector()

	languages := detector.SupportedLanguages()

	expected := []config.Language{
		config.LanguageGo,
		config.LanguagePython,
		config.LanguageTypeScript,
		config.LanguageJavaScript,
	}

	if len(languages) != len(expected) {
		t.Errorf("Expected %d supported languages, got %d", len(expected), len(languages))
	}

	expectedMap := make(map[config.Language]bool)
	for _, lang := range expected {
		expectedMap[lang] = true
	}

	for _, lang := range languages {
		if !expectedMap[lang] {
			t.Errorf("Unexpected supported language: %v", lang)
		}
	}
}

func TestProjectDetector_SupportedBuildSystems(t *testing.T) {
	detector := NewProjectDetector()

	buildSystems := detector.SupportedBuildSystems()

	expected := []config.BuildSystem{
		config.BuildSystemBazel,
		config.BuildSystemMake,
	}

	if len(buildSystems) != len(expected) {
		t.Errorf("Expected %d supported build systems, got %d", len(expected), len(buildSystems))
	}

	expectedMap := make(map[config.BuildSystem]bool)
	for _, bs := range expected {
		expectedMap[bs] = true
	}

	for _, bs := range buildSystems {
		if !expectedMap[bs] {
			t.Errorf("Unexpected supported build system: %v", bs)
		}
	}
}
