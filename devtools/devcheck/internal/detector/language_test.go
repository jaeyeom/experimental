package detector

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestGoDetector_DetectLanguage(t *testing.T) {
	// Create temporary directory for test
	tempDir, err := os.MkdirTemp("", "go_detector_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name     string
		files    []string
		expected bool
	}{
		{
			name:     "go project with go.mod",
			files:    []string{"go.mod", "main.go"},
			expected: true,
		},
		{
			name:     "go files without go.mod",
			files:    []string{"main.go", "utils.go"},
			expected: false,
		},
		{
			name:     "no go files",
			files:    []string{"README.md", "Makefile"},
			expected: false,
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

			// Create test files
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			detector := NewGoDetector()
			result, err := detector.DetectLanguage(testDir)
			if err != nil {
				t.Errorf("DetectLanguage() error = %v", err)
				return
			}

			if result != tt.expected {
				t.Errorf("DetectLanguage() = %v, expected %v", result, tt.expected)
			}
		})
	}
}

func TestGoDetector_GetTools(t *testing.T) {
	detector := NewGoDetector()

	// Create temporary directory for test
	tempDir, err := os.MkdirTemp("", "go_tools_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tools := detector.GetTools(tempDir)

	// Should have format and lint tools
	if len(tools[config.ToolTypeFormat]) == 0 {
		t.Error("Expected format tools for Go")
	}

	if len(tools[config.ToolTypeLint]) == 0 {
		t.Error("Expected lint tools for Go")
	}

	// Check for specific tools
	formatTools := tools[config.ToolTypeFormat]
	found := false
	for _, tool := range formatTools {
		if tool == "gofumpt" || tool == "gofmt" {
			found = true
			break
		}
	}
	if !found {
		t.Error("Expected gofumpt or gofmt in format tools")
	}
}

func TestPythonDetector_DetectLanguage(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "python_detector_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name     string
		files    []string
		expected bool
	}{
		{
			name:     "python project with pyproject.toml",
			files:    []string{"pyproject.toml", "main.py"},
			expected: true,
		},
		{
			name:     "python project with requirements.txt",
			files:    []string{"requirements.txt", "script.py"},
			expected: true,
		},
		{
			name:     "python files only",
			files:    []string{"main.py", "utils.py"},
			expected: true,
		},
		{
			name:     "no python files",
			files:    []string{"README.md", "Makefile"},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			detector := NewPythonDetector()
			result, err := detector.DetectLanguage(testDir)
			if err != nil {
				t.Errorf("DetectLanguage() error = %v", err)
				return
			}

			if result != tt.expected {
				t.Errorf("DetectLanguage() = %v, expected %v", result, tt.expected)
			}
		})
	}
}

func TestTypeScriptDetector_DetectLanguage(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "ts_detector_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name     string
		files    []string
		expected bool
	}{
		{
			name:     "typescript project",
			files:    []string{"tsconfig.json", "src/main.ts"},
			expected: true,
		},
		{
			name:     "typescript files without tsconfig",
			files:    []string{"main.ts", "utils.ts"},
			expected: false,
		},
		{
			name:     "no typescript files",
			files:    []string{"README.md", "package.json"},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create subdirectories if needed
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				dir := filepath.Dir(filePath)
				if dir != testDir {
					err := os.MkdirAll(dir, 0o755)
					if err != nil {
						t.Fatal(err)
					}
				}
				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			detector := NewTypeScriptDetector()
			result, err := detector.DetectLanguage(testDir)
			if err != nil {
				t.Errorf("DetectLanguage() error = %v", err)
				return
			}

			if result != tt.expected {
				t.Errorf("DetectLanguage() = %v, expected %v", result, tt.expected)
			}
		})
	}
}
