package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestDetectAndPrint(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "devcheck_cli_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name           string
		files          []string
		expectedOutput []string
	}{
		{
			name:  "go project with bazel",
			files: []string{"go.mod", "main.go", "MODULE.bazel"},
			expectedOutput: []string{
				"Languages: go",
				"Build System: bazel",
				"Tools:",
				"  format: bazel run",
				"  lint: bazel run",
				"  test: bazel test",
			},
		},
		{
			name:  "python project",
			files: []string{"pyproject.toml", "main.py", "requirements.txt"},
			expectedOutput: []string{
				"Languages: python",
				"Build System: none",
				"Tools:",
				"  format: ruff format",
				"  lint: ruff check",
			},
		},
		{
			name:  "mixed project with make",
			files: []string{"go.mod", "main.go", "requirements.txt", "main.py", "Makefile"},
			expectedOutput: []string{
				"Languages: go, python",
				"Build System: make",
				"Tools:",
				"  format: make format",
				"  lint: make lint",
				"  test: make test",
			},
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

			// Capture output
			var buf bytes.Buffer
			err = detectAndPrint(testDir, &buf)
			if err != nil {
				t.Errorf("detectAndPrint() error = %v", err)
				return
			}

			output := buf.String()

			// Check that expected strings are in the output
			for _, expected := range tt.expectedOutput {
				if !strings.Contains(output, expected) {
					t.Errorf("Expected output to contain %q, but got:\n%s", expected, output)
				}
			}
		})
	}
}

func TestDetectAndPrintInvalidPath(t *testing.T) {
	var buf bytes.Buffer
	err := detectAndPrint("/path/that/does/not/exist", &buf)
	if err == nil {
		t.Error("Expected error for non-existent path")
	}
}

func TestDetectAndPrintCurrentDirectory(t *testing.T) {
	// Test that we can run detection on current directory without error
	// Get current working directory for testing
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	err = detectAndPrint(wd, &buf)
	if err != nil {
		t.Errorf("detectAndPrint() on current directory error = %v", err)
	}

	output := buf.String()
	if output == "" {
		t.Error("Expected some output for current directory")
	}

	// Should contain basic structure
	if !strings.Contains(output, "Path:") {
		t.Error("Expected output to contain 'Path:'")
	}
}
