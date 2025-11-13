package main

import (
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestReplaceInFile(t *testing.T) {
	tests := []struct {
		name        string
		content     string
		oldKey      string
		newKey      string
		expected    string
		shouldError bool
	}{
		{
			name:     "basic replacement",
			content:  "This is about PROJ-123 and other stuff.",
			oldKey:   "PROJ-123",
			newKey:   "PROJ-456",
			expected: "This is about PROJ-456 and other stuff.",
		},
		{
			name:     "multiple occurrences",
			content:  "PROJ-123 is related to PROJ-123 task.",
			oldKey:   "PROJ-123",
			newKey:   "PROJ-456",
			expected: "PROJ-456 is related to PROJ-456 task.",
		},
		{
			name:     "word boundary protection",
			content:  "MYPROJ-123 and PROJ-123 are different.",
			oldKey:   "PROJ-123",
			newKey:   "PROJ-456",
			expected: "MYPROJ-123 and PROJ-456 are different.",
		},
		{
			name:        "no matches",
			content:     "This file has no matching keys.",
			oldKey:      "PROJ-123",
			newKey:      "PROJ-456",
			expected:    "This file has no matching keys.",
			shouldError: true,
		},
		{
			name:     "case sensitive",
			content:  "PROJ-123 and proj-123 are different.",
			oldKey:   "PROJ-123",
			newKey:   "PROJ-456",
			expected: "PROJ-456 and proj-123 are different.",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary file
			tmpDir := t.TempDir()
			tmpFile := filepath.Join(tmpDir, "test.md")

			err := os.WriteFile(tmpFile, []byte(tt.content), 0o600)
			if err != nil {
				t.Fatalf("Failed to create test file: %v", err)
			}

			// Test the replacement
			err = replaceInFile(tmpFile, tt.oldKey, tt.newKey)

			if tt.shouldError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
				return
			}

			if err != nil {
				t.Errorf("Unexpected error: %v", err)
				return
			}

			// Check the result
			result, err := os.ReadFile(tmpFile)
			if err != nil {
				t.Fatalf("Failed to read result file: %v", err)
			}

			if string(result) != tt.expected {
				t.Errorf("Expected:\n%s\nGot:\n%s", tt.expected, string(result))
			}
		})
	}
}

func TestReplaceInFileNotFound(t *testing.T) {
	err := replaceInFile("nonexistent.md", "OLD", "NEW")
	if err == nil {
		t.Error("Expected error for nonexistent file")
	}
	if !errors.Is(err, fs.ErrNotExist) {
		t.Errorf("Expected file not found error, got: %v", err)
	}
}

func TestRenameDocFile(t *testing.T) {
	tests := []struct {
		name        string
		setupFunc   func(string) error
		docsDir     string
		oldKey      string
		newKey      string
		expectError bool
		errorMsg    string
	}{
		{
			name: "successful rename",
			setupFunc: func(dir string) error {
				return os.WriteFile(filepath.Join(dir, "PROJ-123.md"), []byte("content"), 0o600)
			},
			docsDir: "",
			oldKey:  "PROJ-123",
			newKey:  "PROJ-456",
		},
		{
			name: "source file not found",
			setupFunc: func(_ string) error {
				return nil // No file created
			},
			docsDir:     "",
			oldKey:      "PROJ-123",
			newKey:      "PROJ-456",
			expectError: true,
			errorMsg:    "no markdown files found",
		},
		{
			name: "destination file exists",
			setupFunc: func(dir string) error {
				if err := os.WriteFile(filepath.Join(dir, "PROJ-123.md"), []byte("old content"), 0o600); err != nil {
					return fmt.Errorf("failed to create old file: %w", err)
				}
				if err := os.WriteFile(filepath.Join(dir, "PROJ-456.md"), []byte("existing content"), 0o600); err != nil {
					return fmt.Errorf("failed to create existing file: %w", err)
				}
				return nil
			},
			docsDir:     "",
			oldKey:      "PROJ-123",
			newKey:      "PROJ-456",
			expectError: true,
			errorMsg:    "already exists",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tmpDir := t.TempDir()
			docsDir := tmpDir
			if tt.docsDir != "" {
				docsDir = filepath.Join(tmpDir, tt.docsDir)
				if err := os.MkdirAll(docsDir, 0o755); err != nil {
					t.Fatalf("Failed to create docs directory: %v", err)
				}
			}

			if err := tt.setupFunc(docsDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			err := renameDocFile(docsDir, tt.oldKey, tt.newKey)

			if tt.expectError {
				if err == nil {
					t.Error("Expected error but got none")
				} else if !strings.Contains(err.Error(), tt.errorMsg) {
					t.Errorf("Expected error containing %q, got: %v", tt.errorMsg, err)
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
				// Verify the file was renamed
				newFile := filepath.Join(docsDir, tt.newKey+".md")
				if _, err := os.Stat(newFile); err != nil {
					t.Errorf("New file %s should exist", newFile)
				}
				oldFile := filepath.Join(docsDir, tt.oldKey+".md")
				if _, err := os.Stat(oldFile); !errors.Is(err, fs.ErrNotExist) {
					t.Errorf("Old file %s should not exist", oldFile)
				}
			}
		})
	}
}

func TestRunRename(t *testing.T) {
	tmpDir := t.TempDir()

	// Create test directory structure
	docsDir := filepath.Join(tmpDir, "docs", "project")
	err := os.MkdirAll(docsDir, 0o755)
	if err != nil {
		t.Fatalf("Failed to create docs directory: %v", err)
	}

	// Create test files
	planFile := filepath.Join(tmpDir, "plan.md")
	planContent := "# Project Plan\nThis covers PROJ-123 implementation.\nPROJ-123 is important."
	err = os.WriteFile(planFile, []byte(planContent), 0o600)
	if err != nil {
		t.Fatalf("Failed to create plan file: %v", err)
	}

	docFile1 := filepath.Join(docsDir, "feature.md")
	docContent1 := "# Feature\nRelated to PROJ-123."
	err = os.WriteFile(docFile1, []byte(docContent1), 0o600)
	if err != nil {
		t.Fatalf("Failed to create doc file 1: %v", err)
	}

	oldKeyFile := filepath.Join(docsDir, "PROJ-123.md")
	oldKeyContent := "# PROJ-123 Details\nThis is about PROJ-123."
	err = os.WriteFile(oldKeyFile, []byte(oldKeyContent), 0o600)
	if err != nil {
		t.Fatalf("Failed to create old key file: %v", err)
	}

	// Create config and run rename
	config := Config{
		OldKey:   "PROJ-123",
		NewKey:   "PROJ-456",
		PlanFile: planFile,
		DocsDir:  docsDir,
	}

	err = runRename(config)
	if err != nil {
		t.Fatalf("runRename failed: %v", err)
	}

	// Verify results
	planResult, err := os.ReadFile(planFile)
	if err != nil {
		t.Fatalf("Failed to read plan file: %v", err)
	}
	if !strings.Contains(string(planResult), "PROJ-456") {
		t.Error("Plan file should contain PROJ-456")
	}
	if strings.Contains(string(planResult), "PROJ-123") {
		t.Error("Plan file should not contain PROJ-123")
	}

	docResult1, err := os.ReadFile(docFile1)
	if err != nil {
		t.Fatalf("Failed to read doc file 1: %v", err)
	}
	if !strings.Contains(string(docResult1), "PROJ-456") {
		t.Error("Doc file 1 should contain PROJ-456")
	}

	newKeyFile := filepath.Join(docsDir, "PROJ-456.md")
	newKeyResult, err := os.ReadFile(newKeyFile)
	if err != nil {
		t.Fatalf("Failed to read new key file: %v", err)
	}
	if !strings.Contains(string(newKeyResult), "PROJ-456") {
		t.Error("New key file should contain PROJ-456")
	}

	// Verify old file is gone
	if _, err := os.Stat(oldKeyFile); !errors.Is(err, fs.ErrNotExist) {
		t.Error("Old key file should not exist")
	}
}

func TestParseFlags(t *testing.T) {
	tests := []struct {
		name     string
		args     []string
		expected Config
	}{
		{
			name: "basic flags using --old and --new",
			args: []string{"cmd", "--old", "PROJ-123", "--new", "PROJ-456"},
			expected: Config{
				OldKey:   "PROJ-123",
				NewKey:   "PROJ-456",
				PlanFile: "plan.md",
				DocsDir:  "docs/project",
			},
		},
		{
			name: "long form flags --old-key and --new-key",
			args: []string{"cmd", "--old-key", "DEV-100", "--new-key", "DEV-200"},
			expected: Config{
				OldKey:   "DEV-100",
				NewKey:   "DEV-200",
				PlanFile: "plan.md",
				DocsDir:  "docs/project",
			},
		},
		{
			name: "custom plan file using -p",
			args: []string{"cmd", "--old", "PROJ-1", "--new", "PROJ-2", "-p", "custom-plan.md"},
			expected: Config{
				OldKey:   "PROJ-1",
				NewKey:   "PROJ-2",
				PlanFile: "custom-plan.md",
				DocsDir:  "docs/project",
			},
		},
		{
			name: "custom plan file using --plan-file",
			args: []string{"cmd", "--old", "PROJ-1", "--new", "PROJ-2", "--plan-file", "roadmap.md"},
			expected: Config{
				OldKey:   "PROJ-1",
				NewKey:   "PROJ-2",
				PlanFile: "roadmap.md",
				DocsDir:  "docs/project",
			},
		},
		{
			name: "custom docs dir using -d",
			args: []string{"cmd", "--old", "A-1", "--new", "A-2", "-d", "documentation"},
			expected: Config{
				OldKey:   "A-1",
				NewKey:   "A-2",
				PlanFile: "plan.md",
				DocsDir:  "documentation",
			},
		},
		{
			name: "custom docs dir using --docs-dir",
			args: []string{"cmd", "--old", "A-1", "--new", "A-2", "--docs-dir", "docs/tickets"},
			expected: Config{
				OldKey:   "A-1",
				NewKey:   "A-2",
				PlanFile: "plan.md",
				DocsDir:  "docs/tickets",
			},
		},
		{
			name: "all custom options",
			args: []string{"cmd", "--old", "X-99", "--new", "Y-88", "-p", "my-plan.md", "-d", "my-docs"},
			expected: Config{
				OldKey:   "X-99",
				NewKey:   "Y-88",
				PlanFile: "my-plan.md",
				DocsDir:  "my-docs",
			},
		},
		{
			name: "mixed short and long flags",
			args: []string{"cmd", "-old", "PROJ-5", "-new", "PROJ-6", "--plan-file", "plan2.md", "-d", "docs/issues"},
			expected: Config{
				OldKey:   "PROJ-5",
				NewKey:   "PROJ-6",
				PlanFile: "plan2.md",
				DocsDir:  "docs/issues",
			},
		},
		{
			name: "keys with different formats",
			args: []string{"cmd", "--old", "OLDPROJ-123", "--new", "NEWPROJ-456"},
			expected: Config{
				OldKey:   "OLDPROJ-123",
				NewKey:   "NEWPROJ-456",
				PlanFile: "plan.md",
				DocsDir:  "docs/project",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Reset flag.CommandLine to avoid conflicts between tests
			flag.CommandLine = flag.NewFlagSet(tt.args[0], flag.ContinueOnError)

			// Save and restore os.Args
			oldArgs := os.Args
			defer func() { os.Args = oldArgs }()

			os.Args = tt.args

			config := parseFlags()

			if config.OldKey != tt.expected.OldKey {
				t.Errorf("OldKey: expected %q, got %q", tt.expected.OldKey, config.OldKey)
			}
			if config.NewKey != tt.expected.NewKey {
				t.Errorf("NewKey: expected %q, got %q", tt.expected.NewKey, config.NewKey)
			}
			if config.PlanFile != tt.expected.PlanFile {
				t.Errorf("PlanFile: expected %q, got %q", tt.expected.PlanFile, config.PlanFile)
			}
			if config.DocsDir != tt.expected.DocsDir {
				t.Errorf("DocsDir: expected %q, got %q", tt.expected.DocsDir, config.DocsDir)
			}
		})
	}
}

func TestParseFlags_DefaultValues(t *testing.T) {
	// Reset flag.CommandLine
	flag.CommandLine = flag.NewFlagSet("test", flag.ContinueOnError)

	// Save and restore os.Args
	oldArgs := os.Args
	defer func() { os.Args = oldArgs }()

	// Provide only required flags
	os.Args = []string{"cmd", "--old", "TEST-1", "--new", "TEST-2"}

	config := parseFlags()

	// Check that defaults are set correctly
	if config.PlanFile != "plan.md" {
		t.Errorf("Expected default PlanFile to be 'plan.md', got %q", config.PlanFile)
	}
	if config.DocsDir != "docs/project" {
		t.Errorf("Expected default DocsDir to be 'docs/project', got %q", config.DocsDir)
	}
}

func TestParseFlags_FlagOverriding(t *testing.T) {
	// Test that later flags override earlier ones (flag package behavior)
	// This tests the duplicate flag definitions in parseFlags

	// Reset flag.CommandLine
	flag.CommandLine = flag.NewFlagSet("test", flag.ContinueOnError)

	// Save and restore os.Args
	oldArgs := os.Args
	defer func() { os.Args = oldArgs }()

	// Use both --old and --old-key (should use the last one set)
	os.Args = []string{"cmd", "--old", "FIRST", "--old-key", "SECOND", "--new", "NEW-1"}

	config := parseFlags()

	// The way the flags are defined in parseFlags, both --old and --old-key point to the same variable
	// So the last value set should be used
	if config.OldKey != "SECOND" {
		t.Errorf("Expected OldKey to be 'SECOND' (last value), got %q", config.OldKey)
	}
}
