package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestRenameDocFile_DashSeparatedNames tests renaming files with dash-separated names.
func TestRenameDocFile_DashSeparatedNames(t *testing.T) {
	tmpDir := t.TempDir()

	// Create test files with dash-separated names
	files := []string{
		"PROJ-123.md",
		"PROJ-123-description.md",
		"PROJ-123-implementation.md",
	}

	for _, file := range files {
		path := filepath.Join(tmpDir, file)
		if err := os.WriteFile(path, []byte("content"), 0o600); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	err := renameDocFile(tmpDir, "PROJ-123", "PROJ-456")
	if err != nil {
		t.Fatalf("renameDocFile failed: %v", err)
	}

	// Verify all files were renamed
	expectedFiles := []string{
		"PROJ-456.md",
		"PROJ-456-description.md",
		"PROJ-456-implementation.md",
	}

	for _, expected := range expectedFiles {
		path := filepath.Join(tmpDir, expected)
		if _, err := os.Stat(path); err != nil {
			t.Errorf("Expected file %s should exist", expected)
		}
	}

	// Verify old files are gone
	for _, oldFile := range files {
		path := filepath.Join(tmpDir, oldFile)
		if _, err := os.Stat(path); err == nil {
			t.Errorf("Old file %s should not exist", oldFile)
		}
	}
}

// TestRenameDocFile_NoMatchingFiles tests when no files match the pattern.
func TestRenameDocFile_NoMatchingFiles(t *testing.T) {
	tmpDir := t.TempDir()

	// Create files that don't match the pattern
	if err := os.WriteFile(filepath.Join(tmpDir, "other.md"), []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create file: %v", err)
	}

	err := renameDocFile(tmpDir, "PROJ-123", "PROJ-456")
	if err == nil {
		t.Error("Expected error when no matching files")
	}
	if !strings.Contains(err.Error(), "no files found matching pattern") {
		t.Errorf("Expected 'no files found matching pattern' error, got: %v", err)
	}
}

// TestRenameDocFile_PartialMatch tests that partial matches are not renamed.
func TestRenameDocFile_PartialMatch(t *testing.T) {
	tmpDir := t.TempDir()

	// Create files with partial matches (should NOT be renamed)
	partialMatches := []string{
		"MYPROJ-123.md",     // Different prefix
		"PROJ-1234.md",      // Longer number
		"PROJ-12.md",        // Shorter number
		"PROJ-123a.md",      // Letter after number
		"prefixPROJ-123.md", // Starts with different prefix
	}

	for _, file := range partialMatches {
		path := filepath.Join(tmpDir, file)
		if err := os.WriteFile(path, []byte("content"), 0o600); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	err := renameDocFile(tmpDir, "PROJ-123", "PROJ-456")
	if err == nil {
		t.Error("Expected error when no exact matches")
	}

	// Verify none of the partial matches were renamed
	for _, file := range partialMatches {
		path := filepath.Join(tmpDir, file)
		if _, err := os.Stat(path); err != nil {
			t.Errorf("Partial match file %s should still exist", file)
		}
	}
}

// TestRenameDocFile_MixedFiles tests renaming with both matching and non-matching files.
func TestRenameDocFile_MixedFiles(t *testing.T) {
	tmpDir := t.TempDir()

	// Create mix of matching and non-matching files
	files := map[string]bool{
		"PROJ-123.md":         true,  // Should be renamed
		"PROJ-123-feature.md": true,  // Should be renamed
		"OTHER-456.md":        false, // Should NOT be renamed
		"MYPROJ-123.md":       false, // Should NOT be renamed
		"PROJ-123-notes.md":   true,  // Should be renamed
	}

	for file := range files {
		path := filepath.Join(tmpDir, file)
		if err := os.WriteFile(path, []byte("content"), 0o600); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	err := renameDocFile(tmpDir, "PROJ-123", "PROJ-456")
	if err != nil {
		t.Fatalf("renameDocFile failed: %v", err)
	}

	// Verify correct files were renamed
	for file, shouldRename := range files {
		path := filepath.Join(tmpDir, file)
		_, err := os.Stat(path)

		if shouldRename {
			// File should be gone (renamed)
			if err == nil {
				t.Errorf("File %s should have been renamed", file)
			}
		} else {
			// File should still exist
			if err != nil {
				t.Errorf("File %s should not have been renamed: %v", file, err)
			}
		}
	}
}

// TestRunRename_MissingPlanFile tests handling of missing plan file.
func TestRunRename_MissingPlanFile(t *testing.T) {
	tmpDir := t.TempDir()

	docsDir := filepath.Join(tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0o755); err != nil {
		t.Fatalf("Failed to create docs dir: %v", err)
	}

	// Create a doc file
	docFile := filepath.Join(docsDir, "test.md")
	if err := os.WriteFile(docFile, []byte("PROJ-123"), 0o600); err != nil {
		t.Fatalf("Failed to create doc file: %v", err)
	}

	config := Config{
		OldKey:   "PROJ-123",
		NewKey:   "PROJ-456",
		PlanFile: filepath.Join(tmpDir, "nonexistent.md"),
		DocsDir:  docsDir,
	}

	// Should not error even with missing plan file
	err := runRename(config)
	if err != nil {
		t.Errorf("runRename should not fail with missing plan file: %v", err)
	}

	// Verify doc file was still updated
	content, err := os.ReadFile(docFile)
	if err != nil {
		t.Fatalf("Failed to read doc file: %v", err)
	}
	if !strings.Contains(string(content), "PROJ-456") {
		t.Error("Doc file should have been updated")
	}
}

// TestRunRename_EmptyDocsDirectory tests handling of empty docs directory.
func TestRunRename_EmptyDocsDirectory(t *testing.T) {
	tmpDir := t.TempDir()

	docsDir := filepath.Join(tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0o755); err != nil {
		t.Fatalf("Failed to create docs dir: %v", err)
	}

	planFile := filepath.Join(tmpDir, "plan.md")
	if err := os.WriteFile(planFile, []byte("PROJ-123"), 0o600); err != nil {
		t.Fatalf("Failed to create plan file: %v", err)
	}

	config := Config{
		OldKey:   "PROJ-123",
		NewKey:   "PROJ-456",
		PlanFile: planFile,
		DocsDir:  docsDir,
	}

	// Should not error with empty docs directory
	err := runRename(config)
	if err != nil {
		t.Errorf("runRename should not fail with empty docs directory: %v", err)
	}

	// Verify plan file was still updated
	content, err := os.ReadFile(planFile)
	if err != nil {
		t.Fatalf("Failed to read plan file: %v", err)
	}
	if !strings.Contains(string(content), "PROJ-456") {
		t.Error("Plan file should have been updated")
	}
}

// TestRunRename_NoMatchingContent tests files with no matching content.
func TestRunRename_NoMatchingContent(t *testing.T) {
	tmpDir := t.TempDir()

	docsDir := filepath.Join(tmpDir, "docs")
	if err := os.MkdirAll(docsDir, 0o755); err != nil {
		t.Fatalf("Failed to create docs dir: %v", err)
	}

	planFile := filepath.Join(tmpDir, "plan.md")
	if err := os.WriteFile(planFile, []byte("No matching keys here"), 0o600); err != nil {
		t.Fatalf("Failed to create plan file: %v", err)
	}

	docFile := filepath.Join(docsDir, "doc.md")
	if err := os.WriteFile(docFile, []byte("Nothing to replace"), 0o600); err != nil {
		t.Fatalf("Failed to create doc file: %v", err)
	}

	config := Config{
		OldKey:   "PROJ-123",
		NewKey:   "PROJ-456",
		PlanFile: planFile,
		DocsDir:  docsDir,
	}

	// Should not error even when no content matches
	err := runRename(config)
	if err != nil {
		t.Errorf("runRename should not fail when no content matches: %v", err)
	}
}

// TestRenameDocFile_RenameError tests handling of rename operation failures.
func TestRenameDocFile_RenameError(t *testing.T) {
	tmpDir := t.TempDir()

	// Create source file
	sourceFile := filepath.Join(tmpDir, "PROJ-123.md")
	if err := os.WriteFile(sourceFile, []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create source file: %v", err)
	}

	// Create destination file that already exists
	destFile := filepath.Join(tmpDir, "PROJ-456.md")
	if err := os.WriteFile(destFile, []byte("existing"), 0o600); err != nil {
		t.Fatalf("Failed to create dest file: %v", err)
	}

	err := renameDocFile(tmpDir, "PROJ-123", "PROJ-456")
	if err == nil {
		t.Error("Expected error when destination file exists")
	}
	if !strings.Contains(err.Error(), "already exists") {
		t.Errorf("Expected 'already exists' error, got: %v", err)
	}

	// Verify source file still exists
	if _, err := os.Stat(sourceFile); err != nil {
		t.Error("Source file should still exist after failed rename")
	}
}

// TestReplaceInFile_SpecialCharacters tests replacement with special regex characters.
func TestReplaceInFile_SpecialCharacters(t *testing.T) {
	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "test.md")

	// Test with keys containing special regex characters
	content := "This mentions [PROJ-123] and (PROJ-123) in the text."
	expected := "This mentions [PROJ-456] and (PROJ-456) in the text."

	if err := os.WriteFile(tmpFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	err := replaceInFile(tmpFile, "PROJ-123", "PROJ-456")
	if err != nil {
		t.Fatalf("replaceInFile failed: %v", err)
	}

	result, err := os.ReadFile(tmpFile)
	if err != nil {
		t.Fatalf("Failed to read result: %v", err)
	}

	if string(result) != expected {
		t.Errorf("Expected:\n%s\nGot:\n%s", expected, string(result))
	}
}

// TestReplaceInFile_WritePermissionError tests handling of write permission errors.
func TestReplaceInFile_WritePermissionError(t *testing.T) {
	if os.Getuid() == 0 {
		t.Skip("Cannot test write permissions as root")
	}

	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "test.md")

	// Create file with content
	if err := os.WriteFile(tmpFile, []byte("PROJ-123"), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	// Make file read-only
	if err := os.Chmod(tmpFile, 0o400); err != nil {
		t.Fatalf("Failed to change file permissions: %v", err)
	}
	defer func() {
		_ = os.Chmod(tmpFile, 0o600) // Cleanup - ignore error as test is ending
	}()

	err := replaceInFile(tmpFile, "PROJ-123", "PROJ-456")
	if err == nil {
		t.Error("Expected error when writing to read-only file")
	}
	if !strings.Contains(err.Error(), "failed to write") {
		t.Errorf("Expected write error, got: %v", err)
	}
}
