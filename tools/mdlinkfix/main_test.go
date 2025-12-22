package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestProcessFileWithEnv(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()

	// Create a local file that should be found
	localFile := filepath.Join(tmpDir, "local-doc.md")
	if err := os.WriteFile(localFile, []byte("# Local"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create the file to process
	inputFile := filepath.Join(tmpDir, "input.md")
	inputContent := `# Test Document

Check out [Local Doc](/en/local-doc) and [Remote Doc](/en/remote-doc).
`
	if err := os.WriteFile(inputFile, []byte(inputContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Set environment variables
	os.Setenv("MDLINK_BASE_URL", "https://example.com/docs")
	os.Setenv("MDLINK_LOCAL_PREFIX", "/en")
	os.Setenv("MDLINK_BASE_DIR", tmpDir)
	defer func() {
		os.Unsetenv("MDLINK_BASE_URL")
		os.Unsetenv("MDLINK_LOCAL_PREFIX")
		os.Unsetenv("MDLINK_BASE_DIR")
		os.Unsetenv("MDLINK_SUFFIX_DROP")
		os.Unsetenv("MDLINK_SUFFIX_ADD")
	}()

	// Process the file
	*dryRun = false
	*verbose = false
	*validate = false
	if _, err := ProcessFile(inputFile); err != nil {
		t.Fatalf("ProcessFile() error = %v", err)
	}

	// Read the result
	result, err := os.ReadFile(inputFile)
	if err != nil {
		t.Fatal(err)
	}

	expected := `# Test Document

Check out [Local Doc](local-doc.md) and [Remote Doc](https://example.com/docs/en/remote-doc).
`
	if string(result) != expected {
		t.Errorf("ProcessFile() result = %q, want %q", string(result), expected)
	}
}

func TestProcessFileDryRun(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()

	// Create the file to process
	inputFile := filepath.Join(tmpDir, "input.md")
	inputContent := `# Test Document

Check out [Remote Doc](/en/remote-doc).
`
	if err := os.WriteFile(inputFile, []byte(inputContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Set environment variables
	os.Setenv("MDLINK_BASE_URL", "https://example.com/docs")
	os.Setenv("MDLINK_LOCAL_PREFIX", "/en")
	defer func() {
		os.Unsetenv("MDLINK_BASE_URL")
		os.Unsetenv("MDLINK_LOCAL_PREFIX")
		os.Unsetenv("MDLINK_BASE_DIR")
		os.Unsetenv("MDLINK_SUFFIX_DROP")
		os.Unsetenv("MDLINK_SUFFIX_ADD")
	}()

	// Process the file in dry-run mode
	*dryRun = true
	*verbose = false
	*validate = false
	defer func() { *dryRun = false }()

	if _, err := ProcessFile(inputFile); err != nil {
		t.Fatalf("ProcessFile() error = %v", err)
	}

	// Read the result - should be unchanged in dry-run mode
	result, err := os.ReadFile(inputFile)
	if err != nil {
		t.Fatal(err)
	}

	if string(result) != inputContent {
		t.Errorf("ProcessFile() in dry-run mode modified the file: got %q, want %q", string(result), inputContent)
	}
}

func TestProcessFileValidation(t *testing.T) {
	// Create a temporary directory
	tmpDir := t.TempDir()

	// Create an existing file
	existingFile := filepath.Join(tmpDir, "existing.md")
	if err := os.WriteFile(existingFile, []byte("# Existing"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create the file to process with broken links
	inputFile := filepath.Join(tmpDir, "input.md")
	inputContent := `# Test Document

Check out [Existing](existing.md) and [Broken](nonexisting.md).
`
	if err := os.WriteFile(inputFile, []byte(inputContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Set environment variables
	os.Setenv("MDLINK_LOCAL_PREFIX", "/en")
	os.Setenv("MDLINK_BASE_DIR", tmpDir)
	defer func() {
		os.Unsetenv("MDLINK_BASE_URL")
		os.Unsetenv("MDLINK_LOCAL_PREFIX")
		os.Unsetenv("MDLINK_BASE_DIR")
		os.Unsetenv("MDLINK_SUFFIX_DROP")
		os.Unsetenv("MDLINK_SUFFIX_ADD")
	}()

	// Process the file with validation
	*dryRun = false
	*verbose = false
	*validate = true
	defer func() { *validate = false }()

	brokenLinks, err := ProcessFile(inputFile)
	if err != nil {
		t.Fatalf("ProcessFile() error = %v", err)
	}

	if len(brokenLinks) != 1 {
		t.Errorf("ProcessFile() found %d broken links, want 1", len(brokenLinks))
	}

	if len(brokenLinks) > 0 && brokenLinks[0].Path != "nonexisting.md" {
		t.Errorf("brokenLinks[0].Path = %q, want %q", brokenLinks[0].Path, "nonexisting.md")
	}
}
