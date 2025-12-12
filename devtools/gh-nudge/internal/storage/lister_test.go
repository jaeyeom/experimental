package storage

import (
	"os"
	"path/filepath"
	"sort"
	"testing"
)

func TestNewFileSystemLister_CreatesWithAbsolutePath(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	if lister == nil {
		t.Fatal("Expected non-nil lister")
		return
	}

	// Verify the rootPath is absolute
	if !filepath.IsAbs(lister.rootPath) {
		t.Errorf("Expected absolute path, got %q", lister.rootPath)
	}
}

func TestNewFileSystemLister_ConvertsRelativeToAbsolute(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Change to temp directory and use relative path
	originalWd, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working directory: %v", err)
	}
	defer func() { _ = os.Chdir(originalWd) }()

	if err := os.Chdir(tempDir); err != nil {
		t.Fatalf("Failed to change directory: %v", err)
	}

	lister, err := NewFileSystemLister(".")
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	// Verify the rootPath is absolute
	if !filepath.IsAbs(lister.rootPath) {
		t.Errorf("Expected absolute path for relative input, got %q", lister.rootPath)
	}

	// Verify it matches the temp directory (resolve symlinks for comparison on macOS)
	absTemp, err := filepath.Abs(tempDir)
	if err != nil {
		t.Fatalf("Failed to get absolute path: %v", err)
	}
	expectedPath, err := filepath.EvalSymlinks(absTemp)
	if err != nil {
		t.Fatalf("Failed to resolve symlinks: %v", err)
	}
	actualPath, err := filepath.EvalSymlinks(lister.rootPath)
	if err != nil {
		t.Fatalf("Failed to resolve symlinks: %v", err)
	}
	if actualPath != expectedPath {
		t.Errorf("Expected rootPath %q, got %q", expectedPath, actualPath)
	}
}

func TestList_ReturnsAllFilesRecursively(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-list-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create directory structure:
	// tempDir/
	//   dir1/
	//     file1.txt
	//     subdir/
	//       file2.txt
	//   dir2/
	//     file3.txt
	dir1 := filepath.Join(tempDir, "dir1")
	subdir := filepath.Join(dir1, "subdir")
	dir2 := filepath.Join(tempDir, "dir2")

	if err := os.MkdirAll(subdir, 0o755); err != nil {
		t.Fatalf("Failed to create subdirectory: %v", err)
	}
	if err := os.MkdirAll(dir2, 0o755); err != nil {
		t.Fatalf("Failed to create dir2: %v", err)
	}

	files := map[string]string{
		filepath.Join(dir1, "file1.txt"):   "content1",
		filepath.Join(subdir, "file2.txt"): "content2",
		filepath.Join(dir2, "file3.txt"):   "content3",
	}

	for path, content := range files {
		if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
			t.Fatalf("Failed to create test file %s: %v", path, err)
		}
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	// List all files under dir1
	result, err := lister.List("dir1")
	if err != nil {
		t.Fatalf("List failed: %v", err)
	}

	// Sort for consistent comparison
	sort.Strings(result)

	expected := []string{
		filepath.Join("dir1", "file1.txt"),
		filepath.Join("dir1", "subdir", "file2.txt"),
	}
	sort.Strings(expected)

	if len(result) != len(expected) {
		t.Fatalf("Expected %d files, got %d: %v", len(expected), len(result), result)
	}

	for i, path := range expected {
		if result[i] != path {
			t.Errorf("Expected file[%d] = %q, got %q", i, path, result[i])
		}
	}
}

func TestList_ExcludesDirectories(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-nodirs-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create structure with directories and files
	dir := filepath.Join(tempDir, "parent")
	subdir := filepath.Join(dir, "child")

	if err := os.MkdirAll(subdir, 0o755); err != nil {
		t.Fatalf("Failed to create subdirectory: %v", err)
	}

	// Create a file
	file := filepath.Join(dir, "file.txt")
	if err := os.WriteFile(file, []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.List("parent")
	if err != nil {
		t.Fatalf("List failed: %v", err)
	}

	// Should only contain the file, not the directory
	if len(result) != 1 {
		t.Fatalf("Expected 1 file, got %d: %v", len(result), result)
	}

	expected := filepath.Join("parent", "file.txt")
	if result[0] != expected {
		t.Errorf("Expected %q, got %q", expected, result[0])
	}
}

func TestList_EmptyDirectory(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-empty-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	emptyDir := filepath.Join(tempDir, "empty")
	if err := os.MkdirAll(emptyDir, 0o755); err != nil {
		t.Fatalf("Failed to create empty directory: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.List("empty")
	if err != nil {
		t.Fatalf("List failed: %v", err)
	}

	if len(result) != 0 {
		t.Errorf("Expected 0 files in empty directory, got %d: %v", len(result), result)
	}
}

func TestList_NonExistentPrefix(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-notfound-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	_, err = lister.List("nonexistent")
	if err == nil {
		t.Error("Expected error for non-existent prefix, got nil")
	}
}

func TestList_ReturnsRelativePaths(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-relative-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create nested structure
	dir := filepath.Join(tempDir, "a", "b", "c")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("Failed to create directory: %v", err)
	}

	file := filepath.Join(dir, "file.txt")
	if err := os.WriteFile(file, []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.List("a")
	if err != nil {
		t.Fatalf("List failed: %v", err)
	}

	if len(result) != 1 {
		t.Fatalf("Expected 1 file, got %d: %v", len(result), result)
	}

	// Verify path is relative to tempDir, not absolute
	if filepath.IsAbs(result[0]) {
		t.Errorf("Expected relative path, got absolute: %q", result[0])
	}

	expected := filepath.Join("a", "b", "c", "file.txt")
	if result[0] != expected {
		t.Errorf("Expected %q, got %q", expected, result[0])
	}
}

func TestGetChildren_ReturnsImmediateChildren(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-children-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create structure:
	// tempDir/parent/
	//   file1.txt
	//   file2.txt
	//   subdir/
	//     nested.txt (should not be included)
	parent := filepath.Join(tempDir, "parent")
	subdir := filepath.Join(parent, "subdir")

	if err := os.MkdirAll(subdir, 0o755); err != nil {
		t.Fatalf("Failed to create subdirectory: %v", err)
	}

	files := map[string]string{
		filepath.Join(parent, "file1.txt"):  "content1",
		filepath.Join(parent, "file2.txt"):  "content2",
		filepath.Join(subdir, "nested.txt"): "nested",
	}

	for path, content := range files {
		if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
			t.Fatalf("Failed to create test file %s: %v", path, err)
		}
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.GetChildren("parent")
	if err != nil {
		t.Fatalf("GetChildren failed: %v", err)
	}

	// Sort for consistent comparison
	sort.Strings(result)

	// Should include both files and the subdirectory, but not nested files
	expected := []string{"file1.txt", "file2.txt", "subdir"}
	sort.Strings(expected)

	if len(result) != len(expected) {
		t.Fatalf("Expected %d children, got %d: %v", len(expected), len(result), result)
	}

	for i, name := range expected {
		if result[i] != name {
			t.Errorf("Expected child[%d] = %q, got %q", i, name, result[i])
		}
	}
}

func TestGetChildren_ReturnsOnlyNames(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-names-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	dir := filepath.Join(tempDir, "test")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("Failed to create directory: %v", err)
	}

	file := filepath.Join(dir, "file.txt")
	if err := os.WriteFile(file, []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.GetChildren("test")
	if err != nil {
		t.Fatalf("GetChildren failed: %v", err)
	}

	if len(result) != 1 {
		t.Fatalf("Expected 1 child, got %d: %v", len(result), result)
	}

	// Should be just the name, not a path
	if result[0] != "file.txt" {
		t.Errorf("Expected name only, got %q", result[0])
	}

	if filepath.IsAbs(result[0]) || filepath.Dir(result[0]) != "." {
		t.Errorf("Expected bare name, got path: %q", result[0])
	}
}

func TestGetChildren_EmptyDirectory(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-empty-children-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	emptyDir := filepath.Join(tempDir, "empty")
	if err := os.MkdirAll(emptyDir, 0o755); err != nil {
		t.Fatalf("Failed to create empty directory: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.GetChildren("empty")
	if err != nil {
		t.Fatalf("GetChildren failed: %v", err)
	}

	if len(result) != 0 {
		t.Errorf("Expected 0 children in empty directory, got %d: %v", len(result), result)
	}
}

func TestGetChildren_NonExistentDirectory(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-notfound-children-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	_, err = lister.GetChildren("nonexistent")
	if err == nil {
		t.Error("Expected error for non-existent directory, got nil")
	}
}

func TestGetChildren_IncludesDirectoriesAndFiles(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "lister-mixed-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	parent := filepath.Join(tempDir, "parent")
	childDir := filepath.Join(parent, "childdir")

	if err := os.MkdirAll(childDir, 0o755); err != nil {
		t.Fatalf("Failed to create child directory: %v", err)
	}

	childFile := filepath.Join(parent, "childfile.txt")
	if err := os.WriteFile(childFile, []byte("content"), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lister, err := NewFileSystemLister(tempDir)
	if err != nil {
		t.Fatalf("NewFileSystemLister failed: %v", err)
	}

	result, err := lister.GetChildren("parent")
	if err != nil {
		t.Fatalf("GetChildren failed: %v", err)
	}

	sort.Strings(result)

	expected := []string{"childdir", "childfile.txt"}
	sort.Strings(expected)

	if len(result) != len(expected) {
		t.Fatalf("Expected %d children, got %d: %v", len(expected), len(result), result)
	}

	for i, name := range expected {
		if result[i] != name {
			t.Errorf("Expected child[%d] = %q, got %q", i, name, result[i])
		}
	}
}
