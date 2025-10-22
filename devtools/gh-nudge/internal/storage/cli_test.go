package storage

import (
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// setupTestDir creates a test directory structure for testing ListFiles.
func setupTestDir(t *testing.T) string {
	t.Helper()

	tmpDir, err := os.MkdirTemp("", "cli_test_*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}

	// Create directory structure:
	// tmpDir/
	//   file1.txt
	//   file2.json
	//   dir1/
	//     subfile1.txt
	//     subdir1/
	//       deepfile.txt
	//   dir2/
	//     subfile2.json
	//   emptydir/

	if err := os.WriteFile(filepath.Join(tmpDir, "file1.txt"), []byte("content1"), 0o600); err != nil {
		t.Fatalf("Failed to create file1.txt: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tmpDir, "file2.json"), []byte("{}"), 0o600); err != nil {
		t.Fatalf("Failed to create file2.json: %v", err)
	}

	dir1 := filepath.Join(tmpDir, "dir1")
	if err := os.Mkdir(dir1, 0o755); err != nil {
		t.Fatalf("Failed to create dir1: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir1, "subfile1.txt"), []byte("subcontent1"), 0o600); err != nil {
		t.Fatalf("Failed to create subfile1.txt: %v", err)
	}

	subdir1 := filepath.Join(dir1, "subdir1")
	if err := os.Mkdir(subdir1, 0o755); err != nil {
		t.Fatalf("Failed to create subdir1: %v", err)
	}
	if err := os.WriteFile(filepath.Join(subdir1, "deepfile.txt"), []byte("deepcontent"), 0o600); err != nil {
		t.Fatalf("Failed to create deepfile.txt: %v", err)
	}

	dir2 := filepath.Join(tmpDir, "dir2")
	if err := os.Mkdir(dir2, 0o755); err != nil {
		t.Fatalf("Failed to create dir2: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir2, "subfile2.json"), []byte("{}"), 0o600); err != nil {
		t.Fatalf("Failed to create subfile2.json: %v", err)
	}

	emptyDir := filepath.Join(tmpDir, "emptydir")
	if err := os.Mkdir(emptyDir, 0o755); err != nil {
		t.Fatalf("Failed to create emptydir: %v", err)
	}

	return tmpDir
}

func TestCLIInfoDisplayer_ListFiles_NonRecursive(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ListFiles("", false, "table", "")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)
	lines := strings.Split(strings.TrimSpace(output), "\n")

	// Should list files and directories at root level only
	expectedEntries := map[string]bool{
		"file1.txt":  true,
		"file2.json": true,
		"dir1/":      true, // directories should have / suffix
		"dir2/":      true,
		"emptydir/":  true,
	}

	if len(lines) != len(expectedEntries) {
		t.Errorf("Expected %d entries, got %d: %v", len(expectedEntries), len(lines), lines)
	}

	for _, line := range lines {
		if !expectedEntries[line] {
			t.Errorf("Unexpected entry in output: %s", line)
		}
		delete(expectedEntries, line)
	}

	if len(expectedEntries) > 0 {
		t.Errorf("Missing entries in output: %v", expectedEntries)
	}
}

func TestCLIInfoDisplayer_ListFiles_NonRecursive_Subdirectory(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ListFiles("dir1", false, "table", "")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)
	lines := strings.Split(strings.TrimSpace(output), "\n")

	// Should list only immediate children of dir1
	expectedEntries := map[string]bool{
		"dir1/subfile1.txt": true,
		"dir1/subdir1/":     true, // subdirectory with / suffix
	}

	if len(lines) != len(expectedEntries) {
		t.Errorf("Expected %d entries, got %d: %v", len(expectedEntries), len(lines), lines)
	}

	for _, line := range lines {
		if !expectedEntries[line] {
			t.Errorf("Unexpected entry in output: %s", line)
		}
		delete(expectedEntries, line)
	}

	if len(expectedEntries) > 0 {
		t.Errorf("Missing entries in output: %v", expectedEntries)
	}
}

func TestCLIInfoDisplayer_ListFiles_Recursive(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ListFiles("", true, "table", "")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)
	lines := strings.Split(strings.TrimSpace(output), "\n")

	// Should list all files and directories recursively
	expectedEntries := map[string]bool{
		"./":                        true, // root directory
		"file1.txt":                 true,
		"file2.json":                true,
		"dir1/":                     true,
		"dir1/subfile1.txt":         true,
		"dir1/subdir1/":             true,
		"dir1/subdir1/deepfile.txt": true,
		"dir2/":                     true,
		"dir2/subfile2.json":        true,
		"emptydir/":                 true,
	}

	if len(lines) != len(expectedEntries) {
		t.Errorf("Expected %d entries, got %d", len(expectedEntries), len(lines))
		t.Logf("Got entries: %v", lines)
		t.Logf("Expected entries: %v", expectedEntries)
	}

	for _, line := range lines {
		if !expectedEntries[line] {
			t.Errorf("Unexpected entry in output: %s", line)
		}
	}
}

func TestCLIInfoDisplayer_ListFiles_JSONFormat(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ListFiles("", false, "json", "")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)

	// Should be valid JSON array
	var entries []string
	if err := json.Unmarshal([]byte(output), &entries); err != nil {
		t.Fatalf("Output is not valid JSON: %v\nOutput: %s", err, output)
	}

	// Verify directories have / suffix
	hasDir := false
	for _, entry := range entries {
		if strings.HasSuffix(entry, "/") {
			hasDir = true
			break
		}
	}
	if !hasDir {
		t.Errorf("Expected at least one directory with / suffix in JSON output")
	}
}

func TestCLIInfoDisplayer_ListFiles_WithFilter(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	// Filter for "dir1" to test filtering
	err = displayer.ListFiles("", false, "table", "dir1")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)
	lines := strings.Split(strings.TrimSpace(output), "\n")

	// Should only show dir1/
	if len(lines) != 1 {
		t.Errorf("Expected 1 entry with 'dir1' filter, got %d: %v", len(lines), lines)
	}

	if lines[0] != "dir1/" {
		t.Errorf("Expected 'dir1/', got '%s'", lines[0])
	}
}

func TestCLIInfoDisplayer_ListFiles_EmptyDirectory(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ListFiles("emptydir", false, "table", "")
	if err != nil {
		t.Fatalf("ListFiles failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := strings.TrimSpace(string(out))

	// Empty directory should produce empty output
	if output != "" {
		t.Errorf("Expected empty output for empty directory, got: %s", output)
	}
}

func TestCLIInfoDisplayer_ListFiles_NonExistentPath(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	err = displayer.ListFiles("nonexistent", false, "table", "")
	if err == nil {
		t.Errorf("Expected error for non-existent path, got nil")
	}
}

func TestCLIInfoDisplayer_ShowInfo(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ShowInfo("file1.txt", false, "table")
	if err != nil {
		t.Fatalf("ShowInfo failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)

	// Should contain basic file info
	if !strings.Contains(output, "Path:") {
		t.Errorf("Expected 'Path:' in output")
	}
	if !strings.Contains(output, "Size:") {
		t.Errorf("Expected 'Size:' in output")
	}
	if !strings.Contains(output, "Type:") {
		t.Errorf("Expected 'Type:' in output")
	}
}

func TestCLIInfoDisplayer_ShowInfo_JSONFormat(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	displayer, err := NewCLIInfoDisplayer(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create CLIInfoDisplayer: %v", err)
	}

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = displayer.ShowInfo("dir1", false, "json")
	if err != nil {
		t.Fatalf("ShowInfo failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)

	// Should be valid JSON
	var info map[string]interface{}
	if err := json.Unmarshal([]byte(output), &info); err != nil {
		t.Fatalf("Output is not valid JSON: %v\nOutput: %s", err, output)
	}

	// Verify it's marked as directory
	if isDir, ok := info["is_dir"].(bool); !ok || !isDir {
		t.Errorf("Expected is_dir to be true for directory")
	}
}

func TestCLIFormatter_GetFormatted(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	store, err := NewFileSystemStore(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create store: %v", err)
	}

	formatter := NewCLIFormatter(store)

	// Capture stdout
	oldStdout := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	err = formatter.GetFormatted("file1.txt", "raw", false)
	if err != nil {
		t.Fatalf("GetFormatted failed: %v", err)
	}

	w.Close()
	os.Stdout = oldStdout

	out, _ := io.ReadAll(r)
	output := string(out)

	if output != "content1" {
		t.Errorf("Expected 'content1', got '%s'", output)
	}
}

func TestCLIFormatter_SetFormatted(t *testing.T) {
	tmpDir := setupTestDir(t)
	defer os.RemoveAll(tmpDir)

	store, err := NewFileSystemStore(tmpDir)
	if err != nil {
		t.Fatalf("Failed to create store: %v", err)
	}

	formatter := NewCLIFormatter(store)

	err = formatter.SetFormatted("newfile.txt", "newcontent", false, false, false)
	if err != nil {
		t.Fatalf("SetFormatted failed: %v", err)
	}

	// Verify file was created
	content, err := os.ReadFile(filepath.Join(tmpDir, "newfile.txt"))
	if err != nil {
		t.Fatalf("Failed to read created file: %v", err)
	}

	if string(content) != "newcontent" {
		t.Errorf("Expected 'newcontent', got '%s'", string(content))
	}
}
