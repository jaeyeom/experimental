package storage

import (
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestInitialize_CreatesStorageStructure(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-nudge-init-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storageDir := filepath.Join(tempDir, "storage")

	err = Initialize(storageDir, false, false)
	if err != nil {
		t.Fatalf("Initialize failed: %v", err)
	}

	// Verify storage directory was created
	if !directoryExists(storageDir) {
		t.Error("Storage directory was not created")
	}

	// Verify metadata.json exists and has correct structure
	metadataPath := filepath.Join(storageDir, "metadata.json")
	metadataData, err := os.ReadFile(metadataPath)
	if err != nil {
		t.Fatalf("Failed to read metadata.json: %v", err)
	}

	var metadata map[string]interface{}
	if err := json.Unmarshal(metadataData, &metadata); err != nil {
		t.Fatalf("Failed to parse metadata.json: %v", err)
	}

	if metadata["version"] != "1.0.0" {
		t.Errorf("Expected version to be '1.0.0', got %v", metadata["version"])
	}

	if metadata["description"] != "gh-nudge unified storage" {
		t.Errorf("Expected description to be 'gh-nudge unified storage', got %v", metadata["description"])
	}

	if _, ok := metadata["created_at"]; !ok {
		t.Error("Expected created_at field in metadata")
	}

	// Verify subdirectories were created
	subdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range subdirs {
		subdirPath := filepath.Join(storageDir, subdir)
		if !directoryExists(subdirPath) {
			t.Errorf("Subdirectory %s was not created", subdir)
		}
	}
}

func TestInitialize_RespectsForceFlag(t *testing.T) {
	tests := []struct {
		name      string
		force     bool
		expectErr bool
	}{
		{
			name:      "without force flag on existing directory",
			force:     false,
			expectErr: true,
		},
		{
			name:      "with force flag on existing directory",
			force:     true,
			expectErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tempDir, err := os.MkdirTemp("", "gh-nudge-force-test")
			if err != nil {
				t.Fatalf("Failed to create temp directory: %v", err)
			}
			defer os.RemoveAll(tempDir)

			storageDir := filepath.Join(tempDir, "storage")

			// Create directory first
			if err := os.MkdirAll(storageDir, 0o755); err != nil {
				t.Fatalf("Failed to create storage directory: %v", err)
			}

			// Try to initialize
			err = Initialize(storageDir, tt.force, false)

			if tt.expectErr && err == nil {
				t.Error("Expected error but got nil")
			}

			if !tt.expectErr && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestInitialize_MigrationFlag(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-nudge-migrate-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storageDir := filepath.Join(tempDir, "storage")

	// Initialize with migration flag should fail (not implemented)
	err = Initialize(storageDir, false, true)
	if err == nil {
		t.Error("Expected migration to fail with 'not implemented' error, but got nil")
	}

	// Verify the error message
	expectedMsg := "migration not implemented"
	if err != nil && err.Error() != "failed to migrate data: "+expectedMsg {
		t.Errorf("Expected migration error, got: %v", err)
	}
}

func TestDirectoryExists(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-nudge-direxists-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name     string
		setup    func(string) string
		expected bool
	}{
		{
			name: "existing directory",
			setup: func(base string) string {
				dir := filepath.Join(base, "testdir")
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatalf("Failed to create test directory: %v", err)
				}
				return dir
			},
			expected: true,
		},
		{
			name: "existing file",
			setup: func(base string) string {
				file := filepath.Join(base, "testfile.txt")
				if err := os.WriteFile(file, []byte("test"), 0o600); err != nil {
					t.Fatalf("Failed to create test file: %v", err)
				}
				return file
			},
			expected: false,
		},
		{
			name: "non-existent path",
			setup: func(base string) string {
				return filepath.Join(base, "nonexistent")
			},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			path := tt.setup(tempDir)
			result := directoryExists(path)

			if result != tt.expected {
				t.Errorf("directoryExists(%q) = %v, expected %v", path, result, tt.expected)
			}
		})
	}
}

func TestMigrate_NotImplemented(t *testing.T) {
	err := Migrate("storage", "v1", "v2", false, false)
	if err == nil {
		t.Error("Expected Migrate to return error, got nil")
	}

	expectedMsg := "migration not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestCreateBackup(t *testing.T) {
	t.Run("creates uncompressed backup", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create some test files
		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(`{"test": true}`), 0o600); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Create backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, false, "test backup")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		if backupID == "" {
			t.Error("Expected non-empty backup ID")
		}

		// Verify backup file exists
		backupFile := filepath.Join(backupDir, backupID+".tar")
		if !fileExists(backupFile) {
			t.Errorf("Backup file not found: %s", backupFile)
		}

		// Verify metadata file exists
		metadataFile := filepath.Join(backupDir, backupID+".json")
		if !fileExists(metadataFile) {
			t.Errorf("Metadata file not found: %s", metadataFile)
		}
	})

	t.Run("creates compressed backup", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create some test files
		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(`{"test": true}`), 0o600); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Create compressed backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, true, "compressed backup")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// Verify compressed backup file exists
		backupFile := filepath.Join(backupDir, backupID+".tar.gz")
		if !fileExists(backupFile) {
			t.Errorf("Compressed backup file not found: %s", backupFile)
		}
	})

	t.Run("fails for non-existent storage", func(t *testing.T) {
		backupDir := t.TempDir()
		_, err := CreateBackup("/nonexistent/path", backupDir, "", true, false, "")
		if err == nil {
			t.Error("Expected error for non-existent storage")
		}
	})

	t.Run("fails for non-existent path", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		_, err := CreateBackup(storageDir, backupDir, "nonexistent/path", false, false, "")
		if err == nil {
			t.Error("Expected error for non-existent path")
		}
	})
}

func TestRestoreBackup(t *testing.T) {
	t.Run("restores from uncompressed backup", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()
		restoreDir := t.TempDir()

		// Initialize and populate storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		testContent := `{"test": "data"}`
		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(testContent), 0o600); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Create backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, false, "test")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// Restore to new directory
		err = RestoreBackup(restoreDir, backupDir, backupID, "", false)
		if err != nil {
			t.Fatalf("RestoreBackup failed: %v", err)
		}

		// Verify restored file
		restoredFile := filepath.Join(restoreDir, "repos", "test.json")
		restoredContent, err := os.ReadFile(restoredFile)
		if err != nil {
			t.Fatalf("Failed to read restored file: %v", err)
		}

		if string(restoredContent) != testContent {
			t.Errorf("Restored content mismatch: got %q, want %q", string(restoredContent), testContent)
		}
	})

	t.Run("restores from compressed backup", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()
		restoreDir := t.TempDir()

		// Initialize and populate storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		testContent := `{"compressed": true}`
		testFile := filepath.Join(storageDir, "repos", "compressed.json")
		if err := os.WriteFile(testFile, []byte(testContent), 0o600); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Create compressed backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, true, "compressed")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// Restore
		err = RestoreBackup(restoreDir, backupDir, backupID, "", false)
		if err != nil {
			t.Fatalf("RestoreBackup failed: %v", err)
		}

		// Verify
		restoredFile := filepath.Join(restoreDir, "repos", "compressed.json")
		restoredContent, err := os.ReadFile(restoredFile)
		if err != nil {
			t.Fatalf("Failed to read restored file: %v", err)
		}

		if string(restoredContent) != testContent {
			t.Errorf("Restored content mismatch: got %q, want %q", string(restoredContent), testContent)
		}
	})

	t.Run("preview mode does not restore", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()
		restoreDir := t.TempDir()

		// Initialize and populate storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(`{}`), 0o600); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Create backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, false, "")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// Preview restore
		err = RestoreBackup(restoreDir, backupDir, backupID, "", true)
		if err != nil {
			t.Fatalf("RestoreBackup preview failed: %v", err)
		}

		// Verify file was NOT restored
		restoredFile := filepath.Join(restoreDir, "repos", "test.json")
		if fileExists(restoredFile) {
			t.Error("File should not exist in preview mode")
		}
	})

	t.Run("fails for non-existent backup", func(t *testing.T) {
		restoreDir := t.TempDir()
		backupDir := t.TempDir()

		err := RestoreBackup(restoreDir, backupDir, "nonexistent", "", false)
		if err == nil {
			t.Error("Expected error for non-existent backup")
		}
	})

	t.Run("restores specific path", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()
		restoreDir := t.TempDir()

		// Initialize and populate storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create multiple files
		file1 := filepath.Join(storageDir, "repos", "file1.json")
		file2 := filepath.Join(storageDir, "cache", "file2.json")
		if err := os.MkdirAll(filepath.Dir(file1), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.MkdirAll(filepath.Dir(file2), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(file1, []byte(`{"file": 1}`), 0o600); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(file2, []byte(`{"file": 2}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create backup
		backupID, err := CreateBackup(storageDir, backupDir, "", true, false, "")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// Restore only repos path
		err = RestoreBackup(restoreDir, backupDir, backupID, "repos", false)
		if err != nil {
			t.Fatalf("RestoreBackup failed: %v", err)
		}

		// Verify only repos file was restored
		if !fileExists(filepath.Join(restoreDir, "repos", "file1.json")) {
			t.Error("repos/file1.json should be restored")
		}
		if fileExists(filepath.Join(restoreDir, "cache", "file2.json")) {
			t.Error("cache/file2.json should NOT be restored")
		}
	})
}

func TestListBackups(t *testing.T) {
	t.Run("lists backups", func(t *testing.T) {
		storageDir := t.TempDir()
		backupDir := t.TempDir()

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create test file
		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(`{}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create multiple backups
		_, err := CreateBackup(storageDir, backupDir, "", true, false, "first backup")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		time.Sleep(time.Second) // Ensure different timestamps

		_, err = CreateBackup(storageDir, backupDir, "", true, true, "second backup")
		if err != nil {
			t.Fatalf("CreateBackup failed: %v", err)
		}

		// List backups (should not error)
		err = ListBackups(backupDir)
		if err != nil {
			t.Errorf("ListBackups failed: %v", err)
		}
	})

	t.Run("handles empty backup directory", func(t *testing.T) {
		backupDir := t.TempDir()

		err := ListBackups(backupDir)
		if err != nil {
			t.Errorf("ListBackups failed for empty directory: %v", err)
		}
	})

	t.Run("handles non-existent backup directory", func(t *testing.T) {
		err := ListBackups("/nonexistent/backup/dir")
		if err != nil {
			t.Errorf("ListBackups should not error for non-existent directory: %v", err)
		}
	})
}

func TestFilterFilesByPath(t *testing.T) {
	files := []string{
		"repos/owner/repo/file1.json",
		"repos/owner/repo/file2.json",
		"cache/data.json",
		"metadata.json",
	}

	tests := []struct {
		name     string
		path     string
		expected []string
	}{
		{
			name:     "filter by repos prefix",
			path:     "repos",
			expected: []string{"repos/owner/repo/file1.json", "repos/owner/repo/file2.json"},
		},
		{
			name:     "filter by specific file",
			path:     "metadata.json",
			expected: []string{"metadata.json"},
		},
		{
			name:     "filter with no matches",
			path:     "nonexistent",
			expected: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := filterFilesByPath(files, tt.path)
			if len(result) != len(tt.expected) {
				t.Errorf("filterFilesByPath() returned %d items, want %d", len(result), len(tt.expected))
				return
			}
			for i, f := range result {
				if f != tt.expected[i] {
					t.Errorf("filterFilesByPath()[%d] = %q, want %q", i, f, tt.expected[i])
				}
			}
		})
	}
}

func TestFormatSize(t *testing.T) {
	tests := []struct {
		bytes    int64
		expected string
	}{
		{0, "0B"},
		{512, "512B"},
		{1024, "1.0KB"},
		{1536, "1.5KB"},
		{1048576, "1.0MB"},
		{1572864, "1.5MB"},
		{1073741824, "1.0GB"},
	}

	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			result := formatSize(tt.bytes)
			if result != tt.expected {
				t.Errorf("formatSize(%d) = %q, want %q", tt.bytes, result, tt.expected)
			}
		})
	}
}

func TestClean(t *testing.T) {
	tests := []struct {
		name      string
		cleanType string
		olderThan time.Duration
		dryRun    bool
		setup     func(t *testing.T, storageHome string)
		wantErr   bool
		validate  func(t *testing.T, storageHome string)
	}{
		{
			name:      "clean cache directory - dry run",
			cleanType: "cache",
			olderThan: time.Hour,
			dryRun:    true,
			setup: func(t *testing.T, storageHome string) {
				cacheDir := filepath.Join(storageHome, "cache")
				if err := os.MkdirAll(cacheDir, 0o755); err != nil {
					t.Fatal(err)
				}
				// Create an old file
				oldFile := filepath.Join(cacheDir, "old.txt")
				if err := os.WriteFile(oldFile, []byte("old"), 0o600); err != nil {
					t.Fatal(err)
				}
				// Set modification time to 2 hours ago
				oldTime := time.Now().Add(-2 * time.Hour)
				if err := os.Chtimes(oldFile, oldTime, oldTime); err != nil {
					t.Fatal(err)
				}
			},
			validate: func(t *testing.T, storageHome string) {
				// In dry run mode, file should still exist
				oldFile := filepath.Join(storageHome, "cache", "old.txt")
				if _, err := os.Stat(oldFile); errors.Is(err, fs.ErrNotExist) {
					t.Error("File should still exist in dry run mode")
				}
			},
		},
		{
			name:      "clean temp directory",
			cleanType: "temp",
			olderThan: time.Hour,
			dryRun:    false,
			setup: func(t *testing.T, storageHome string) {
				tempDir := filepath.Join(storageHome, "temp")
				if err := os.MkdirAll(tempDir, 0o755); err != nil {
					t.Fatal(err)
				}
				// Create an old file
				oldFile := filepath.Join(tempDir, "old.tmp")
				if err := os.WriteFile(oldFile, []byte("old"), 0o600); err != nil {
					t.Fatal(err)
				}
				oldTime := time.Now().Add(-2 * time.Hour)
				if err := os.Chtimes(oldFile, oldTime, oldTime); err != nil {
					t.Fatal(err)
				}
				// Create a recent file
				newFile := filepath.Join(tempDir, "new.tmp")
				if err := os.WriteFile(newFile, []byte("new"), 0o600); err != nil {
					t.Fatal(err)
				}
			},
			validate: func(t *testing.T, storageHome string) {
				oldFile := filepath.Join(storageHome, "temp", "old.tmp")
				if _, err := os.Stat(oldFile); !errors.Is(err, fs.ErrNotExist) {
					t.Error("Old file should be removed")
				}
				newFile := filepath.Join(storageHome, "temp", "new.tmp")
				if _, err := os.Stat(newFile); err != nil {
					t.Error("New file should still exist")
				}
			},
		},
		{
			name:      "clean all directories",
			cleanType: "all",
			olderThan: time.Hour,
			dryRun:    false,
			setup: func(t *testing.T, storageHome string) {
				for _, dir := range []string{"cache", "temp"} {
					dirPath := filepath.Join(storageHome, dir)
					if err := os.MkdirAll(dirPath, 0o755); err != nil {
						t.Fatal(err)
					}
					oldFile := filepath.Join(dirPath, "old.txt")
					if err := os.WriteFile(oldFile, []byte("old"), 0o600); err != nil {
						t.Fatal(err)
					}
					oldTime := time.Now().Add(-2 * time.Hour)
					if err := os.Chtimes(oldFile, oldTime, oldTime); err != nil {
						t.Fatal(err)
					}
				}
			},
			validate: func(t *testing.T, storageHome string) {
				for _, dir := range []string{"cache", "temp"} {
					oldFile := filepath.Join(storageHome, dir, "old.txt")
					if _, err := os.Stat(oldFile); !errors.Is(err, fs.ErrNotExist) {
						t.Errorf("Old file in %s should be removed", dir)
					}
				}
			},
		},
		{
			name:      "invalid clean type",
			cleanType: "invalid",
			olderThan: time.Hour,
			dryRun:    false,
			setup:     func(_ *testing.T, _ string) {},
			wantErr:   true,
		},
		{
			name:      "missing directory - should not error",
			cleanType: "cache",
			olderThan: time.Hour,
			dryRun:    false,
			setup:     func(_ *testing.T, _ string) {},
			validate: func(_ *testing.T, _ string) {
				// No validation needed - just ensuring no error
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary storage directory
			storageHome, err := os.MkdirTemp("", "storage-test-")
			if err != nil {
				t.Fatal(err)
			}
			defer os.RemoveAll(storageHome)

			// Run setup
			if tt.setup != nil {
				tt.setup(t, storageHome)
			}

			// Run Clean function
			err = Clean(storageHome, tt.olderThan, tt.cleanType, tt.dryRun)

			if (err != nil) != tt.wantErr {
				t.Errorf("Clean() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			// Run validation
			if !tt.wantErr && tt.validate != nil {
				tt.validate(t, storageHome)
			}
		})
	}
}

func TestCleanDirectory(t *testing.T) {
	// Create temporary directory
	tempDir, err := os.MkdirTemp("", "clean-test-")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	// Create test files
	oldFile := filepath.Join(tempDir, "old.txt")
	if err := os.WriteFile(oldFile, []byte("old"), 0o600); err != nil {
		t.Fatal(err)
	}
	oldTime := time.Now().Add(-2 * time.Hour)
	if err := os.Chtimes(oldFile, oldTime, oldTime); err != nil {
		t.Fatal(err)
	}

	newFile := filepath.Join(tempDir, "new.txt")
	if err := os.WriteFile(newFile, []byte("new"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Clean files older than 1 hour
	cutoffTime := time.Now().Add(-1 * time.Hour)
	removed, err := cleanDirectory(tempDir, cutoffTime, false)
	if err != nil {
		t.Fatalf("cleanDirectory() error = %v", err)
	}

	if removed != 1 {
		t.Errorf("cleanDirectory() removed %d files, want 1", removed)
	}

	// Verify old file is removed
	if _, err := os.Stat(oldFile); !errors.Is(err, fs.ErrNotExist) {
		t.Error("Old file should be removed")
	}

	// Verify new file still exists
	if _, err := os.Stat(newFile); err != nil {
		t.Error("New file should still exist")
	}
}

func TestVacuum(t *testing.T) {
	t.Run("returns error for non-existent storage", func(t *testing.T) {
		err := Vacuum("/nonexistent/path")
		if err == nil {
			t.Error("Expected Vacuum to return error, got nil")
		}
	})

	t.Run("succeeds with no options on valid storage", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		err := Vacuum(storageDir)
		if err != nil {
			t.Errorf("Expected Vacuum to succeed, got error: %v", err)
		}
	})

	t.Run("compacts JSON files", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create a JSON file with extra whitespace
		testFile := filepath.Join(storageDir, "repos", "test.json")
		prettyJSON := `{
    "key1": "value1",
    "key2": "value2",
    "nested": {
        "a": 1,
        "b": 2
    }
}`
		if err := os.WriteFile(testFile, []byte(prettyJSON), 0o600); err != nil {
			t.Fatal(err)
		}

		originalSize := int64(len(prettyJSON))

		err := Vacuum(storageDir, VacuumCompress{})
		if err != nil {
			t.Errorf("Expected Vacuum to succeed, got error: %v", err)
		}

		// Verify file was compacted
		compactedData, err := os.ReadFile(testFile)
		if err != nil {
			t.Fatal(err)
		}

		if int64(len(compactedData)) >= originalSize {
			t.Errorf("Expected file to be smaller after compaction, got %d bytes (original: %d)", len(compactedData), originalSize)
		}

		// Verify JSON is still valid
		var parsed map[string]interface{}
		if err := json.Unmarshal(compactedData, &parsed); err != nil {
			t.Errorf("Compacted JSON is invalid: %v", err)
		}

		// Verify data is preserved
		if parsed["key1"] != "value1" {
			t.Errorf("Expected key1 to be 'value1', got %v", parsed["key1"])
		}
	})

	t.Run("removes empty directories", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create nested empty directories
		emptyDir := filepath.Join(storageDir, "repos", "owner", "repo", "empty")
		if err := os.MkdirAll(emptyDir, 0o755); err != nil {
			t.Fatal(err)
		}

		err := Vacuum(storageDir, VacuumDefragment{})
		if err != nil {
			t.Errorf("Expected Vacuum to succeed, got error: %v", err)
		}

		// Verify empty directories were removed
		if directoryExists(emptyDir) {
			t.Error("Expected empty directory to be removed")
		}
	})

	t.Run("verifies integrity after vacuum", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		err := Vacuum(storageDir, VacuumVerify{})
		if err != nil {
			t.Errorf("Expected Vacuum with verify to succeed, got error: %v", err)
		}
	})

	t.Run("all options combined", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create a JSON file with extra whitespace
		testFile := filepath.Join(storageDir, "repos", "test.json")
		prettyJSON := `{"key": "value"  }`
		if err := os.WriteFile(testFile, []byte(prettyJSON), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create an empty directory
		emptyDir := filepath.Join(storageDir, "repos", "empty")
		if err := os.MkdirAll(emptyDir, 0o755); err != nil {
			t.Fatal(err)
		}

		err := Vacuum(storageDir, VacuumCompress{}, VacuumDefragment{}, VacuumVerify{})
		if err != nil {
			t.Errorf("Expected Vacuum to succeed, got error: %v", err)
		}

		// Verify empty directory was removed
		if directoryExists(emptyDir) {
			t.Error("Expected empty directory to be removed")
		}

		// Verify JSON file still exists and is valid
		data, err := os.ReadFile(testFile)
		if err != nil {
			t.Fatal(err)
		}
		var parsed map[string]interface{}
		if err := json.Unmarshal(data, &parsed); err != nil {
			t.Errorf("JSON file should still be valid: %v", err)
		}
	})
}

func TestManageLocks(t *testing.T) {
	t.Run("fails for non-existent storage", func(t *testing.T) {
		err := ManageLocks("/nonexistent/storage", true, false, false, false, "")
		if err == nil {
			t.Error("Expected error for non-existent storage")
		}
		if !contains(err.Error(), "storage directory does not exist") {
			t.Errorf("Expected error about non-existent storage, got %q", err.Error())
		}
	})

	t.Run("fails when no operation specified", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, false, false, false, false, "")
		if err == nil {
			t.Error("Expected error when no operation specified")
		}
		if !contains(err.Error(), "no operation specified") {
			t.Errorf("Expected error about no operation, got %q", err.Error())
		}
	})

	t.Run("fails when multiple operations specified", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, true, true, false, false, "")
		if err == nil {
			t.Error("Expected error when multiple operations specified")
		}
		if !contains(err.Error(), "only one operation") {
			t.Errorf("Expected error about multiple operations, got %q", err.Error())
		}
	})

	t.Run("list succeeds with no locks", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, true, false, false, false, "")
		if err != nil {
			t.Errorf("Expected list to succeed, got error: %v", err)
		}
	})

	t.Run("list shows existing locks", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create a lock file manually
		lockPath := filepath.Join(storageDir, "repos", "test.lock")
		lockContent := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, true, false, false, false, "")
		if err != nil {
			t.Errorf("Expected list to succeed, got error: %v", err)
		}
	})

	t.Run("status requires path", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, false, false, true, false, "")
		if err == nil {
			t.Error("Expected error when status has no path")
		}
		if !contains(err.Error(), "requires a path") {
			t.Errorf("Expected error about missing path, got %q", err.Error())
		}
	})

	t.Run("status shows lock info", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create a lock file
		lockPath := filepath.Join(storageDir, "repos", "test.lock")
		lockContent := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Check status using relative path
		err := ManageLocks(storageDir, false, false, true, false, "repos/test")
		if err != nil {
			t.Errorf("Expected status to succeed, got error: %v", err)
		}
	})

	t.Run("status shows no lock for non-existent path", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, false, false, true, false, "nonexistent/path")
		if err != nil {
			t.Errorf("Expected status to succeed even for missing lock, got error: %v", err)
		}
	})

	t.Run("release requires path", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, false, true, false, false, "")
		if err == nil {
			t.Error("Expected error when release has no path")
		}
		if !contains(err.Error(), "requires a path") {
			t.Errorf("Expected error about missing path, got %q", err.Error())
		}
	})

	t.Run("release fails for non-existent lock", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		err := ManageLocks(storageDir, false, true, false, false, "nonexistent/path")
		if err == nil {
			t.Error("Expected error when releasing non-existent lock")
		}
		if !contains(err.Error(), "no lock exists") {
			t.Errorf("Expected error about non-existent lock, got %q", err.Error())
		}
	})

	t.Run("release stale lock succeeds", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create a stale lock file (PID that doesn't exist)
		lockPath := filepath.Join(storageDir, "repos", "test.lock")
		lockContent := fmt.Sprintf("locked_at: %s\npid: 99999999\n", time.Now().Format(time.RFC3339))
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Release should succeed for stale lock
		err := ManageLocks(storageDir, false, true, false, false, "repos/test")
		if err != nil {
			t.Errorf("Expected release of stale lock to succeed, got error: %v", err)
		}

		// Verify lock file was removed
		if fileExists(lockPath) {
			t.Error("Lock file should be removed after release")
		}
	})

	t.Run("release active lock requires force", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create an active lock file (current PID)
		lockPath := filepath.Join(storageDir, "repos", "test.lock")
		lockContent := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Release without force should fail
		err := ManageLocks(storageDir, false, true, false, false, "repos/test")
		if err == nil {
			t.Error("Expected error when releasing active lock without force")
		}
		if !contains(err.Error(), "use --force") {
			t.Errorf("Expected error suggesting --force, got %q", err.Error())
		}

		// Verify lock file still exists
		if !fileExists(lockPath) {
			t.Error("Lock file should not be removed without force")
		}
	})

	t.Run("release active lock with force succeeds", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create an active lock file (current PID)
		lockPath := filepath.Join(storageDir, "repos", "test.lock")
		lockContent := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Release with force should succeed
		err := ManageLocks(storageDir, false, true, false, true, "repos/test")
		if err != nil {
			t.Errorf("Expected release with force to succeed, got error: %v", err)
		}

		// Verify lock file was removed
		if fileExists(lockPath) {
			t.Error("Lock file should be removed after force release")
		}
	})
}

func TestParseLockFile(t *testing.T) {
	t.Run("parses valid lock file", func(t *testing.T) {
		lockPath := filepath.Join(t.TempDir(), "test.lock")
		lockedAt := time.Now().Truncate(time.Second)
		lockContent := fmt.Sprintf("locked_at: %s\npid: 12345\n", lockedAt.Format(time.RFC3339))
		if err := os.WriteFile(lockPath, []byte(lockContent), 0o600); err != nil {
			t.Fatal(err)
		}

		info, err := parseLockFile(lockPath)
		if err != nil {
			t.Errorf("Expected parse to succeed, got error: %v", err)
		}
		if info.PID != 12345 {
			t.Errorf("Expected PID 12345, got %d", info.PID)
		}
		if !info.LockedAt.Equal(lockedAt) {
			t.Errorf("Expected LockedAt %v, got %v", lockedAt, info.LockedAt)
		}
	})

	t.Run("handles empty lock file", func(t *testing.T) {
		lockPath := filepath.Join(t.TempDir(), "test.lock")
		if err := os.WriteFile(lockPath, []byte(""), 0o600); err != nil {
			t.Fatal(err)
		}

		info, err := parseLockFile(lockPath)
		if err != nil {
			t.Errorf("Expected parse to succeed for empty file, got error: %v", err)
		}
		if info.PID != 0 {
			t.Errorf("Expected PID 0 for empty file, got %d", info.PID)
		}
	})

	t.Run("handles malformed lock file", func(t *testing.T) {
		lockPath := filepath.Join(t.TempDir(), "test.lock")
		if err := os.WriteFile(lockPath, []byte("garbage content"), 0o600); err != nil {
			t.Fatal(err)
		}

		info, err := parseLockFile(lockPath)
		if err != nil {
			t.Errorf("Expected parse to succeed for malformed file, got error: %v", err)
		}
		if info.PID != 0 {
			t.Errorf("Expected PID 0 for malformed file, got %d", info.PID)
		}
	})
}

func TestResolveLockPath(t *testing.T) {
	storageHome := "/storage"

	tests := []struct {
		name     string
		path     string
		expected string
	}{
		{
			name:     "relative path without .lock",
			path:     "repos/test",
			expected: "/storage/repos/test.lock",
		},
		{
			name:     "relative path with .lock",
			path:     "repos/test.lock",
			expected: "/storage/repos/test.lock",
		},
		{
			name:     "absolute path without .lock",
			path:     "/absolute/path/test",
			expected: "/absolute/path/test.lock",
		},
		{
			name:     "absolute path with .lock",
			path:     "/absolute/path/test.lock",
			expected: "/absolute/path/test.lock",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := resolveLockPath(storageHome, tt.path)
			if result != tt.expected {
				t.Errorf("resolveLockPath(%q, %q) = %q, want %q", storageHome, tt.path, result, tt.expected)
			}
		})
	}
}

func TestFindLockFiles(t *testing.T) {
	t.Run("finds all lock files", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create multiple lock files
		lockPaths := []string{
			filepath.Join(storageDir, "repos", "lock1.lock"),
			filepath.Join(storageDir, "repos", "owner", "lock2.lock"),
			filepath.Join(storageDir, "cache", "lock3.lock"),
		}
		for _, path := range lockPaths {
			if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
				t.Fatal(err)
			}
			lockContent := fmt.Sprintf("locked_at: %s\npid: %d\n", time.Now().Format(time.RFC3339), os.Getpid())
			if err := os.WriteFile(path, []byte(lockContent), 0o600); err != nil {
				t.Fatal(err)
			}
		}

		// Create a non-lock file (should be ignored)
		nonLockPath := filepath.Join(storageDir, "repos", "data.json")
		if err := os.WriteFile(nonLockPath, []byte("{}"), 0o600); err != nil {
			t.Fatal(err)
		}

		locks, err := findLockFiles(storageDir)
		if err != nil {
			t.Fatalf("findLockFiles failed: %v", err)
		}

		if len(locks) != 3 {
			t.Errorf("Expected 3 lock files, got %d", len(locks))
		}
	})

	t.Run("returns empty for no locks", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		locks, err := findLockFiles(storageDir)
		if err != nil {
			t.Fatalf("findLockFiles failed: %v", err)
		}

		if len(locks) != 0 {
			t.Errorf("Expected 0 lock files, got %d", len(locks))
		}
	})
}

func TestExport(t *testing.T) {
	t.Run("exports to JSON format", func(t *testing.T) {
		storageDir := t.TempDir()
		outputPath := filepath.Join(t.TempDir(), "export.json")

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create test file
		testFile := filepath.Join(storageDir, "repos", "test.json")
		testContent := `{"key": "value"}`
		if err := os.WriteFile(testFile, []byte(testContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Export
		err := Export(storageDir, outputPath, "json", false, false)
		if err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		// Verify export file exists and contains data
		data, err := os.ReadFile(outputPath)
		if err != nil {
			t.Fatalf("Failed to read export file: %v", err)
		}

		var exportData ExportData
		if err := json.Unmarshal(data, &exportData); err != nil {
			t.Fatalf("Failed to parse export JSON: %v", err)
		}

		if exportData.Version != "1.0.0" {
			t.Errorf("Expected version 1.0.0, got %s", exportData.Version)
		}

		if content, ok := exportData.Files["repos/test.json"]; !ok || content != testContent {
			t.Errorf("Expected file content %q, got %q", testContent, content)
		}
	})

	t.Run("exports to JSON with metadata", func(t *testing.T) {
		storageDir := t.TempDir()
		outputPath := filepath.Join(t.TempDir(), "export.json")

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Export with metadata
		err := Export(storageDir, outputPath, "json", false, true)
		if err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		data, err := os.ReadFile(outputPath)
		if err != nil {
			t.Fatalf("Failed to read export file: %v", err)
		}

		var exportData ExportData
		if err := json.Unmarshal(data, &exportData); err != nil {
			t.Fatalf("Failed to parse export JSON: %v", err)
		}

		if exportData.Metadata == nil {
			t.Error("Expected metadata to be included")
		}
	})

	t.Run("exports compressed JSON", func(t *testing.T) {
		storageDir := t.TempDir()
		outputPath := filepath.Join(t.TempDir(), "export.json.gz")

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Export compressed
		err := Export(storageDir, outputPath, "json", true, false)
		if err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		// Verify file is gzip compressed
		data, err := os.ReadFile(outputPath)
		if err != nil {
			t.Fatalf("Failed to read export file: %v", err)
		}

		if len(data) < 2 || data[0] != 0x1f || data[1] != 0x8b {
			t.Error("Expected gzip compressed file")
		}
	})

	t.Run("exports to tar format", func(t *testing.T) {
		storageDir := t.TempDir()
		outputPath := filepath.Join(t.TempDir(), "export.tar")

		// Initialize storage
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatalf("Failed to initialize storage: %v", err)
		}

		// Create test file
		testFile := filepath.Join(storageDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(`{}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Export
		err := Export(storageDir, outputPath, "tar", false, false)
		if err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		// Verify tar file exists
		if !fileExists(outputPath) {
			t.Error("Export tar file not created")
		}
	})

	t.Run("fails for non-existent storage", func(t *testing.T) {
		outputPath := filepath.Join(t.TempDir(), "export.json")
		err := Export("/nonexistent/path", outputPath, "json", false, false)
		if err == nil {
			t.Error("Expected error for non-existent storage")
		}
	})

	t.Run("fails for unsupported format", func(t *testing.T) {
		storageDir := t.TempDir()
		if err := Initialize(storageDir, true, false); err != nil {
			t.Fatal(err)
		}

		outputPath := filepath.Join(t.TempDir(), "export.xml")
		err := Export(storageDir, outputPath, "xml", false, false)
		if err == nil {
			t.Error("Expected error for unsupported format")
		}
	})
}

func TestImport(t *testing.T) {
	t.Run("imports from JSON format", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.json")

		// Create import file
		exportData := ExportData{
			Version: "1.0.0",
			Files: map[string]string{
				"repos/imported.json": `{"imported": true}`,
			},
		}
		data, _ := json.MarshalIndent(exportData, "", "  ")
		if err := os.WriteFile(importFile, data, 0o600); err != nil {
			t.Fatal(err)
		}

		// Import
		err := Import(storageDir, importFile, "json", false, false)
		if err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify imported file
		importedPath := filepath.Join(storageDir, "repos", "imported.json")
		content, err := os.ReadFile(importedPath)
		if err != nil {
			t.Fatalf("Failed to read imported file: %v", err)
		}

		if string(content) != `{"imported": true}` {
			t.Errorf("Imported content mismatch: got %q", string(content))
		}
	})

	t.Run("import with merge adds new files", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.json")

		// Create existing file
		existingPath := filepath.Join(storageDir, "repos", "existing.json")
		if err := os.MkdirAll(filepath.Dir(existingPath), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(existingPath, []byte(`{"existing": true}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create import file with new file
		exportData := ExportData{
			Version: "1.0.0",
			Files: map[string]string{
				"repos/new.json": `{"new": true}`,
			},
		}
		data, _ := json.MarshalIndent(exportData, "", "  ")
		if err := os.WriteFile(importFile, data, 0o600); err != nil {
			t.Fatal(err)
		}

		// Import with merge
		err := Import(storageDir, importFile, "json", true, false)
		if err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify both files exist
		if !fileExists(existingPath) {
			t.Error("Existing file should still exist")
		}
		if !fileExists(filepath.Join(storageDir, "repos", "new.json")) {
			t.Error("New file should be imported")
		}
	})

	t.Run("import with merge and overwrite updates existing", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.json")

		// Create existing file
		existingPath := filepath.Join(storageDir, "repos", "file.json")
		if err := os.MkdirAll(filepath.Dir(existingPath), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(existingPath, []byte(`{"old": true}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create import file with same filename
		exportData := ExportData{
			Version: "1.0.0",
			Files: map[string]string{
				"repos/file.json": `{"new": true}`,
			},
		}
		data, _ := json.MarshalIndent(exportData, "", "  ")
		if err := os.WriteFile(importFile, data, 0o600); err != nil {
			t.Fatal(err)
		}

		// Import with merge and overwrite
		err := Import(storageDir, importFile, "json", true, true)
		if err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify file was overwritten
		content, err := os.ReadFile(existingPath)
		if err != nil {
			t.Fatal(err)
		}

		if string(content) != `{"new": true}` {
			t.Errorf("File should be overwritten, got %q", string(content))
		}
	})

	t.Run("import without merge fails on existing file", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.json")

		// Create existing file
		existingPath := filepath.Join(storageDir, "repos", "file.json")
		if err := os.MkdirAll(filepath.Dir(existingPath), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(existingPath, []byte(`{}`), 0o600); err != nil {
			t.Fatal(err)
		}

		// Create import file with same filename
		exportData := ExportData{
			Version: "1.0.0",
			Files: map[string]string{
				"repos/file.json": `{"new": true}`,
			},
		}
		data, _ := json.MarshalIndent(exportData, "", "  ")
		if err := os.WriteFile(importFile, data, 0o600); err != nil {
			t.Fatal(err)
		}

		// Import without merge should fail
		err := Import(storageDir, importFile, "json", false, false)
		if err == nil {
			t.Error("Expected error when file exists and merge not enabled")
		}
	})

	t.Run("imports compressed JSON", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.json.gz")

		// Create compressed import file
		exportData := ExportData{
			Version: "1.0.0",
			Files: map[string]string{
				"repos/compressed.json": `{"compressed": true}`,
			},
		}
		jsonData, _ := json.MarshalIndent(exportData, "", "  ")

		file, err := os.Create(importFile)
		if err != nil {
			t.Fatal(err)
		}
		gw := gzip.NewWriter(file)
		if _, err := gw.Write(jsonData); err != nil {
			t.Fatal(err)
		}
		gw.Close()
		file.Close()

		// Import
		err = Import(storageDir, importFile, "json", false, false)
		if err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify imported file
		content, _ := os.ReadFile(filepath.Join(storageDir, "repos", "compressed.json"))
		if string(content) != `{"compressed": true}` {
			t.Errorf("Imported content mismatch: got %q", string(content))
		}
	})

	t.Run("fails for non-existent input file", func(t *testing.T) {
		storageDir := t.TempDir()
		err := Import(storageDir, "/nonexistent/file.json", "json", false, false)
		if err == nil {
			t.Error("Expected error for non-existent input file")
		}
	})

	t.Run("fails for unsupported format", func(t *testing.T) {
		storageDir := t.TempDir()
		importFile := filepath.Join(t.TempDir(), "import.xml")
		if err := os.WriteFile(importFile, []byte("<xml/>"), 0o600); err != nil {
			t.Fatal(err)
		}

		err := Import(storageDir, importFile, "xml", false, false)
		if err == nil {
			t.Error("Expected error for unsupported format")
		}
	})
}

func TestExportImportRoundTrip(t *testing.T) {
	t.Run("JSON round trip preserves data", func(t *testing.T) {
		sourceDir := t.TempDir()
		destDir := t.TempDir()
		exportFile := filepath.Join(t.TempDir(), "export.json")

		// Initialize source storage
		if err := Initialize(sourceDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create test files
		testFiles := map[string]string{
			"repos/owner/repo/pr/123.json": `{"number": 123}`,
			"repos/owner/repo/pr/456.json": `{"number": 456}`,
		}
		for relPath, content := range testFiles {
			fullPath := filepath.Join(sourceDir, relPath)
			if err := os.MkdirAll(filepath.Dir(fullPath), 0o755); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(fullPath, []byte(content), 0o600); err != nil {
				t.Fatal(err)
			}
		}

		// Export
		if err := Export(sourceDir, exportFile, "json", false, false); err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		// Import to new location
		if err := Import(destDir, exportFile, "json", false, false); err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify all files were preserved
		for relPath, expectedContent := range testFiles {
			fullPath := filepath.Join(destDir, relPath)
			content, err := os.ReadFile(fullPath)
			if err != nil {
				t.Errorf("Failed to read %s: %v", relPath, err)
				continue
			}
			if string(content) != expectedContent {
				t.Errorf("Content mismatch for %s: got %q, want %q", relPath, string(content), expectedContent)
			}
		}
	})

	t.Run("tar round trip preserves data", func(t *testing.T) {
		sourceDir := t.TempDir()
		destDir := t.TempDir()
		exportFile := filepath.Join(t.TempDir(), "export.tar")

		// Initialize source storage
		if err := Initialize(sourceDir, true, false); err != nil {
			t.Fatal(err)
		}

		// Create test file
		testContent := `{"data": "test"}`
		testFile := filepath.Join(sourceDir, "repos", "test.json")
		if err := os.WriteFile(testFile, []byte(testContent), 0o600); err != nil {
			t.Fatal(err)
		}

		// Export
		if err := Export(sourceDir, exportFile, "tar", false, false); err != nil {
			t.Fatalf("Export failed: %v", err)
		}

		// Import to new location
		if err := Import(destDir, exportFile, "tar", false, false); err != nil {
			t.Fatalf("Import failed: %v", err)
		}

		// Verify file was preserved
		content, err := os.ReadFile(filepath.Join(destDir, "repos", "test.json"))
		if err != nil {
			t.Fatalf("Failed to read imported file: %v", err)
		}
		if string(content) != testContent {
			t.Errorf("Content mismatch: got %q, want %q", string(content), testContent)
		}
	})
}

func TestVerify_StorageDirectoryDoesNotExist(t *testing.T) {
	err := Verify("/nonexistent/storage/path")
	if err == nil {
		t.Error("Expected Verify to return error for nonexistent storage, got nil")
	}

	if !contains(err.Error(), "storage directory does not exist") {
		t.Errorf("Expected error about storage directory not existing, got %q", err.Error())
	}
}

func TestVerify_MissingReposSubdirectory(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create metadata.json
	metadataPath := filepath.Join(tmpDir, "metadata.json")
	metadata := map[string]interface{}{
		"version":     "1.0.0",
		"created_at":  "2024-01-01T00:00:00Z",
		"description": "test storage",
	}
	metadataData, _ := json.MarshalIndent(metadata, "", "  ")
	if err := os.WriteFile(metadataPath, metadataData, 0o600); err != nil {
		t.Fatalf("Failed to create metadata.json: %v", err)
	}

	// Don't create repos directory - this should cause verification to fail
	// (cache and temp are optional)

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for missing repos subdirectory, got nil")
	}

	if !contains(err.Error(), "required subdirectory missing") || !contains(err.Error(), "repos") {
		t.Errorf("Expected error about missing repos subdirectory, got %q", err.Error())
	}
}

func TestVerify_MissingMetadata(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create required repos subdirectory but no metadata.json
	if err := os.MkdirAll(filepath.Join(tmpDir, "repos"), 0o755); err != nil {
		t.Fatalf("Failed to create repos dir: %v", err)
	}

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for missing metadata, got nil")
	}

	if !contains(err.Error(), "metadata.json not found") || !contains(err.Error(), "gh-storage init --force") {
		t.Errorf("Expected helpful error about missing metadata.json, got %q", err.Error())
	}
}

func TestVerify_InvalidMetadataJSON(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create required repos subdirectory
	if err := os.MkdirAll(filepath.Join(tmpDir, "repos"), 0o755); err != nil {
		t.Fatalf("Failed to create repos dir: %v", err)
	}

	// Create invalid metadata.json
	metadataPath := filepath.Join(tmpDir, "metadata.json")
	if err := os.WriteFile(metadataPath, []byte("invalid json"), 0o600); err != nil {
		t.Fatalf("Failed to create metadata.json: %v", err)
	}

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for invalid metadata JSON, got nil")
	}

	if !contains(err.Error(), "invalid metadata.json format") {
		t.Errorf("Expected error about invalid JSON format, got %q", err.Error())
	}
}

func TestVerify_MissingMetadataField(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create required repos subdirectory
	if err := os.MkdirAll(filepath.Join(tmpDir, "repos"), 0o755); err != nil {
		t.Fatalf("Failed to create repos dir: %v", err)
	}

	// Create metadata.json missing a required field
	metadataPath := filepath.Join(tmpDir, "metadata.json")
	metadata := map[string]interface{}{
		"version":    "1.0.0",
		"created_at": "2024-01-01T00:00:00Z",
		// missing "description"
	}
	metadataData, _ := json.MarshalIndent(metadata, "", "  ")
	if err := os.WriteFile(metadataPath, metadataData, 0o600); err != nil {
		t.Fatalf("Failed to create metadata.json: %v", err)
	}

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for missing metadata field, got nil")
	}

	if !contains(err.Error(), "metadata.json missing required field: description") {
		t.Errorf("Expected error about missing field, got %q", err.Error())
	}
}

func TestVerify_Success(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Initialize it properly (use force=true since TempDir already created the directory)
	if err := Initialize(tmpDir, true, false); err != nil {
		t.Fatalf("Failed to initialize storage: %v", err)
	}

	// Verify should succeed
	err := Verify(tmpDir)
	if err != nil {
		t.Errorf("Expected Verify to succeed, got error: %v", err)
	}
}

func TestVerify_SuccessWithoutCacheAndTemp(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create only the essential components (repos and metadata.json)
	// Cache and temp directories are optional
	if err := os.MkdirAll(filepath.Join(tmpDir, "repos"), 0o755); err != nil {
		t.Fatalf("Failed to create repos dir: %v", err)
	}

	metadataPath := filepath.Join(tmpDir, "metadata.json")
	metadata := map[string]interface{}{
		"version":     "1.0.0",
		"created_at":  time.Now(),
		"description": "test storage without cache/temp",
	}
	metadataData, _ := json.MarshalIndent(metadata, "", "  ")
	if err := os.WriteFile(metadataPath, metadataData, 0o600); err != nil {
		t.Fatalf("Failed to create metadata.json: %v", err)
	}

	// Verify should succeed even without cache and temp directories
	err := Verify(tmpDir)
	if err != nil {
		t.Errorf("Expected Verify to succeed without cache/temp dirs, got error: %v", err)
	}
}

func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && stringContains(s, substr))
}

func stringContains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
