package storage

import (
	"encoding/json"
	"errors"
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

func TestCreateBackup_NotImplemented(t *testing.T) {
	backupID, err := CreateBackup("storage", "backups", "path", false, false, "test")
	if err == nil {
		t.Error("Expected CreateBackup to return error, got nil")
	}

	if backupID != "" {
		t.Errorf("Expected empty backup ID, got %q", backupID)
	}

	expectedMsg := "backup not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestRestoreBackup_NotImplemented(t *testing.T) {
	err := RestoreBackup("storage", "backups", "backup-id", "path", false)
	if err == nil {
		t.Error("Expected RestoreBackup to return error, got nil")
	}

	expectedMsg := "restore not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestListBackups_NotImplemented(t *testing.T) {
	err := ListBackups("backups")
	if err == nil {
		t.Error("Expected ListBackups to return error, got nil")
	}

	expectedMsg := "list backups not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
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

func TestVacuum_NotImplemented(t *testing.T) {
	err := Vacuum("storage", false, false, false)
	if err == nil {
		t.Error("Expected Vacuum to return error, got nil")
	}

	expectedMsg := "vacuum not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestManageLocks_NotImplemented(t *testing.T) {
	err := ManageLocks("storage", false, false, false, false, "")
	if err == nil {
		t.Error("Expected ManageLocks to return error, got nil")
	}

	expectedMsg := "lock management not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestExport_NotImplemented(t *testing.T) {
	err := Export("storage", "export.json", "json", false, false)
	if err == nil {
		t.Error("Expected Export to return error, got nil")
	}

	expectedMsg := "export not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
}

func TestImport_NotImplemented(t *testing.T) {
	err := Import("storage", "import.json", "json", false, false)
	if err == nil {
		t.Error("Expected Import to return error, got nil")
	}

	expectedMsg := "import not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
	}
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
