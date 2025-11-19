package storage

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
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

func TestClean_NotImplemented(t *testing.T) {
	err := Clean("storage", "30d", "temp", false)
	if err == nil {
		t.Error("Expected Clean to return error, got nil")
	}

	expectedMsg := "clean not implemented"
	if err.Error() != expectedMsg {
		t.Errorf("Expected error message %q, got %q", expectedMsg, err.Error())
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

func TestVerify_MissingSubdirectory(t *testing.T) {
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

	// Create only some subdirectories (missing "temp")
	if err := os.MkdirAll(filepath.Join(tmpDir, "repos"), 0o755); err != nil {
		t.Fatalf("Failed to create repos dir: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(tmpDir, "cache"), 0o755); err != nil {
		t.Fatalf("Failed to create cache dir: %v", err)
	}

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for missing subdirectory, got nil")
	}

	if !contains(err.Error(), "required subdirectory missing") || !contains(err.Error(), "temp") {
		t.Errorf("Expected error about missing subdirectory temp, got %q", err.Error())
	}
}

func TestVerify_MissingMetadata(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create subdirectories but no metadata.json
	subdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range subdirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, subdir), 0o755); err != nil {
			t.Fatalf("Failed to create subdir %s: %v", subdir, err)
		}
	}

	err := Verify(tmpDir)
	if err == nil {
		t.Error("Expected Verify to return error for missing metadata, got nil")
	}

	if !contains(err.Error(), "failed to read metadata.json") {
		t.Errorf("Expected error about missing metadata.json, got %q", err.Error())
	}
}

func TestVerify_InvalidMetadataJSON(t *testing.T) {
	// Create a temporary storage directory
	tmpDir := t.TempDir()

	// Create subdirectories
	subdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range subdirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, subdir), 0o755); err != nil {
			t.Fatalf("Failed to create subdir %s: %v", subdir, err)
		}
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

	// Create subdirectories
	subdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range subdirs {
		if err := os.MkdirAll(filepath.Join(tmpDir, subdir), 0o755); err != nil {
			t.Fatalf("Failed to create subdir %s: %v", subdir, err)
		}
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
