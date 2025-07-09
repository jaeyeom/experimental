package storage

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"testing"
	"time"
)

func TestFileSystemStorage_NewFileSystemStorage(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	// Test that the storage works correctly by testing its interface methods
	if storage == nil {
		t.Error("Expected storage to be initialized")
	}
}

func TestFileSystemStorage_SetAndGet(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testData := map[string]interface{}{
		"name":    "test-repo",
		"owner":   "test-user",
		"private": false,
		"created": float64(time.Now().Unix()), // Use float64 to match JSON unmarshaling
	}

	testKey := "repos/test-user/test-repo/metadata"

	// Test Set
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Verify file was created by checking if the key exists
	if !storage.Exists(testKey) {
		t.Error("Expected file to be created")
	}

	// Test Get
	var retrievedData map[string]interface{}
	err = storage.Get(testKey, &retrievedData)
	if err != nil {
		t.Fatalf("Failed to get data: %v", err)
	}

	if !reflect.DeepEqual(testData, retrievedData) {
		t.Errorf("Retrieved data does not match original. Expected: %v, Got: %v", testData, retrievedData)
	}
}

func TestFileSystemStorage_GetNonExistentKey(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	var data map[string]interface{}
	err = storage.Get("nonexistent/key", &data)
	if err == nil {
		t.Error("Expected error when getting non-existent key")
	}
}

func TestFileSystemStorage_Delete(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/key"
	testData := map[string]string{"test": "value"}

	// First set the data
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Verify it exists
	if !storage.Exists(testKey) {
		t.Error("Expected key to exist before deletion")
	}

	// Delete the data
	err = storage.Delete(testKey)
	if err != nil {
		t.Fatalf("Failed to delete data: %v", err)
	}

	// Verify it's gone
	if storage.Exists(testKey) {
		t.Error("Expected key to not exist after deletion")
	}

	// Deleting non-existent key should not error
	err = storage.Delete("nonexistent/key")
	if err != nil {
		t.Errorf("Expected no error when deleting non-existent key, got: %v", err)
	}
}

func TestFileSystemStorage_Exists(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/exists"
	testData := map[string]string{"test": "value"}

	// Key should not exist initially
	if storage.Exists(testKey) {
		t.Error("Expected key to not exist initially")
	}

	// Set the data
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Key should now exist
	if !storage.Exists(testKey) {
		t.Error("Expected key to exist after setting")
	}
}

func TestFileSystemStorage_List(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testData := map[string]string{"test": "value"}
	testKeys := []string{
		"repos/owner1/repo1/metadata",
		"repos/owner1/repo2/metadata",
		"repos/owner2/repo1/metadata",
		"cache/github/users/user1",
	}

	// Set up test data
	for _, key := range testKeys {
		err = storage.Set(key, testData)
		if err != nil {
			t.Fatalf("Failed to set data for key %s: %v", key, err)
		}
	}

	// Test listing with prefix
	files, err := storage.List("repos/owner1")
	if err != nil {
		t.Fatalf("Failed to list files: %v", err)
	}

	expectedCount := 2
	if len(files) != expectedCount {
		t.Errorf("Expected %d files, got %d", expectedCount, len(files))
	}

	// Test listing all files
	files, err = storage.List("")
	if err != nil {
		t.Fatalf("Failed to list all files: %v", err)
	}

	if len(files) != len(testKeys) {
		t.Errorf("Expected %d files, got %d", len(testKeys), len(files))
	}
}

func TestFileSystemStorage_GetChildren(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testData := map[string]string{"test": "value"}
	testKeys := []string{
		"repos/owner1/repo1/metadata",
		"repos/owner1/repo2/metadata",
		"repos/owner2/repo1/metadata",
	}

	// Set up test data
	for _, key := range testKeys {
		err = storage.Set(key, testData)
		if err != nil {
			t.Fatalf("Failed to set data for key %s: %v", key, err)
		}
	}

	// Test getting children of repos directory
	children, err := storage.GetChildren("repos")
	if err != nil {
		t.Fatalf("Failed to get children: %v", err)
	}

	expectedChildren := []string{"owner1", "owner2"}
	if len(children) != len(expectedChildren) {
		t.Errorf("Expected %d children, got %d", len(expectedChildren), len(children))
	}

	for _, expectedChild := range expectedChildren {
		found := false
		for _, child := range children {
			if child == expectedChild {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected child %s not found", expectedChild)
		}
	}
}

func TestFileSystemStorage_GetMetadata(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/metadata"
	testData := map[string]string{"test": "value"}

	// Set some data
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Get metadata
	metadata, err := storage.GetMetadata(testKey)
	if err != nil {
		t.Fatalf("Failed to get metadata: %v", err)
	}

	if metadata.Type != "file" {
		t.Errorf("Expected type 'file', got %s", metadata.Type)
	}

	if metadata.Size <= 0 {
		t.Error("Expected size to be greater than 0")
	}

	if metadata.CreatedAt.IsZero() || metadata.ModifiedAt.IsZero() {
		t.Error("Expected created and modified times to be set")
	}
}

func TestFileSystemStorage_WithLock(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/lock"
	executed := false

	err = storage.WithLock(testKey, func() error {
		executed = true
		return nil
	})
	if err != nil {
		t.Fatalf("Failed to execute with lock: %v", err)
	}

	if !executed {
		t.Error("Expected function to be executed")
	}
}

func TestFileSystemStorage_WithLockError(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/lock"
	expectedError := fmt.Errorf("test error")

	err = storage.WithLock(testKey, func() error {
		return expectedError
	})

	if err != expectedError {
		t.Errorf("Expected error %v, got %v", expectedError, err)
	}
}

func TestFileSystemStorage_GetFormatted(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/formatted"
	testData := map[string]interface{}{
		"name":  "test",
		"value": 42,
	}

	// Set some data
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Test getting formatted data (this would normally output to stdout)
	// For testing, we'll just verify it doesn't error
	err = storage.GetFormatted(testKey, "json", true)
	if err != nil {
		t.Errorf("Failed to get formatted data: %v", err)
	}

	err = storage.GetFormatted(testKey, "raw", false)
	if err != nil {
		t.Errorf("Failed to get raw data: %v", err)
	}
}

func TestFileSystemStorage_SetFormatted(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testKey := "test/formatted"
	testJSON := `{"name":"test","value":42}`

	// Test setting JSON data
	err = storage.SetFormatted(testKey, testJSON, false, true, false)
	if err != nil {
		t.Fatalf("Failed to set formatted data: %v", err)
	}

	// Verify the data was stored correctly
	var retrievedData map[string]interface{}
	err = storage.Get(testKey, &retrievedData)
	if err != nil {
		t.Fatalf("Failed to get data: %v", err)
	}

	if retrievedData["name"] != "test" {
		t.Errorf("Expected name 'test', got %v", retrievedData["name"])
	}

	if retrievedData["value"] != float64(42) { // JSON unmarshals numbers as float64
		t.Errorf("Expected value 42, got %v", retrievedData["value"])
	}
}

func TestFileSystemStorage_SetFormattedFromFile(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	// Create a test file
	testFile := filepath.Join(tempDir, "testdata.json")
	testJSON := `{"name":"test","value":42}`
	err = os.WriteFile(testFile, []byte(testJSON), 0o600)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	testKey := "test/from-file"

	// Test setting data from file
	err = storage.SetFormatted(testKey, testFile, true, true, false)
	if err != nil {
		t.Fatalf("Failed to set formatted data from file: %v", err)
	}

	// Verify the data was stored correctly
	var retrievedData map[string]interface{}
	err = storage.Get(testKey, &retrievedData)
	if err != nil {
		t.Fatalf("Failed to get data: %v", err)
	}

	if retrievedData["name"] != "test" {
		t.Errorf("Expected name 'test', got %v", retrievedData["name"])
	}
}

func TestInitialize(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storageDir := filepath.Join(tempDir, "storage")

	// Test initialization
	err = Initialize(storageDir, false, false)
	if err != nil {
		t.Fatalf("Failed to initialize storage: %v", err)
	}

	// Verify directory was created
	if !directoryExists(storageDir) {
		t.Error("Expected storage directory to be created")
	}

	// Verify metadata file was created
	metadataPath := filepath.Join(storageDir, "metadata.json")
	if _, err := os.Stat(metadataPath); errors.Is(err, os.ErrNotExist) {
		t.Error("Expected metadata file to be created")
	}

	// Verify subdirectories were created
	expectedSubdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range expectedSubdirs {
		subdirPath := filepath.Join(storageDir, subdir)
		if !directoryExists(subdirPath) {
			t.Errorf("Expected subdirectory %s to be created", subdir)
		}
	}

	// Test that initialization fails without force flag when directory exists
	err = Initialize(storageDir, false, false)
	if err == nil {
		t.Error("Expected error when initializing existing directory without force flag")
	}

	// Test that initialization succeeds with force flag
	err = Initialize(storageDir, true, false)
	if err != nil {
		t.Errorf("Expected no error when initializing existing directory with force flag: %v", err)
	}
}

func TestInitializeWithNonExistentParent(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storageDir := filepath.Join(tempDir, "nonexistent", "storage")

	// Test initialization with non-existent parent directory
	err = Initialize(storageDir, false, false)
	if err != nil {
		t.Fatalf("Failed to initialize storage with non-existent parent: %v", err)
	}

	// Verify directory was created
	if !directoryExists(storageDir) {
		t.Error("Expected storage directory to be created")
	}
}

func TestDirectoryExists(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Test existing directory
	if !directoryExists(tempDir) {
		t.Error("Expected existing directory to return true")
	}

	// Test non-existent directory
	nonExistentDir := filepath.Join(tempDir, "nonexistent")
	if directoryExists(nonExistentDir) {
		t.Error("Expected non-existent directory to return false")
	}

	// Test file (not directory)
	testFile := filepath.Join(tempDir, "testfile")
	err = os.WriteFile(testFile, []byte("test"), 0o600)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	if directoryExists(testFile) {
		t.Error("Expected file to return false for directory check")
	}
}

func TestFileSystemStorage_ListFiles(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testData := map[string]string{"test": "value"}
	testKeys := []string{
		"repos/owner1/repo1/metadata",
		"repos/owner1/repo2/metadata",
		"repos/owner2/repo1/metadata",
		"cache/github/users/user1",
	}

	// Set up test data
	for _, key := range testKeys {
		err = storage.Set(key, testData)
		if err != nil {
			t.Fatalf("Failed to set data for key %s: %v", key, err)
		}
	}

	// Test listing with different formats and options
	// This would normally output to stdout, so we just verify it doesn't error
	err = storage.ListFiles("repos", false, "table", "")
	if err != nil {
		t.Errorf("Failed to list files: %v", err)
	}

	err = storage.ListFiles("repos", true, "json", "")
	if err != nil {
		t.Errorf("Failed to list files recursively: %v", err)
	}

	err = storage.ListFiles("repos", true, "json", "owner1")
	if err != nil {
		t.Errorf("Failed to list files with filter: %v", err)
	}
}

func TestFileSystemStorage_ShowInfo(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	storage, err := NewFileSystemStorage(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStorage: %v", err)
	}

	testData := map[string]string{"test": "value"}
	testKey := "test/info"

	// Set up test data
	err = storage.Set(testKey, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	// Test showing info with different formats
	// This would normally output to stdout, so we just verify it doesn't error
	err = storage.ShowInfo(testKey, false, "table")
	if err != nil {
		t.Errorf("Failed to show info: %v", err)
	}

	err = storage.ShowInfo(testKey, true, "json")
	if err != nil {
		t.Errorf("Failed to show detailed info: %v", err)
	}

	err = storage.ShowInfo("", false, "table")
	if err != nil {
		t.Errorf("Failed to show root info: %v", err)
	}
}
