package storage

import (
	"os"
	"testing"
)

func TestFileSystemStore_BasicOperations(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	store, err := NewFileSystemStore(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStore: %v", err)
	}

	// Test Set and Get
	testData := map[string]interface{}{
		"name":  "test",
		"value": 42,
	}

	key := "test/data.json"
	err = store.Set(key, testData)
	if err != nil {
		t.Fatalf("Failed to set data: %v", err)
	}

	var retrievedData map[string]interface{}
	err = store.Get(key, &retrievedData)
	if err != nil {
		t.Fatalf("Failed to get data: %v", err)
	}

	if retrievedData["name"] != "test" {
		t.Errorf("Expected name to be 'test', got %v", retrievedData["name"])
	}
	if retrievedData["value"] != float64(42) { // JSON unmarshaling converts numbers to float64
		t.Errorf("Expected value to be 42, got %v", retrievedData["value"])
	}

	// Test Exists
	if !store.Exists(key) {
		t.Error("Expected key to exist")
	}

	// Test Delete
	err = store.Delete(key)
	if err != nil {
		t.Fatalf("Failed to delete data: %v", err)
	}

	if store.Exists(key) {
		t.Error("Expected key to not exist after deletion")
	}
}

func TestFileSystemStore_NonExistentKey(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "gh-storage-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	store, err := NewFileSystemStore(tempDir)
	if err != nil {
		t.Fatalf("Failed to create FileSystemStore: %v", err)
	}

	// Test getting non-existent key
	var data map[string]interface{}
	err = store.Get("nonexistent/key.json", &data)
	if err == nil {
		t.Error("Expected error when getting non-existent key")
	}

	// Test that non-existent key doesn't exist
	if store.Exists("nonexistent/key.json") {
		t.Error("Expected non-existent key to not exist")
	}
}
