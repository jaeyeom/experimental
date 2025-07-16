package storage

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
)

// FileSystemStore implements the Store interface using the local filesystem.
type FileSystemStore struct {
	rootPath string
}

// GetRootPath returns the root path of the filesystem store.
func (fs *FileSystemStore) GetRootPath() string {
	return fs.rootPath
}

// NewFileSystemStore creates a new FileSystemStore instance with the specified root path.
func NewFileSystemStore(rootPath string) (*FileSystemStore, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	return &FileSystemStore{
		rootPath: absPath,
	}, nil
}

func (fs *FileSystemStore) getFilePath(key string) string {
	return filepath.Join(fs.rootPath, key)
}

func (fs *FileSystemStore) Get(key string, dest interface{}) error {
	filePath := fs.getFilePath(key)

	data, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %w", key, err)
	}

	if err := json.Unmarshal(data, dest); err != nil {
		return fmt.Errorf("failed to unmarshal data for key %s: %w", key, err)
	}

	return nil
}

func (fs *FileSystemStore) Set(key string, data interface{}) error {
	filePath := fs.getFilePath(key)

	if err := os.MkdirAll(filepath.Dir(filePath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for key %s: %w", key, err)
	}

	jsonData, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal data for key %s: %w", key, err)
	}

	if err := os.WriteFile(filePath, jsonData, 0o600); err != nil {
		return fmt.Errorf("failed to write file for key %s: %w", key, err)
	}

	return nil
}

func (fs *FileSystemStore) Delete(key string) error {
	filePath := fs.getFilePath(key)

	if err := os.Remove(filePath); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("failed to delete file for key %s: %w", key, err)
	}

	return nil
}

func (fs *FileSystemStore) Exists(key string) bool {
	filePath := fs.getFilePath(key)
	_, err := os.Stat(filePath)
	return err == nil
}
