package storage

import (
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// Metadata represents file metadata with creation time, modification time, size, and other attributes.
type Metadata struct {
	CreatedAt   time.Time `json:"createdAt"`
	ModifiedAt  time.Time `json:"modifiedAt"`
	Size        int64     `json:"size"`
	Type        string    `json:"type"`
	Version     string    `json:"version"`
	Description string    `json:"description,omitempty"`
	Checksum    string    `json:"checksum,omitempty"`
}

// FileSystemMetadataManager implements the MetadataManager interface for filesystem metadata.
type FileSystemMetadataManager struct {
	rootPath string
}

// NewFileSystemMetadataManager creates a new FileSystemMetadataManager instance.
func NewFileSystemMetadataManager(rootPath string) (*FileSystemMetadataManager, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	return &FileSystemMetadataManager{
		rootPath: absPath,
	}, nil
}

func (fsmm *FileSystemMetadataManager) getFilePath(key string) string {
	return filepath.Join(fsmm.rootPath, key)
}

func (fsmm *FileSystemMetadataManager) GetMetadata(key string) (*Metadata, error) {
	filePath := fsmm.getFilePath(key)

	info, err := os.Stat(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to get file info for key %s: %w", key, err)
	}

	metadata := &Metadata{
		CreatedAt:  info.ModTime(),
		ModifiedAt: info.ModTime(),
		Size:       info.Size(),
		Type:       "file",
	}

	if info.IsDir() {
		metadata.Type = "directory"
	}

	return metadata, nil
}

func (fsmm *FileSystemMetadataManager) SetMetadata(key string, metadata *Metadata) error {
	filePath := fsmm.getFilePath(key)

	if metadata.ModifiedAt.IsZero() {
		return nil
	}

	err := os.Chtimes(filePath, time.Now(), metadata.ModifiedAt)
	if err != nil {
		return fmt.Errorf("failed to set modification time for key %s: %w", key, err)
	}

	return nil
}
