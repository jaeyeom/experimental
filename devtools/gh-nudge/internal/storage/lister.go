package storage

import (
	"fmt"
	"os"
	"path/filepath"
)

// FileSystemLister implements the Lister interface for filesystem operations.
type FileSystemLister struct {
	rootPath string
}

// NewFileSystemLister creates a new FileSystemLister instance.
func NewFileSystemLister(rootPath string) (*FileSystemLister, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	return &FileSystemLister{
		rootPath: absPath,
	}, nil
}

func (fsl *FileSystemLister) getFilePath(key string) string {
	return filepath.Join(fsl.rootPath, key)
}

func (fsl *FileSystemLister) List(prefix string) ([]string, error) {
	prefixPath := fsl.getFilePath(prefix)

	var files []string
	err := filepath.Walk(prefixPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() {
			relPath, err := filepath.Rel(fsl.rootPath, path)
			if err != nil {
				return fmt.Errorf("failed to get relative path: %w", err)
			}
			files = append(files, relPath)
		}

		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to list files with prefix %s: %w", prefix, err)
	}

	return files, nil
}

func (fsl *FileSystemLister) GetChildren(path string) ([]string, error) {
	dirPath := fsl.getFilePath(path)

	entries, err := os.ReadDir(dirPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read directory %s: %w", path, err)
	}

	var children []string
	for _, entry := range entries {
		children = append(children, entry.Name())
	}

	return children, nil
}
