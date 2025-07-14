package storage

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Metadata represents file metadata with creation time, modification time, size, and other attributes.
type Metadata struct {
	CreatedAt   time.Time `json:"created_at"`
	ModifiedAt  time.Time `json:"modified_at"`
	Size        int64     `json:"size"`
	Type        string    `json:"type"`
	Version     string    `json:"version"`
	Description string    `json:"description,omitempty"`
	Checksum    string    `json:"checksum,omitempty"`
}

// Storage defines the interface for unified storage operations including data persistence,
// metadata management, locking, backup/restore, and extended CLI functionality.
type Storage interface {
	// Get retrieves data by key and unmarshals it into dest.
	Get(key string, dest interface{}) error
	// Set stores data with the given key, marshaling it to JSON.
	Set(key string, data interface{}) error
	// Delete removes the data associated with the given key.
	Delete(key string) error
	// Exists checks if data exists for the given key.
	Exists(key string) bool

	// WithLock executes the given function while holding an exclusive lock for the key.
	WithLock(key string, fn func() error) error

	// List returns all keys with the given prefix.
	List(prefix string) ([]string, error)
	// GetChildren returns direct child entries in the given path.
	GetChildren(path string) ([]string, error)

	// GetMetadata retrieves metadata for the given key.
	GetMetadata(key string) (*Metadata, error)
	// SetMetadata updates metadata for the given key.
	SetMetadata(key string, metadata *Metadata) error

	// Backup creates a backup of data for the given key.
	Backup(key string) error
	// Restore restores data for the given key from a backup at the specified timestamp.
	Restore(key string, timestamp time.Time) error

	// Cleanup removes temporary files and performs maintenance.
	Cleanup() error
	// Vacuum optimizes storage by compacting and reorganizing data.
	Vacuum() error

	// Extended methods for CLI interface
	// ShowInfo displays information about the given path with optional detailed output and format.
	ShowInfo(path string, detailed bool, format string) error
	// ListFiles lists files in the given path with optional recursion, format, and filtering.
	ListFiles(path string, recursive bool, format string, filter string) error
	// GetFormatted retrieves and displays data in the specified format with optional pretty printing.
	GetFormatted(key string, format string, pretty bool) error
	// SetFormatted stores data from value or file with format validation.
	SetFormatted(key string, value string, fromFile bool, isJSON bool, isYAML bool) error
}

// FileSystemStorage implements the Storage interface using the local filesystem.
type FileSystemStorage struct {
	rootPath    string
	lockManager *LockManager
}

// NewFileSystemStorage creates a new FileSystemStorage instance with the specified root path.
func NewFileSystemStorage(rootPath string) (*FileSystemStorage, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	lockManager := NewLockManager()

	return &FileSystemStorage{
		rootPath:    absPath,
		lockManager: lockManager,
	}, nil
}

// NewStorage creates a new Storage instance with the FileSystemStorage implementation.
//
//nolint:iface // Interface is used for future extensibility
func NewStorage(rootPath string) (Storage, error) {
	return NewFileSystemStorage(rootPath)
}

func (fs *FileSystemStorage) getFilePath(key string) string {
	return filepath.Join(fs.rootPath, key)
}

func (fs *FileSystemStorage) Get(key string, dest interface{}) error {
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

func (fs *FileSystemStorage) Set(key string, data interface{}) error {
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

func (fs *FileSystemStorage) Delete(key string) error {
	filePath := fs.getFilePath(key)

	if err := os.Remove(filePath); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("failed to delete file for key %s: %w", key, err)
	}

	return nil
}

func (fs *FileSystemStorage) Exists(key string) bool {
	filePath := fs.getFilePath(key)
	_, err := os.Stat(filePath)
	return err == nil
}

func (fs *FileSystemStorage) WithLock(key string, fn func() error) error {
	lockPath := fs.getFilePath(key + ".lock")

	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for lock file: %w", err)
	}

	if err := fs.lockManager.AcquireLock(lockPath); err != nil {
		return fmt.Errorf("failed to acquire lock for key %s: %w", key, err)
	}
	defer func() {
		if err := fs.lockManager.ReleaseLock(lockPath); err != nil {
			// Log error but don't return it since we're in defer
			fmt.Fprintf(os.Stderr, "Warning: failed to release lock %s: %v\n", lockPath, err)
		}
	}()

	return fn()
}

func (fs *FileSystemStorage) List(prefix string) ([]string, error) {
	prefixPath := fs.getFilePath(prefix)

	var files []string
	err := filepath.Walk(prefixPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() {
			relPath, err := filepath.Rel(fs.rootPath, path)
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

func (fs *FileSystemStorage) GetChildren(path string) ([]string, error) {
	dirPath := fs.getFilePath(path)

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

func (fs *FileSystemStorage) GetMetadata(key string) (*Metadata, error) {
	filePath := fs.getFilePath(key)

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

func (fs *FileSystemStorage) SetMetadata(key string, metadata *Metadata) error {
	return fmt.Errorf("setting metadata not supported for filesystem storage")
}

func (fs *FileSystemStorage) Backup(key string) error {
	return fmt.Errorf("backup not implemented")
}

func (fs *FileSystemStorage) Restore(key string, timestamp time.Time) error {
	return fmt.Errorf("restore not implemented")
}

func (fs *FileSystemStorage) Cleanup() error {
	return fmt.Errorf("cleanup not implemented")
}

func (fs *FileSystemStorage) Vacuum() error {
	return fmt.Errorf("vacuum not implemented")
}

func (fs *FileSystemStorage) ShowInfo(path string, detailed bool, format string) error {
	var targetPath string
	if path == "" {
		targetPath = fs.rootPath
	} else {
		targetPath = fs.getFilePath(path)
	}

	info, err := os.Stat(targetPath)
	if err != nil {
		return fmt.Errorf("failed to get info for path %s: %w", path, err)
	}

	if format == "json" {
		data := map[string]interface{}{
			"path":        path,
			"size":        info.Size(),
			"modified_at": info.ModTime(),
			"is_dir":      info.IsDir(),
		}

		if detailed {
			if info.IsDir() {
				var totalFiles, totalSize int64
				err := filepath.Walk(targetPath, func(path string, info os.FileInfo, err error) error {
					if err != nil {
						return err
					}
					if !info.IsDir() {
						totalFiles++
						totalSize += info.Size()
					}
					return nil
				})
				if err == nil {
					data["total_files"] = totalFiles
					data["total_size"] = totalSize
				}
			}
		}

		jsonData, err := json.MarshalIndent(data, "", "  ")
		if err != nil {
			return fmt.Errorf("failed to marshal info: %w", err)
		}

		fmt.Println(string(jsonData))
	} else {
		fmt.Printf("Path: %s\n", path)
		fmt.Printf("Size: %d bytes\n", info.Size())
		fmt.Printf("Modified: %s\n", info.ModTime().Format(time.RFC3339))
		fmt.Printf("Type: %s\n", fileTypeString(info.IsDir()))

		if detailed && info.IsDir() {
			var totalFiles, totalSize int64
			err := filepath.Walk(targetPath, func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() {
					totalFiles++
					totalSize += info.Size()
				}
				return nil
			})
			if err == nil {
				fmt.Printf("Total files: %d\n", totalFiles)
				fmt.Printf("Total size: %d bytes\n", totalSize)
			}
		}
	}

	return nil
}

func (fs *FileSystemStorage) ListFiles(path string, recursive bool, format string, filter string) error {
	var targetPath string
	if path == "" {
		targetPath = fs.rootPath
	} else {
		targetPath = fs.getFilePath(path)
	}

	var files []string
	var walkFunc filepath.WalkFunc

	if recursive {
		walkFunc = func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			relPath, err := filepath.Rel(fs.rootPath, path)
			if err != nil {
				return fmt.Errorf("failed to get relative path: %w", err)
			}

			if filter != "" && !strings.Contains(relPath, filter) {
				return nil
			}

			files = append(files, relPath)
			return nil
		}
	} else {
		walkFunc = func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if path != targetPath {
				return filepath.SkipDir
			}

			relPath, err := filepath.Rel(fs.rootPath, path)
			if err != nil {
				return fmt.Errorf("failed to get relative path: %w", err)
			}

			if filter != "" && !strings.Contains(relPath, filter) {
				return nil
			}

			files = append(files, relPath)
			return nil
		}
	}

	err := filepath.Walk(targetPath, walkFunc)
	if err != nil {
		return fmt.Errorf("failed to walk directory: %w", err)
	}

	if format == "json" {
		jsonData, err := json.MarshalIndent(files, "", "  ")
		if err != nil {
			return fmt.Errorf("failed to marshal files list: %w", err)
		}
		fmt.Println(string(jsonData))
	} else {
		for _, file := range files {
			fmt.Println(file)
		}
	}

	return nil
}

func (fs *FileSystemStorage) GetFormatted(key string, format string, pretty bool) error {
	filePath := fs.getFilePath(key)

	data, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %w", key, err)
	}

	switch format {
	case "raw":
		fmt.Print(string(data))
	case "json":
		if pretty {
			var jsonData interface{}
			if err := json.Unmarshal(data, &jsonData); err != nil {
				return fmt.Errorf("failed to parse JSON: %w", err)
			}
			prettyData, err := json.MarshalIndent(jsonData, "", "  ")
			if err != nil {
				return fmt.Errorf("failed to format JSON: %w", err)
			}
			fmt.Print(string(prettyData))
		} else {
			fmt.Print(string(data))
		}
	default:
		fmt.Print(string(data))
	}

	return nil
}

func (fs *FileSystemStorage) SetFormatted(key string, value string, fromFile bool, isJSON bool, isYAML bool) error {
	var data []byte
	var err error

	if fromFile {
		data, err = os.ReadFile(value)
		if err != nil {
			return fmt.Errorf("failed to read file %s: %w", value, err)
		}
	} else {
		data = []byte(value)
	}

	if isJSON {
		var jsonData interface{}
		if err := json.Unmarshal(data, &jsonData); err != nil {
			return fmt.Errorf("failed to parse JSON: %w", err)
		}

		data, err = json.MarshalIndent(jsonData, "", "  ")
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	}

	filePath := fs.getFilePath(key)

	if err := os.MkdirAll(filepath.Dir(filePath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for key %s: %w", key, err)
	}

	if err := os.WriteFile(filePath, data, 0o600); err != nil {
		return fmt.Errorf("failed to write file for key %s: %w", key, err)
	}

	return nil
}

// Initialize creates a new storage directory structure with metadata and subdirectories.
// If force is true, it will overwrite existing directories. If migrate is true, it will
// attempt to migrate data from existing sources.
func Initialize(storageHome string, force bool, migrate bool) error {
	if !force && directoryExists(storageHome) {
		return fmt.Errorf("storage directory already exists: %s", storageHome)
	}

	if err := os.MkdirAll(storageHome, 0o755); err != nil {
		return fmt.Errorf("failed to create storage directory: %w", err)
	}

	metadataPath := filepath.Join(storageHome, "metadata.json")
	metadata := map[string]interface{}{
		"version":     "1.0.0",
		"created_at":  time.Now(),
		"description": "gh-nudge unified storage",
	}

	metadataData, err := json.MarshalIndent(metadata, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal metadata: %w", err)
	}

	if err := os.WriteFile(metadataPath, metadataData, 0o600); err != nil {
		return fmt.Errorf("failed to write metadata file: %w", err)
	}

	subdirs := []string{"repos", "cache", "temp"}
	for _, subdir := range subdirs {
		subdirPath := filepath.Join(storageHome, subdir)
		if err := os.MkdirAll(subdirPath, 0o755); err != nil {
			return fmt.Errorf("failed to create subdirectory %s: %w", subdir, err)
		}
	}

	if migrate {
		if err := migrateFrom(storageHome); err != nil {
			return fmt.Errorf("failed to migrate data: %w", err)
		}
	}

	return nil
}

func directoryExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}

func fileTypeString(isDir bool) string {
	if isDir {
		return "directory"
	}
	return "file"
}

func migrateFrom(storageHome string) error {
	return fmt.Errorf("migration not implemented")
}

func Migrate(storageHome string, from string, to string, dryRun bool, backup bool) error {
	return fmt.Errorf("migration not implemented")
}

func CreateBackup(storageHome string, backupDir string, path string, all bool, compress bool, description string) (string, error) {
	return "", fmt.Errorf("backup not implemented")
}

func RestoreBackup(storageHome string, backupDir string, backupID string, path string, preview bool) error {
	return fmt.Errorf("restore not implemented")
}

func ListBackups(backupDir string) error {
	return fmt.Errorf("list backups not implemented")
}

func Clean(storageHome string, olderThan string, cleanType string, dryRun bool) error {
	return fmt.Errorf("clean not implemented")
}

func Vacuum(storageHome string, compress bool, defragment bool, verify bool) error {
	return fmt.Errorf("vacuum not implemented")
}

func ManageLocks(storageHome string, list bool, release bool, status bool, force bool, path string) error {
	return fmt.Errorf("lock management not implemented")
}

func Export(storageHome string, path string, format string, compress bool, includeMetadata bool) error {
	return fmt.Errorf("export not implemented")
}

func Import(storageHome string, path string, format string, merge bool, overwrite bool) error {
	return fmt.Errorf("import not implemented")
}

func Verify(storageHome string) error {
	return fmt.Errorf("verify not implemented")
}
