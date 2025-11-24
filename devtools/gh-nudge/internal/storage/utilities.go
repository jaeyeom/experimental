package storage

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"
)

// Initialize creates a new storage directory structure with metadata and subdirectories.
func Initialize(storageHome string, force bool, migrate bool) error {
	if !force && directoryExists(storageHome) {
		return fmt.Errorf("storage directory already exists: %q", storageHome)
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

func migrateFrom(storageHome string) error {
	_ = storageHome // TODO: implement migration
	return fmt.Errorf("migration not implemented")
}

// Migrate migrates storage data between formats.
func Migrate(storageHome string, from string, to string, dryRun bool, backup bool) error {
	_, _, _, _, _ = storageHome, from, to, dryRun, backup // TODO: implement migration
	return fmt.Errorf("migration not implemented")
}

// CreateBackup creates a backup of storage data.
func CreateBackup(storageHome string, backupDir string, path string, all bool, compress bool, description string) (string, error) {
	_, _, _, _, _, _ = storageHome, backupDir, path, all, compress, description // TODO: implement backup
	return "", fmt.Errorf("backup not implemented")
}

// RestoreBackup restores data from a backup.
func RestoreBackup(storageHome string, backupDir string, backupID string, path string, preview bool) error {
	_, _, _, _, _ = storageHome, backupDir, backupID, path, preview // TODO: implement restore
	return fmt.Errorf("restore not implemented")
}

// ListBackups lists available backups.
func ListBackups(backupDir string) error {
	_ = backupDir // TODO: implement list backups
	return fmt.Errorf("list backups not implemented")
}

// Clean removes old or temporary storage data.
// Parameters:
//   - storageHome: path to the storage directory
//   - olderThan: files older than this duration will be removed
//   - cleanType: type of cleaning - "cache", "temp", or "all"
//   - dryRun: if true, only report what would be deleted without actually deleting
func Clean(storageHome string, olderThan time.Duration, cleanType string, dryRun bool) error {
	// Calculate cutoff time
	cutoffTime := time.Now().Add(-olderThan)

	// Determine which directories to clean
	var dirsToClean []string
	switch cleanType {
	case "cache":
		dirsToClean = []string{"cache"}
	case "temp":
		dirsToClean = []string{"temp"}
	case "all":
		dirsToClean = []string{"cache", "temp"}
	default:
		return fmt.Errorf("invalid clean type %q: must be 'cache', 'temp', or 'all'", cleanType)
	}

	// Clean each directory
	totalRemoved := 0
	for _, dir := range dirsToClean {
		dirPath := filepath.Join(storageHome, dir)

		// Skip if directory doesn't exist
		if !directoryExists(dirPath) {
			continue
		}

		removed, err := cleanDirectory(dirPath, cutoffTime, dryRun)
		if err != nil {
			return fmt.Errorf("failed to clean %s: %w", dir, err)
		}
		totalRemoved += removed
	}

	if dryRun {
		fmt.Printf("Dry run: would remove %d file(s) older than %s\n", totalRemoved, olderThan)
	} else {
		fmt.Printf("Removed %d file(s) older than %s\n", totalRemoved, olderThan)
	}

	return nil
}

// cleanDirectory recursively removes files older than cutoffTime from the given directory.
func cleanDirectory(dirPath string, cutoffTime time.Time, dryRun bool) (int, error) {
	removed := 0

	err := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip the root directory itself
		if path == dirPath {
			return nil
		}

		// Only process files, not directories
		if !info.IsDir() && info.ModTime().Before(cutoffTime) {
			if dryRun {
				fmt.Printf("Would remove: %s (modified: %s)\n", path, info.ModTime().Format(time.RFC3339))
			} else {
				if err := os.Remove(path); err != nil {
					return fmt.Errorf("failed to remove %s: %w", path, err)
				}
				fmt.Printf("Removed: %s (modified: %s)\n", path, info.ModTime().Format(time.RFC3339))
			}
			removed++
		}

		return nil
	})
	if err != nil {
		return removed, fmt.Errorf("failed to walk directory: %w", err)
	}

	return removed, nil
}

// Vacuum optimizes storage by compacting and reorganizing data.
func Vacuum(storageHome string, compress bool, defragment bool, verify bool) error {
	_, _, _, _ = storageHome, compress, defragment, verify // TODO: implement vacuum
	return fmt.Errorf("vacuum not implemented")
}

// ManageLocks manages storage locks.
func ManageLocks(storageHome string, list bool, release bool, status bool, force bool, path string) error {
	_, _, _, _, _, _ = storageHome, list, release, status, force, path // TODO: implement lock management
	return fmt.Errorf("lock management not implemented")
}

// Export exports storage data to external formats.
func Export(storageHome string, path string, format string, compress bool, includeMetadata bool) error {
	_, _, _, _, _ = storageHome, path, format, compress, includeMetadata // TODO: implement export
	return fmt.Errorf("export not implemented")
}

// Import imports external data into storage.
func Import(storageHome string, path string, format string, merge bool, overwrite bool) error {
	_, _, _, _, _ = storageHome, path, format, merge, overwrite // TODO: implement import
	return fmt.Errorf("import not implemented")
}

// Verify verifies storage integrity by checking:
//   - Storage directory exists
//   - Required subdirectory (repos) exists
//   - metadata.json file exists and contains valid JSON
//   - metadata.json contains required fields (version, created_at, description)
//
// Note: cache and temp directories are not verified as they are created on-demand
// and are not required for the tools to function.
//
// Returns an error if any integrity check fails.
func Verify(storageHome string) error {
	// Check if storage directory exists
	if !directoryExists(storageHome) {
		return fmt.Errorf("storage directory does not exist: %q", storageHome)
	}

	// Check required subdirectories (only repos is essential)
	reposPath := filepath.Join(storageHome, "repos")
	if !directoryExists(reposPath) {
		return fmt.Errorf("required subdirectory missing: %q", reposPath)
	}

	// Check metadata.json exists and is valid
	metadataPath := filepath.Join(storageHome, "metadata.json")
	metadataData, err := os.ReadFile(metadataPath)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("metadata.json not found at %q. Did you run 'gh-storage init --force'?", metadataPath)
		}
		return fmt.Errorf("failed to read metadata.json: %w", err)
	}

	var metadata map[string]interface{}
	if err := json.Unmarshal(metadataData, &metadata); err != nil {
		return fmt.Errorf("invalid metadata.json format: %w", err)
	}

	// Verify required metadata fields
	requiredFields := []string{"version", "created_at", "description"}
	for _, field := range requiredFields {
		if _, exists := metadata[field]; !exists {
			return fmt.Errorf("metadata.json missing required field: %s", field)
		}
	}

	return nil
}
