package storage

import (
	"archive/tar"
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
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

// BackupMetadata contains information about a backup.
type BackupMetadata struct {
	ID          string    `json:"id"`
	CreatedAt   time.Time `json:"createdAt"`
	Description string    `json:"description"`
	Path        string    `json:"path"`
	All         bool      `json:"all"`
	Compressed  bool      `json:"compressed"`
	Files       []string  `json:"files"`
	Size        int64     `json:"size"`
}

// CreateBackup creates a backup of storage data.
// Parameters:
//   - storageHome: path to the storage directory
//   - backupDir: path to the backup directory
//   - path: specific path within storage to backup (empty for all or when all=true)
//   - all: if true, backup entire storage
//   - compress: if true, create compressed (gzip) backup
//   - description: optional description for the backup
//
// Returns the backup ID and any error.
func CreateBackup(storageHome string, backupDir string, path string, all bool, compress bool, description string) (string, error) {
	if !directoryExists(storageHome) {
		return "", fmt.Errorf("storage directory does not exist: %q", storageHome)
	}

	if err := os.MkdirAll(backupDir, 0o755); err != nil {
		return "", fmt.Errorf("failed to create backup directory: %w", err)
	}

	backupID := time.Now().Format("20060102-150405")

	sourcePath, err := getBackupSourcePath(storageHome, path, all)
	if err != nil {
		return "", err
	}

	files, err := collectFilesToBackup(storageHome, sourcePath)
	if err != nil {
		return "", fmt.Errorf("failed to collect files: %w", err)
	}

	backupFilePath := getBackupFilePath(backupDir, backupID, compress)

	if err := createBackupArchive(storageHome, backupFilePath, files, compress); err != nil {
		return "", fmt.Errorf("failed to create backup archive: %w", err)
	}

	if err := saveBackupMetadata(backupDir, backupID, path, all, compress, description, files, backupFilePath); err != nil {
		return "", err
	}

	return backupID, nil
}

func getBackupSourcePath(storageHome, path string, all bool) (string, error) {
	if all || path == "" {
		return storageHome, nil
	}
	sourcePath := filepath.Join(storageHome, path)
	if !directoryExists(sourcePath) && !fileExists(sourcePath) {
		return "", fmt.Errorf("path does not exist: %q", path)
	}
	return sourcePath, nil
}

func collectFilesToBackup(storageHome, sourcePath string) ([]string, error) {
	var files []string
	err := filepath.Walk(sourcePath, func(filePath string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			relPath, err := filepath.Rel(storageHome, filePath)
			if err != nil {
				return fmt.Errorf("failed to get relative path: %w", err)
			}
			files = append(files, relPath)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to walk source path: %w", err)
	}
	return files, nil
}

func getBackupFilePath(backupDir, backupID string, compress bool) string {
	if compress {
		return filepath.Join(backupDir, backupID+".tar.gz")
	}
	return filepath.Join(backupDir, backupID+".tar")
}

func saveBackupMetadata(backupDir, backupID, path string, all, compress bool, description string, files []string, backupFilePath string) error {
	backupInfo, err := os.Stat(backupFilePath)
	if err != nil {
		return fmt.Errorf("failed to stat backup file: %w", err)
	}

	metadata := BackupMetadata{
		ID:          backupID,
		CreatedAt:   time.Now(),
		Description: description,
		Path:        path,
		All:         all || path == "",
		Compressed:  compress,
		Files:       files,
		Size:        backupInfo.Size(),
	}

	metadataPath := filepath.Join(backupDir, backupID+".json")
	metadataData, err := json.MarshalIndent(metadata, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal metadata: %w", err)
	}

	if err := os.WriteFile(metadataPath, metadataData, 0o600); err != nil {
		return fmt.Errorf("failed to write metadata: %w", err)
	}

	return nil
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

func createBackupArchive(storageHome string, backupPath string, files []string, compress bool) error {
	backupFile, err := os.Create(backupPath)
	if err != nil {
		return fmt.Errorf("failed to create backup file: %w", err)
	}
	defer backupFile.Close()

	var tw *tar.Writer
	if compress {
		gw := gzip.NewWriter(backupFile)
		defer gw.Close()
		tw = tar.NewWriter(gw)
	} else {
		tw = tar.NewWriter(backupFile)
	}
	defer tw.Close()

	for _, file := range files {
		filePath := filepath.Join(storageHome, file)
		if err := addFileToTar(tw, filePath, file); err != nil {
			return fmt.Errorf("failed to add file %s to archive: %w", file, err)
		}
	}

	return nil
}

func addFileToTar(tw *tar.Writer, filePath string, name string) error {
	file, err := os.Open(filePath)
	if err != nil {
		return fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	info, err := file.Stat()
	if err != nil {
		return fmt.Errorf("failed to stat file: %w", err)
	}

	header, err := tar.FileInfoHeader(info, "")
	if err != nil {
		return fmt.Errorf("failed to create tar header: %w", err)
	}
	header.Name = name

	if err := tw.WriteHeader(header); err != nil {
		return fmt.Errorf("failed to write tar header: %w", err)
	}

	if _, err := io.Copy(tw, file); err != nil {
		return fmt.Errorf("failed to copy file content: %w", err)
	}

	return nil
}

// RestoreBackup restores data from a backup.
// Parameters:
//   - storageHome: path to the storage directory
//   - backupDir: path to the backup directory
//   - backupID: ID of the backup to restore
//   - path: specific path within the backup to restore (empty for all)
//   - preview: if true, only preview what would be restored without making changes
func RestoreBackup(storageHome string, backupDir string, backupID string, path string, preview bool) error {
	// Load backup metadata
	metadataPath := filepath.Join(backupDir, backupID+".json")
	metadataData, err := os.ReadFile(metadataPath)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("backup not found: %q", backupID)
		}
		return fmt.Errorf("failed to read backup metadata: %w", err)
	}

	var metadata BackupMetadata
	if err := json.Unmarshal(metadataData, &metadata); err != nil {
		return fmt.Errorf("failed to parse backup metadata: %w", err)
	}

	// Find the backup archive
	var backupFilePath string
	if metadata.Compressed {
		backupFilePath = filepath.Join(backupDir, backupID+".tar.gz")
	} else {
		backupFilePath = filepath.Join(backupDir, backupID+".tar")
	}

	if !fileExists(backupFilePath) {
		return fmt.Errorf("backup archive not found: %q", backupFilePath)
	}

	// Filter files if path is specified
	filesToRestore := metadata.Files
	if path != "" {
		filesToRestore = filterFilesByPath(metadata.Files, path)
		if len(filesToRestore) == 0 {
			return fmt.Errorf("no files match path: %q", path)
		}
	}

	if preview {
		fmt.Printf("Would restore %d file(s) from backup %s:\n", len(filesToRestore), backupID)
		for _, file := range filesToRestore {
			fmt.Printf("  %s\n", file)
		}
		return nil
	}

	// Create storage directory if it doesn't exist
	if err := os.MkdirAll(storageHome, 0o755); err != nil {
		return fmt.Errorf("failed to create storage directory: %w", err)
	}

	// Extract files from backup
	if err := extractFromBackup(backupFilePath, storageHome, filesToRestore, metadata.Compressed); err != nil {
		return fmt.Errorf("failed to extract backup: %w", err)
	}

	fmt.Printf("Restored %d file(s) from backup %s\n", len(filesToRestore), backupID)
	return nil
}

func filterFilesByPath(files []string, path string) []string {
	var filtered []string
	for _, file := range files {
		if strings.HasPrefix(file, path) || file == path {
			filtered = append(filtered, file)
		}
	}
	return filtered
}

// maxDecompressedSize is the maximum allowed size for decompressed files (1GB).
const maxDecompressedSize = 1 << 30

func extractFromBackup(backupPath string, destDir string, filesToExtract []string, compressed bool) error {
	// Create a set for quick lookup
	fileSet := make(map[string]bool)
	for _, f := range filesToExtract {
		fileSet[f] = true
	}

	backupFile, err := os.Open(backupPath)
	if err != nil {
		return fmt.Errorf("failed to open backup file: %w", err)
	}
	defer backupFile.Close()

	var tr *tar.Reader
	if compressed {
		gr, err := gzip.NewReader(backupFile)
		if err != nil {
			return fmt.Errorf("failed to create gzip reader: %w", err)
		}
		defer gr.Close()
		tr = tar.NewReader(gr)
	} else {
		tr = tar.NewReader(backupFile)
	}

	for {
		header, err := tr.Next()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return fmt.Errorf("failed to read tar header: %w", err)
		}

		// Skip files not in our list
		if !fileSet[header.Name] {
			continue
		}

		if err := extractTarEntry(destDir, header, tr); err != nil {
			return err
		}
	}

	return nil
}

func extractTarEntry(destDir string, header *tar.Header, tr *tar.Reader) error {
	// Sanitize the file path to prevent path traversal attacks (G305)
	cleanName := filepath.Clean(header.Name)
	if strings.HasPrefix(cleanName, "..") || filepath.IsAbs(cleanName) {
		return fmt.Errorf("invalid file path in archive: %s", header.Name)
	}

	targetPath := filepath.Join(destDir, cleanName)

	// Verify the target path is within the destination directory
	if !strings.HasPrefix(filepath.Clean(targetPath), filepath.Clean(destDir)) {
		return fmt.Errorf("file path escapes destination directory: %s", header.Name)
	}

	// Create parent directories
	if err := os.MkdirAll(filepath.Dir(targetPath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for %s: %w", header.Name, err)
	}

	// Use safe file mode (G115: avoid integer overflow by masking to valid permission bits)
	// #nosec G115 -- Mode is masked to 9-bit permission value which fits safely in uint32
	mode := os.FileMode(header.Mode & 0o777)

	// Create and write the file
	outFile, err := os.OpenFile(targetPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
	if err != nil {
		return fmt.Errorf("failed to create file %s: %w", header.Name, err)
	}
	defer outFile.Close()

	// Use limited reader to prevent decompression bomb attacks (G110)
	limitedReader := io.LimitReader(tr, maxDecompressedSize)
	written, err := io.Copy(outFile, limitedReader)
	if err != nil {
		return fmt.Errorf("failed to write file %s: %w", header.Name, err)
	}

	// Check if we hit the limit (potential decompression bomb)
	if written >= maxDecompressedSize {
		return fmt.Errorf("file %s exceeds maximum allowed size", header.Name)
	}

	return nil
}

// ListBackups lists available backups.
// It reads all backup metadata files from the backup directory and displays
// them in a formatted table sorted by creation time (newest first).
func ListBackups(backupDir string) error {
	if !directoryExists(backupDir) {
		fmt.Println("No backups found")
		return nil
	}

	// Find all metadata files
	entries, err := os.ReadDir(backupDir)
	if err != nil {
		return fmt.Errorf("failed to read backup directory: %w", err)
	}

	var backups []BackupMetadata
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".json") {
			continue
		}

		metadataPath := filepath.Join(backupDir, entry.Name())
		metadataData, err := os.ReadFile(metadataPath)
		if err != nil {
			continue // Skip files we can't read
		}

		var metadata BackupMetadata
		if err := json.Unmarshal(metadataData, &metadata); err != nil {
			continue // Skip invalid metadata
		}

		backups = append(backups, metadata)
	}

	if len(backups) == 0 {
		fmt.Println("No backups found")
		return nil
	}

	// Sort by creation time, newest first
	sort.Slice(backups, func(i, j int) bool {
		return backups[i].CreatedAt.After(backups[j].CreatedAt)
	})

	// Print header
	fmt.Printf("%-20s %-20s %-10s %-12s %s\n", "BACKUP ID", "CREATED", "FILES", "SIZE", "DESCRIPTION")
	fmt.Println(strings.Repeat("-", 80))

	// Print each backup
	for _, b := range backups {
		compressedMark := ""
		if b.Compressed {
			compressedMark = " (gz)"
		}
		desc := b.Description
		if len(desc) > 25 {
			desc = desc[:22] + "..."
		}
		fmt.Printf("%-20s %-20s %-10d %-12s %s\n",
			b.ID,
			b.CreatedAt.Format("2006-01-02 15:04:05"),
			len(b.Files),
			formatSize(b.Size)+compressedMark,
			desc,
		)
	}

	return nil
}

func formatSize(bytes int64) string {
	const (
		KB = 1024
		MB = KB * 1024
		GB = MB * 1024
	)

	switch {
	case bytes >= GB:
		return fmt.Sprintf("%.1fGB", float64(bytes)/float64(GB))
	case bytes >= MB:
		return fmt.Sprintf("%.1fMB", float64(bytes)/float64(MB))
	case bytes >= KB:
		return fmt.Sprintf("%.1fKB", float64(bytes)/float64(KB))
	default:
		return fmt.Sprintf("%dB", bytes)
	}
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
