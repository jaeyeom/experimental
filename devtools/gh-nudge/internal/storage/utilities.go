package storage

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

// Initialize creates a new storage directory structure with metadata and subdirectories.
func Initialize(storageHome string, force bool) error {
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

	return nil
}

func directoryExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
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

// VacuumResult contains statistics about the vacuum operation.
type VacuumResult struct {
	FilesProcessed   int   `json:"filesProcessed"`
	FilesCompressed  int   `json:"filesCompressed"`
	BytesSaved       int64 `json:"bytesSaved"`
	EmptyDirsRemoved int   `json:"emptyDirsRemoved"`
	ErrorCount       int   `json:"errorCount"`
}

// VacuumOption is a functional option for configuring vacuum operations.
type VacuumOption interface {
	applyVacuum(storageHome string, result *VacuumResult) error
	printSummary(result *VacuumResult)
}

// VacuumCompress compacts JSON files by removing extra whitespace.
type VacuumCompress struct{}

func (VacuumCompress) applyVacuum(storageHome string, result *VacuumResult) error {
	if err := compactJSONFiles(storageHome, result); err != nil {
		return fmt.Errorf("failed to compact JSON files: %w", err)
	}
	return nil
}

func (VacuumCompress) printSummary(result *VacuumResult) {
	fmt.Printf("  Files compacted: %d\n", result.FilesCompressed)
	fmt.Printf("  Bytes saved: %s\n", formatSize(result.BytesSaved))
}

// VacuumDefragment removes empty directories.
type VacuumDefragment struct{}

func (VacuumDefragment) applyVacuum(storageHome string, result *VacuumResult) error {
	if err := removeEmptyDirectories(storageHome, result); err != nil {
		return fmt.Errorf("failed to remove empty directories: %w", err)
	}
	return nil
}

func (VacuumDefragment) printSummary(result *VacuumResult) {
	fmt.Printf("  Empty directories removed: %d\n", result.EmptyDirsRemoved)
}

// VacuumVerify verifies storage integrity after vacuum.
type VacuumVerify struct{}

func (VacuumVerify) applyVacuum(storageHome string, _ *VacuumResult) error {
	if err := Verify(storageHome); err != nil {
		return fmt.Errorf("verification failed after vacuum: %w", err)
	}
	return nil
}

func (VacuumVerify) printSummary(_ *VacuumResult) {
	// Verify has no summary output
}

// Vacuum optimizes storage by compacting and reorganizing data.
// Parameters:
//   - storageHome: path to the storage directory
//   - opts: vacuum options (VacuumCompress, VacuumDefragment, VacuumVerify)
//
// Example:
//
//	Vacuum(storageHome, VacuumCompress{}, VacuumDefragment{}, VacuumVerify{})
func Vacuum(storageHome string, opts ...VacuumOption) error {
	if !directoryExists(storageHome) {
		return fmt.Errorf("storage directory does not exist: %q", storageHome)
	}

	result := &VacuumResult{}

	// Apply each option
	for _, opt := range opts {
		if err := opt.applyVacuum(storageHome, result); err != nil {
			return err
		}
	}

	// Print summary
	fmt.Printf("Vacuum completed:\n")
	fmt.Printf("  Files processed: %d\n", result.FilesProcessed)
	for _, opt := range opts {
		opt.printSummary(result)
	}
	if result.ErrorCount > 0 {
		fmt.Printf("  Errors encountered: %d\n", result.ErrorCount)
	}

	return nil
}

// compactJSONFiles compacts all JSON files by removing extra whitespace.
func compactJSONFiles(storageHome string, result *VacuumResult) error {
	err := filepath.Walk(storageHome, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Skip directories
		if info.IsDir() {
			return nil
		}

		// Only process JSON files
		if !strings.HasSuffix(strings.ToLower(info.Name()), ".json") {
			return nil
		}

		result.FilesProcessed++

		saved, err := compactJSONFile(path, info.Size())
		if err != nil {
			result.ErrorCount++
			fmt.Printf("Warning: failed to compact %s: %v\n", path, err)
			return nil // Continue processing other files
		}

		if saved > 0 {
			result.FilesCompressed++
			result.BytesSaved += saved
		}

		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to walk storage directory: %w", err)
	}
	return nil
}

// compactJSONFile compacts a single JSON file and returns bytes saved.
func compactJSONFile(path string, originalSize int64) (int64, error) {
	// Read the file
	data, err := os.ReadFile(path)
	if err != nil {
		return 0, fmt.Errorf("failed to read file: %w", err)
	}

	// Parse JSON
	// Unmarshaling into interface{} for JSON compaction only.
	// nosemgrep: go-unsafe-deserialization-interface
	var parsed interface{}
	if err := json.Unmarshal(data, &parsed); err != nil {
		return 0, fmt.Errorf("failed to parse JSON: %w", err)
	}

	// Re-encode without extra whitespace (compact format)
	compacted, err := json.Marshal(parsed)
	if err != nil {
		return 0, fmt.Errorf("failed to marshal JSON: %w", err)
	}

	// Add a trailing newline for better compatibility
	compacted = append(compacted, '\n')

	newSize := int64(len(compacted))

	// Only write if we actually saved space
	if newSize < originalSize {
		if err := os.WriteFile(path, compacted, 0o600); err != nil {
			return 0, fmt.Errorf("failed to write compacted file: %w", err)
		}
		return originalSize - newSize, nil
	}

	return 0, nil
}

// removeEmptyDirectories removes all empty directories under storageHome.
// It processes directories from deepest to shallowest to handle nested empty dirs.
func removeEmptyDirectories(storageHome string, result *VacuumResult) error {
	// Collect all directories first
	var dirs []string
	err := filepath.Walk(storageHome, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() && path != storageHome {
			dirs = append(dirs, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to walk directory: %w", err)
	}

	// Sort directories by depth (deepest first) to handle nested empty dirs
	sort.Slice(dirs, func(i, j int) bool {
		return strings.Count(dirs[i], string(os.PathSeparator)) > strings.Count(dirs[j], string(os.PathSeparator))
	})

	// Try to remove empty directories
	for _, dir := range dirs {
		removed, err := tryRemoveEmptyDir(dir)
		if err != nil {
			result.ErrorCount++
			fmt.Printf("Warning: failed to check directory %s: %v\n", dir, err)
			continue
		}
		if removed {
			result.EmptyDirsRemoved++
		}
	}

	return nil
}

// tryRemoveEmptyDir attempts to remove a directory if it's empty.
// Returns true if the directory was removed.
func tryRemoveEmptyDir(dir string) (bool, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return false, nil // Already removed (parent was removed)
		}
		return false, fmt.Errorf("failed to read directory %s: %w", dir, err)
	}

	if len(entries) == 0 {
		if err := os.Remove(dir); err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				return false, nil
			}
			return false, fmt.Errorf("failed to remove empty directory %s: %w", dir, err)
		}
		return true, nil
	}

	return false, nil
}

// LockInfo contains information about a lock file.
type LockInfo struct {
	Path     string    `json:"path"`
	LockedAt time.Time `json:"lockedAt"`
	PID      int       `json:"pid"`
	IsStale  bool      `json:"isStale"`
}

// ManageLocks manages storage locks.
// Parameters:
//   - storageHome: path to the storage directory
//   - list: if true, list all active locks
//   - release: if true, release the lock at the specified path
//   - status: if true, show lock status for the specified path
//   - force: if true, force release even if lock is held by another process
//   - path: the path to operate on (required for release and status)
func ManageLocks(storageHome string, list bool, release bool, status bool, force bool, path string) error {
	if !directoryExists(storageHome) {
		return fmt.Errorf("storage directory does not exist: %q", storageHome)
	}

	// Count how many operations were requested
	opCount := 0
	if list {
		opCount++
	}
	if release {
		opCount++
	}
	if status {
		opCount++
	}

	if opCount == 0 {
		return fmt.Errorf("no operation specified: use --list, --release, or --status")
	}
	if opCount > 1 {
		return fmt.Errorf("only one operation can be specified at a time")
	}

	if list {
		return listLocks(storageHome)
	}

	if status {
		if path == "" {
			return fmt.Errorf("--status requires a path argument")
		}
		return showLockStatus(storageHome, path)
	}

	if release {
		if path == "" {
			return fmt.Errorf("--release requires a path argument")
		}
		return releaseLock(storageHome, path, force)
	}

	return nil
}

// listLocks lists all lock files in the storage directory.
func listLocks(storageHome string) error {
	locks, err := findLockFiles(storageHome)
	if err != nil {
		return fmt.Errorf("failed to find lock files: %w", err)
	}

	if len(locks) == 0 {
		fmt.Println("No active locks found.")
		return nil
	}

	fmt.Printf("Found %d lock(s):\n\n", len(locks))
	for _, lock := range locks {
		staleStatus := ""
		if lock.IsStale {
			staleStatus = " (STALE)"
		}
		fmt.Printf("  Path: %s%s\n", lock.Path, staleStatus)
		if !lock.LockedAt.IsZero() {
			fmt.Printf("    Locked at: %s\n", lock.LockedAt.Format(time.RFC3339))
		}
		if lock.PID > 0 {
			fmt.Printf("    PID: %d\n", lock.PID)
		}
		fmt.Println()
	}

	return nil
}

// showLockStatus shows the lock status for a specific path.
func showLockStatus(storageHome string, path string) error {
	lockPath := resolveLockPath(storageHome, path)

	info, err := os.Stat(lockPath)
	if errors.Is(err, fs.ErrNotExist) {
		fmt.Printf("No lock exists for: %s\n", path)
		return nil
	}
	if err != nil {
		return fmt.Errorf("failed to stat lock file: %w", err)
	}

	lockInfo, err := parseLockFile(lockPath)
	if err != nil {
		return fmt.Errorf("failed to parse lock file: %w", err)
	}

	fmt.Printf("Lock status for: %s\n", path)
	fmt.Printf("  Lock file: %s\n", lockPath)
	fmt.Printf("  File size: %d bytes\n", info.Size())
	fmt.Printf("  File modified: %s\n", info.ModTime().Format(time.RFC3339))
	if !lockInfo.LockedAt.IsZero() {
		fmt.Printf("  Locked at: %s\n", lockInfo.LockedAt.Format(time.RFC3339))
	}
	if lockInfo.PID > 0 {
		fmt.Printf("  PID: %d\n", lockInfo.PID)
		if lockInfo.IsStale {
			fmt.Printf("  Status: STALE (process not running)\n")
		} else {
			fmt.Printf("  Status: ACTIVE (process running)\n")
		}
	}

	return nil
}

// releaseLock releases a lock at the specified path.
func releaseLock(storageHome string, path string, force bool) error {
	lockPath := resolveLockPath(storageHome, path)

	if _, err := os.Stat(lockPath); errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("no lock exists for: %s", path)
	}

	lockInfo, err := parseLockFile(lockPath)
	if err != nil {
		return fmt.Errorf("failed to parse lock file: %w", err)
	}

	// Check if the lock is held by a running process
	if lockInfo.PID > 0 && !lockInfo.IsStale {
		if !force {
			return fmt.Errorf("lock is held by running process (PID %d); use --force to release anyway", lockInfo.PID)
		}
		fmt.Printf("Warning: Force releasing lock held by running process (PID %d)\n", lockInfo.PID)
	}

	if err := os.Remove(lockPath); err != nil {
		return fmt.Errorf("failed to remove lock file: %w", err)
	}

	fmt.Printf("Released lock: %s\n", path)
	return nil
}

// findLockFiles finds all .lock files in the storage directory.
func findLockFiles(storageHome string) ([]LockInfo, error) {
	var locks []LockInfo

	err := filepath.WalkDir(storageHome, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		if strings.HasSuffix(path, ".lock") {
			lockInfo, parseErr := parseLockFile(path)
			if parseErr != nil {
				// Include lock file even if we can't parse it
				lockInfo = LockInfo{Path: path}
			}
			lockInfo.Path = path
			locks = append(locks, lockInfo)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("failed to walk storage directory: %w", err)
	}

	return locks, nil
}

// parseLockFile parses a lock file and returns its information.
func parseLockFile(lockPath string) (LockInfo, error) {
	info := LockInfo{Path: lockPath}

	content, err := os.ReadFile(lockPath)
	if err != nil {
		return info, fmt.Errorf("failed to read lock file: %w", err)
	}

	lines := strings.Split(string(content), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "locked_at: ") {
			timeStr := strings.TrimPrefix(line, "locked_at: ")
			if t, err := time.Parse(time.RFC3339, timeStr); err == nil {
				info.LockedAt = t
			}
		} else if strings.HasPrefix(line, "pid: ") {
			pidStr := strings.TrimPrefix(line, "pid: ")
			if pid, err := strconv.Atoi(pidStr); err == nil {
				info.PID = pid
				info.IsStale = !isProcessRunning(pid)
			}
		}
	}

	return info, nil
}

// resolveLockPath resolves the full path to a lock file.
func resolveLockPath(storageHome string, path string) string {
	// If the path already ends with .lock, use it as-is
	if strings.HasSuffix(path, ".lock") {
		if filepath.IsAbs(path) {
			return path
		}
		return filepath.Join(storageHome, path)
	}

	// Otherwise, append .lock
	if filepath.IsAbs(path) {
		return path + ".lock"
	}
	return filepath.Join(storageHome, path+".lock")
}

// ExportData represents the structure of exported data.
type ExportData struct {
	Version  string                 `json:"version"`
	Metadata map[string]interface{} `json:"metadata,omitempty"`
	Files    map[string]string      `json:"files"`
}

// Export exports storage data to external formats.
// Parameters:
//   - storageHome: path to the storage directory
//   - outputPath: path to write the exported data
//   - format: output format ("json", "yaml", or "tar")
//   - compress: if true, compress the output (gzip for json/yaml, tar.gz for tar)
//   - includeMetadata: if true, include storage metadata in the export
func Export(storageHome string, outputPath string, format string, compress bool, includeMetadata bool) error {
	if !directoryExists(storageHome) {
		return fmt.Errorf("storage directory does not exist: %q", storageHome)
	}

	switch format {
	case "json":
		return exportJSON(storageHome, outputPath, compress, includeMetadata)
	case "yaml":
		return exportYAML(storageHome, outputPath, compress, includeMetadata)
	case "tar":
		return exportTar(storageHome, outputPath, compress)
	default:
		return fmt.Errorf("unsupported export format: %q (supported: json, yaml, tar)", format)
	}
}

func collectExportData(storageHome string, includeMetadata bool) (*ExportData, error) {
	data := &ExportData{
		Version: "1.0.0",
		Files:   make(map[string]string),
	}

	// Include metadata if requested
	if includeMetadata {
		metadataPath := filepath.Join(storageHome, "metadata.json")
		if fileExists(metadataPath) {
			metadataBytes, err := os.ReadFile(metadataPath)
			if err != nil {
				return nil, fmt.Errorf("failed to read metadata: %w", err)
			}
			var metadata map[string]interface{}
			if err := json.Unmarshal(metadataBytes, &metadata); err != nil {
				return nil, fmt.Errorf("failed to parse metadata: %w", err)
			}
			data.Metadata = metadata
		}
	}

	// Collect all files from repos directory (the main data)
	reposPath := filepath.Join(storageHome, "repos")
	if directoryExists(reposPath) {
		err := filepath.Walk(reposPath, func(filePath string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.IsDir() {
				return nil
			}

			relPath, err := filepath.Rel(storageHome, filePath)
			if err != nil {
				return fmt.Errorf("failed to get relative path: %w", err)
			}

			content, err := os.ReadFile(filePath)
			if err != nil {
				return fmt.Errorf("failed to read file %s: %w", relPath, err)
			}

			data.Files[relPath] = string(content)
			return nil
		})
		if err != nil {
			return nil, fmt.Errorf("failed to collect files: %w", err)
		}
	}

	return data, nil
}

func exportJSON(storageHome, outputPath string, compress, includeMetadata bool) error {
	data, err := collectExportData(storageHome, includeMetadata)
	if err != nil {
		return err
	}

	jsonData, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	if compress {
		return writeCompressedFile(outputPath, jsonData)
	}
	if err := os.WriteFile(outputPath, jsonData, 0o600); err != nil {
		return fmt.Errorf("failed to write JSON file: %w", err)
	}
	return nil
}

func exportYAML(storageHome, outputPath string, compress, includeMetadata bool) error {
	data, err := collectExportData(storageHome, includeMetadata)
	if err != nil {
		return err
	}

	// Convert to YAML-like format (simple key-value representation)
	// Since we don't have a YAML library, we'll create a simple format
	var buf strings.Builder
	buf.WriteString("version: " + data.Version + "\n")

	if data.Metadata != nil {
		buf.WriteString("metadata:\n")
		metaJSON, _ := json.MarshalIndent(data.Metadata, "  ", "  ")
		buf.WriteString("  ")
		buf.WriteString(strings.ReplaceAll(string(metaJSON), "\n", "\n  "))
		buf.WriteString("\n")
	}

	buf.WriteString("files:\n")
	for path, content := range data.Files {
		buf.WriteString("  " + path + ": |\n")
		lines := strings.Split(content, "\n")
		for _, line := range lines {
			buf.WriteString("    " + line + "\n")
		}
	}

	yamlData := []byte(buf.String())

	if compress {
		return writeCompressedFile(outputPath, yamlData)
	}
	if err := os.WriteFile(outputPath, yamlData, 0o600); err != nil {
		return fmt.Errorf("failed to write YAML file: %w", err)
	}
	return nil
}

func exportTar(storageHome, outputPath string, compress bool) error {
	// Collect files from repos directory
	reposPath := filepath.Join(storageHome, "repos")
	if !directoryExists(reposPath) {
		return fmt.Errorf("repos directory does not exist")
	}

	files, err := collectFilesToBackup(storageHome, reposPath)
	if err != nil {
		return err
	}

	// Also include metadata.json if it exists
	metadataPath := filepath.Join(storageHome, "metadata.json")
	if fileExists(metadataPath) {
		files = append([]string{"metadata.json"}, files...)
	}

	return createBackupArchive(storageHome, outputPath, files, compress)
}

func writeCompressedFile(outputPath string, data []byte) error {
	file, err := os.Create(outputPath)
	if err != nil {
		return fmt.Errorf("failed to create output file: %w", err)
	}
	defer file.Close()

	gw := gzip.NewWriter(file)
	defer gw.Close()

	if _, err := gw.Write(data); err != nil {
		return fmt.Errorf("failed to write compressed data: %w", err)
	}

	return nil
}

// Import imports external data into storage.
// Parameters:
//   - storageHome: path to the storage directory
//   - inputPath: path to the file to import
//   - format: input format ("json", "yaml", or "tar")
//   - merge: if true, merge with existing data instead of replacing
//   - overwrite: if true, overwrite existing files (only used with merge)
func Import(storageHome string, inputPath string, format string, merge bool, overwrite bool) error {
	if !fileExists(inputPath) {
		return fmt.Errorf("input file does not exist: %q", inputPath)
	}

	// Create storage directory if it doesn't exist
	if err := os.MkdirAll(storageHome, 0o755); err != nil {
		return fmt.Errorf("failed to create storage directory: %w", err)
	}

	switch format {
	case "json":
		return importJSON(storageHome, inputPath, merge, overwrite)
	case "yaml":
		return importYAML(storageHome, inputPath, merge, overwrite)
	case "tar":
		return importTar(storageHome, inputPath, merge, overwrite)
	default:
		return fmt.Errorf("unsupported import format: %q (supported: json, yaml, tar)", format)
	}
}

func readInputFile(inputPath string) ([]byte, error) {
	data, err := os.ReadFile(inputPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read input file: %w", err)
	}

	// Check if it's gzip compressed
	if len(data) >= 2 && data[0] == 0x1f && data[1] == 0x8b {
		return decompressGzip(data)
	}

	return data, nil
}

func decompressGzip(data []byte) ([]byte, error) {
	reader := bytes.NewReader(data)
	gr, err := gzip.NewReader(reader)
	if err != nil {
		return nil, fmt.Errorf("failed to create gzip reader: %w", err)
	}
	defer gr.Close()

	decompressed, err := io.ReadAll(gr)
	if err != nil {
		return nil, fmt.Errorf("failed to decompress: %w", err)
	}

	return decompressed, nil
}

func importJSON(storageHome, inputPath string, merge, overwrite bool) error {
	data, err := readInputFile(inputPath)
	if err != nil {
		return err
	}

	var exportData ExportData
	if err := json.Unmarshal(data, &exportData); err != nil {
		return fmt.Errorf("failed to parse JSON: %w", err)
	}

	return writeImportedFiles(storageHome, exportData.Files, merge, overwrite)
}

func importYAML(storageHome, inputPath string, merge, overwrite bool) error {
	data, err := readInputFile(inputPath)
	if err != nil {
		return err
	}

	// Simple YAML parsing for our format
	// Look for "files:" section and parse key-value pairs
	content := string(data)
	files := make(map[string]string)

	lines := strings.Split(content, "\n")
	inFiles := false
	currentFile := ""
	var currentContent strings.Builder

	for _, line := range lines {
		if strings.TrimSpace(line) == "files:" {
			inFiles = true
			continue
		}

		if !inFiles {
			continue
		}

		// Check if this is a new file entry (starts with 2 spaces, ends with : |)
		if strings.HasPrefix(line, "  ") && !strings.HasPrefix(line, "    ") {
			// Save previous file if any
			if currentFile != "" {
				files[currentFile] = strings.TrimSuffix(currentContent.String(), "\n")
			}

			// Parse new file name
			trimmed := strings.TrimPrefix(line, "  ")
			if idx := strings.Index(trimmed, ": |"); idx > 0 {
				currentFile = trimmed[:idx]
				currentContent.Reset()
			}
		} else if strings.HasPrefix(line, "    ") && currentFile != "" {
			// Content line (4 spaces indentation)
			currentContent.WriteString(strings.TrimPrefix(line, "    "))
			currentContent.WriteString("\n")
		}
	}

	// Save last file
	if currentFile != "" {
		files[currentFile] = strings.TrimSuffix(currentContent.String(), "\n")
	}

	return writeImportedFiles(storageHome, files, merge, overwrite)
}

func importTar(storageHome, inputPath string, merge, overwrite bool) error {
	compressed, err := isGzipCompressed(inputPath)
	if err != nil {
		return err
	}

	tr, cleanup, err := openTarReader(inputPath, compressed)
	if err != nil {
		return err
	}
	defer cleanup()

	return extractTarFiles(tr, storageHome, merge, overwrite)
}

func isGzipCompressed(inputPath string) (bool, error) {
	file, err := os.Open(inputPath)
	if err != nil {
		return false, fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	header := make([]byte, 2)
	n, err := file.Read(header)
	if err != nil || n < 2 {
		return false, nil // Not enough data, assume uncompressed
	}

	// #nosec G602 -- We've verified n >= 2 above, so indices 0 and 1 are safe
	return header[0] == 0x1f && header[1] == 0x8b, nil
}

func openTarReader(inputPath string, compressed bool) (*tar.Reader, func(), error) {
	file, err := os.Open(inputPath)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to open tar file: %w", err)
	}

	if compressed {
		gr, err := gzip.NewReader(file)
		if err != nil {
			file.Close()
			return nil, nil, fmt.Errorf("failed to create gzip reader: %w", err)
		}
		cleanup := func() {
			gr.Close()
			file.Close()
		}
		return tar.NewReader(gr), cleanup, nil
	}

	return tar.NewReader(file), func() { file.Close() }, nil
}

func extractTarFiles(tr *tar.Reader, destDir string, merge, overwrite bool) error {
	for {
		header, err := tr.Next()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return fmt.Errorf("failed to read tar header: %w", err)
		}

		if err := importTarEntry(destDir, header, tr, merge, overwrite); err != nil {
			return err
		}
	}
	return nil
}

func importTarEntry(destDir string, header *tar.Header, tr *tar.Reader, merge, overwrite bool) error {
	// Security: validate path to prevent path traversal (G305)
	cleanName := filepath.Clean(header.Name)
	if strings.HasPrefix(cleanName, "..") || filepath.IsAbs(cleanName) {
		return fmt.Errorf("invalid file path in archive: %s", header.Name)
	}

	targetPath := filepath.Join(destDir, cleanName)

	// Verify the target path is within the destination directory
	if !strings.HasPrefix(filepath.Clean(targetPath), filepath.Clean(destDir)) {
		return fmt.Errorf("file path escapes destination directory: %s", header.Name)
	}

	// Check if file exists
	if fileExists(targetPath) {
		if !merge {
			return fmt.Errorf("file already exists and merge not enabled: %s", header.Name)
		}
		if !overwrite {
			return nil // Skip existing files when merge is enabled but overwrite is not
		}
	}

	// Create parent directories
	if err := os.MkdirAll(filepath.Dir(targetPath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory: %w", err)
	}

	// Write file with safe mode
	// #nosec G115 -- Mode is masked to 9-bit permission value
	mode := os.FileMode(header.Mode & 0o777)
	outFile, err := os.OpenFile(targetPath, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
	if err != nil {
		return fmt.Errorf("failed to create file: %w", err)
	}
	defer outFile.Close()

	// Use limited reader for safety
	limitedReader := io.LimitReader(tr, maxDecompressedSize)
	if _, err := io.Copy(outFile, limitedReader); err != nil {
		return fmt.Errorf("failed to write file: %w", err)
	}

	return nil
}

func writeImportedFiles(storageHome string, files map[string]string, merge, overwrite bool) error {
	for relPath, content := range files {
		targetPath := filepath.Join(storageHome, relPath)

		// Check if file exists
		if fileExists(targetPath) {
			if !merge {
				return fmt.Errorf("file already exists and merge not enabled: %s", relPath)
			}
			if !overwrite {
				// Skip existing files when merge is enabled but overwrite is not
				continue
			}
		}

		// Create parent directories
		if err := os.MkdirAll(filepath.Dir(targetPath), 0o755); err != nil {
			return fmt.Errorf("failed to create directory for %s: %w", relPath, err)
		}

		// Write file
		if err := os.WriteFile(targetPath, []byte(content), 0o600); err != nil {
			return fmt.Errorf("failed to write file %s: %w", relPath, err)
		}
	}

	return nil
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
