// Package storage provides storage implementations for gh-nudge.
package storage

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// CLIInfoDisplayer implements the InfoDisplayer interface for CLI information display.
type CLIInfoDisplayer struct {
	rootPath string
}

// NewCLIInfoDisplayer creates a new CLIInfoDisplayer instance.
func NewCLIInfoDisplayer(rootPath string) (*CLIInfoDisplayer, error) {
	absPath, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to get absolute path: %w", err)
	}

	return &CLIInfoDisplayer{
		rootPath: absPath,
	}, nil
}

func (cid *CLIInfoDisplayer) getFilePath(path string) string {
	if path == "" {
		return cid.rootPath
	}
	return filepath.Join(cid.rootPath, path)
}

func (cid *CLIInfoDisplayer) ShowInfo(path string, detailed bool, format string) error {
	targetPath := cid.getFilePath(path)

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

		if detailed && info.IsDir() {
			totalFiles, totalSize := cid.calculateDirStats(targetPath)
			data["total_files"] = totalFiles
			data["total_size"] = totalSize
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
			totalFiles, totalSize := cid.calculateDirStats(targetPath)
			fmt.Printf("Total files: %d\n", totalFiles)
			fmt.Printf("Total size: %d bytes\n", totalSize)
		}
	}

	return nil
}

func (cid *CLIInfoDisplayer) ListFiles(path string, recursive bool, format string, filter string) error {
	targetPath := cid.getFilePath(path)

	var files []string
	var walkFunc filepath.WalkFunc

	if recursive {
		walkFunc = cid.createRecursiveWalkFunc(&files, filter)
	} else {
		walkFunc = cid.createNonRecursiveWalkFunc(&files, filter, targetPath)
	}

	err := filepath.Walk(targetPath, walkFunc)
	if err != nil {
		return fmt.Errorf("failed to walk directory: %w", err)
	}

	return cid.printFilesList(files, format)
}

func (cid *CLIInfoDisplayer) createRecursiveWalkFunc(files *[]string, filter string) filepath.WalkFunc {
	return func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		relPath, err := filepath.Rel(cid.rootPath, path)
		if err != nil {
			return fmt.Errorf("failed to get relative path: %w", err)
		}

		if filter != "" && !strings.Contains(relPath, filter) {
			return nil
		}

		// Add "/" suffix for directories
		if info.IsDir() {
			relPath += "/"
		}

		*files = append(*files, relPath)
		return nil
	}
}

func (cid *CLIInfoDisplayer) createNonRecursiveWalkFunc(files *[]string, filter string, targetPath string) filepath.WalkFunc {
	return func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Don't include the target directory itself, only its contents
		if path == targetPath {
			return nil
		}

		relPath, err := filepath.Rel(cid.rootPath, path)
		if err != nil {
			return fmt.Errorf("failed to get relative path: %w", err)
		}

		if filter != "" && !strings.Contains(relPath, filter) {
			return nil
		}

		// Add "/" suffix for directories
		if info.IsDir() {
			relPath += "/"
		}

		// Add this entry (file or directory)
		*files = append(*files, relPath)

		// Don't recurse into subdirectories when not in recursive mode
		if info.IsDir() {
			return filepath.SkipDir
		}

		return nil
	}
}

func (cid *CLIInfoDisplayer) printFilesList(files []string, format string) error {
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

func (cid *CLIInfoDisplayer) calculateDirStats(path string) (int64, int64) {
	var totalFiles, totalSize int64
	_ = filepath.Walk(path, func(_ string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			totalFiles++
			totalSize += info.Size()
		}
		return nil
	})
	return totalFiles, totalSize
}

// CLIFormatter implements the Formatter interface for CLI formatting operations.
type CLIFormatter struct {
	store Store
}

// NewCLIFormatter creates a new CLIFormatter instance.
func NewCLIFormatter(store Store) *CLIFormatter {
	return &CLIFormatter{
		store: store,
	}
}

func (cf *CLIFormatter) getFilePath(key string) string {
	if fs, ok := cf.store.(*FileSystemStore); ok {
		return filepath.Join(fs.GetRootPath(), key)
	}
	return key
}

func (cf *CLIFormatter) GetFormatted(key string, format string, pretty bool) error {
	filePath := cf.getFilePath(key)

	data, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %w", key, err)
	}

	switch format {
	case "raw":
		fmt.Print(string(data))
	case "json":
		if pretty {
			// Unmarshaling into interface{} for JSON pretty-printing only.
			// nosemgrep: go-unsafe-deserialization-interface
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

func (cf *CLIFormatter) SetFormatted(key string, value string, fromFile bool, isJSON bool, _ bool) error {
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
		// Unmarshaling into interface{} for JSON re-formatting only.
		// nosemgrep: go-unsafe-deserialization-interface
		var jsonData interface{}
		if err := json.Unmarshal(data, &jsonData); err != nil {
			return fmt.Errorf("failed to parse JSON: %w", err)
		}

		data, err = json.MarshalIndent(jsonData, "", "  ")
		if err != nil {
			return fmt.Errorf("failed to format JSON: %w", err)
		}
	}

	filePath := cf.getFilePath(key)

	if err := os.MkdirAll(filepath.Dir(filePath), 0o755); err != nil {
		return fmt.Errorf("failed to create directory for key %s: %w", key, err)
	}

	if err := os.WriteFile(filePath, data, 0o600); err != nil {
		return fmt.Errorf("failed to write file for key %s: %w", key, err)
	}

	return nil
}

func fileTypeString(isDir bool) string {
	if isDir {
		return "directory"
	}
	return "file"
}
