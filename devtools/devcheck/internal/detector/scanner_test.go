package detector

import (
	"os"
	"path/filepath"
	"testing"
)

func TestScanner_Scan(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "scanner_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name          string
		files         []string
		directories   []string
		options       ScanOptions
		expectedFiles []string
		expectedDirs  []string
		expectError   bool
	}{
		{
			name:          "basic file and directory scan",
			files:         []string{"main.go", "README.md", "src/utils.go"},
			directories:   []string{"src", "docs"},
			options:       DefaultScanOptions(),
			expectedFiles: []string{"main.go", "README.md", "src/utils.go"},
			expectedDirs:  []string{"src", "docs"},
			expectError:   false,
		},
		{
			name:          "hidden files excluded by default",
			files:         []string{"main.go", ".gitignore", ".hidden/secret.txt"},
			directories:   []string{".hidden"},
			options:       DefaultScanOptions(),
			expectedFiles: []string{"main.go"},
			expectedDirs:  []string{},
			expectError:   false,
		},
		{
			name:        "hidden files included when specified",
			files:       []string{"main.go", ".gitignore", ".env"},
			directories: []string{},
			options: ScanOptions{
				MaxDepth:       5,
				FollowSymlinks: false,
				IgnorePatterns: []string{"node_modules"},
				IncludeHidden:  true,
			},
			expectedFiles: []string{"main.go", ".gitignore", ".env"},
			expectedDirs:  []string{},
			expectError:   false,
		},
		{
			name:          "ignore patterns work",
			files:         []string{"main.go", "node_modules/package.json", "vendor/lib.go"},
			directories:   []string{"node_modules", "vendor"},
			options:       DefaultScanOptions(),
			expectedFiles: []string{"main.go"},
			expectedDirs:  []string{},
			expectError:   false,
		},
		{
			name:        "max depth limiting",
			files:       []string{"main.go", "src/utils.go", "src/deep/nested/file.go"},
			directories: []string{"src", "src/deep", "src/deep/nested"},
			options: ScanOptions{
				MaxDepth:       1,
				FollowSymlinks: false,
				IgnorePatterns: []string{},
				IncludeHidden:  false,
			},
			expectedFiles: []string{"main.go", "src/utils.go"},
			expectedDirs:  []string{"src", "src/deep"},
			expectError:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test directory
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create test directories
			for _, dir := range tt.directories {
				dirPath := filepath.Join(testDir, dir)
				err := os.MkdirAll(dirPath, 0o755)
				if err != nil {
					t.Fatal(err)
				}
			}

			// Create test files
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				dir := filepath.Dir(filePath)
				if dir != testDir {
					err := os.MkdirAll(dir, 0o755)
					if err != nil {
						t.Fatal(err)
					}
				}
				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			scanner := NewScanner(tt.options)
			result, err := scanner.Scan(testDir)

			if tt.expectError {
				if err == nil {
					t.Error("Expected error, but got none")
				}
				return
			}

			if err != nil {
				t.Errorf("Unexpected error: %v", err)
				return
			}

			// Check files
			if len(result.Files) != len(tt.expectedFiles) {
				t.Errorf("Expected %d files, got %d: %v", len(tt.expectedFiles), len(result.Files), result.Files)
			} else {
				expectedFileMap := make(map[string]bool)
				for _, file := range tt.expectedFiles {
					expectedFileMap[file] = true
				}

				for _, file := range result.Files {
					if !expectedFileMap[file] {
						t.Errorf("Unexpected file: %s", file)
					}
					delete(expectedFileMap, file)
				}

				if len(expectedFileMap) > 0 {
					t.Errorf("Missing expected files: %v", expectedFileMap)
				}
			}

			// Check directories
			if len(result.Directories) != len(tt.expectedDirs) {
				t.Errorf("Expected %d directories, got %d: %v", len(tt.expectedDirs), len(result.Directories), result.Directories)
			} else {
				expectedDirMap := make(map[string]bool)
				for _, dir := range tt.expectedDirs {
					expectedDirMap[dir] = true
				}

				for _, dir := range result.Directories {
					if !expectedDirMap[dir] {
						t.Errorf("Unexpected directory: %s", dir)
					}
					delete(expectedDirMap, dir)
				}

				if len(expectedDirMap) > 0 {
					t.Errorf("Missing expected directories: %v", expectedDirMap)
				}
			}

			// Check root path
			if result.Root != testDir {
				t.Errorf("Expected root %s, got %s", testDir, result.Root)
			}
		})
	}
}

func TestScanner_ScanInvalidPath(t *testing.T) {
	scanner := NewScanner(DefaultScanOptions())

	// Test non-existent path
	_, err := scanner.Scan("/path/that/does/not/exist")
	if err == nil {
		t.Error("Expected error for non-existent path")
	}

	// Test file instead of directory
	tempFile, err := os.CreateTemp("", "test_file")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tempFile.Name())
	tempFile.Close()

	_, err = scanner.Scan(tempFile.Name())
	if err == nil {
		t.Error("Expected error when scanning a file instead of directory")
	}
}

func TestScanResult_GetFilesByExtension(t *testing.T) {
	result := &ScanResult{
		Files: []string{
			"main.go",
			"utils.py",
			"README.md",
			"src/component.ts",
			"test.GO", // Test case sensitivity
		},
	}

	tests := []struct {
		name       string
		extensions []string
		expected   []string
	}{
		{
			name:       "go files",
			extensions: []string{".go"},
			expected:   []string{"main.go", "test.GO"},
		},
		{
			name:       "python files",
			extensions: []string{"py"},
			expected:   []string{"utils.py"},
		},
		{
			name:       "multiple extensions",
			extensions: []string{".go", ".py"},
			expected:   []string{"main.go", "utils.py", "test.GO"},
		},
		{
			name:       "typescript files",
			extensions: []string{".ts"},
			expected:   []string{"src/component.ts"},
		},
		{
			name:       "no matches",
			extensions: []string{".java"},
			expected:   []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filtered := result.GetFilesByExtension(tt.extensions...)

			if len(filtered) != len(tt.expected) {
				t.Errorf("Expected %d files, got %d: %v", len(tt.expected), len(filtered), filtered)
				return
			}

			expectedMap := make(map[string]bool)
			for _, file := range tt.expected {
				expectedMap[file] = true
			}

			for _, file := range filtered {
				if !expectedMap[file] {
					t.Errorf("Unexpected file: %s", file)
				}
			}
		})
	}
}

func TestScanResult_GetFilesByPattern(t *testing.T) {
	result := &ScanResult{
		Files: []string{
			"main.go",
			"main_test.go",
			"utils.py",
			"Makefile",
			"README.md",
			"go.mod",
		},
	}

	tests := []struct {
		name     string
		patterns []string
		expected []string
	}{
		{
			name:     "test files",
			patterns: []string{"*_test.go"},
			expected: []string{"main_test.go"},
		},
		{
			name:     "makefile",
			patterns: []string{"Makefile", "makefile"},
			expected: []string{"Makefile"},
		},
		{
			name:     "go module files",
			patterns: []string{"go.*"},
			expected: []string{"go.mod"},
		},
		{
			name:     "multiple patterns",
			patterns: []string{"*.py", "*.md"},
			expected: []string{"utils.py", "README.md"},
		},
		{
			name:     "no matches",
			patterns: []string{"*.java"},
			expected: []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filtered := result.GetFilesByPattern(tt.patterns...)

			if len(filtered) != len(tt.expected) {
				t.Errorf("Expected %d files, got %d: %v", len(tt.expected), len(filtered), filtered)
				return
			}

			expectedMap := make(map[string]bool)
			for _, file := range tt.expected {
				expectedMap[file] = true
			}

			for _, file := range filtered {
				if !expectedMap[file] {
					t.Errorf("Unexpected file: %s", file)
				}
			}
		})
	}
}

func TestScanResult_HasFile(t *testing.T) {
	result := &ScanResult{
		Files: []string{
			"go.mod",
			"src/main.go",
			"README.md",
		},
	}

	tests := []struct {
		name     string
		filename string
		expected bool
	}{
		{
			name:     "existing file in root",
			filename: "go.mod",
			expected: true,
		},
		{
			name:     "existing file in subdirectory",
			filename: "main.go",
			expected: true,
		},
		{
			name:     "non-existing file",
			filename: "package.json",
			expected: false,
		},
		{
			name:     "full path should not match",
			filename: "src/main.go",
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := result.HasFile(tt.filename)
			if result != tt.expected {
				t.Errorf("HasFile(%s) = %v, expected %v", tt.filename, result, tt.expected)
			}
		})
	}
}

func TestScanResult_HasPattern(t *testing.T) {
	result := &ScanResult{
		Files: []string{
			"main.go",
			"utils_test.go",
			"README.md",
			"src/component.ts",
		},
	}

	tests := []struct {
		name     string
		pattern  string
		expected bool
	}{
		{
			name:     "go files pattern",
			pattern:  "*.go",
			expected: true,
		},
		{
			name:     "test files pattern",
			pattern:  "*_test.go",
			expected: true,
		},
		{
			name:     "typescript files pattern",
			pattern:  "*.ts",
			expected: true,
		},
		{
			name:     "non-matching pattern",
			pattern:  "*.java",
			expected: false,
		},
		{
			name:     "specific file pattern",
			pattern:  "README.md",
			expected: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := result.HasPattern(tt.pattern)
			if result != tt.expected {
				t.Errorf("HasPattern(%s) = %v, expected %v", tt.pattern, result, tt.expected)
			}
		})
	}
}

func TestDefaultScanOptions(t *testing.T) {
	options := DefaultScanOptions()

	if options.MaxDepth != 5 {
		t.Errorf("Expected MaxDepth 5, got %d", options.MaxDepth)
	}

	if options.FollowSymlinks {
		t.Error("Expected FollowSymlinks to be false")
	}

	if options.IncludeHidden {
		t.Error("Expected IncludeHidden to be false")
	}

	// Check that common ignore patterns are included
	expectedPatterns := []string{"node_modules", ".git", "vendor", "__pycache__"}
	for _, pattern := range expectedPatterns {
		found := false
		for _, ignorePattern := range options.IgnorePatterns {
			if ignorePattern == pattern {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected ignore pattern %s not found", pattern)
		}
	}
}

func TestNewScanner(t *testing.T) {
	options := ScanOptions{
		MaxDepth:       3,
		FollowSymlinks: true,
		IgnorePatterns: []string{"*.tmp"},
		IncludeHidden:  true,
	}

	scanner := NewScanner(options)

	if scanner == nil {
		t.Fatal("NewScanner returned nil")
		return
	}

	if scanner.options.MaxDepth != 3 {
		t.Errorf("Expected MaxDepth 3, got %d", scanner.options.MaxDepth)
	}

	if !scanner.options.FollowSymlinks {
		t.Error("Expected FollowSymlinks to be true")
	}

	if !scanner.options.IncludeHidden {
		t.Error("Expected IncludeHidden to be true")
	}
}
