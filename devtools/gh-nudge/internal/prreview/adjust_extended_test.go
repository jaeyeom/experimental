package prreview

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

// TestParseMappingFile tests the mapping file parsing functionality.
func TestParseMappingFile(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    map[string][]MappingFileEntry
		wantErr bool
	}{
		{
			name: "valid mapping file",
			content: `src/main.js:15:-2
src/main.js:30:+3
src/utils.js:45:-1
# This is a comment
# Another comment

src/auth.js:20:+5`,
			want: map[string][]MappingFileEntry{
				"src/main.js": {
					{FilePath: "src/main.js", Line: 15, Offset: -2},
					{FilePath: "src/main.js", Line: 30, Offset: 3},
				},
				"src/utils.js": {
					{FilePath: "src/utils.js", Line: 45, Offset: -1},
				},
				"src/auth.js": {
					{FilePath: "src/auth.js", Line: 20, Offset: 5},
				},
			},
			wantErr: false,
		},
		{
			name:    "invalid format - missing colon",
			content: "src/main.js 15:-2",
			wantErr: true,
		},
		{
			name:    "invalid format - invalid line number",
			content: "src/main.js:abc:-2",
			wantErr: true,
		},
		{
			name:    "invalid format - invalid offset",
			content: "src/main.js:15:abc",
			wantErr: true,
		},
		{
			name:    "empty file",
			content: "",
			wantErr: true,
		},
		{
			name: "only comments and empty lines",
			content: `# This is a comment

# Another comment
`,
			wantErr: true,
		},
		{
			name: "mixed valid and invalid lines",
			content: `src/main.js:15:-2
invalid line format
src/utils.js:30:+3`,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary file
			tmpDir := t.TempDir()
			tmpFile := filepath.Join(tmpDir, "mapping.txt")

			err := os.WriteFile(tmpFile, []byte(tt.content), 0o600)
			if err != nil {
				t.Fatalf("Failed to create temp file: %v", err)
			}

			got, err := parseMappingFile(tmpFile)
			if (err != nil) != tt.wantErr {
				t.Errorf("parseMappingFile() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if !tt.wantErr && !reflect.DeepEqual(got, tt.want) {
				t.Errorf("parseMappingFile() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestConvertMappingEntriesToDiffSpec tests the conversion from mapping entries to diff spec.
func TestConvertMappingEntriesToDiffSpec(t *testing.T) {
	tests := []struct {
		name    string
		entries []MappingFileEntry
		want    string
	}{
		{
			name: "single entry with negative offset",
			entries: []MappingFileEntry{
				{FilePath: "src/main.js", Line: 15, Offset: -2},
			},
			want: "15:-2",
		},
		{
			name: "single entry with positive offset",
			entries: []MappingFileEntry{
				{FilePath: "src/main.js", Line: 30, Offset: 3},
			},
			want: "30:+3",
		},
		{
			name: "multiple entries",
			entries: []MappingFileEntry{
				{FilePath: "src/main.js", Line: 15, Offset: -2},
				{FilePath: "src/main.js", Line: 30, Offset: 3},
				{FilePath: "src/main.js", Line: 45, Offset: -1},
			},
			want: "15:-2;30:+3;45:-1",
		},
		{
			name:    "empty entries",
			entries: []MappingFileEntry{},
			want:    "",
		},
		{
			name: "zero offset",
			entries: []MappingFileEntry{
				{FilePath: "src/main.js", Line: 20, Offset: 0},
			},
			want: "20:+0",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := convertMappingEntriesToDiffSpec(tt.entries)
			if got != tt.want {
				t.Errorf("convertMappingEntriesToDiffSpec() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestParseMappingFileNotFound tests error handling for non-existent files.
func TestParseMappingFileNotFound(t *testing.T) {
	_, err := parseMappingFile("/nonexistent/file.txt")
	if err == nil {
		t.Error("parseMappingFile() should return error for non-existent file")
	}
}
