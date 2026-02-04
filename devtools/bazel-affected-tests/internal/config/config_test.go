package config

import (
	"os"
	"reflect"
	"sort"
	"testing"
)

func TestLoadConfig(t *testing.T) {
	tests := []struct {
		name       string
		content    string
		wantConfig *Config
		wantErr    bool
	}{
		{
			name: "valid config with rules",
			content: `version: 1
rules:
  - patterns:
      - "**/BUILD"
      - "**/BUILD.bazel"
    targets:
      - "//..."
  - patterns:
      - "**/*.bzl"
    targets:
      - "//tools/..."
`,
			wantConfig: &Config{
				Version: 1,
				Rules: []Rule{
					{
						Patterns: []string{"**/BUILD", "**/BUILD.bazel"},
						Targets:  []string{"//..."},
					},
					{
						Patterns: []string{"**/*.bzl"},
						Targets:  []string{"//tools/..."},
					},
				},
			},
			wantErr: false,
		},
		{
			name:    "invalid YAML",
			content: "invalid: [yaml: content",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary directory
			tmpDir := t.TempDir()
			oldWd, err := os.Getwd()
			if err != nil {
				t.Fatal(err)
			}
			defer func() {
				if err := os.Chdir(oldWd); err != nil {
					t.Errorf("failed to restore working directory: %v", err)
				}
			}()

			if err := os.Chdir(tmpDir); err != nil {
				t.Fatal(err)
			}

			// Write config file
			if err := os.WriteFile(ConfigFileName, []byte(tt.content), 0o600); err != nil {
				t.Fatal(err)
			}

			got, err := LoadConfig()
			if (err != nil) != tt.wantErr {
				t.Errorf("LoadConfig() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if !tt.wantErr && !reflect.DeepEqual(got, tt.wantConfig) {
				t.Errorf("LoadConfig() = %+v, want %+v", got, tt.wantConfig)
			}
		})
	}
}

func TestLoadConfig_MissingFile(t *testing.T) {
	// Create temporary directory
	tmpDir := t.TempDir()
	oldWd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(oldWd); err != nil {
			t.Errorf("failed to restore working directory: %v", err)
		}
	}()

	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	got, err := LoadConfig()
	if err != nil {
		t.Errorf("LoadConfig() error = %v, want nil", err)
	}
	if got != nil {
		t.Errorf("LoadConfig() = %v, want nil", got)
	}
}

func TestConfig_MatchTargets(t *testing.T) {
	config := &Config{
		Version: 1,
		Rules: []Rule{
			{
				Patterns: []string{"**/BUILD", "**/BUILD.bazel"},
				Targets:  []string{"//..."},
			},
			{
				Patterns: []string{"**/*.bzl"},
				Targets:  []string{"//tools/...", "//build/..."},
			},
			{
				Patterns: []string{"WORKSPACE"},
				Targets:  []string{"//..."},
			},
		},
	}

	tests := []struct {
		name  string
		files []string
		want  []string
	}{
		{
			name:  "BUILD file matches first rule",
			files: []string{"foo/bar/BUILD"},
			want:  []string{"//..."},
		},
		{
			name:  ".bzl file matches second rule",
			files: []string{"tools/defs.bzl"},
			want:  []string{"//tools/...", "//build/..."},
		},
		{
			name:  "multiple files match different rules",
			files: []string{"foo/BUILD", "bar/defs.bzl"},
			want:  []string{"//...", "//tools/...", "//build/..."},
		},
		{
			name:  "WORKSPACE matches third rule",
			files: []string{"WORKSPACE"},
			want:  []string{"//..."},
		},
		{
			name:  "no matching files",
			files: []string{"README.md", "foo.txt"},
			want:  []string{},
		},
		{
			name:  "empty files list",
			files: []string{},
			want:  []string{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := config.MatchTargets(tt.files)
			sort.Strings(got)
			sort.Strings(tt.want)

			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("MatchTargets() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestLoadConfig_InvalidPath(t *testing.T) {
	// Create a directory with the config file name to cause read error
	tmpDir := t.TempDir()
	oldWd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(oldWd); err != nil {
			t.Errorf("failed to restore working directory: %v", err)
		}
	}()

	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Create a directory instead of a file
	if err := os.Mkdir(ConfigFileName, 0o755); err != nil {
		t.Fatal(err)
	}

	_, err = LoadConfig()
	if err == nil {
		t.Error("LoadConfig() expected error when config is a directory, got nil")
	}
}

func TestConfig_MatchTargets_Deduplication(t *testing.T) {
	config := &Config{
		Version: 1,
		Rules: []Rule{
			{
				Patterns: []string{"**/BUILD"},
				Targets:  []string{"//..."},
			},
			{
				Patterns: []string{"**/BUILD.bazel"},
				Targets:  []string{"//..."},
			},
		},
	}

	files := []string{"foo/BUILD", "bar/BUILD.bazel"}
	got := config.MatchTargets(files)

	if len(got) != 1 {
		t.Errorf("MatchTargets() returned %d targets, want 1 (deduplicated)", len(got))
	}

	if len(got) > 0 && got[0] != "//..." {
		t.Errorf("MatchTargets() = %v, want [//...]", got)
	}
}
