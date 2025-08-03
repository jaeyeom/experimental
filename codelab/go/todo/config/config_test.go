package config

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	"github.com/jaeyeom/experimental/codelab/go/todo/testutil"
)

func TestStorageConfig(t *testing.T) {
	// Save original env
	origEnv := os.Getenv("TODO_STORAGE_TYPE")
	origPath := os.Getenv("TODO_STORAGE_PATH")
	defer func() {
		os.Setenv("TODO_STORAGE_TYPE", origEnv)
		os.Setenv("TODO_STORAGE_PATH", origPath)
	}()

	tests := []struct {
		name    string
		args    []string
		env     map[string]string
		want    *StorageConfig
		wantErr bool
	}{
		{
			name: "default configuration",
			args: []string{},
			env:  map[string]string{},
			want: &StorageConfig{
				Type: StorageTypeJSON,
				// Path will be set dynamically in the test
			},
		},
		{
			name: "json storage with custom path via flags",
			args: []string{"--storage-type", "json", "--storage-path", "/custom/path.json"},
			env:  map[string]string{},
			want: &StorageConfig{
				Type: StorageTypeJSON,
				Path: "/custom/path.json",
			},
		},
		{
			name: "sqlite storage with custom path via flags",
			args: []string{"--storage-type", "sqlite", "--storage-path", "/custom/path.db"},
			env:  map[string]string{},
			want: &StorageConfig{
				Type: StorageTypeSQLite,
				Path: "/custom/path.db",
			},
		},
		{
			name: "json storage with custom path via env",
			args: []string{},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "json",
				"TODO_STORAGE_PATH": "/custom/path.json",
			},
			want: &StorageConfig{
				Type: StorageTypeJSON,
				Path: "/custom/path.json",
			},
		},
		{
			name: "sqlite storage with custom path via env",
			args: []string{},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "sqlite",
				"TODO_STORAGE_PATH": "/custom/path.db",
			},
			want: &StorageConfig{
				Type: StorageTypeSQLite,
				Path: "/custom/path.db",
			},
		},
		{
			name: "flags override env variables",
			args: []string{"--storage-type", "sqlite", "--storage-path", "/flag/path.db"},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "json",
				"TODO_STORAGE_PATH": "/env/path.json",
			},
			want: &StorageConfig{
				Type: StorageTypeSQLite,
				Path: "/flag/path.db",
			},
		},
		{
			name:    "invalid storage type",
			args:    []string{"--storage-type", "invalid"},
			env:     map[string]string{},
			wantErr: true,
		},
		{
			name: "invalid storage type in env",
			args: []string{},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "invalid",
			},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Set up temporary HOME directory for all tests
			cleanup := testutil.WithTempHome(t)
			defer cleanup()

			// Update expected path to use the real temp directory for default config test
			if tt.want != nil && tt.want.Path == "" {
				tempHome := os.Getenv("HOME")
				tt.want.Path = filepath.Join(tempHome, ".todo", "todos.json")
			}

			// Set test env
			for k, v := range tt.env {
				os.Setenv(k, v)
			}

			got, _, err := ParseStorageConfig(tt.args)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseStorageConfig() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ParseStorageConfig() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNewStorage(t *testing.T) {
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	tests := []struct {
		name    string
		config  *StorageConfig
		wantErr bool
	}{
		{
			name: "json storage",
			config: &StorageConfig{
				Type: StorageTypeJSON,
				Path: filepath.Join(dir, "todo.json"),
			},
		},
		{
			name: "sqlite storage",
			config: &StorageConfig{
				Type: StorageTypeSQLite,
				Path: filepath.Join(dir, "todo.db"),
			},
		},
		{
			name: "invalid storage type",
			config: &StorageConfig{
				Type: "invalid",
				Path: filepath.Join(dir, "todo.db"),
			},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s, err := tt.config.NewStorage()
			if (err != nil) != tt.wantErr {
				t.Errorf("NewStorage() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err != nil {
				return
			}
			defer s.Close()

			// Test basic operations
			list := core.NewList()
			list.Add("test item")

			if err := s.Save(list); err != nil {
				t.Errorf("Save() error = %v", err)
			}

			loaded, err := s.Load()
			if err != nil {
				t.Errorf("Load() error = %v", err)
			}

			if len(loaded.Items) != len(list.Items) {
				t.Errorf("Load() got %d items, want %d", len(loaded.Items), len(list.Items))
			}
		})
	}
}
