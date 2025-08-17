// Package config provides configuration management for the todo application.
package config

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	jsonstorage "github.com/jaeyeom/experimental/codelab/go/todo/storage/json"
	"github.com/jaeyeom/experimental/codelab/go/todo/storage/sqlite"
)

// StorageType represents the type of storage backend.
type StorageType string

const (
	// StorageTypeJSON represents JSON file storage.
	StorageTypeJSON StorageType = "json"
	// StorageTypeSQLite represents SQLite database storage.
	StorageTypeSQLite StorageType = "sqlite"
)

// StorageConfig holds configuration for the storage backend.
type StorageConfig struct {
	// Type specifies the type of storage backend.
	Type StorageType
	// Path specifies the path to the storage file.
	Path string
}

// ParseStorageConfig parses command line arguments and environment variables to
// determine storage configuration. Command line flags take precedence over
// environment variables. Returns the config and the FlagSet for further
// argument parsing.
func ParseStorageConfig(args []string) (*StorageConfig, *flag.FlagSet, error) {
	// Set up flags
	fs := flag.NewFlagSet("todo", flag.ContinueOnError)
	storageType := fs.String("storage-type", "", "Storage backend type (json or sqlite)")
	storagePath := fs.String("storage-path", "", "Path to storage file")

	// Parse flags
	if err := fs.Parse(args); err != nil {
		return nil, fs, fmt.Errorf("parse flags: %v", err)
	}

	// Get values from environment if not set by flags
	if *storageType == "" {
		*storageType = os.Getenv("TODO_STORAGE_TYPE")
	}
	if *storagePath == "" {
		*storagePath = os.Getenv("TODO_STORAGE_PATH")
	}

	// Apply defaults if still not set
	if *storageType == "" {
		*storageType = "json"
	}
	if *storagePath == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fs, fmt.Errorf("get home dir: %v", err)
		}
		switch *storageType {
		case "json":
			*storagePath = filepath.Join(home, ".todo", "todos.json")
		case "sqlite":
			*storagePath = filepath.Join(home, ".todo", "todos.db")
		}
	}

	// Validate storage type
	switch *storageType {
	case "json", "sqlite":
		// Valid types
	default:
		return nil, fs, fmt.Errorf("invalid storage type: %s", *storageType)
	}

	return &StorageConfig{
		Type: StorageType(*storageType),
		Path: *storagePath,
	}, fs, nil
}

// NewStorage creates a new storage instance based on the configuration.
func (c *StorageConfig) NewStorage() (Storage, error) {
	switch c.Type {
	case StorageTypeJSON:
		s, err := jsonstorage.New(c.Path)
		if err != nil {
			return nil, fmt.Errorf("create JSON storage: %w", err)
		}
		return s, nil
	case StorageTypeSQLite:
		s, err := sqlite.New(c.Path)
		if err != nil {
			return nil, fmt.Errorf("create SQLite storage: %w", err)
		}
		return s, nil
	default:
		return nil, fmt.Errorf("invalid storage type %q", c.Type)
	}
}

// Storage is an interface for persisting todo lists.
type Storage interface {
	Save(list *core.List) error
	Load() (*core.List, error)
	Close() error
}
