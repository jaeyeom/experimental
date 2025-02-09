package main

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
	json_storage "github.com/jaeyeom/experimental/codelab/go/todo/storage/json"
)

func Example_listItems() {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// You have no todo items.
}

func Example_addItem() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding first item:", err)
		return
	}
	if err := addItem(s, "write code", opt); err != nil {
		fmt.Println("Error adding second item:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func Example_completeItem() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding first item:", err)
		return
	}
	if err := addItem(s, "write code", opt); err != nil {
		fmt.Println("Error adding second item:", err)
		return
	}
	if err := completeItem(s, "1"); err != nil {
		fmt.Println("Error completing item:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func Example_removeItem() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding first item:", err)
		return
	}
	if err := addItem(s, "write code", opt); err != nil {
		fmt.Println("Error adding second item:", err)
		return
	}
	if err := removeItem(s, "1"); err != nil {
		fmt.Println("Error removing item:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func Example_noCommand() {
	origArgs := os.Args
	defer func() { os.Args = origArgs }()

	os.Args = []string{"todo"}
	if err := run(); err != nil {
		fmt.Printf("Error: %v\n", err)
	}
	// Output:
	// Usage:
	//	todo ls			List todo items
	//	todo add "todo item"	Add a new todo item
	//	todo complete <id>	Complete a todo item
	//	todo uncomplete <id>	Mark a completed item as incomplete
	//	todo remove <id>	Remove a todo item
	//
	// Storage configuration:
	//	--storage-type		Storage backend type (json or sqlite)
	//	--storage-path		Path to storage file
	//
	// Environment variables:
	//	TODO_STORAGE_TYPE	Storage backend type
	//	TODO_STORAGE_PATH	Path to storage file
	// Error: no command provided
}

func Example_invalidCommand() {
	origArgs := os.Args
	defer func() { os.Args = origArgs }()

	os.Args = []string{"todo", "invalid"}
	if err := run(); err != nil {
		fmt.Printf("Error: %v\n", err)
	}
	// Output:
	// Usage:
	//	todo ls			List todo items
	//	todo add "todo item"	Add a new todo item
	//	todo complete <id>	Complete a todo item
	//	todo uncomplete <id>	Mark a completed item as incomplete
	//	todo remove <id>	Remove a todo item
	//
	// Storage configuration:
	//	--storage-type		Storage backend type (json or sqlite)
	//	--storage-path		Path to storage file
	//
	// Environment variables:
	//	TODO_STORAGE_TYPE	Storage backend type
	//	TODO_STORAGE_PATH	Path to storage file
	// Error: unknown command: invalid
}

func TestStorageConfiguration(t *testing.T) {
	// Create temp directory for test files
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	// Save original args and env
	origArgs := os.Args
	origEnv := os.Getenv("TODO_STORAGE_TYPE")
	origPath := os.Getenv("TODO_STORAGE_PATH")
	defer func() {
		os.Args = origArgs
		os.Setenv("TODO_STORAGE_TYPE", origEnv)
		os.Setenv("TODO_STORAGE_PATH", origPath)
	}()

	tests := []struct {
		name    string
		args    []string
		env     map[string]string
		wantErr bool
	}{
		{
			name:    "default configuration",
			args:    []string{"todo"},
			env:     map[string]string{},
			wantErr: true, // Expect error for no command
		},
		{
			name: "json storage with custom path via flags",
			args: []string{"todo", "--storage-type", "json", "--storage-path", filepath.Join(dir, "todo.json"), "ls"},
			env:  map[string]string{},
		},
		{
			name: "sqlite storage with custom path via flags",
			args: []string{"todo", "--storage-type", "sqlite", "--storage-path", filepath.Join(dir, "todo.db"), "ls"},
			env:  map[string]string{},
		},
		{
			name: "json storage with custom path via env",
			args: []string{"todo", "ls"},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "json",
				"TODO_STORAGE_PATH": filepath.Join(dir, "todo.json"),
			},
		},
		{
			name: "sqlite storage with custom path via env",
			args: []string{"todo", "ls"},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "sqlite",
				"TODO_STORAGE_PATH": filepath.Join(dir, "todo.db"),
			},
		},
		{
			name: "flags override env variables",
			args: []string{"todo", "--storage-type", "sqlite", "--storage-path", filepath.Join(dir, "flag.db"), "ls"},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "json",
				"TODO_STORAGE_PATH": filepath.Join(dir, "env.json"),
			},
		},
		{
			name:    "invalid storage type",
			args:    []string{"todo", "--storage-type", "invalid", "ls"},
			env:     map[string]string{},
			wantErr: true,
		},
		{
			name: "invalid storage type in env",
			args: []string{"todo", "ls"},
			env: map[string]string{
				"TODO_STORAGE_TYPE": "invalid",
			},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Set test args
			os.Args = tt.args

			// Set test env
			for k, v := range tt.env {
				os.Setenv(k, v)
			}

			err := run()
			if (err != nil) != tt.wantErr {
				t.Errorf("run() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
