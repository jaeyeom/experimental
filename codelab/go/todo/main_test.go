package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/config"
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
	//	todo addsubtask <id> "item"	Add a subtask to an existing item
	//	todo complete <id>	Complete a todo item
	//	todo uncomplete <id>	Mark a completed item as incomplete
	//	todo undo <id>		Undo a completed item
	//	todo remove <id>	Remove a todo item
	//	todo moveup <id>	Move a todo item up among its siblings
	//	todo movedown <id>	Move a todo item down among its siblings
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
	//	todo addsubtask <id> "item"	Add a subtask to an existing item
	//	todo complete <id>	Complete a todo item
	//	todo uncomplete <id>	Mark a completed item as incomplete
	//	todo undo <id>		Undo a completed item
	//	todo remove <id>	Remove a todo item
	//	todo moveup <id>	Move a todo item up among its siblings
	//	todo movedown <id>	Move a todo item down among its siblings
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

func Example_addSubtask() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
		"33333333-3333-3333-3333-333333333333",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := filepath.Join(dir, "todo.json")

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding parent item:", err)
		return
	}
	if err := addSubtask(s, "1", "buy milk", opt); err != nil {
		fmt.Println("Error adding first subtask:", err)
		return
	}
	if err := addSubtask(s, "1", "buy eggs", opt); err != nil {
		fmt.Println("Error adding second subtask:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	//   22222222-2222-2222-2222-222222222222. [ ] buy milk
	//   33333333-3333-3333-3333-333333333333. [ ] buy eggs
}

func Example_completeWithSubtasks() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
		"33333333-3333-3333-3333-333333333333",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := filepath.Join(dir, "todo.json")

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding parent item:", err)
		return
	}
	if err := addSubtask(s, "1", "buy milk", opt); err != nil {
		fmt.Println("Error adding first subtask:", err)
		return
	}
	if err := addSubtask(s, "1", "buy eggs", opt); err != nil {
		fmt.Println("Error adding second subtask:", err)
		return
	}

	// Complete one subtask
	if err := completeItem(s, "2"); err != nil {
		fmt.Println("Error completing subtask:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	fmt.Println("---")

	// Complete parent task
	if err := completeItem(s, "1"); err != nil {
		fmt.Println("Error completing parent task:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 11111111-1111-1111-1111-111111111111. [-] buy groceries
	//   22222222-2222-2222-2222-222222222222. [x] buy milk
	//   33333333-3333-3333-3333-333333333333. [ ] buy eggs
	// ---
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	//   22222222-2222-2222-2222-222222222222. [x] buy milk
	//   33333333-3333-3333-3333-333333333333. [x] buy eggs
}

func Example_undoWithSubtasks() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
		"33333333-3333-3333-3333-333333333333",
	)
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		fmt.Println("Error creating temp dir:", err)
		return
	}
	defer os.RemoveAll(dir)
	path := filepath.Join(dir, "todo.json")

	s, err := json_storage.New(path)
	if err != nil {
		fmt.Println("Error creating storage:", err)
		return
	}
	defer s.Close()

	opt := core.WithNewID(ig)
	if err := addItem(s, "buy groceries", opt); err != nil {
		fmt.Println("Error adding parent item:", err)
		return
	}
	if err := addSubtask(s, "1", "buy milk", opt); err != nil {
		fmt.Println("Error adding first subtask:", err)
		return
	}
	if err := addSubtask(s, "1", "buy eggs", opt); err != nil {
		fmt.Println("Error adding second subtask:", err)
		return
	}

	// Complete all tasks
	if err := completeItem(s, "1"); err != nil {
		fmt.Println("Error completing parent task:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	fmt.Println("---")

	// Undo one subtask
	if err := undoItem(s, "2"); err != nil {
		fmt.Println("Error undoing subtask:", err)
		return
	}
	if err := listItems(s); err != nil {
		fmt.Println("Error listing items:", err)
		return
	}
	// Output:
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	//   22222222-2222-2222-2222-222222222222. [x] buy milk
	//   33333333-3333-3333-3333-333333333333. [x] buy eggs
	// ---
	// 11111111-1111-1111-1111-111111111111. [-] buy groceries
	//   22222222-2222-2222-2222-222222222222. [ ] buy milk
	//   33333333-3333-3333-3333-333333333333. [x] buy eggs
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

func TestMoveUpCommand(t *testing.T) {
	// Save original args and env
	origArgs := os.Args
	origStorageType := os.Getenv("TODO_STORAGE_TYPE")
	origStoragePath := os.Getenv("TODO_STORAGE_PATH")
	defer func() {
		os.Args = origArgs
		os.Setenv("TODO_STORAGE_TYPE", origStorageType)
		os.Setenv("TODO_STORAGE_PATH", origStoragePath)
	}()

	tests := []struct {
		name      string
		setup     func(t *testing.T, s storage)
		args      []string
		wantError bool
		wantMsg   string
	}{
		{
			name: "move middle item up",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1", "2", "3")))
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"moveup", "2"},
			wantError: false,
		},
		{
			name: "move first item up",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1", "2")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"moveup", "1"},
			wantError: true,
			wantMsg:   "cannot move first item up",
		},
		{
			name: "move non-existent item up",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1")))
				list.Add("First")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"moveup", "nonexistent"},
			wantError: true,
			wantMsg:   "item not found",
		},
		{
			name: "move item up using prefix",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("abc123", "def456")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"moveup", "def"},
			wantError: false,
		},
		{
			name: "move item up with ambiguous prefix",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("abc123", "abc456")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"moveup", "abc"},
			wantError: true,
			wantMsg:   "ambiguous item ID",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a temporary directory for test storage
			tmpDir := t.TempDir()
			storageFile := filepath.Join(tmpDir, "todo.json")

			// Set up test environment
			args := append([]string{
				"--storage-type", "json",
				"--storage-path", storageFile,
			}, tt.args...)

			// Create storage
			cfg, fs, err := config.ParseStorageConfig(args)
			if err != nil {
				t.Fatal(err)
			}
			s, err := cfg.NewStorage()
			if err != nil {
				t.Fatal(err)
			}
			defer s.Close()

			if tt.setup != nil {
				tt.setup(t, s)
			}

			// Set args and env for run()
			os.Args = append([]string{"todo"}, fs.Args()...)
			os.Setenv("TODO_STORAGE_TYPE", string(cfg.Type))
			os.Setenv("TODO_STORAGE_PATH", cfg.Path)

			err = run()
			if tt.wantError {
				if err == nil {
					t.Error("expected error but got nil")
				} else if !strings.Contains(err.Error(), tt.wantMsg) {
					t.Errorf("expected error message containing %q but got %q", tt.wantMsg, err.Error())
				}
			} else if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
		})
	}
}

func TestMoveDownCommand(t *testing.T) {
	// Save original args and env
	origArgs := os.Args
	origStorageType := os.Getenv("TODO_STORAGE_TYPE")
	origStoragePath := os.Getenv("TODO_STORAGE_PATH")
	defer func() {
		os.Args = origArgs
		os.Setenv("TODO_STORAGE_TYPE", origStorageType)
		os.Setenv("TODO_STORAGE_PATH", origStoragePath)
	}()

	tests := []struct {
		name      string
		setup     func(t *testing.T, s storage)
		args      []string
		wantError bool
		wantMsg   string
	}{
		{
			name: "move middle item down",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1", "2", "3")))
				list.Add("First")
				list.Add("Second")
				list.Add("Third")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"movedown", "2"},
			wantError: false,
		},
		{
			name: "move last item down",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1", "2")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"movedown", "2"},
			wantError: true,
			wantMsg:   "cannot move last item down",
		},
		{
			name: "move non-existent item down",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("1")))
				list.Add("First")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"movedown", "nonexistent"},
			wantError: true,
			wantMsg:   "item not found",
		},
		{
			name: "move item down using prefix",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("abc123", "def456")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"movedown", "abc"},
			wantError: false,
		},
		{
			name: "move item down with ambiguous prefix",
			setup: func(t *testing.T, s storage) {
				list := core.NewList(core.WithNewID(coretest.NewIDGen("abc123", "abc456")))
				list.Add("First")
				list.Add("Second")
				if err := s.Save(list); err != nil {
					t.Fatal(err)
				}
			},
			args:      []string{"movedown", "abc"},
			wantError: true,
			wantMsg:   "ambiguous item ID",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a temporary directory for test storage
			tmpDir := t.TempDir()
			storageFile := filepath.Join(tmpDir, "todo.json")

			// Set up test environment
			args := append([]string{
				"--storage-type", "json",
				"--storage-path", storageFile,
			}, tt.args...)

			// Create storage
			cfg, fs, err := config.ParseStorageConfig(args)
			if err != nil {
				t.Fatal(err)
			}
			s, err := cfg.NewStorage()
			if err != nil {
				t.Fatal(err)
			}
			defer s.Close()

			if tt.setup != nil {
				tt.setup(t, s)
			}

			// Set args and env for run()
			os.Args = append([]string{"todo"}, fs.Args()...)
			os.Setenv("TODO_STORAGE_TYPE", string(cfg.Type))
			os.Setenv("TODO_STORAGE_PATH", cfg.Path)

			err = run()
			if tt.wantError {
				if err == nil {
					t.Error("expected error but got nil")
				} else if !strings.Contains(err.Error(), tt.wantMsg) {
					t.Errorf("expected error message containing %q but got %q", tt.wantMsg, err.Error())
				}
			} else if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
		})
	}
}
