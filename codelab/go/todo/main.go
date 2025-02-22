// Binary todo is a command line todo application that helps users manage their
// tasks. It supports basic operations like listing, adding, completing, and
// removing todo items.
//
// Usage:
//
//	todo ls                   List todo items
//	todo add "item"           Add a new todo item
//	todo addsubtask id "item" Add a subtask to an existing item
//	todo complete id          Complete a todo item
//	todo uncomplete id        Mark a completed item as incomplete
//	todo undo id              Undo a completed item
//	todo remove id            Remove a todo item
//	todo moveup id            Move a todo item up among its siblings
//	todo movedown id          Move a todo item down among its siblings
//
// Items can be stored in either JSON or SQLite format. By default, items are
// stored in JSON format in ~/.todo/todos.json. The storage backend can be
// configured using flags or environment variables:
//
// Flags:
//
//	--storage-type: Type of storage backend (json or sqlite)
//	--storage-path: Path to the storage file
//
// Environment variables:
//
//	TODO_STORAGE_TYPE: Type of storage backend
//	TODO_STORAGE_PATH: Path to the storage file
package main

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/codelab/go/todo/config"
	"github.com/jaeyeom/experimental/codelab/go/todo/core"
)

// storage is an interface for persisting todo lists.
type storage interface {
	Save(list *core.List) error
	Load() (*core.List, error)
	Close() error
}

func init() {
	// Initialize structured logging
	opts := &slog.HandlerOptions{
		Level: slog.LevelInfo,
	}
	handler := slog.NewTextHandler(os.Stderr, opts)
	logger := slog.New(handler)
	slog.SetDefault(logger)
}

// printUsage prints the command usage information to stdout.
func printUsage() {
	fmt.Println(`Usage:
	todo ls			List todo items
	todo add "todo item"	Add a new todo item
	todo addsubtask <id> "item"	Add a subtask to an existing item
	todo complete <id>	Complete a todo item
	todo uncomplete <id>	Mark a completed item as incomplete
	todo undo <id>		Undo a completed item
	todo remove <id>	Remove a todo item
	todo moveup <id>	Move a todo item up among its siblings
	todo movedown <id>	Move a todo item down among its siblings

Storage configuration:
	--storage-type		Storage backend type (json or sqlite)
	--storage-path		Path to storage file

Environment variables:
	TODO_STORAGE_TYPE	Storage backend type
	TODO_STORAGE_PATH	Path to storage file`)
}

// listItems loads and displays all todo items. If there are no items, it prints
// a message indicating an empty list.
func listItems(s storage) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if len(todos.Items) == 0 {
		fmt.Println("You have no todo items.")
		return nil
	}
	fmt.Println(todos)
	return nil
}

// addItem adds a new todo item to the list and saves it. It accepts options to
// customize the list behavior (e.g., ID generation).
func addItem(s storage, item string, opts ...core.ListOption) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	for _, opt := range opts {
		opt(todos)
	}
	todos.Add(item)
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// addSubtask adds a new subtask to an existing todo item and saves it. It accepts
// options to customize the list behavior (e.g., ID generation).
func addSubtask(s storage, parentID, item string, opts ...core.ListOption) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	for _, opt := range opts {
		opt(todos)
	}
	if err := todos.AddSubtask(parentID, item); err != nil {
		return fmt.Errorf("add subtask: %v", err)
	}
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// completeItem marks a todo item as completed by its ID. The ID can be a prefix
// of the full ID as long as it uniquely identifies an item.
func completeItem(s storage, id string) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Complete(id); err != nil {
		return fmt.Errorf("complete todo item: %v", err)
	}
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// uncompleteItem marks a completed todo item as incomplete by its ID. The ID can be
// a prefix of the full ID as long as it uniquely identifies an item.
func uncompleteItem(s storage, id string) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Uncomplete(id); err != nil {
		return fmt.Errorf("uncomplete todo item: %v", err)
	}
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// undoItem marks a todo item and its subtasks as not started by its ID. The ID can be
// a prefix of the full ID as long as it uniquely identifies an item.
func undoItem(s storage, id string) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Undo(id); err != nil {
		return fmt.Errorf("undo todo item: %v", err)
	}
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// removeItem removes a todo item from the list by its ID. The ID can be a
// prefix of the full ID as long as it uniquely identifies an item.
func removeItem(s storage, id string) error {
	todos, err := s.Load()
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Remove(id); err != nil {
		return fmt.Errorf("remove todo item: %v", err)
	}
	if err := s.Save(todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// moveItemUp moves a todo item up among its siblings by its ID. The ID can be a
// prefix of the full ID as long as it uniquely identifies an item.
func moveItemUp(s storage, id string) error {
	list, err := s.Load()
	if err != nil {
		return fmt.Errorf("load list: %v", err)
	}

	if err := list.MoveUp(id); err != nil {
		if err == core.ErrNoSibling {
			return fmt.Errorf("cannot move first item up")
		}
		if err == core.ErrItemNotFound {
			return fmt.Errorf("item not found: %s", id)
		}
		if err == core.ErrAmbiguousItem {
			return fmt.Errorf("ambiguous item ID: %s", id)
		}
		return fmt.Errorf("move item up: %v", err)
	}

	if err := s.Save(list); err != nil {
		return fmt.Errorf("save list: %v", err)
	}
	return nil
}

// moveItemDown moves a todo item down among its siblings by its ID. The ID can be a
// prefix of the full ID as long as it uniquely identifies an item.
func moveItemDown(s storage, id string) error {
	list, err := s.Load()
	if err != nil {
		return fmt.Errorf("load list: %v", err)
	}

	if err := list.MoveDown(id); err != nil {
		if err == core.ErrNoSibling {
			return fmt.Errorf("cannot move last item down")
		}
		if err == core.ErrItemNotFound {
			return fmt.Errorf("item not found: %s", id)
		}
		if err == core.ErrAmbiguousItem {
			return fmt.Errorf("ambiguous item ID: %s", id)
		}
		return fmt.Errorf("move item down: %v", err)
	}

	if err := s.Save(list); err != nil {
		return fmt.Errorf("save list: %v", err)
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		slog.Error("command failed", "error", err)
		os.Exit(1)
	}
}

func run() error {
	// Parse configuration first
	cfg, fs, err := config.ParseStorageConfig(os.Args[1:])
	if err != nil {
		printUsage()
		return fmt.Errorf("parse config: %v", err)
	}

	// Create parent directory if it doesn't exist
	if err := os.MkdirAll(filepath.Dir(cfg.Path), 0o755); err != nil {
		return fmt.Errorf("create storage directory: %v", err)
	}

	s, err := cfg.NewStorage()
	if err != nil {
		return fmt.Errorf("create storage: %v", err)
	}
	defer s.Close()

	// Get remaining args after flag parsing
	args := fs.Args()
	if len(args) == 0 {
		printUsage()
		return fmt.Errorf("no command provided")
	}

	switch args[0] {
	case "ls":
		if err := listItems(s); err != nil {
			return fmt.Errorf("list items: %v", err)
		}
	case "add":
		if len(args) != 2 {
			return fmt.Errorf("add command requires exactly one argument")
		}
		if err := addItem(s, args[1]); err != nil {
			return fmt.Errorf("add item: %v", err)
		}
	case "addsubtask":
		if len(args) != 3 {
			return fmt.Errorf("addsubtask command requires exactly two arguments")
		}
		if err := addSubtask(s, args[1], args[2]); err != nil {
			return fmt.Errorf("add subtask: %v", err)
		}
	case "complete":
		if len(args) != 2 {
			return fmt.Errorf("complete command requires exactly one argument")
		}
		if err := completeItem(s, args[1]); err != nil {
			return fmt.Errorf("complete item: %v", err)
		}
	case "uncomplete":
		if len(args) != 2 {
			return fmt.Errorf("uncomplete command requires exactly one argument")
		}
		if err := uncompleteItem(s, args[1]); err != nil {
			return fmt.Errorf("uncomplete item: %v", err)
		}
	case "undo":
		if len(args) != 2 {
			return fmt.Errorf("undo command requires exactly one argument")
		}
		if err := undoItem(s, args[1]); err != nil {
			return fmt.Errorf("undo item: %v", err)
		}
	case "remove":
		if len(args) != 2 {
			return fmt.Errorf("remove command requires exactly one argument")
		}
		if err := removeItem(s, args[1]); err != nil {
			return fmt.Errorf("remove item: %v", err)
		}
	case "moveup":
		if len(args) != 2 {
			return fmt.Errorf("moveup command requires exactly one argument")
		}
		if err := moveItemUp(s, args[1]); err != nil {
			return fmt.Errorf("move item up: %v", err)
		}
	case "movedown":
		if len(args) != 2 {
			return fmt.Errorf("movedown command requires exactly one argument")
		}
		if err := moveItemDown(s, args[1]); err != nil {
			return fmt.Errorf("move item down: %v", err)
		}
	default:
		printUsage()
		return fmt.Errorf("unknown command: %s", args[0])
	}
	return nil
}
