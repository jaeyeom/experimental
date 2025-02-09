// Binary todo is a command line todo application that helps users manage their
// tasks. It supports basic operations like listing, adding, completing, and
// removing todo items.
//
// Usage:
//
//	todo ls           List todo items
//	todo add "item"   Add a new todo item
//	todo complete id  Complete a todo item
//	todo uncomplete id  Mark a completed item as incomplete
//	todo remove id    Remove a todo item
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
	todo complete <id>	Complete a todo item
	todo uncomplete <id>	Mark a completed item as incomplete
	todo remove <id>	Remove a todo item

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
	if err := os.MkdirAll(filepath.Dir(cfg.Path), 0755); err != nil {
		return fmt.Errorf("create storage directory: %v", err)
	}

	storage, err := cfg.NewStorage()
	if err != nil {
		return fmt.Errorf("create storage: %v", err)
	}
	defer storage.Close()

	// Get remaining args after flag parsing
	args := fs.Args()
	if len(args) == 0 {
		printUsage()
		return fmt.Errorf("no command provided")
	}

	switch args[0] {
	case "ls":
		return listItems(storage)
	case "add":
		if len(args) != 2 {
			return fmt.Errorf("usage: %s add <description>", os.Args[0])
		}
		return addItem(storage, args[1])
	case "complete":
		if len(args) != 2 {
			return fmt.Errorf("usage: %s complete <id>", os.Args[0])
		}
		return completeItem(storage, args[1])
	case "uncomplete":
		if len(args) != 2 {
			return fmt.Errorf("usage: %s uncomplete <id>", os.Args[0])
		}
		return uncompleteItem(storage, args[1])
	case "remove":
		if len(args) != 2 {
			return fmt.Errorf("usage: %s remove <id>", os.Args[0])
		}
		return removeItem(storage, args[1])
	default:
		printUsage()
		return fmt.Errorf("unknown command: %s", args[0])
	}
}
