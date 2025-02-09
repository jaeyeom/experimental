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
// Items are stored in JSON format in the user's home directory.
package main

import (
	"flag"
	"fmt"
	"log/slog"
	"os"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	json_storage "github.com/jaeyeom/experimental/codelab/go/todo/storage/json"
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
	todo remove <id>	Remove a todo item`)
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
	flag.Parse()
	path := os.Getenv("HOME") + "/.todo/todos.json"

	s, err := json_storage.New(path)
	if err != nil {
		slog.Error("failed to create storage", "error", err)
		os.Exit(1)
	}
	defer s.Close()

	switch flag.Arg(0) {
	case "ls":
		err = listItems(s)
	case "add":
		if flag.NArg() != 2 {
			fmt.Println("Error: Missing todo item")
			os.Exit(1)
		}
		err = addItem(s, flag.Arg(1))
	case "complete":
		if flag.NArg() != 2 {
			printUsage()
			os.Exit(1)
		}
		err = completeItem(s, flag.Arg(1))
	case "uncomplete":
		if flag.NArg() != 2 {
			printUsage()
			os.Exit(1)
		}
		err = uncompleteItem(s, flag.Arg(1))
	case "remove":
		if flag.NArg() != 2 {
			fmt.Println("Error: Missing id")
			os.Exit(1)
		}
		err = removeItem(s, flag.Arg(1))
	default:
		printUsage()
		os.Exit(1)
	}

	if err != nil {
		slog.Error("command failed",
			"command", flag.Arg(0),
			"error", err,
		)
		os.Exit(1)
	}
}
