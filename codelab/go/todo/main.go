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
func listItems(path string) error {
	todos, err := json_storage.Load(path)
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
func addItem(path, item string, opts ...core.ListOption) error {
	todos, err := json_storage.Load(path)
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	for _, opt := range opts {
		opt(todos)
	}
	todos.Add(item)
	if err := json_storage.Save(path, todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// completeItem marks a todo item as completed by its ID. The ID can be a prefix
// of the full ID as long as it uniquely identifies an item.
func completeItem(path, id string) error {
	todos, err := json_storage.Load(path)
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Complete(id); err != nil {
		return fmt.Errorf("complete todo item: %v", err)
	}
	if err := json_storage.Save(path, todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// uncompleteItem marks a completed todo item as incomplete by its ID. The ID can be
// a prefix of the full ID as long as it uniquely identifies an item.
func uncompleteItem(path, id string) error {
	todos, err := json_storage.Load(path)
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Uncomplete(id); err != nil {
		return fmt.Errorf("uncomplete todo item: %v", err)
	}
	if err := json_storage.Save(path, todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

// removeItem removes a todo item from the list by its ID. The ID can be a
// prefix of the full ID as long as it uniquely identifies an item.
func removeItem(path, id string) error {
	todos, err := json_storage.Load(path)
	if err != nil {
		return fmt.Errorf("load todo items: %v", err)
	}
	if err := todos.Remove(id); err != nil {
		return fmt.Errorf("remove todo item: %v", err)
	}
	if err := json_storage.Save(path, todos); err != nil {
		return fmt.Errorf("save todo items: %v", err)
	}
	return nil
}

func main() {
	flag.Parse()
	path := os.Getenv("HOME") + "/.todo/todos.json"

	var err error
	switch flag.Arg(0) {
	case "ls":
		err = listItems(path)
	case "add":
		if flag.NArg() != 2 {
			fmt.Println("Error: Missing todo item")
			os.Exit(1)
		}
		err = addItem(path, flag.Arg(1))
	case "complete":
		if flag.NArg() != 2 {
			printUsage()
			os.Exit(1)
		}
		err = completeItem(path, flag.Arg(1))
	case "uncomplete":
		if flag.NArg() != 2 {
			printUsage()
			os.Exit(1)
		}
		err = uncompleteItem(path, flag.Arg(1))
	case "remove":
		if flag.NArg() != 2 {
			fmt.Println("Error: Missing id")
			os.Exit(1)
		}
		err = removeItem(path, flag.Arg(1))
	default:
		printUsage()
		os.Exit(1)
	}

	if err != nil {
		slog.Error(
			"command failed",
			"command", flag.Arg(0),
			"error", err,
		)
		os.Exit(1)
	}
}
