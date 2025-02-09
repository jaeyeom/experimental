// Binary todo is a command line todo application that helps users manage their
// tasks. It supports basic operations like listing, adding, completing, and
// removing todo items.
//
// Usage:
//
//	todo ls           List todo items
//	todo add "item"   Add a new todo item
//	todo complete id  Complete a todo item
//	todo remove id    Remove a todo item
//
// Items are stored in JSON format in the user's home directory.
package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	todo "github.com/jaeyeom/experimental/codelab/go/todo/core"
	json_storage "github.com/jaeyeom/experimental/codelab/go/todo/storage/json"
)

// printUsage prints the command usage information to stdout.
func printUsage() {
	fmt.Println(`Usage:
	todo ls			List todo items
	todo add "todo item"	Add a new todo item
	todo complete <id>	Complete a todo item
	todo remove <id>	Remove a todo item`)
}

// listItems loads and displays all todo items. If there are no items, it prints
// a message indicating an empty list.
func listItems(path string) {
	todos, err := json_storage.Load(path)
	if err != nil {
		log.Fatal("Error:", err)
	}
	if len(todos.Items) == 0 {
		fmt.Println("You have no todo items.")
		return
	}
	fmt.Println(todos)
}

// addItem adds a new todo item to the list and saves it. It accepts options to
// customize the list behavior (e.g., ID generation).
func addItem(path, item string, opts ...todo.ListOption) {
	todos, err := json_storage.Load(path)
	if err != nil {
		log.Fatal("Error:", err)
	}
	for _, opt := range opts {
		opt(todos)
	}
	todos.Add(item)
	if err := json_storage.Save(path, todos); err != nil {
		log.Fatal("Error:", err)
	}
}

// completeItem marks a todo item as completed by its ID. The ID can be a prefix
// of the full ID as long as it uniquely identifies an item.
func completeItem(path, id string) {
	todos, err := json_storage.Load(path)
	if err != nil {
		log.Fatal("Error:", err)
	}
	if err := todos.Complete(id); err != nil {
		log.Fatal("Error:", err)
	}
	if err := json_storage.Save(path, todos); err != nil {
		log.Fatal("Error:", err)
	}
}

// removeItem removes a todo item from the list by its ID. The ID can be a
// prefix of the full ID as long as it uniquely identifies an item.
func removeItem(path, id string) {
	todos, err := json_storage.Load(path)
	if err != nil {
		log.Fatal("Error:", err)
	}
	if err := todos.Remove(id); err != nil {
		log.Fatal("Error:", err)
	}
	if err := json_storage.Save(path, todos); err != nil {
		log.Fatal("Error:", err)
	}
}

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		printUsage()
		os.Exit(1)
	}
	path := os.Getenv("HOME") + "/.todo/todos.json"

	switch args[0] {
	case "ls":
		listItems(path)
	case "add":
		if len(args) == 1 {
			fmt.Println("Error: Missing todo item")
			os.Exit(1)
		}
		addItem(path, args[1])
	case "complete":
		if len(args) != 2 {
			fmt.Println("Error: Missing id")
			os.Exit(1)
		}
		completeItem(path, args[1])
	case "remove":
		if len(args) != 2 {
			fmt.Println("Error: Missing id")
			os.Exit(1)
		}
		removeItem(path, args[1])
	default:
		printUsage()
	}
}
