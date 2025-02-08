// Binary todo is a command line todo tool.
package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	todo "github.com/jaeyeom/experimental/codelab/go/todo/core"
	json_storage "github.com/jaeyeom/experimental/codelab/go/todo/storage/json"
)

func printUsage() {
	fmt.Println(`Usage:
	todo ls			List todo items
	todo add "todo item"	Add a new todo item
	todo complete <id>	Complete a todo item
	todo remove <id>	Remove a todo item`)
}

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
