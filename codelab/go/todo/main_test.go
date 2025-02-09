package main

import (
	"log"
	"os"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
)

func Example_listItems() {
	// Get a temporary directory
	dir, err := os.MkdirTemp("", "todo")
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	listItems(path)
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
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	opt := core.WithNewID(ig)
	addItem(path, "buy groceries", opt)
	addItem(path, "write code", opt)
	listItems(path)
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
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	opt := core.WithNewID(ig)
	addItem(path, "buy groceries", opt)
	addItem(path, "write code", opt)
	completeItem(path, "1")
	listItems(path)
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
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	opt := core.WithNewID(ig)
	addItem(path, "buy groceries", opt)
	addItem(path, "write code", opt)
	removeItem(path, "1")
	listItems(path)
	// Output:
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}
