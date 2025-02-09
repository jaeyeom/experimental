package main

import (
	"fmt"
	"os"

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
