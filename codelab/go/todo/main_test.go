package main

import (
	"io/ioutil"
	"log"
	"os"
)

func Example_listItems() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
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
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	addItem(path, "buy groceries")
	addItem(path, "write code")
	listItems(path)
	// Output:
	// 1. [ ] buy groceries
	// 2. [ ] write code
}

func Example_completeItem() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	addItem(path, "buy groceries")
	addItem(path, "write code")
	completeItem(path, "1")
	listItems(path)
	// Output:
	// 1. [x] buy groceries
	// 2. [ ] write code
}

func Example_removeItem() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	addItem(path, "buy groceries")
	addItem(path, "write code")
	removeItem(path, "1")
	listItems(path)
	// Output:
	// 2. [ ] write code
}
