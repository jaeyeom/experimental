package json_storage

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"

	todo "github.com/jaeyeom/experimental/codelab/go/todo/core"
)

func ExampleSave() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	list := todo.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	fmt.Println(Save(path, list))
	// Output:
	// <nil>
}

func ExampleLoad() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	list := todo.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	if err := Save(path, list); err != nil {
		panic(err)
	}
	loaded, err := Load(path)
	if err != nil {
		panic(err)
	}
	fmt.Println(loaded)
	// Output:
	// 1. [ ] buy groceries
	// 2. [ ] write code
}

func TestSave_createDirectory(t *testing.T) {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todosub/todo.json"
	list := todo.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	if err := Save(path, list); err != nil {
		t.Fatal(err)
	}
}
