package json_storage

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
)

func ExampleSave() {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	list := core.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	fmt.Println(Save(path, list))
	// Output:
	// <nil>
}

func ExampleLoad() {
	ig := coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todo.json"
	list := core.NewList(core.WithNewID(ig))
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
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func TestSave_createDirectory(t *testing.T) {
	// Get a temporary directory
	dir, err := ioutil.TempDir("", "todo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	path := dir + "/todosub/todo.json"
	list := core.NewList()
	list.Add("buy groceries")
	list.Add("write code")
	if err := Save(path, list); err != nil {
		t.Fatal(err)
	}
}
