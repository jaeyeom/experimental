package json_storage

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	todo "github.com/jaeyeom/experimental/codelab/go/todo/core"
)

func createParentDir(path string) error {
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return fmt.Errorf("os.MkdirAll: %v", err)
	}
	return nil
}

// Save saves list in JSON format to the file specified by path.
func Save(path string, list *todo.List) error {
	if err := createParentDir(path); err != nil {
		return fmt.Errorf("createParentDir: %v", err)
	}
	b, err := json.Marshal(list)
	if err != nil {
		return fmt.Errorf("json.Marshal: %v", err)
	}
	if err := ioutil.WriteFile(path, b, 0644); err != nil {
		return fmt.Errorf("ioutil.Write: %v", err)
	}
	return nil
}

func Load(path string) (*todo.List, error) {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return todo.NewList(), nil
		}
		return nil, fmt.Errorf("ioutil.ReadFile: %v", err)
	}
	var list todo.List
	if err := json.Unmarshal(b, &list); err != nil {
		return nil, fmt.Errorf("json.Unmarshal: %v", err)
	}
	return &list, nil
}
