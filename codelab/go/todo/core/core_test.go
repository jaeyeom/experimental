package core

import (
	"fmt"

	"github.com/jaeyeom/experimental/codelab/go/todo/core/coretest"
)

func ExampleList_Add() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	fmt.Println(l)
	// Output:
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code

}

func ExampleList_Complete() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("1"); err != nil {
		panic(err)
	}
	fmt.Println(l)
	// Output:
	// 11111111-1111-1111-1111-111111111111. [x] buy groceries
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Complete_ambiguos() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"12222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Complete("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// item ambiguous
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 12222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Remove() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"22222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	l.Remove("1")
	fmt.Println(l)
	// Output:
	// 22222222-2222-2222-2222-222222222222. [ ] write code
}

func ExampleList_Remove_ambiguous() {
	l := NewList(WithNewID(coretest.NewIDGen(
		"11111111-1111-1111-1111-111111111111",
		"12222222-2222-2222-2222-222222222222",
	)))
	l.Add("buy groceries")
	l.Add("write code")
	if err := l.Remove("1"); err != nil {
		fmt.Println(err)
	}
	fmt.Println(l)
	// Output:
	// item ambiguous
	// 11111111-1111-1111-1111-111111111111. [ ] buy groceries
	// 12222222-2222-2222-2222-222222222222. [ ] write code
}
