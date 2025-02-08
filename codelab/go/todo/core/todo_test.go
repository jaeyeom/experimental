package todo

import "fmt"

func ExampleList_Add() {
	l := NewList()
	l.Add("buy groceries")
	l.Add("write code")
	fmt.Println(l)
	// Output:
	// 1. [ ] buy groceries
	// 2. [ ] write code
}

func ExampleList_Complete() {
	l := NewList()
	l.Add("buy groceries")
	l.Add("write code")
	l.Complete(1)
	fmt.Println(l)
	// Output:
	// 1. [x] buy groceries
	// 2. [ ] write code
}

func ExampleList_Remove() {
	l := NewList()
	l.Add("buy groceries")
	l.Add("write code")
	l.Remove(1)
	fmt.Println(l)
	// Output:
	// 2. [ ] write code
}
