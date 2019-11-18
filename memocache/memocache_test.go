package memocache

import (
	"fmt"
	"strings"
	"sync/atomic"

	"github.com/jaeyeom/sugo/par"
)

func ExampleMap() {
	var m Map

	fmt.Println(m.LoadOrCall(1, func() interface{} { return "one" }))
	fmt.Println(m.LoadOrCall("two", func() interface{} { return 2 }))
	fmt.Println(m.LoadOrCall(1, func() interface{} { return "not one" }))
	fmt.Println(m.LoadOrCall("two", func() interface{} { return 20 }))
	m.Delete(1)
	fmt.Println(m.LoadOrCall(1, func() interface{} { return "maybe not one" }))
	fmt.Println(m.LoadOrCall("two", func() interface{} { return 200 }))
	// Output:
	// one
	// 2
	// one
	// 2
	// maybe not one
	// 2
}

func ExampleMap_callsOnce() {
	var m Map

	keys := []string{
		"abc",
		"ab",
		"abc",
		"88",
		"abc",
		"abc",
		"88",
		"abc",
		"abc",
		"abc",
	}
	res := make([]string, len(keys))

	var numCalls int32

	par.For(len(keys), func(i int) {
		key := keys[i]
		res[i] = m.LoadOrCall(key, func() interface{} {
			atomic.AddInt32(&numCalls, 1)
			return strings.ToUpper(key)
		}).(string)
	})

	fmt.Printf("Number of calls: %d\n", numCalls)

	for i := 0; i < len(keys); i++ {
		fmt.Printf("%q => %q\n", keys[i], res[i])
	}
	// Output:
	// Number of calls: 3
	// "abc" => "ABC"
	// "ab" => "AB"
	// "abc" => "ABC"
	// "88" => "88"
	// "abc" => "ABC"
	// "abc" => "ABC"
	// "88" => "88"
	// "abc" => "ABC"
	// "abc" => "ABC"
	// "abc" => "ABC"
}
