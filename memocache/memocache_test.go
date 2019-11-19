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

func ExampleMap_callsOncePerKey() {
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

func ExampleMap_differentKeysNotBlocked() {
	// This example shows that different keys are not blocked. Key "b" and
	// "c" starts after the function for key "a" is called run processing.
	// But key "a" waits until key "b" finishes. If key "b" is blocked while
	// the value of "a" is being evaluated, there will be a deadlock, which
	// doesn't happen in this example. The order in the output is always the
	// same.
	var m Map

	started := map[string]chan struct{}{
		"":  make(chan struct{}),
		"a": make(chan struct{}),
		"b": make(chan struct{}),
		"c": make(chan struct{}),
	}
	done := []chan struct{}{make(chan struct{}), make(chan struct{}), make(chan struct{}), make(chan struct{})}
	ops := []struct {
		key        string
		startAfter chan struct{}
		waitUntil  chan struct{}
	}{
		{key: "a", startAfter: started[""], waitUntil: done[2]},
		{key: "a", startAfter: started[""], waitUntil: done[2]},
		{key: "b", startAfter: started["a"], waitUntil: done[3]},
		{key: "c", startAfter: started["b"], waitUntil: started[""]},
	}

	par.Do(
		func() {
			par.For(len(ops), func(i int) {
				op := ops[i]
				<-op.startAfter
				m.LoadOrCall(op.key, func() interface{} {
					close(started[op.key])
					<-op.waitUntil
					fmt.Printf("key %q was looked up\n", op.key)
					return op.key
				})
				close(done[i])
			})
		},
		func() {
			close(started[""])
		},
	)
	// Output:
	// key "c" was looked up
	// key "b" was looked up
	// key "a" was looked up
}

func ExampleMultiLevelMap() {
	var m MultiLevelMap

	names := []string{"John", "Mary", "Linda", "Oscar"}
	gender := []string{"m", "f", "f", "m"}
	lookup := func(id int, category string) string {
		return m.LoadOrCall(func() interface{} {
			return names[id]
		}, category, id).(string)
	}
	lookupAll := func(header string) {
		fmt.Println(header)

		for i := 0; i < len(names); i++ {
			fmt.Println(lookup(i, gender[i]))
		}
	}

	lookupAll("== First Calls ==")

	fmt.Println("Now we change all names upper case in the backend")

	for i := 0; i < len(names); i++ {
		names[i] = strings.ToUpper(names[i])
	}

	lookupAll("== Call again (All cache hit/Nothing should change) ==")

	fmt.Println("Prune males")
	m.Prune("m")

	lookupAll("== Call again (All males should change) ==")

	fmt.Println("Prune Linda only")
	m.Prune("f", 2)

	lookupAll("== Call again ==")
	// Output:
	// == First Calls ==
	// John
	// Mary
	// Linda
	// Oscar
	// Now we change all names upper case in the backend
	// == Call again (All cache hit/Nothing should change) ==
	// John
	// Mary
	// Linda
	// Oscar
	// Prune males
	// == Call again (All males should change) ==
	// JOHN
	// Mary
	// Linda
	// OSCAR
	// Prune Linda only
	// == Call again ==
	// JOHN
	// Mary
	// LINDA
	// OSCAR
}

func ExampleMultiLevelMap_differentKeysNotBlocked() {
	// This example shows that different keys are not blocked even if they
	// share the same prefix path. Key "b" and "c" starts after the function
	// for key "a" is called run processing. But key "a" waits until key "b"
	// finishes. If key "b" is blocked while the value of "a" is being
	// evaluated, there will be a deadlock, which doesn't happen in this
	// example. The order in the output is always the same.
	var m MultiLevelMap

	started := map[string]chan struct{}{
		"":  make(chan struct{}),
		"a": make(chan struct{}),
		"b": make(chan struct{}),
		"c": make(chan struct{}),
	}
	done := []chan struct{}{make(chan struct{}), make(chan struct{}), make(chan struct{}), make(chan struct{})}
	ops := []struct {
		key        string
		startAfter chan struct{}
		waitUntil  chan struct{}
	}{
		{key: "a", startAfter: started[""], waitUntil: done[2]},
		{key: "a", startAfter: started[""], waitUntil: done[2]},
		{key: "b", startAfter: started["a"], waitUntil: done[3]},
		{key: "c", startAfter: started["b"], waitUntil: started[""]},
	}

	par.Do(
		func() {
			par.For(len(ops), func(i int) {
				op := ops[i]
				<-op.startAfter
				m.LoadOrCall(func() interface{} {
					close(started[op.key])
					<-op.waitUntil
					fmt.Printf("key %q was looked up\n", op.key)
					return op.key
				}, "common", "path", "and", op.key)
				close(done[i])
			})
		},
		func() {
			close(started[""])
		},
	)
	// Output:
	// key "c" was looked up
	// key "b" was looked up
	// key "a" was looked up
}
