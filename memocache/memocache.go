package memocache

import "sync"

// Map is a kind of key value cache map but it is safe for concurrent use by
// multiple goroutines. It can avoid multiple duplicate function calls
// associated with the same key. When the cache is missing, the given function
// is used to compute or fetch the value for the key. Subsequent calls to the
// same key waits until the function returns, but calls to a different key are
// not blocked. Map should not be copied after first use.
type Map struct {
	m sync.Map
}

// LoadOrStore gets pre-cached value associated with the given key or calls
// getValue to get the value for the key. The function getValue is called only
// once for the given key. Even if different getValue is given for the same key,
// only one function is called.
func (m *Map) LoadOrCall(key interface{}, getValue func() interface{}) interface{} {
	e, _ := m.m.LoadOrStore(key, &Value{})
	return e.(*Value).LoadOrStore(getValue)
}

// Delete deletes the cache value for the key. Prior LoadOrCall() with the same
// key won't be affected by the delete calls. Later LoadOrCall() with the same
// key will have to call getValue, since the cache is cleared for the key.
func (m *Map) Delete(key interface{}) {
	m.m.Delete(key)
}

// Value is a single value that is initialized once by calling the given
// function only once. Value should not be copied after first use.
type Value struct {
	once  sync.Once
	value interface{}
}

// LoadOrStore gets the value. If the value isn't ready it calls f to get the
// value.
func (e *Value) LoadOrStore(getValue func() interface{}) interface{} {
	e.once.Do(func() {
		e.value = getValue()
	})

	return e.value
}

// MultiLevelMap is an expansion of a Map that can manage tree like structure.
// It's possible to prune a subtree. MultiLevelMap should not be copied after
// first use.
type MultiLevelMap struct {
	v Value
}

func findLeafNode(root *Map, path ...interface{}) *Map {
	if len(path) == 0 {
		return root
	}

	newRoot := root.LoadOrCall(path[0], func() interface{} {
		return &Map{}
	}).(*Map)

	return findLeafNode(newRoot, path[1:]...)
}

func (m *MultiLevelMap) getRoot() *Map {
	return m.v.LoadOrStore(func() interface{} {
		return &Map{}
	}).(*Map)
}

func (m *MultiLevelMap) LoadOrCall(getValue func() interface{}, path ...interface{}) interface{} {
	n := len(path)
	if n == 0 {
		panic("path was not given")
	}

	return findLeafNode(m.getRoot(), path[:n-1]...).LoadOrCall(path[n-1], getValue)
}

func (m *MultiLevelMap) Prune(path ...interface{}) {
	n := len(path)
	if n == 0 {
		panic("pruning the whole tree is not supported yet")
	}

	findLeafNode(m.getRoot(), path[:n-1]...).Delete(path[n-1])
}
