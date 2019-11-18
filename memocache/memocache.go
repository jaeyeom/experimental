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
