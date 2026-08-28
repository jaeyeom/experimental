// Package unnecessary is a fixture with a single-implementation interface
// used only in a compile-time assertion.
package unnecessary

type UnusedInterface interface {
	DoSomething()
}

type UnusedType struct{}

func (t *UnusedType) DoSomething() {}

var _ UnusedInterface = (*UnusedType)(nil)
