// Package necessary is a fixture with an interface used beyond a compile-time
// assertion.
package necessary

import "fmt"

type UsedInterface interface {
	DoSomethingElse()
}

type UsedType struct{}

func (t *UsedType) DoSomethingElse() {
	fmt.Println("Hello")
}

var _ UsedInterface = (*UsedType)(nil)

func UseTheInterface(i UsedInterface) {
	i.DoSomethingElse()
}
