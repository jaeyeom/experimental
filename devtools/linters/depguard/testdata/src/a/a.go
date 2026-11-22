// Package a is a depguard fixture.
package a

import (
	"fmt"
	"io/ioutil" // want `import 'io/ioutil' is not allowed from list 'Base'`
	"log"       // want `import 'log' is not allowed from list 'Base'`
)

// Use references the imports.
func Use() {
	fmt.Println(ioutil.Discard)
	log.Println("x")
}
