// Package blank is an analysistest fixture for blankassign.
package blank

import (
	"net/http"
	"os"
	"regexp"

	pkg "example.com/other"
)

func f() int { return 1 }

func pair() (int, int) { return 1, 2 }

func wrap(int) int { return 0 }

type T struct{}

func (T) M() int { return 1 }

type handler struct{}

func (handler) ServeHTTP(http.ResponseWriter, *http.Request) {}

var ch chan int

var someIdent = 1

func inside() {
	// The type keeps this a declaration. gofumpt rewrites an untyped
	// var _ = f() inside a function into an assignment.
	var _ int = f()
}

var (
	_    = f()                            // want "package-level blank assignment is effectful"
	_, _ = pair()                         // want "package-level blank assignment is effectful"
	_    = pkg.F()                        // want "package-level blank assignment is effectful"
	_    = T{}.M()                        // want "package-level blank assignment is effectful"
	_    = new(int)                       // want "package-level blank assignment is effectful"
	_    = make(chan int)                 // want "package-level blank assignment is effectful"
	_    = <-ch                           // want "package-level blank assignment is effectful"
	_    = wrap(f())                      // want "package-level blank assignment is effectful"
	_    = int(<-ch)                      // want "package-level blank assignment is effectful"
	_    = []int{f()}                     // want "package-level blank assignment is effectful"
	_    = regexp.MustCompile("^[a-z]+$") // want "package-level blank assignment is effectful"
)

// The call of the literal runs during initialization.
var _ = func() int { return f() }() // want "package-level blank assignment is effectful"

// The call inside this literal does not run during initialization.
var _ = func() int { return f() }

var (
	_      http.Handler = (*handler)(nil)
	_      http.Handler = handler{}
	_                   = handler{}
	_                   = someIdent
	_                   = 1
	named               = f()
	x, _                = pair()
	_, err              = os.Open("missing")
)

var (
	_ = f() // want "package-level blank assignment is effectful"
	_ = 1
)
