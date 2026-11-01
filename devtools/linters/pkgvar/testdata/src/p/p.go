// Package p is an analysistest fixture for pkgvar.
//
// p.go is the base fixture. It covers an unexported constant, a discarded
// result, a direct impure call, a channel receive, and the quiet cases in
// the block at the bottom of this file. edges.go covers the cases that sit
// behind a callee expression, a selector or pointer, a call argument, a
// range, or an instantiation. alias.go covers an allowlisted function
// imported under another name. All three files are package p, and
// analysistest loads them together. A declaration in this file is visible
// to edges.go.
package p

import (
	"bytes"
	"errors"
	"fmt"
	"net/http"
	"os"
	"regexp"
	"time"

	pkg "example.com/other"
)

const c = 1

type Point struct{ X, Y int }

type handler struct{}

func (handler) ServeHTTP(http.ResponseWriter, *http.Request) {}

type object struct{}

func (object) Method() int { return os.Getpid() }

func (object) Pure() int { return 1 }

func start() int { return os.Getpid() }

func startPair() (int, int) { return os.Getpid(), 1 }

func dial() int { return pkg.F() }

func load() string { return os.Getenv("LOAD") }

func readPattern() string { return os.Getenv("PAT") }

func purePattern() string { return "^[a-z]+$" }

func two() int { return 2 }

func build() *regexp.Regexp { return regexp.MustCompile("^[a-z]+$") }

func buildImpure() string { return os.Getenv("A") }

func callsPanic() int { panic("x") }

func callsPrint() int {
	print("x")
	return 1
}

func callsPrintln() int {
	println("x")
	return 1
}

func wrap(int) int { return 0 }

func returnsClosure() func() { return func() { os.Getenv("Z") } }

var sink int

func assignsSink() int {
	sink = 1
	return sink
}

func takesAddr() int {
	p := &sink
	return *p
}

func startsGoroutine() int {
	go func() {}()
	return 1
}

func defers() int {
	defer func() {}()
	return 1
}

func sends() int {
	ch <- 1
	return 0
}

var ch chan int

func inside() {
	// The type keeps this a declaration. gofumpt rewrites an untyped
	// var _ = f() inside a function into an assignment.
	var _ int = start()
}

func setAssigned() { assigned = 2 }

func addrAddressed() *int { return &addressed }

var assigned = 1

var addressed = 1

var (
	x            = 1                                 // want "can be a constant"
	s            = "hello"                           // want "can be a constant"
	n            = len("abc")                        // want "can be a constant"
	sum          = 1 + 2                             // want "can be a constant"
	fromConst    = c                                 // want "can be a constant"
	clients      = dial()                            // want "impure function or receives from a channel"
	env          = os.Getenv("K")                    // want "impure function or receives from a channel"
	now          = time.Now()                        // want "impure function or receives from a channel"
	_            = start()                           // want "impure function or receives from a channel"
	_, _         = startPair()                       // want "impure function or receives from a channel"
	_            = errors.New("x")                   // want "discards its result"
	_            = errors.New(load())                // want "impure function or receives from a channel"
	reBad        = regexp.MustCompile(readPattern()) // want "impure function or receives from a channel"
	_            = pkg.F()                           // want "impure function or receives from a channel"
	methodResult = object{}.Method()                 // want "impure function or receives from a channel"
	panicked     = callsPanic()                      // want "impure function or receives from a channel"
	printed      = callsPrint()                      // want "impure function or receives from a channel"
	printlined   = callsPrintln()                    // want "impure function or receives from a channel"
	viaArg       = wrap(func() int {                 // want "impure function or receives from a channel"
		panic("x")
		return 0
	}())
	loaded      = []string{os.Getenv("A")}            // want "impure function or receives from a channel"
	_           = <-ch                                // want "impure function or receives from a channel"
	tableBad    = buildImpure()                       // want "impure function or receives from a channel"
	_           = new(int)                            // want "discards its result"
	ExportedEnv = os.Getenv("EXP")                    // want "impure function or receives from a channel"
	blockX      = 1                                   // want "can be a constant"
	viaAssign   = assignsSink()                       // want "impure function or receives from a channel"
	viaAddr     = takesAddr()                         // want "impure function or receives from a channel"
	viaGo       = startsGoroutine()                   // want "impure function or receives from a channel"
	viaDefer    = defers()                            // want "impure function or receives from a channel"
	viaSend     = sends()                             // want "impure function or receives from a channel"
	invoked     = func() int { return os.Getpid() }() // want "impure function or receives from a channel"
)

// These initializers stay quiet. The comment on each line is the reason.
var (
	X                       = 1                                 // Exported. Another package can assign to it.
	Version                 = "dev"                             // Exported string. -ldflags -X requires a variable.
	_                       = 1                                 // The blank name is not a package-level variable.
	xs                      = []int{1, 2, 3}                    // A slice composite is not a Go constant.
	m                       = map[string]int{"a": 1}            // A map composite is not a Go constant.
	arr                     = [2]int{1, 2}                      // An array composite is not a Go constant.
	pt                      = Point{X: 1, Y: 2}                 // A struct composite is not a Go constant.
	ErrX                    = errors.New("x")                   // Allowlisted, the argument is pure, and the name keeps the result.
	msg                     = fmt.Errorf("bad %s", "x")         // Allowlisted, both arguments are pure, and the name keeps the result.
	re                      = regexp.MustCompile("^[a-z]+$")    // Allowlisted, the pattern is pure, and the name keeps the result.
	rePure                  = regexp.MustCompile(purePattern()) // purePattern is pure, so the allowlisted call stays pure.
	buf                     = new(bytes.Buffer)                 // new is a pure builtin, and the name keeps the result.
	made                    = make([]int, 0)                    // make is a pure builtin, and the name keeps the result.
	table                   = build()                           // build only calls allowlisted MustCompile with a pure argument.
	pureMethod              = object{}.Pure()                   // The method body is pure. Methods are judged by their body, not the allowlist.
	lo                      = min(two(), 3)                     // min is a pure builtin, and two is a pure same-package function.
	closure                 = returnsClosure()                  // The returned function literal is not called, so os.Getenv inside it does not run.
	unused                  = func() int { return os.Getpid() } // The function literal is not called, so os.Getpid does not count.
	_          http.Handler = (*handler)(nil)                   // A conversion is not a call. Interface assertions belong to the other linter.
	_          http.Handler = handler{}                         // A composite used as an interface value is not a call.
)
