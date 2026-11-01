// Package p is an analysistest fixture for pkgvar.
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

var (
	X                       = 1
	Version                 = "dev"
	_                       = 1
	xs                      = []int{1, 2, 3}
	m                       = map[string]int{"a": 1}
	arr                     = [2]int{1, 2}
	pt                      = Point{X: 1, Y: 2}
	ErrX                    = errors.New("x")
	msg                     = fmt.Errorf("bad %s", "x")
	re                      = regexp.MustCompile("^[a-z]+$")
	rePure                  = regexp.MustCompile(purePattern())
	buf                     = new(bytes.Buffer)
	made                    = make([]int, 0)
	table                   = build()
	pureMethod              = object{}.Pure()
	lo                      = min(two(), 3)
	closure                 = returnsClosure()
	unused                  = func() int { return os.Getpid() }
	_          http.Handler = (*handler)(nil)
	_          http.Handler = handler{}
)
