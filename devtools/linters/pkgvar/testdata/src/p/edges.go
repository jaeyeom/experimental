package p

import (
	"os"

	pkg "example.com/other"
)

func bad() object {
	os.Getpid()
	return object{}
}

type box struct{ N int }

func (box) Pure() int { return 1 }

var boxes chan box

func wrapper() int { return bad().Pure() }

func setField() int {
	pt.X = 2
	return pt.X
}

func addrField() *int { return &pt.X }

var pint = new(int)

func setThrough() int {
	*pint = 2
	return *pint
}

func setOther() int {
	pkg.N = 2
	return pkg.N
}

func incOther() int {
	pkg.N++
	return pkg.N
}

func addrOther() *int { return &pkg.N }

func setElem() int {
	xs[0] = 2
	return xs[0]
}

type MyInt int

func (m *MyInt) Inc() { *m++ }

var mi = MyInt(1)

func useMI() int {
	mi.Inc()
	return int(mi)
}

func localInc() int {
	n := MyInt(1)
	n.Inc()
	return int(n)
}

var miPtr = new(MyInt)

func useMIPtr() int {
	miPtr.Inc()
	return int(*miPtr)
}

func localPtr() int {
	n := new(MyInt)
	n.Inc()
	return int(*n)
}

type Count int

func (c *Count) Inc() { *c++ }

var counter = Count(1)

var inc = counter.Inc

type valInt int

func (v valInt) Get() int { return int(v) }

var vi = valInt(1) // want "can be a constant"

func drain() int {
	sum := 0
	for v := range ch {
		sum += v
	}
	return sum
}

func seq(yield func(int) bool) {
	os.Getpid()
	yield(1)
}

func rangeSeq() int {
	sum := 0
	for v := range seq {
		sum += v
	}
	return sum
}

func rangeLit() int {
	sum := 0
	for v := range func(yield func(int) bool) {
		os.Getpid()
		yield(1)
	} {
		sum += v
	}
	return sum
}

func rangeSlice() int {
	sum := 0
	for _, v := range []int{1, 2} {
		sum += v
	}
	return sum
}

func rangeInt() int {
	sum := 0
	for i := range 3 {
		sum += i
	}
	return sum
}

func recA() int { return recB() + os.Getpid() }

func recB() int { return recA() }

func fact(n int) int {
	if n < 2 {
		return 1
	}
	return n * fact(n-1)
}

func id[T any](v T) T { return v }

func both[T, U any](v T, u U) T { return v }

type Box[T any] struct{ V T }

func (b Box[T]) Get() T { return b.V }

var fns = []func() int{func() int { return 1 }}

var (
	viaBad       = bad().Pure()                      // want "impure function or receives from a channel"
	viaBoxLit    = box{N: os.Getpid()}.Pure()        // want "impure function or receives from a channel"
	viaBoxIdx    = []box{{N: os.Getpid()}}[0].Pure() // want "impure function or receives from a channel"
	viaBoxChan   = (<-boxes).Pure()                  // want "impure function or receives from a channel"
	viaWrapper   = wrapper()                         // want "impure function or receives from a channel"
	viaField     = setField()                        // want "impure function or receives from a channel"
	viaFieldAddr = addrField()                       // want "impure function or receives from a channel"
	viaStar      = setThrough()                      // want "impure function or receives from a channel"
	viaOther     = setOther()                        // want "impure function or receives from a channel"
	viaInc       = incOther()                        // want "impure function or receives from a channel"
	viaPtr       = addrOther()                       // want "impure function or receives from a channel"
	viaElem      = setElem()                         // want "impure function or receives from a channel"
	viaMI        = useMI()                           // want "impure function or receives from a channel"
	viaMIPtr     = useMIPtr()                        // want "impure function or receives from a channel"
	viaLocal     = localInc()
	viaLocalPtr  = localPtr()
	viaDrain     = drain()    // want "impure function or receives from a channel"
	viaSeq       = rangeSeq() // want "impure function or receives from a channel"
	viaLit       = rangeLit() // want "impure function or receives from a channel"
	viaSlice     = rangeSlice()
	viaInts      = rangeInt()
	viaRecA      = recA() // want "impure function or receives from a channel"
	viaRecB      = recB() // want "impure function or receives from a channel"
	viaFact      = fact(3)
	viaID        = id(1)
	viaIDInst    = id[int](1)
	viaBoth      = both[int, string](1, "a")
	viaGet       = Box[int]{V: 1}.Get()
	indirect     = fns[0]() // want "impure function or receives from a channel"
	viaValue     = vi.Get()
)
