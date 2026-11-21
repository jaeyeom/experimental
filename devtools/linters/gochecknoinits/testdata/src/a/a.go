package a

// S is a sample struct.
type S struct{}

func (S) init() {} // ok: method named init is allowed

func helper() {} // ok: regular function

func init() {} // want "don't use init function"
