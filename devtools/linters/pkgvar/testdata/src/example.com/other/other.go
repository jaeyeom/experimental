// Package other is imported through an alias by the pkgvar fixture.
package other

// F is a package function used to test aliased calls.
func F() int { return 1 }

// N is a package variable used to test cross-package mutation.
var N int
