// Package allow is an analysistest fixture for the blankassign allowlist.
package allow

import (
	re "regexp"
)

func readPattern() string { return "a" }

// Quiet is an allowlisted package function with no parameters.
func Quiet() int { return 0 }

// T has a method with the same name as Quiet.
type T struct{}

// Quiet is a method, so the allowlist does not cover it.
func (T) Quiet() int { return 0 }

const pattern = "^[a-z]+$"

var patternVar = "^[a-z]+$"

var ch chan int

var (
	_ = re.MustCompile("^[a-z]+$")
	_ = re.MustCompile(pattern)
	_ = re.MustCompile("a" + "b")
	_ = re.MustCompile(readPattern()) // want "package-level blank assignment is effectful"
	_ = re.MustCompile(patternVar)    // want "package-level blank assignment is effectful"
	_ = []*re.Regexp{re.MustCompile(pattern)}
	_ = []any{re.MustCompile("^[a-z]+$"), <-ch} // want "package-level blank assignment is effectful"
	_ = Quiet()
	_ = T{}.Quiet() // want "package-level blank assignment is effectful"
)
