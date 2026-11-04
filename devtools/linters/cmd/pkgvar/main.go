// Command pkgvar reports package-level variables that should be constants
// and package-level initializers that do work at import time.
package main

import (
	"github.com/jaeyeom/experimental/devtools/linters/pkgvar"
	"golang.org/x/tools/go/analysis/singlechecker"
)

func main() {
	pkgvar.RegisterFlags()
	singlechecker.Main(pkgvar.Analyzer)
}
