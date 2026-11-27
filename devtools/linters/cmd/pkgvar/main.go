// Command pkgvar reports package-level variables that should be constants
// and package-level initializers that do work at import time.
package main

import (
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/linters/pkgvar"
	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
	"golang.org/x/tools/go/analysis/singlechecker"
)

func main() {
	if handleVersion(os.Args[1:], os.Stdout) {
		return
	}
	pkgvar.RegisterFlags()
	singlechecker.Main(pkgvar.Analyzer)
}

func handleVersion(args []string, stdout io.Writer) bool {
	if len(args) == 1 && (args[0] == "--version" || args[0] == "-version") {
		fmt.Fprintln(stdout, pkgversion.Format("pkgvar"))
		return true
	}
	return false
}
