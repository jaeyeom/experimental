// Command blankassign reports package-level blank assignments whose
// initializers are effectful.
package main

import (
	"github.com/jaeyeom/experimental/devtools/linters/blankassign"
	"golang.org/x/tools/go/analysis/singlechecker"
)

func main() {
	singlechecker.Main(blankassign.Analyzer)
}
