// Command org-lint-go lints org files without starting Emacs.
//
// It prints "file:line: checker: message" for each finding. This is the
// normalized format that org-lint-parity compares with Emacs org-lint, so
// ORG_LINT_NORMALIZED is accepted and ignored. Exit status is 1 if any
// finding was printed and 2 on errors.
package main

import (
	"fmt"
	"io"
	"os"

	pkgversion "github.com/chorong-sh/chorong/pkg/version"
	"github.com/jaeyeom/experimental/devtools/orglint"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) == 1 && (args[0] == "--version" || args[0] == "-version") {
		fmt.Fprintln(stdout, pkgversion.Format("org-lint-go"))
		return 0
	}
	found, err := orglint.Run(stdout, args, orglint.Checkers())
	if err != nil {
		fmt.Fprintln(stderr, "org-lint-go:", err)
		return 2
	}
	if found {
		return 1
	}
	return 0
}
