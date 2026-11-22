// Command org-lint-go lints org files without starting Emacs.
//
// It prints "file:line: checker: message" for each finding. This is the
// normalized format that org-lint-parity compares with Emacs org-lint, so
// ORG_LINT_NORMALIZED is accepted and ignored. Exit status is 1 if any
// finding was printed and 2 on errors.
package main

import (
	"fmt"
	"os"

	"github.com/jaeyeom/experimental/devtools/orglint"
)

func main() {
	found, err := orglint.Run(os.Stdout, os.Args[1:], orglint.Checkers())
	if err != nil {
		fmt.Fprintln(os.Stderr, "org-lint-go:", err)
		os.Exit(2)
	}
	if found {
		os.Exit(1)
	}
}
