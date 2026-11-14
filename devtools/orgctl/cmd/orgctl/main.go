// Command orgctl checks an organization spec.
//
//	orgctl check [--approval-word WORD]... SPEC.json
//
// --approval-word extends the default approval-word list. The spec's
// approvalWords field does the same. allowApprovalWords drops a default
// word and is printed as a warning.
package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stderr))
}

func run(args []string, stderr io.Writer) int {
	if len(args) == 0 || args[0] != "check" {
		fmt.Fprintln(stderr, "usage: orgctl check [--approval-word WORD]... SPEC.json")
		return 2
	}
	fs := flag.NewFlagSet("check", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var extra stringList
	fs.Var(&extra, "approval-word", "approval word added to the default list")
	if err := fs.Parse(args[1:]); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl check [--approval-word WORD]... SPEC.json")
		return 2
	}
	data, err := os.ReadFile(fs.Arg(0)) //nolint:gosec // G304: the operator named the spec file
	if err != nil {
		fmt.Fprintf(stderr, "read spec: %v\n", err)
		return 1
	}
	spec, err := orgctl.Decode(data)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	warnings, err := spec.CheckDecides([]string(extra))
	for _, warning := range warnings {
		fmt.Fprintln(stderr, warning)
	}
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

// stringList is a repeatable flag value.
type stringList []string

func (s *stringList) String() string { return strings.Join(*s, ",") }

func (s *stringList) Set(v string) error {
	*s = append(*s, v)
	return nil
}
