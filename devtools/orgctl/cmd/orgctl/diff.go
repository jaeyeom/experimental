package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

const diffUsage = "orgctl diff [--full] [--json] OLD.json NEW.json"

// runDiff prints the changes between two rendered specs. Like
// git diff --exit-code it exits 0 with no difference, 1 with one, and 2
// on bad input or usage.
func runDiff(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("diff", flag.ContinueOnError)
	fs.SetOutput(stderr)
	full := fs.Bool("full", false, "show every word-diff hunk of a long text")
	asJSON := fs.Bool("json", false, "print the changes as a JSON list")
	if err := fs.Parse(args[1:]); err != nil {
		return 2
	}
	if fs.NArg() != 2 {
		fmt.Fprintln(stderr, "usage: "+diffUsage)
		return 2
	}
	var specs [2]orgctl.Spec
	for i, path := range fs.Args() {
		data, err := os.ReadFile(path) //nolint:gosec // G304: the operator named the spec file
		if err != nil {
			fmt.Fprintf(stderr, "read spec: %v\n", err)
			return 2
		}
		if specs[i], err = orgctl.Decode(data); err != nil {
			fmt.Fprintf(stderr, "%s: %v\n", path, err)
			return 2
		}
	}
	changes := orgctl.DiffSpecs(specs[0], specs[1])
	if *asJSON {
		if changes == nil {
			changes = []orgctl.Change{}
		}
		enc := json.NewEncoder(stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(changes); err != nil {
			fmt.Fprintln(stderr, err)
			return 2
		}
	} else {
		for _, c := range changes {
			for _, line := range c.Lines(*full) {
				fmt.Fprintln(stdout, line)
			}
		}
	}
	if len(changes) > 0 {
		return 1
	}
	return 0
}
