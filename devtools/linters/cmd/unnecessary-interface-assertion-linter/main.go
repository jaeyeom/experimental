// Command unnecessary-interface-assertion-linter reports compile-time
// interface assertions that are unnecessary.
package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/linters/unnecessaryinterfaceassertion"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("unnecessary-interface-assertion-linter", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "emit issues as JSON")
	dir := fs.String("dir", "", "directory to load packages from")
	fs.Usage = func() {
		fmt.Fprintf(stderr, `unnecessary-interface-assertion-linter - flag unused var _ Iface = (*T)(nil) assertions

USAGE:
    unnecessary-interface-assertion-linter [OPTIONS] [packages...]

OPTIONS:
    -h, --help    Show this help message
    --json        Emit issues as JSON
    --dir DIR     Directory to load packages from

If no packages are given, ./... is used.
`)
	}
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}

	paths := fs.Args()
	if len(paths) == 0 {
		paths = []string{"./..."}
	}

	linter := unnecessaryinterfaceassertion.New()
	linter.Dir = *dir
	issues, err := linter.Lint(paths)
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	if err := writeIssues(stdout, issues, *jsonOut); err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	if len(issues) > 0 {
		return 1
	}
	return 0
}

func writeIssues(w io.Writer, issues []unnecessaryinterfaceassertion.Issue, asJSON bool) error {
	if issues == nil {
		issues = []unnecessaryinterfaceassertion.Issue{}
	}
	if asJSON {
		enc := json.NewEncoder(w)
		enc.SetIndent("", "  ")
		if err := enc.Encode(issues); err != nil {
			return fmt.Errorf("encode issues: %w", err)
		}
		return nil
	}
	for _, issue := range issues {
		if _, err := fmt.Fprintf(w, "%s:%d:%d: %s\n", issue.FilePath, issue.Line, issue.Column, issue.Message); err != nil {
			return fmt.Errorf("write issue: %w", err)
		}
	}
	return nil
}
