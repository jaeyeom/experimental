package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
)

func cmdCatalog(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool catalog validate [path-to-repo-or-catalog.json]")
		return 2
	}
	switch args[0] {
	case "validate":
		return cmdCatalogValidate(args[1:], stdout, stderr)
	case "-h", "--help", "help":
		fmt.Fprintln(stdout, "Usage: agent-pool catalog validate [path-to-repo-or-catalog.json]")
		return 0
	default:
		fmt.Fprintf(stderr, "agent-pool catalog: unknown subcommand %q (expected validate)\n", args[0])
		return 2
	}
}

func cmdCatalogValidate(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("catalog validate", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() > 1 {
		fmt.Fprintln(stderr, "Usage: agent-pool catalog validate [path-to-repo-or-catalog.json]")
		return 2
	}

	target := "."
	if fs.NArg() == 1 {
		target = fs.Arg(0)
	}

	cat, err := validateCatalogTarget(target)
	if err != nil {
		fmt.Fprintf(stderr, "catalog validation failed: %v\n", err)
		return 1
	}

	fmt.Fprintf(stdout, "catalog valid (%d skill(s)): %s\n", len(cat.Skills), target)
	return 0
}

func validateCatalogTarget(target string) (*catalog.Catalog, error) {
	info, err := os.Stat(target)
	if err != nil {
		return nil, fmt.Errorf("stat %q: %w", target, err)
	}
	if info.IsDir() {
		cat, err := catalog.LoadFromDir(target)
		if err != nil {
			return nil, fmt.Errorf("load catalog from repo %q: %w", target, err)
		}
		return cat, nil
	}
	data, err := os.ReadFile(target)
	if err != nil {
		return nil, fmt.Errorf("read catalog file %q: %w", target, err)
	}
	cat, err := catalog.Parse(data)
	if err != nil {
		return nil, fmt.Errorf("parse catalog %q: %w", target, err)
	}
	return cat, nil
}
