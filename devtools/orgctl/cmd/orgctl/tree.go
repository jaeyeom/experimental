package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

const treeUsage = "orgctl tree [--root ROLE] [--root-label TEXT] [--depth N] [--live] [--json] [--ascii] SPEC.json"

// runTree prints the org chart. It reads live state only with --live.
// Exit 2 is a usage error, including an unknown --root; 1 is any other
// error, including a reportsTo cycle.
func runTree(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	fs := flag.NewFlagSet("tree", flag.ContinueOnError)
	fs.SetOutput(stderr)
	root := fs.String("root", "", "start the chart at this role")
	rootLabel := fs.String("root-label", "user", "label of the operator line")
	depth := fs.Int("depth", -1, "stop below this depth; the root is 0")
	withLive := fs.Bool("live", false, "annotate roles from the live snapshot")
	asJSON := fs.Bool("json", false, "print a nested JSON structure")
	ascii := fs.Bool("ascii", false, "draw with ASCII characters")
	if err := fs.Parse(args[1:]); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: "+treeUsage)
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
	if *root != "" {
		if _, ok := spec.Roles[*root]; !ok {
			fmt.Fprintf(stderr, "orgctl tree: --root: unknown role %q\n", *root)
			return 2
		}
	}
	opts := orgctl.TreeOptions{RootLabel: *rootLabel, Root: *root, Depth: *depth}
	if *withLive {
		if load == nil {
			load = loadLive
		}
		live, err := load(spec)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		opts.Live = &live
	}
	node, err := orgctl.BuildTree(spec, opts)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if !*asJSON {
		fmt.Fprint(stdout, orgctl.RenderTree(node, *ascii))
		return 0
	}
	enc := json.NewEncoder(stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(node); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}
