// Command goimports formats Go import declarations without invoking the host
// go toolchain (FormatOnly mode). It implements the -l and -w flags expected
// by aspect_rules_lint's Go formatter wrapper.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"os"

	"golang.org/x/tools/imports"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("goimports", flag.ContinueOnError)
	fs.SetOutput(stderr)
	list := fs.Bool("l", false, "list files whose formatting differs from goimports's")
	write := fs.Bool("w", false, "write result to (source) file instead of stdout")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if !*list && !*write {
		fmt.Fprintln(stderr, "goimports: either -l or -w must be specified")
		return 2
	}

	exitCode := 0
	for _, path := range fs.Args() {
		if err := processFile(path, *list, *write, stdout); err != nil {
			fmt.Fprintf(stderr, "%s: %v\n", path, err)
			exitCode = 1
		}
	}
	return exitCode
}

func processFile(path string, list, write bool, stdout io.Writer) error {
	info, err := os.Stat(path)
	if err != nil {
		return fmt.Errorf("stat file: %w", err)
	}
	src, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read file: %w", err)
	}
	res, err := imports.Process(path, src, &imports.Options{
		Comments:   true,
		TabIndent:  true,
		TabWidth:   8,
		FormatOnly: true,
	})
	if err != nil {
		return fmt.Errorf("format imports: %w", err)
	}
	if bytes.Equal(src, res) {
		return nil
	}
	if list {
		if _, err := fmt.Fprintln(stdout, path); err != nil {
			return fmt.Errorf("write filename: %w", err)
		}
	}
	if write {
		if err := os.WriteFile(path, res, info.Mode().Perm()); err != nil {
			return fmt.Errorf("write file: %w", err)
		}
	}
	return nil
}
