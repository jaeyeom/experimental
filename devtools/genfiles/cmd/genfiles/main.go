// Command genfiles checks a repository's checked-in list of generated files
// (generated_files.yaml) against its tracked files and .gitattributes.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
)

const (
	defaultList = "generated_files.yaml"
	usage       = `Usage:
  genfiles check [--list FILE] [--root DIR]
  genfiles --version
`
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprint(stderr, usage)
		return 2
	}
	switch args[0] {
	case "--version", "-version", "version":
		fmt.Fprintf(stdout, "genfiles %s\n", pkgversion.Resolve())
		return 0
	case "-h", "--help", "help":
		fmt.Fprint(stdout, usage)
		return 0
	case "check":
		return runCheck(args[1:], stdout, stderr)
	}
	fmt.Fprintf(stderr, "Error: unknown command %q\n%s", args[0], usage)
	return 2
}

func runCheck(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("genfiles check", flag.ContinueOnError)
	fs.SetOutput(stderr)
	listPath := fs.String("list", defaultList, "List file; a relative path is resolved from the repository root")
	root := fs.String("root", ".", "Any directory inside the repository to check")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(stderr, "Error: unexpected arguments %q\n", fs.Args())
		return 2
	}
	top, err := git(*root, "rev-parse", "--show-toplevel")
	if err != nil {
		fmt.Fprintf(stderr, "Error: --root: %v\n", err)
		return 2
	}
	top = strings.TrimSpace(top)
	path := *listPath
	if !filepath.IsAbs(path) {
		path = filepath.Join(top, path)
	}
	list, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(stderr, "Error: read list: %v\n", err)
		return 2
	}
	attrs, err := os.ReadFile(filepath.Join(top, ".gitattributes"))
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		fmt.Fprintf(stderr, "Error: read .gitattributes: %v\n", err)
		return 2
	}
	out, err := git(top, "ls-files", "-z")
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 2
	}
	tracked := strings.Split(strings.TrimSuffix(out, "\x00"), "\x00")
	problems, patterns, matched := checkRepo(list, string(attrs), tracked)
	if len(problems) > 0 {
		for _, p := range problems {
			fmt.Fprintf(stderr, "%s: %s\n", *listPath, p)
		}
		return 1
	}
	fmt.Fprintf(stdout, "genfiles check: PASSED (%d patterns, %d files)\n", patterns, matched)
	return 0
}

func git(dir string, args ...string) (string, error) {
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	var stderr strings.Builder
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("git %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return string(out), nil
}
