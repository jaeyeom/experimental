// Command org-lint-parity compares a Go org linter with Emacs org-lint.
//
// It is run by hand (make org-lint-parity) and never from make check, because
// it needs Emacs. Both linters are started as "CMD files..." with
// ORG_LINT_NORMALIZED=1 and must print "file:line: checker: message".
//
// Usage, from the repository root:
//
//	org-lint-parity -emacs-lint ./org-lint                # survey + golden drift check
//	org-lint-parity -emacs-lint ./org-lint -update        # regenerate fixture goldens
//	org-lint-parity -emacs-lint ./org-lint -go-lint CMD   # compare against the Go linter
//
// Without file arguments it covers every .org file known to git and every
// fixture in the fixtures directory.
package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"github.com/jaeyeom/experimental/devtools/linters/orglintparity"
)

func main() {
	emacsLint := flag.String("emacs-lint", "org-lint", "Emacs-based org-lint command")
	goLint := flag.String("go-lint", "", "Go org linter command to compare with Emacs")
	fixtures := flag.String("fixtures", "devtools/linters/testdata/orglint", "fixture directory")
	update := flag.Bool("update", false, "regenerate fixture goldens from Emacs")
	flag.Parse()
	if err := run(options{*emacsLint, *goLint, *fixtures, *update}, flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, "org-lint-parity:", err)
		os.Exit(1)
	}
}

type options struct {
	emacsLint, goLint, fixtureDir string
	update                        bool
}

func run(o options, files []string) error {
	failed, err := checkFixtures(o)
	if err != nil {
		return err
	}
	if len(files) == 0 {
		if files, err = gitOrgFiles(); err != nil {
			return err
		}
	}
	want, err := lint(o.emacsLint, ".", files...)
	if err != nil {
		return err
	}
	if o.goLint == "" {
		survey(want, len(files))
	} else {
		got, err := lint(o.goLint, ".", files...)
		if err != nil {
			return err
		}
		failed = report("repository", want, got) || failed
	}
	if failed {
		return errors.New("differences found")
	}
	return nil
}

// checkFixtures runs from the fixture directory so findings carry the base
// name. One process per linter handles all fixtures: Emacs startup is slow.
func checkFixtures(o options) (failed bool, err error) {
	fixtures, err := filepath.Glob(filepath.Join(o.fixtureDir, "*.orgtest"))
	if err != nil {
		return false, fmt.Errorf("list fixtures: %w", err)
	}
	if len(fixtures) == 0 {
		return false, nil
	}
	names := make([]string, len(fixtures))
	for i, fx := range fixtures {
		names[i] = filepath.Base(fx)
	}
	wantAll, err := lint(o.emacsLint, o.fixtureDir, names...)
	if err != nil {
		return false, err
	}
	gotAll := wantAll
	if o.goLint != "" {
		if gotAll, err = lint(o.goLint, o.fixtureDir, names...); err != nil {
			return false, err
		}
	}
	for _, fx := range fixtures {
		name := filepath.Base(fx)
		differs, err := checkFixture(o, fx, forFile(wantAll, name), forFile(gotAll, name))
		if err != nil {
			return false, err
		}
		failed = failed || differs
	}
	return failed, nil
}

func checkFixture(o options, fixture string, want, got []orglintparity.Finding) (bool, error) {
	golden := orglintparity.GoldenPath(fixture)
	switch {
	case o.update:
		if err := os.WriteFile(golden, []byte(orglintparity.Format(want)), 0o644); err != nil { //nolint:gosec // checked-in golden
			return false, fmt.Errorf("write golden: %w", err)
		}
		fmt.Printf("updated %s (%d findings)\n", golden, len(want))
		return false, nil
	case o.goLint != "":
		return report(fixture, want, got), nil
	}
	data, err := os.ReadFile(golden)
	if err != nil {
		return false, fmt.Errorf("read golden: %w", err)
	}
	old, err := orglintparity.Parse(bytes.NewReader(data))
	if err != nil {
		return false, fmt.Errorf("%s: %w", golden, err)
	}
	return report(fixture+" (golden vs Emacs)", want, old), nil
}

func forFile(fs []orglintparity.Finding, name string) []orglintparity.Finding {
	var out []orglintparity.Finding
	for _, f := range fs {
		if f.File == name {
			out = append(out, f)
		}
	}
	return out
}

func report(name string, want, got []orglintparity.Finding) bool {
	r := orglintparity.Diff(want, got)
	if r.Empty() {
		fmt.Printf("ok   %s (%d findings)\n", name, len(want))
		return false
	}
	fmt.Printf("DIFF %s\n%s", name, r)
	return true
}

func survey(fs []orglintparity.Finding, nfiles int) {
	counts := map[string]int{}
	for _, f := range fs {
		counts[f.Checker]++
	}
	names := make([]string, 0, len(counts))
	for n := range counts {
		names = append(names, n)
	}
	sort.Strings(names)
	fmt.Printf("survey: %d findings in %d files\n", len(fs), nfiles)
	for _, n := range names {
		fmt.Printf("%6d %s\n", counts[n], n)
	}
}

func gitOrgFiles() ([]string, error) {
	out, err := exec.Command("git", "ls-files", "*.org").Output()
	if err != nil {
		return nil, fmt.Errorf("git ls-files: %w", err)
	}
	var files []string
	for _, f := range strings.Fields(string(out)) {
		if !strings.Contains("/"+f, "/testdata/") { // fixtures are covered separately
			files = append(files, f)
		}
	}
	return files, nil
}

// lint runs a linter command in dir and parses its normalized output. A
// non-zero exit is expected when there are findings, so only unparsable
// output or a failure to start is an error.
func lint(command, dir string, files ...string) ([]orglintparity.Finding, error) {
	argv := strings.Fields(command)
	if len(argv) == 0 {
		return nil, errors.New("empty linter command")
	}
	if strings.ContainsRune(argv[0], filepath.Separator) {
		abs, err := filepath.Abs(argv[0])
		if err != nil {
			return nil, fmt.Errorf("resolve %s: %w", argv[0], err)
		}
		argv[0] = abs
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(argv[0], append(argv[1:], files...)...) //nolint:gosec // developer-run tool
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), "ORG_LINT_NORMALIZED=1")
	var stdout, stderr bytes.Buffer
	cmd.Stdout, cmd.Stderr = &stdout, &stderr
	if err := cmd.Run(); err != nil {
		var ee *exec.ExitError
		if !errors.As(err, &ee) {
			return nil, fmt.Errorf("%s: %w", command, err)
		}
	}
	fs, err := orglintparity.Parse(&stdout)
	if err != nil {
		return nil, fmt.Errorf("%s: %w (stderr: %s)", command, err, stderr.String())
	}
	return fs, nil
}
