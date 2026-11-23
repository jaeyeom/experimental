// Command migration-immutability fails when a change edits, deletes, or
// renames a database migration file that the base branch already has.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/linters/migrationimmutability"
	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
)

type listFlag []string

func (l *listFlag) String() string { return fmt.Sprint(*l) }

func (l *listFlag) Set(v string) error {
	*l = append(*l, v)
	return nil
}

type options struct {
	root           string
	base           string
	baselinePath   string
	patterns       listFlag
	disable        listFlag
	excludes       listFlag
	updateBaseline bool
	reason         string
	issue          string
	showVersion    bool
}

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, os.Getenv))
}

func run(args []string, stdout, stderr io.Writer, getenv func(string) string) int {
	opts, exitCode, done := parseFlags(args, stderr)
	if done {
		return exitCode
	}
	if opts.showVersion {
		fmt.Fprintf(stdout, "migration-immutability %s\n", pkgversion.Resolve())
		return 0
	}
	if err := runCheck(opts, stdout, stderr, getenv); err != nil {
		var blocked errBlocked
		if errors.As(err, &blocked) {
			return 1
		}
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 2
	}
	return 0
}

type errBlocked struct{}

func (errBlocked) Error() string { return "blocked" }

func parseFlags(args []string, stderr io.Writer) (options, int, bool) {
	var opts options
	fs := flag.NewFlagSet("migration-immutability", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.StringVar(&opts.root, "root", ".", "Any directory inside the repository to check")
	fs.StringVar(&opts.base, "base", "", "Base ref to diff against (default: origin/$GITHUB_BASE_REF, a differently named upstream, then origin/HEAD)")
	fs.StringVar(&opts.baselinePath, "baseline", "", "Path to baseline JSON file (default <toplevel>/.baselines/migration-immutability.json)")
	fs.Var(&opts.patterns, "pattern", "Extra migration pattern NAME=REGEXP (repeatable)")
	fs.Var(&opts.disable, "disable-pattern", "Disable a built-in pattern: django, alembic, sql, prisma (repeatable)")
	fs.Var(&opts.excludes, "exclude", "Glob of paths never treated as migrations, e.g. 'db/init/**' (repeatable)")
	fs.BoolVar(&opts.updateBaseline, "update-baseline", false, "Record every blocked change (except symlinks) as an allowed transition")
	fs.StringVar(&opts.reason, "reason", "", "Why the recorded changes are safe (required with --update-baseline)")
	fs.StringVar(&opts.issue, "issue", "", "Tracking reference for recorded changes (optional)")
	fs.BoolVar(&opts.showVersion, "version", false, "Print version information and exit")

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return opts, 0, true
		}
		return opts, 2, true
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(stderr, "Error: unexpected arguments %q\n", fs.Args())
		return opts, 2, true
	}
	if opts.updateBaseline && opts.reason == "" {
		fmt.Fprintln(stderr, "Error: --update-baseline requires --reason")
		return opts, 2, true
	}
	if !opts.updateBaseline && (opts.reason != "" || opts.issue != "") {
		fmt.Fprintln(stderr, "Error: --reason and --issue only apply with --update-baseline")
		return opts, 2, true
	}
	return opts, 0, false
}

func runCheck(opts options, stdout, stderr io.Writer, getenv func(string) string) error {
	var extra []migrationimmutability.Pattern
	for _, s := range opts.patterns {
		p, err := migrationimmutability.ParsePattern(s)
		if err != nil {
			return fmt.Errorf("--pattern: %w", err)
		}
		extra = append(extra, p)
	}
	patterns, err := migrationimmutability.SelectPatterns(extra, opts.disable)
	if err != nil {
		return fmt.Errorf("select patterns: %w", err)
	}
	top, err := migrationimmutability.TopLevel(opts.root)
	if err != nil {
		return fmt.Errorf("--root: %w", err)
	}
	baselinePath := opts.baselinePath
	if baselinePath == "" {
		baselinePath = filepath.Join(top, ".baselines", migrationimmutability.CheckerName+".json")
	}
	bf, err := baseline.Load(baselinePath)
	if err != nil {
		return fmt.Errorf("load baseline: %w", err)
	}
	if bf.Checker == "" {
		bf.Checker = migrationimmutability.CheckerName
	}

	rep, err := migrationimmutability.Check(migrationimmutability.Config{
		Root:     top,
		Base:     opts.base,
		Patterns: patterns,
		Excludes: opts.excludes,
		Baseline: bf,
		Getenv:   getenv,
	})
	if err != nil {
		return fmt.Errorf("check: %w", err)
	}

	if opts.updateBaseline {
		if err := rep.RecordTransitions(bf, opts.reason, opts.issue); err != nil {
			return fmt.Errorf("update baseline: %w", err)
		}
		if err := baseline.Save(baselinePath, bf); err != nil {
			return fmt.Errorf("save baseline: %w", err)
		}
		fmt.Fprintf(stdout, "Updated baseline %s\n", baselinePath)
		// Re-evaluate so the output and exit code reflect the saved baseline;
		// only symlinks can remain blocked.
		for i := range rep.Findings {
			f := &rep.Findings[i]
			if t, ok := bf.AllowsTransition(f.Path, f.Before, f.After); ok && !f.Symlink {
				f.Allowed = &t
			}
		}
	}
	return printReport(rep, stdout, stderr)
}

func printReport(rep *migrationimmutability.Report, stdout, stderr io.Writer) error {
	for _, f := range rep.Findings {
		if !f.IsBlocked() {
			fmt.Fprintf(stdout, "allowed: %s %s: baseline (%s)\n", f.Path, f.Status, f.Allowed.Reason)
		}
	}
	blocked := rep.Blocked()
	if len(blocked) == 0 {
		fmt.Fprintf(stdout, "PASSED: no migration file that %s already has was changed.\n", rep.BaseRef)
		return nil
	}
	fmt.Fprintf(stderr, "FAILED: disallowed migration change(s) against %s:\n\n", rep.BaseRef)
	symlinks, others := 0, 0
	for _, f := range blocked {
		what := f.Status
		if f.Symlink {
			what += ", and is a symlink"
			symlinks++
		} else {
			others++
		}
		fmt.Fprintf(stderr, "  %s [%s] %s\n", f.Path, f.Pattern, what)
	}
	if symlinks > 0 {
		fmt.Fprintln(stderr, "\nA migration must be a regular file: a symlink's target is not guarded and\n"+
			"could change what the migration runs. Symlinks cannot be allowed by the baseline.")
	}
	if others > 0 {
		fmt.Fprintln(stderr, "\nA merged migration may already be applied somewhere, and editing it does not\n"+
			"re-run it there. Revert the change and put it in a NEW migration.\n"+
			"For a genuine exception (an app retired with its migrations, a squash removing\n"+
			"the migrations it replaces, a formatter sweep, a migration that never ran\n"+
			"anywhere), run with --update-baseline --reason \"...\" and commit the result.")
	}
	return errBlocked{}
}
