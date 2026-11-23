// Command agents-md-size checks per-file and cascading ancestor byte size
// limits for AGENTS.md files with support for standard baseline files.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/linters/agentsmdsize"
	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
)

type options struct {
	root              string
	baselinePath      string
	maxBytes          int64
	maxCascadingBytes int64
	updateBaseline    bool
	tightenBaseline   bool
	strictBaseline    bool
	showVersion       bool
}

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	opts, exitCode, done := parseFlags(args, stderr)
	if done {
		return exitCode
	}
	if opts.showVersion {
		fmt.Fprintf(stdout, "agents-md-size %s\n", pkgversion.Resolve())
		return 0
	}

	baselinePath := opts.baselinePath
	if baselinePath == "" {
		baselinePath = filepath.Join(opts.root, ".baselines", agentsmdsize.CheckerName+".json")
	}

	bf, err := baseline.Load(baselinePath)
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 2
	}
	if bf.Checker == "" {
		bf.Checker = agentsmdsize.CheckerName
	}

	report, err := agentsmdsize.CheckDir(opts.root, agentsmdsize.Config{
		MaxBytes:          opts.maxBytes,
		MaxCascadingBytes: opts.maxCascadingBytes,
		Baseline:          bf,
		StrictBaseline:    opts.strictBaseline,
	})
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 2
	}

	if opts.updateBaseline || opts.tightenBaseline {
		return handleBaselineUpdate(opts, baselinePath, report, stdout, stderr)
	}

	return handleCheckResult(opts.strictBaseline, report, stdout, stderr)
}

func parseFlags(args []string, stderr io.Writer) (options, int, bool) {
	var opts options
	fs := flag.NewFlagSet("agents-md-size", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.StringVar(&opts.root, "root", ".", "Repository root directory to scan")
	fs.StringVar(&opts.baselinePath, "baseline", "", "Path to baseline JSON file (default <root>/.baselines/agents-md-size.json)")
	fs.Int64Var(&opts.maxBytes, "max-bytes", agentsmdsize.DefaultMaxBytes, "Default maximum bytes per AGENTS.md file")
	fs.Int64Var(&opts.maxCascadingBytes, "max-cascading-bytes", agentsmdsize.DefaultMaxCascadingBytes, "Default maximum cascading ancestor bytes")
	fs.BoolVar(&opts.updateBaseline, "update-baseline", false, "Rewrite baseline to record new violations, remove stale entries, and tighten limits")
	fs.BoolVar(&opts.tightenBaseline, "tighten-baseline", false, "Rewrite baseline to only remove stale entries and tighten limits")
	fs.BoolVar(&opts.strictBaseline, "strict-baseline", true, "Fail when stale or ratchetable baseline entries exist")
	fs.BoolVar(&opts.showVersion, "version", false, "Print version information and exit")

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return opts, 0, true
		}
		return opts, 2, true
	}
	if opts.showVersion {
		return opts, 0, false
	}
	if opts.updateBaseline && opts.tightenBaseline {
		fmt.Fprintln(stderr, "Error: --update-baseline and --tighten-baseline are mutually exclusive")
		return opts, 2, true
	}
	if opts.maxBytes <= 0 || opts.maxCascadingBytes <= 0 {
		fmt.Fprintln(stderr, "Error: --max-bytes and --max-cascading-bytes must be positive")
		return opts, 2, true
	}
	return opts, 0, false
}

func handleBaselineUpdate(opts options, baselinePath string, report *agentsmdsize.Report, stdout, stderr io.Writer) int {
	mode := baseline.ModeUpdate
	if opts.tightenBaseline {
		mode = baseline.ModeTighten
	}
	updated := report.UpdatedBaseline(mode)
	if err := baseline.Save(baselinePath, updated); err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 2
	}
	fmt.Fprintf(stdout, "Updated baseline %s (%d entries, %d files checked)\n", baselinePath, len(updated.Entries), report.FilesChecked)
	if opts.tightenBaseline && len(report.Violations) > 0 {
		printViolations(stderr, report.Violations)
		return 1
	}
	return 0
}

func handleCheckResult(strict bool, report *agentsmdsize.Report, stdout, stderr io.Writer) int {
	if len(report.Violations) > 0 {
		printViolations(stderr, report.Violations)
	}
	if strict && len(report.StaleFindings) > 0 {
		printStaleFindings(stderr, report.StaleFindings)
	}
	if !report.Passed() {
		return 1
	}
	fmt.Fprintf(stdout, "agents-md-size: %d file(s) passed\n", report.FilesChecked)
	return 0
}

func printViolations(w io.Writer, violations []agentsmdsize.Violation) {
	for _, v := range violations {
		fmt.Fprintf(w, "%s: %s %d bytes exceeds limit %d bytes (default %d)\n",
			v.Path, v.Metric, v.Actual, v.Limit, v.Default)
	}
	fmt.Fprintln(w, "Run agents-md-size with --update-baseline to record or update baseline limits.")
}

func printStaleFindings(w io.Writer, findings []baseline.StaleFinding) {
	for _, f := range findings {
		if f.Metric != "" {
			fmt.Fprintf(w, "stale baseline (%s): %s [%s] current=%d suggested=%d\n",
				f.Kind, f.Key, f.Metric, f.Current, f.Suggested)
		} else {
			fmt.Fprintf(w, "stale baseline (%s): %s\n", f.Kind, f.Key)
		}
	}
	fmt.Fprintln(w, "Run agents-md-size with --tighten-baseline or --update-baseline to clean up stale entries.")
}
