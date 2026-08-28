// Command devcheck detects repository context, runs format/lint/test tools, and
// reports results for humans and AI agents.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/detector"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/runner"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	os.Exit(realMain())
}

func realMain() int {
	signalExec := executor.NewWithSignalHandling()
	ctx, err := signalExec.Start()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return 1
	}
	defer signalExec.Stop()
	return run(ctx, os.Args[1:], os.Stdout, os.Stderr, signalExec)
}

func run(ctx context.Context, args []string, stdout, stderr io.Writer, exec executor.Executor) int {
	opts, err := parseArgs(args, stderr)
	if err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}

	dir, err := resolveDir(opts.path)
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}

	project, err := detector.NewProjectDetector().Detect(dir)
	if err != nil {
		fmt.Fprintf(stderr, "Error: failed to detect project configuration: %v\n", err)
		return 1
	}

	report, err := runner.Run(ctx, project, opts.options, exec)
	if err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	if err := runner.Write(stdout, opts.options.OutputFormat, report); err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	if report.Failed() {
		return 1
	}
	return 0
}

type cliOptions struct {
	options runner.Options
	path    string
}

func parseArgs(args []string, stderr io.Writer) (cliOptions, error) {
	fs := flag.NewFlagSet("devcheck", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() {
		fmt.Fprintf(stderr, "Usage: devcheck [OPTIONS] [PATH]\n\n")
		fmt.Fprintf(stderr, "Detect the repository context and run format, lint, and test tools.\n\n")
		fmt.Fprintf(stderr, "Options:\n")
		fs.PrintDefaults()
	}

	var opts cliOptions
	var format string
	var filters filterFlag
	fs.BoolVar(&opts.options.DryRun, "dry-run", false, "Show what would be done without executing")
	fs.BoolVar(&opts.options.DryRun, "n", false, "Show what would be done without executing")
	fs.BoolVar(&opts.options.Verbose, "verbose", false, "Verbose output for debugging")
	fs.BoolVar(&opts.options.Verbose, "v", false, "Verbose output for debugging")
	fs.StringVar(&format, "format", string(runner.FormatPrompt), "Output format (prompt, summary, json)")
	fs.Var(&filters, "filter", "Run only specific tool types (format, lint, test); repeatable or comma-separated")
	fs.BoolVar(&opts.options.ChangedOnly, "changed-only", false, "Run only on changed files (requires git)")
	fs.BoolVar(&opts.options.ForceFallback, "force-fallback", false, "Skip Bazel/Make and use language-specific tools")

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return cliOptions{}, flag.ErrHelp
		}
		return cliOptions{}, fmt.Errorf("parse flags: %w", err)
	}

	outputFormat, err := parseOutputFormat(format)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return cliOptions{}, err
	}
	opts.options.OutputFormat = outputFormat
	opts.options.Filters = []config.ToolType(filters)
	if fs.NArg() > 1 {
		err := fmt.Errorf("unexpected extra arguments: %s", strings.Join(fs.Args()[1:], " "))
		fmt.Fprintln(stderr, err)
		return cliOptions{}, err
	}
	if fs.NArg() == 1 {
		opts.path = fs.Arg(0)
	}
	return opts, nil
}

func parseOutputFormat(raw string) (runner.OutputFormat, error) {
	switch runner.OutputFormat(raw) {
	case runner.FormatPrompt, runner.FormatSummary, runner.FormatJSON:
		return runner.OutputFormat(raw), nil
	default:
		return "", fmt.Errorf("invalid format %q (want prompt, summary, or json)", raw)
	}
}

func resolveDir(path string) (string, error) {
	if path == "" {
		if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
			path = wd
		} else {
			wd, err := os.Getwd()
			if err != nil {
				return "", fmt.Errorf("get current directory: %w", err)
			}
			path = wd
		}
	}
	abs, err := filepath.Abs(path)
	if err != nil {
		return "", fmt.Errorf("resolve path: %w", err)
	}
	return abs, nil
}

type filterFlag []config.ToolType

func (f *filterFlag) String() string {
	parts := make([]string, len(*f))
	for i, toolType := range *f {
		parts[i] = string(toolType)
	}
	return strings.Join(parts, ",")
}

func (f *filterFlag) Set(value string) error {
	for _, part := range strings.Split(value, ",") {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		toolType, err := parseToolType(part)
		if err != nil {
			return err
		}
		*f = append(*f, toolType)
	}
	return nil
}

func parseToolType(raw string) (config.ToolType, error) {
	switch config.ToolType(raw) {
	case config.ToolTypeFormat, config.ToolTypeLint, config.ToolTypeTest:
		return config.ToolType(raw), nil
	default:
		return "", fmt.Errorf("invalid filter %q (want format, lint, or test)", raw)
	}
}
