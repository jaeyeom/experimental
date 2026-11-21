// Command quotapace records usage readings of rolling quota windows and
// projects them against a plan.
//
//	quotapace record --profile P --window 7d --percent 27 --resets-in 6d5h
//	quotapace plan start --profile P --window 7d [--baseline 27 --at T]
//	quotapace status [--profile P] [--json]
//	quotapace report [--profile P] [--window W] [--json]
//	quotapace check [--profile P] [--stale 6h] [--repeat 6h] [--notify-cmd CMD]
//
// status, report, and check exit 0 green, 1 amber, 2 red, and 3 for a
// missing or stale reading. A usage error exits 64; any other error 70.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

const (
	exitUsage    = 64
	exitSoftware = 70
)

const usage = `usage: quotapace record --profile P --window W --percent N (--resets-in D | --resets-at T) [--source S] [--at T] [--auto-start]
       quotapace record --profile P [--format jsonl|claude-usage] [--auto-start] < input
       quotapace plan start --profile P --window W [--baseline N [--at T]] [--resets-at T | --resets-in D]
                            [--offset N] [--target N] [--amber N] [--every D] [--checkpoints HH:MM,...] [--tz Area/City]
       quotapace status [--profile P] [--json] [--recent 6h] [--stale 6h]
       quotapace report [--profile P] [--window W] [--json] [--recent 6h] [--stale 6h]
       quotapace check  [--profile P] [--stale 6h] [--repeat 6h] [--notify-cmd CMD] [--recent 6h]`

// errUsage marks an error that exits 64.
var errUsage = errors.New("usage")

type env struct {
	getenv func(string) string
	stdin  io.Reader
	stdout io.Writer
	stderr io.Writer
	now    func() time.Time
	notify func(argv []string, text string) error
}

func main() {
	os.Exit(run(os.Args[1:], env{
		getenv: os.Getenv, stdin: os.Stdin, stdout: os.Stdout, stderr: os.Stderr,
		now: time.Now, notify: runNotify,
	}))
}

func run(args []string, e env) int {
	if len(args) == 0 {
		fmt.Fprintln(e.stderr, usage)
		return exitUsage
	}
	if code, ok := newQuotapaceTool().Handle(context.Background(), args, e.stdout, e.stderr); ok {
		return code
	}
	dir, err := quotapace.DataDir(e.getenv)
	if err != nil {
		fmt.Fprintln(e.stderr, "quotapace:", err)
		return exitSoftware
	}
	store := quotapace.Store{Dir: dir}
	code, err := dispatch(args, store, e)
	switch {
	case errors.Is(err, errUsage):
		fmt.Fprintln(e.stderr, "quotapace:", err)
		return exitUsage
	case err != nil:
		fmt.Fprintln(e.stderr, "quotapace:", err)
		return exitSoftware
	}
	return code
}

func dispatch(args []string, store quotapace.Store, e env) (int, error) {
	switch args[0] {
	case "record":
		return 0, runRecord(args[1:], store, e)
	case "plan":
		if len(args) < 2 || args[1] != "start" {
			return 0, fmt.Errorf("%w: plan needs the start subcommand", errUsage)
		}
		return 0, runPlanStart(args[2:], store, e)
	case "status":
		return runStatus(args[1:], store, e, false)
	case "report":
		return runStatus(args[1:], store, e, true)
	case "check":
		return runCheck(args[1:], store, e)
	case "help", "-h", "--help":
		fmt.Fprintln(e.stdout, usage)
		return 0, nil
	default:
		return 0, fmt.Errorf("%w: unknown command %q\n%s", errUsage, args[0], usage)
	}
}

func newFlags(name string, e env) *flag.FlagSet {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.SetOutput(e.stderr)
	return fs
}

func parse(fs *flag.FlagSet, args []string) error {
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("%w: %s: %w", errUsage, fs.Name(), err)
	}
	if fs.NArg() != 0 {
		return fmt.Errorf("%w: %s: unexpected argument %q", errUsage, fs.Name(), fs.Arg(0))
	}
	return nil
}

func usageErr(format string, a ...any) error {
	return fmt.Errorf("%w: %s", errUsage, fmt.Sprintf(format, a...))
}

// parseTime reads RFC 3339; empty is the zero time.
func parseTime(name, value string) (time.Time, error) {
	if value == "" {
		return time.Time{}, nil
	}
	t, err := time.Parse(time.RFC3339, value)
	if err != nil {
		return time.Time{}, usageErr("--%s %q: want RFC 3339, such as 2026-11-20T09:00:00Z", name, value)
	}
	return t, nil
}

// resetTime is --resets-at, or now plus --resets-in. Both empty is zero.
func resetTime(resetsAt, resetsIn string, now time.Time) (time.Time, error) {
	if resetsAt != "" && resetsIn != "" {
		return time.Time{}, usageErr("pass --resets-at or --resets-in, not both")
	}
	if resetsIn != "" {
		d, err := quotapace.ParseDuration(resetsIn)
		if err != nil {
			return time.Time{}, usageErr("--resets-in: %v", err)
		}
		return now.Add(d), nil
	}
	return parseTime("resets-at", resetsAt)
}

func runNotify(argv []string, text string) error {
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	// The operator configured argv; it runs without a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: operator-supplied notify command
	cmd.Stdin = strings.NewReader(text)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("notify command: %w", err)
	}
	return nil
}
