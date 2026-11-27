// Command quotapace records usage readings of rolling quota windows and
// projects them against a plan.
//
//	quotapace
//	quotapace dashboard [--profile P] [--recent D] [--stale D] [--watch D]
//	quotapace collect claude [--json | --jsonl] [--profile P [--auto-start]]
//	quotapace record --profile P --window 7d --percent 27 --resets-in 6d5h
//	quotapace plan start --profile P --window 7d [--baseline 27 --at T]
//	quotapace plan set --profile P --window 7d [--reset "Thu 15:00"] [--tz Area/City] --rate R [--offset O]
//	quotapace status [--profile P] [--json]
//	quotapace report [--profile P] [--window W] [--json]
//	quotapace check [--profile P] [--stale 6h] [--repeat 6h] [--notify-cmd CMD]
//	quotapace gate --profile P --window 7d --min-gap N [--stale D] [--json]
//
// status, report, and check exit 0 green, 1 amber, 2 red, and 3 for a
// missing or stale reading. gate exits 0 open, 1 below min-gap, and 3 when
// unreadable (fail closed). collect claude exits 2 when no usage data is
// available (non-subscription login, custom ANTHROPIC_BASE_URL, or expired
// credentials). A usage error exits 64; any other error 70.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"strings"
	"time"
	_ "time/tzdata"

	"github.com/jaeyeom/experimental/devtools/quotapace"
	"github.com/jaeyeom/experimental/devtools/quotapace/internal/collect/claude"
)

const (
	exitUsage    = 64
	exitSoftware = 70
)

const usage = `usage: quotapace [dashboard] [--profile P] [--window W] [--recent 6h] [--stale 6h] [--watch D]
       quotapace collect claude [--json | --jsonl] [--profile P [--auto-start]]
       quotapace record --profile P --window W --percent N (--resets-in D | --resets-at T) [--source S] [--at T] [--auto-start]
       quotapace record --profile P [--stdin] [--format jsonl|claude-usage] [--auto-start] < input
       quotapace plan start --profile P --window W [--baseline N [--at T]] [--resets-at T | --resets-in D]
                            [--offset N] [--target N] [--amber N] [--every D] [--checkpoints HH:MM,...] [--tz Area/City]
       quotapace plan set --profile P --window W [--reset "Thu 15:00"] [--tz Area/City] [--rate R] [--offset O] [--amber N]
       quotapace status [--profile P] [--json] [--recent 6h] [--stale 6h]
       quotapace report [--profile P] [--window W] [--json] [--recent 6h] [--stale 6h]
       quotapace check  [--profile P] [--stale 6h] [--repeat 6h] [--notify-cmd CMD] [--recent 6h]
       quotapace gate   --profile P [--window 7d] [--min-gap N] [--stale 6h] [--json] [--watch --every 1m --notify-cmd CMD]`

// errUsage marks an error that exits 64.
var errUsage = errors.New("usage")

type env struct {
	getenv        func(string) string
	stdin         io.Reader
	stdout        io.Writer
	stderr        io.Writer
	now           func() time.Time
	notify        func(argv []string, text string) error
	ctx           context.Context
	after         func(time.Duration) <-chan time.Time
	collectClaude func() claude.Fetcher
}

func main() {
	os.Exit(runMain())
}

func runMain() int {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()
	return run(os.Args[1:], env{
		getenv: os.Getenv, stdin: os.Stdin, stdout: os.Stdout, stderr: os.Stderr,
		now: time.Now, notify: runNotify, ctx: ctx, after: time.After,
	})
}

func run(args []string, e env) int {
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
	if len(args) == 0 {
		return runDashboard(nil, store, e)
	}
	switch args[0] {
	case "dashboard":
		return runDashboard(args[1:], store, e)
	case "collect":
		return dispatchCollect(args[1:], store, e)
	case "record":
		return 0, runRecord(args[1:], store, e)
	case "plan":
		return 0, dispatchPlan(args[1:], store, e)
	case "status":
		return runStatus(args[1:], store, e, false)
	case "report":
		return runStatus(args[1:], store, e, true)
	case "check":
		return runCheck(args[1:], store, e)
	case "gate":
		return runGate(args[1:], store, e)
	case "help", "-h", "--help":
		fmt.Fprintln(e.stdout, usage)
		return 0, nil
	default:
		return 0, fmt.Errorf("%w: unknown command %q\n%s", errUsage, args[0], usage)
	}
}

func dispatchCollect(args []string, store quotapace.Store, e env) (int, error) {
	if len(args) < 1 {
		return 0, fmt.Errorf("%w: collect needs a collector name (such as claude)", errUsage)
	}
	switch args[0] {
	case "claude":
		return runCollectClaude(args[1:], store, e)
	default:
		return 0, fmt.Errorf("%w: unknown collector %q (want claude)", errUsage, args[0])
	}
}

func dispatchPlan(args []string, store quotapace.Store, e env) error {
	if len(args) < 1 {
		return fmt.Errorf("%w: plan needs start or set subcommand", errUsage)
	}
	switch args[0] {
	case "start":
		return runPlanStart(args[1:], store, e)
	case "set":
		return runPlanSet(args[1:], store, e)
	default:
		return fmt.Errorf("%w: unknown plan subcommand %q (want start or set)", errUsage, args[0])
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
