// Command agentbus is the client for the local agentbus daemon.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

func main() {
	os.Exit(runProcess())
}

func runProcess() int {
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	return run(ctx, os.Args[1:], os.Environ(), os.Stdin, os.Stdout, os.Stderr)
}

// Tests replace these so help can prove it never touches the daemon.
var (
	ensureDaemon = ensure
	dialDaemon   = client.Call
)

func run(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	if len(args) > 0 && isHelpFlag(args[0]) {
		printUsage(stdout)
		return 0
	}
	if len(args) > 0 && args[0] == "help" {
		return cmdHelp(args[1:], stdout, stderr)
	}
	var cmd command
	if len(args) > 0 {
		var ok bool
		if cmd, ok = findCommand(args[0]); !ok {
			return unknownCommand(args[0], stderr)
		}
		// Only the first argument counts, so a body word such as --help is still sent.
		if len(args) > 1 && isHelpFlag(args[1]) {
			printCommandHelp(stdout, cmd)
			return 0
		}
	}
	if len(args) == 0 || !localCommands[args[0]] {
		cfg, err := hubSettings(env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
		if cfg.Hub != "" {
			return runHubMode(ctx, cfg, cmd, args, env, stdin, stdout, stderr)
		}
	}
	if len(args) == 0 {
		printUsage(stdout)
		return ensureDaemon(env, stderr)
	}
	return cmd.run(ctx, args[1:], env, stdin, stdout, stderr)
}

func ensure(env []string, stderr io.Writer) int {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	exe, err := os.Executable()
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: cannot find this executable: %s\n", err)
		return 1
	}
	bin, err := client.DaemonBinBeside(exe)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	if err := client.Ensure(ctx, dirs, bin, env); err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	fmt.Fprintf(stderr, "agentbusd ready at %s\n", dirs.SocketPath)
	return 0
}

// logNow is the clock --since durations count back from; tests replace it.
var logNow = time.Now

var errSinceFormat = errors.New("want a duration like 30m, 6h, 2d, or an RFC3339 time like 2026-11-09T13:00:00Z")

// parseSince resolves a --since value: a positive duration before now
// (time.ParseDuration syntax plus a leading whole-day "Nd" part), an RFC3339
// time, or a YYYY-MM-DD date meaning local midnight.
func parseSince(s string, now time.Time) (time.Time, error) {
	if ts, err := time.Parse(time.RFC3339Nano, s); err == nil {
		return ts, nil
	}
	if ts, err := time.ParseInLocation(time.DateOnly, s, time.Local); err == nil {
		return ts, nil
	}
	d, ok := parseDays(s)
	if !ok || d <= 0 {
		return time.Time{}, errSinceFormat
	}
	return now.Add(-d), nil
}

// parseDays parses time.ParseDuration syntax with an optional leading "Nd".
func parseDays(s string) (time.Duration, bool) {
	var days time.Duration
	if before, after, found := strings.Cut(s, "d"); found {
		n, err := strconv.ParseUint(before, 10, 16)
		if err != nil {
			return 0, false
		}
		days = time.Duration(n) * 24 * time.Hour
		if after == "" {
			return days, true
		}
		s = after
	}
	d, err := time.ParseDuration(s)
	if err != nil || d < 0 {
		return 0, false
	}
	return days + d, true
}

func cmdLog(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("log", flag.ContinueOnError)
	fs.SetOutput(stderr)
	since := fs.String("since", "", "include events at or after this time: a duration ago (30m, 6h, 2d), an RFC3339 time, or a YYYY-MM-DD date")
	name := fs.String("name", "", "include events for this name")
	msgID := fs.String("msg", "", "include events for this message id")
	eventName := fs.String("event", "", "include events of this type")
	follow := fs.Bool("follow", false, "wait for new events until interrupted")
	jsonOut := fs.Bool("json", false, "print each log line as JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintf(stderr, "agentbus log: unexpected arguments\n")
		return 2
	}
	filter := eventlog.Filter{Name: *name, MsgID: *msgID, Event: *eventName}
	if *since != "" {
		ts, err := parseSince(*since, logNow())
		if err != nil {
			fmt.Fprintf(stderr, "agentbus log: --since %q: %s\n", *since, err)
			return 2
		}
		filter.Since = ts
	}
	dir, err := paths.StateDir(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	lg := eventlog.Open(dir)
	emit := func(line string) error {
		if *jsonOut {
			if _, err := fmt.Fprintln(stdout, line); err != nil {
				return fmt.Errorf("write log line: %w", err)
			}
			return nil
		}
		text, err := eventlog.Format(line)
		if err != nil {
			return fmt.Errorf("format log line: %w", err)
		}
		if _, err := fmt.Fprintln(stdout, text); err != nil {
			return fmt.Errorf("write log line: %w", err)
		}
		return nil
	}
	if *follow {
		if err := lg.Follow(ctx, filter, emit); err != nil {
			fmt.Fprintf(stderr, "agentbus log: %s\n", err)
			return 1
		}
		return 0
	}
	lines, err := lg.Read(filter)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus log: %s\n", err)
		return 1
	}
	for _, line := range lines {
		if err := emit(line); err != nil {
			fmt.Fprintf(stderr, "agentbus log: %s\n", err)
			return 1
		}
	}
	return 0
}
