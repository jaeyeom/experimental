// Command agentbus is the client for the local agentbus daemon.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
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
	if len(args) == 0 {
		printUsage(stdout)
		return ensureDaemon(env, stderr)
	}
	if isHelpFlag(args[0]) {
		printUsage(stdout)
		return 0
	}
	if args[0] == "help" {
		return cmdHelp(args[1:], stdout, stderr)
	}
	cmd, ok := findCommand(args[0])
	if !ok {
		return unknownCommand(args[0], stderr)
	}
	// Only the first argument counts, so a body word such as --help is still sent.
	if len(args) > 1 && isHelpFlag(args[1]) {
		printCommandHelp(stdout, cmd)
		return 0
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

func cmdLog(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("log", flag.ContinueOnError)
	fs.SetOutput(stderr)
	since := fs.String("since", "", "include events at or after this RFC3339 time")
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
		ts, err := time.Parse(time.RFC3339Nano, *since)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus log: --since: %s\n", err)
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
