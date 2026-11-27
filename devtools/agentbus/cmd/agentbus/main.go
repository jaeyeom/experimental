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

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
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

// Tests replace these so help can prove it never touches the daemon,
// and so a dropped reply is not confused with a daemon that never started.
var (
	ensureDaemon   = ensure
	dialDaemon     = client.Call
	ensureDaemonUp = startDaemonForRetry
)

type hooksKey struct{}

// cmdHooks carries per-call test overrides on context.Context so tests can
// inject fakes without mutating package globals and run concurrently.
type cmdHooks struct {
	dialDaemon       func(context.Context, string, daemon.Request) (daemon.Response, error)
	ensureDaemon     func([]string, io.Writer) int
	ensureDaemonUp   func(context.Context, paths.Dirs, []string) error
	mintClientID     func(string) (string, error)
	outboxNow        func() time.Time
	listProcs        func() ([]checkout.Proc, error)
	memProcs         func() procinfo.Table
	memMemory        func() procinfo.Memory
	hibernateProcs   func(context.Context) ([]procinfo.Process, error)
	hibernateCwd     procinfo.CwdFunc
	hibernateClock   hibernate.Clock
	policyMemory     func() procinfo.Memory
	policyTicks      func(time.Duration) (<-chan time.Time, func())
	policyTicked     func()
	leaseHookTimeout time.Duration
	tunnelExitStatus func(context.Context, []string) (int, bool)
	doctorProbe      func(string, []string) remotecall.Probe
	listDoctorProcs  func(context.Context) ([]doctorProc, error)
	daemonLockHolder func(string) (int, bool, error)
	daemonShow       func(context.Context) (string, error)
}

func hooksFrom(ctx context.Context) cmdHooks {
	h, _ := ctx.Value(hooksKey{}).(cmdHooks)
	return h
}

func mergeBaseHooks(h, cur cmdHooks) cmdHooks {
	if h.dialDaemon == nil {
		h.dialDaemon = cur.dialDaemon
	}
	if h.ensureDaemon == nil {
		h.ensureDaemon = cur.ensureDaemon
	}
	if h.ensureDaemonUp == nil {
		h.ensureDaemonUp = cur.ensureDaemonUp
	}
	if h.mintClientID == nil {
		h.mintClientID = cur.mintClientID
	}
	if h.outboxNow == nil {
		h.outboxNow = cur.outboxNow
	}
	if h.listProcs == nil {
		h.listProcs = cur.listProcs
	}
	if h.memProcs == nil {
		h.memProcs = cur.memProcs
	}
	if h.memMemory == nil {
		h.memMemory = cur.memMemory
	}
	return h
}

func mergeHibAndDoctorHooks(h, cur cmdHooks) cmdHooks {
	if h.hibernateProcs == nil {
		h.hibernateProcs = cur.hibernateProcs
	}
	if h.hibernateCwd == nil {
		h.hibernateCwd = cur.hibernateCwd
	}
	if h.hibernateClock == nil {
		h.hibernateClock = cur.hibernateClock
	}
	if h.policyMemory == nil {
		h.policyMemory = cur.policyMemory
	}
	if h.policyTicks == nil {
		h.policyTicks = cur.policyTicks
	}
	if h.policyTicked == nil {
		h.policyTicked = cur.policyTicked
	}
	if h.leaseHookTimeout == 0 {
		h.leaseHookTimeout = cur.leaseHookTimeout
	}
	if h.tunnelExitStatus == nil {
		h.tunnelExitStatus = cur.tunnelExitStatus
	}
	if h.doctorProbe == nil {
		h.doctorProbe = cur.doctorProbe
	}
	if h.listDoctorProcs == nil {
		h.listDoctorProcs = cur.listDoctorProcs
	}
	if h.daemonLockHolder == nil {
		h.daemonLockHolder = cur.daemonLockHolder
	}
	if h.daemonShow == nil {
		h.daemonShow = cur.daemonShow
	}
	return h
}

func withCmdHooks(ctx context.Context, h cmdHooks) context.Context {
	cur := hooksFrom(ctx)
	h = mergeBaseHooks(h, cur)
	h = mergeHibAndDoctorHooks(h, cur)
	return context.WithValue(ctx, hooksKey{}, h)
}

func ensureDaemonFn(ctx context.Context) func([]string, io.Writer) int {
	if fn := hooksFrom(ctx).ensureDaemon; fn != nil {
		return fn
	}
	return ensureDaemon
}

func memProcsFn(ctx context.Context) func() procinfo.Table {
	if fn := hooksFrom(ctx).memProcs; fn != nil {
		return fn
	}
	return memProcs
}

func memMemoryFn(ctx context.Context) func() procinfo.Memory {
	if fn := hooksFrom(ctx).memMemory; fn != nil {
		return fn
	}
	return memMemory
}

func policyMemoryFn(ctx context.Context) func() procinfo.Memory {
	if fn := hooksFrom(ctx).policyMemory; fn != nil {
		return fn
	}
	return policyMemory
}

func policyTicksFn(ctx context.Context) func(time.Duration) (<-chan time.Time, func()) {
	if fn := hooksFrom(ctx).policyTicks; fn != nil {
		return fn
	}
	return policyTicks
}

func policyTickedFn(ctx context.Context) func() {
	if fn := hooksFrom(ctx).policyTicked; fn != nil {
		return fn
	}
	return policyTicked
}

func daemonShowFn(ctx context.Context) func(context.Context) (string, error) {
	if fn := hooksFrom(ctx).daemonShow; fn != nil {
		return fn
	}
	return daemonShow
}

func dialDaemonFn(ctx context.Context) func(context.Context, string, daemon.Request) (daemon.Response, error) {
	if fn := hooksFrom(ctx).dialDaemon; fn != nil {
		return fn
	}
	return dialDaemon
}

func mintClientIDFn(ctx context.Context) func(string) (string, error) {
	if fn := hooksFrom(ctx).mintClientID; fn != nil {
		return fn
	}
	return mintClientID
}

func outboxNowFn(ctx context.Context) func() time.Time {
	if fn := hooksFrom(ctx).outboxNow; fn != nil {
		return fn
	}
	return outboxNow
}

func listProcsFn(ctx context.Context) func() ([]checkout.Proc, error) {
	if fn := hooksFrom(ctx).listProcs; fn != nil {
		return fn
	}
	return listProcs
}

func leaseHookTimeoutVal(ctx context.Context) time.Duration {
	if d := hooksFrom(ctx).leaseHookTimeout; d > 0 {
		return d
	}
	return leaseHookTimeout
}

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
	if shouldHandleToolCLI(args) {
		code, _ := newAgentbusToolWithEnv(env).Handle(ctx, args, stdout, stderr)
		return code
	}
	cfg, cfgErr := hubSettings(env)
	maybeFlush(ctx, cfg, cfgErr, env)
	if len(args) == 0 || !localCommands[args[0]] {
		if cfgErr != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", cfgErr)
			return cmdErrorCode(args)
		}
		if cfg.Hub != "" {
			return runHubMode(ctx, cfg, cmd, args, env, stdin, stdout, stderr)
		}
	}
	if len(args) == 0 {
		printUsage(stdout)
		return ensureDaemonFn(ctx)(env, stderr)
	}
	return cmd.run(ctx, args[1:], env, stdin, stdout, stderr)
}

// startDaemonForRetry is the down-daemon path: the socket was missing
// or the connection was refused before the request was written.
func startDaemonForRetry(ctx context.Context, dirs paths.Dirs, env []string) error {
	if cfg, err := hubSettings(env); err != nil {
		return err
	} else if cfg.Hub != "" {
		return fmt.Errorf("%w", config.RefuseHubDaemon(cfg.Hub))
	}
	exe, err := os.Executable()
	if err != nil {
		return fmt.Errorf("cannot find this executable: %w", err)
	}
	bin, err := client.DaemonBinBeside(exe)
	if err != nil {
		return fmt.Errorf("%w", err)
	}
	if err := client.Ensure(ctx, dirs, bin, env); err != nil {
		return fmt.Errorf("%w", err)
	}
	return nil
}

func ensure(env []string, stderr io.Writer) int {
	if cfg, err := hubSettings(env); err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	} else if cfg.Hub != "" {
		fmt.Fprintf(stderr, "agentbus: %s\n", config.RefuseHubDaemon(cfg.Hub))
		return 1
	}
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
	emit := logEmitter(stdout, *jsonOut)
	var err error
	if t, ok := tunnelFrom(ctx); ok {
		err = t.streamLog(ctx, filter, *follow, emit)
	} else {
		err = readLog(ctx, env, filter, *follow, emit)
	}
	if err != nil {
		fmt.Fprintf(stderr, "agentbus log: %s\n", err)
		return 1
	}
	return 0
}

// logEmitter prints one event log line as JSON or as text.
func logEmitter(stdout io.Writer, jsonOut bool) func(string) error {
	return func(line string) error {
		if jsonOut {
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
}

// readLog reads this machine's event log file without a daemon.
func readLog(ctx context.Context, env []string, filter eventlog.Filter, follow bool, emit func(string) error) error {
	dir, err := paths.StateDir(env)
	if err != nil {
		return fmt.Errorf("locate the log: %w", err)
	}
	lg := eventlog.Open(dir)
	if follow {
		if err := lg.Follow(ctx, filter, emit); err != nil {
			return fmt.Errorf("follow: %w", err)
		}
		return nil
	}
	lines, err := lg.Read(filter)
	if err != nil {
		return fmt.Errorf("read: %w", err)
	}
	for _, line := range lines {
		if err := emit(line); err != nil {
			return err
		}
	}
	return nil
}
