// Command authwatch monitors agent CLI OAuth token expiries and notifies on level changes.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"runtime"
	"time"

	"github.com/jaeyeom/experimental/devtools/authwatch"
	"golang.org/x/sys/unix"
)

const errExitCode = 5

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, authwatch.UserEnv(), realRuntime()))
}

type appRuntime struct {
	readStore authwatch.StoreReader
	cliStatus authwatch.CLIStatusChecker
	notify    authwatch.NotifyRunner
	now       func() time.Time
}

func realRuntime() appRuntime {
	return appRuntime{
		readStore: authwatch.DefaultStoreReader(runtime.GOOS, authwatch.ExecOutput),
		cliStatus: authwatch.DefaultCLIStatusChecker(authwatch.ExecOutput),
		notify:    authwatch.ExecNotify,
		now:       time.Now,
	}
}

func run(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: authwatch check|run|status|report-failure")
		return errExitCode
	}
	switch args[0] {
	case "check":
		return cmdCheck(args[1:], stdout, stderr, env, rt)
	case "run":
		return cmdRun(args[1:], stdout, stderr, env, rt)
	case "status":
		return cmdStatus(args[1:], stdout, stderr, env, rt)
	case "report-failure":
		return cmdReportFailure(args[1:], stdout, stderr, env, rt)
	default:
		fmt.Fprintf(stderr, "authwatch: unknown command %q\n", args[0])
		return errExitCode
	}
}

func cmdCheck(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("authwatch check", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return errExitCode
	}
	return runOnce(context.Background(), stdout, stderr, env, rt, *configPath)
}

func runOnce(ctx context.Context, stdout, stderr io.Writer, env []string, rt appRuntime, configPath string) int {
	cfg, paths, code := load(stderr, env, configPath)
	if code != 0 {
		return code
	}
	state, err := authwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	rep, next := authwatch.Check(ctx, authwatch.Input{
		Config:    cfg,
		State:     state,
		Now:       rt.now(),
		BaseEnv:   env,
		ReadStore: rt.readStore,
		CLIStatus: rt.cliStatus,
		Notify:    rt.notify,
	})
	if err := authwatch.SaveState(paths.StateFile, next); err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	return printCheckReport(stdout, stderr, rep)
}

func cmdRun(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("authwatch run", flag.ContinueOnError)
	fs.SetOutput(stderr)
	every := fs.Duration("every", 5*time.Minute, "check interval")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return errExitCode
	}
	if *every <= 0 {
		fmt.Fprintln(stderr, "authwatch: --every must be positive")
		return errExitCode
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()

	_ = runOnce(ctx, stdout, stderr, env, rt, *configPath)
	ticker := time.NewTicker(*every)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return 0
		case <-ticker.C:
			_ = runOnce(ctx, stdout, stderr, env, rt, *configPath)
		}
	}
}

func cmdStatus(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("authwatch status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	profile := fs.String("profile", "", "profile name")
	asJSON := fs.Bool("json", false, "print JSON")
	oneLine := fs.Bool("line", false, "print one line for status bars")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return errExitCode
	}
	if *asJSON && *oneLine {
		fmt.Fprintln(stderr, "authwatch: --json and --line are mutually exclusive")
		return errExitCode
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := authwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	rep, err := authwatch.Status(context.Background(), authwatch.Input{
		Config:    cfg,
		State:     state,
		Now:       rt.now(),
		BaseEnv:   env,
		ReadStore: rt.readStore,
		CLIStatus: rt.cliStatus,
	}, *profile)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	text, err := authwatch.FormatStatus(rep, *asJSON, *oneLine)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	fmt.Fprint(stdout, text)
	return rep.WorstExitCode()
}

func cmdReportFailure(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("authwatch report-failure", flag.ContinueOnError)
	fs.SetOutput(stderr)
	profile := fs.String("profile", "", "profile name")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return errExitCode
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := authwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	pr, next, err := authwatch.ReportFailure(context.Background(), authwatch.Input{
		Config:    cfg,
		State:     state,
		Now:       rt.now(),
		BaseEnv:   env,
		ReadStore: rt.readStore,
		Notify:    rt.notify,
	}, *profile)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	if err := authwatch.SaveState(paths.StateFile, next); err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return errExitCode
	}
	if pr.NotifyError != "" {
		fmt.Fprintf(stderr, "authwatch: %s: notify: %s\n", pr.Profile, pr.NotifyError)
		return errExitCode
	}
	fmt.Fprintf(stdout, "%s %s since %s\n", pr.Profile, pr.Level, pr.Since.UTC().Format(time.RFC3339))
	return 0
}

func load(stderr io.Writer, env []string, configPath string) (authwatch.Config, authwatch.Paths, int) {
	paths, err := authwatch.LocatePaths(env)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return authwatch.Config{}, authwatch.Paths{}, errExitCode
	}
	optional := false
	if configPath == "" {
		configPath = paths.ConfigFile
		optional = true
	}
	cfg, err := authwatch.LoadConfig(configPath, optional)
	if err != nil {
		fmt.Fprintf(stderr, "authwatch: %v\n", err)
		return authwatch.Config{}, authwatch.Paths{}, errExitCode
	}
	return cfg, paths, 0
}

func printCheckReport(stdout, stderr io.Writer, rep authwatch.Report) int {
	for _, p := range rep.Profiles {
		if p.ReadError != "" {
			fmt.Fprintf(stderr, "authwatch: %s: %s\n", p.Profile, p.ReadError)
		}
		if p.FormatError != "" {
			fmt.Fprintf(stderr, "authwatch: %s: %s\n", p.Profile, p.FormatError)
		}
		if p.NotifyError != "" {
			fmt.Fprintf(stderr, "authwatch: %s: notify: %s\n", p.Profile, p.NotifyError)
		}
		if p.Notified {
			fmt.Fprintf(stdout, "notified %s %s\n", p.Profile, p.Level)
		}
	}
	return rep.WorstExitCode()
}
