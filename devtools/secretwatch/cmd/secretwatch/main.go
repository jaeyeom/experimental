// Command secretwatch scans systemd journals for leaked secrets and alerts
// without ever exposing the secret values.
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
	"github.com/jaeyeom/experimental/devtools/secretwatch"
	"golang.org/x/sys/unix"
)

const errExitCode = 2

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, secretwatch.UserEnv(), realRuntime()))
}

type appRuntime struct {
	stream secretwatch.JournalStreamer
	notify notify.Runner
	now    func() time.Time
}

func realRuntime() appRuntime {
	return appRuntime{
		stream: secretwatch.ExecJournal,
		notify: notify.Exec,
		now:    time.Now,
	}
}

func run(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	if len(args) == 0 {
		writeUsage(stderr)
		return errExitCode
	}
	switch args[0] {
	case "help", "-h", "--help":
		return runHelp(args[1:], stdout, stderr, env, rt)
	}
	if code, ok := newSecretwatchTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
	if c, ok := findCommand(args[0]); ok {
		return c.run(args[1:], stdout, stderr, env, rt)
	}
	fmt.Fprintf(stderr, "secretwatch: unknown command %q\n\n", args[0])
	writeUsage(stderr)
	return errExitCode
}

func cmdCheck(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("secretwatch check", flag.ContinueOnError)
	since := fs.String("since", "", "start scanning from this journalctl --since time")
	follow := fs.Bool("follow", false, "continuously follow journals")
	asJSON := fs.Bool("json", false, "print JSON output")
	configPath := fs.String("config", "", "Pkl config file")
	if code, done := parseFlags(fs, args, stdout, stderr); done {
		return code
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "secretwatch check: unexpected positional arguments")
		writeCommandUsage(fs, stderr)
		return errExitCode
	}

	cfg, paths, code := loadConfigAndPaths(context.Background(), stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := secretwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return errExitCode
	}

	if *follow {
		return runFollow(stdout, stderr, env, rt, cfg, paths, state, *since, *asJSON)
	}
	return runCheckOnce(stdout, stderr, env, rt, cfg, paths, state, *since, *asJSON)
}

func runCheckOnce(
	stdout, stderr io.Writer,
	env []string,
	rt appRuntime,
	cfg secretwatch.Config,
	paths secretwatch.Paths,
	state secretwatch.State,
	since string,
	asJSON bool,
) int {
	rep, next, err := secretwatch.Check(context.Background(), secretwatch.CheckInput{
		Config:  cfg,
		State:   state,
		Since:   since,
		Now:     rt.now(),
		BaseEnv: env,
		Stream:  rt.stream,
		Notify:  rt.notify,
	})
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return errExitCode
	}
	if saveErr := secretwatch.SaveState(paths.StateFile, next); saveErr != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", saveErr)
		return errExitCode
	}
	return writeReportOutput(stdout, stderr, rep, asJSON)
}

func runFollow(
	stdout, stderr io.Writer,
	env []string,
	rt appRuntime,
	cfg secretwatch.Config,
	paths secretwatch.Paths,
	state secretwatch.State,
	since string,
	asJSON bool,
) int {
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()

	var outMu sync.Mutex
	rep, next, err := secretwatch.Follow(ctx, secretwatch.FollowInput{
		CheckInput: secretwatch.CheckInput{
			Config:  cfg,
			State:   state,
			Since:   since,
			Now:     rt.now(),
			BaseEnv: env,
			Stream:  rt.stream,
			Notify:  rt.notify,
		},
		OnFinding: func(f secretwatch.Finding) {
			outMu.Lock()
			defer outMu.Unlock()
			if asJSON {
				_ = json.NewEncoder(stdout).Encode(f)
			} else {
				fmt.Fprintln(stdout, secretwatch.FormatFindingLine(f))
			}
			if f.NotifyError != "" {
				fmt.Fprintf(stderr, "secretwatch: notify %s: %s\n", f.Rule, f.NotifyError)
			}
		},
		OnNote: func(_ secretwatch.JournalKind, note string) {
			outMu.Lock()
			defer outMu.Unlock()
			fmt.Fprintf(stderr, "secretwatch: note: %s\n", note)
		},
		OnSave: func(s secretwatch.State) error {
			return secretwatch.SaveState(paths.StateFile, s)
		},
	})
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return errExitCode
	}
	if saveErr := secretwatch.SaveState(paths.StateFile, next); saveErr != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", saveErr)
		return errExitCode
	}
	for _, j := range rep.Journals {
		if j.Error != "" {
			fmt.Fprintf(stderr, "secretwatch: %s journal: %s\n", j.Journal, j.Error)
		}
	}
	return rep.ExitCode()
}

func cmdScan(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("secretwatch scan", flag.ContinueOnError)
	since := fs.String("since", "", "scan entries since this journalctl time (required)")
	until := fs.String("until", "", "stop scanning at this journalctl time")
	unit := fs.String("unit", "", "filter to a single systemd unit")
	asJSON := fs.Bool("json", false, "print JSON output")
	configPath := fs.String("config", "", "Pkl config file")
	if code, done := parseFlags(fs, args, stdout, stderr); done {
		return code
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "secretwatch scan: unexpected positional arguments")
		writeCommandUsage(fs, stderr)
		return errExitCode
	}
	if *since == "" {
		fmt.Fprintln(stderr, "secretwatch scan: --since is required")
		writeCommandUsage(fs, stderr)
		return errExitCode
	}

	cfg, _, code := loadConfigAndPaths(context.Background(), stderr, env, *configPath)
	if code != 0 {
		return code
	}
	rep, err := secretwatch.Scan(context.Background(), secretwatch.ScanInput{
		Config: cfg,
		Since:  *since,
		Until:  *until,
		Unit:   *unit,
		Stream: rt.stream,
	})
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return errExitCode
	}
	return writeReportOutput(stdout, stderr, rep, *asJSON)
}

func loadConfigAndPaths(
	ctx context.Context,
	stderr io.Writer,
	env []string,
	configPath string,
) (secretwatch.Config, secretwatch.Paths, int) {
	paths, err := secretwatch.LocatePaths(env)
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return secretwatch.Config{}, secretwatch.Paths{}, errExitCode
	}
	optional := false
	if configPath == "" {
		configPath = paths.ConfigFile
		optional = true
	}
	cfg, err := secretwatch.LoadConfig(ctx, configPath, optional)
	if err != nil {
		fmt.Fprintf(stderr, "secretwatch: %v\n", err)
		return secretwatch.Config{}, secretwatch.Paths{}, errExitCode
	}
	return cfg, paths, 0
}

func writeReportOutput(stdout, stderr io.Writer, rep secretwatch.Report, asJSON bool) int {
	for _, j := range rep.Journals {
		if j.Skipped && j.Note != "" {
			fmt.Fprintf(stderr, "secretwatch: note: %s\n", j.Note)
		}
		if j.Error != "" {
			fmt.Fprintf(stderr, "secretwatch: %s journal: %s\n", j.Journal, j.Error)
		}
	}
	for _, f := range rep.Findings {
		if f.NotifyError != "" {
			fmt.Fprintf(stderr, "secretwatch: notify %s: %s\n", f.Rule, f.NotifyError)
		}
	}
	if asJSON || len(rep.Findings) > 0 {
		text, err := secretwatch.FormatReport(rep, asJSON)
		if err != nil {
			fmt.Fprintf(stderr, "secretwatch: %v\n", err)
			return errExitCode
		}
		fmt.Fprint(stdout, text)
	}
	return rep.ExitCode()
}
