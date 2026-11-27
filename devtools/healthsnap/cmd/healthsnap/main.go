// Command healthsnap runs config-driven health checks, writes a snapshot,
// and sends deduplicated alerts only when a rule fires or clears.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/healthsnap"
	"github.com/jaeyeom/experimental/devtools/pkg/notify"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

const errExitCode = 2

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, healthsnap.UserEnv(), realRuntime()))
}

type appRuntime struct {
	deps   healthsnap.CheckDeps
	notify notify.Runner
	tool   func() toolcli.Tool
}

func realRuntime() appRuntime {
	return appRuntime{
		deps: healthsnap.CheckDeps{
			RunCmd:   healthsnap.ExecCmd,
			StatFile: os.Stat,
			Host:     healthsnap.SystemHostReader{},
			Now:      time.Now,
		},
		notify: notify.Exec,
		tool:   newHealthsnapTool,
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
	toolFactory := rt.tool
	if toolFactory == nil {
		toolFactory = newHealthsnapTool
	}
	if code, ok := toolFactory().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
	if c, ok := findCommand(args[0]); ok {
		return c.run(args[1:], stdout, stderr, env, rt)
	}
	fmt.Fprintf(stderr, "healthsnap: unknown command %q\n\n", args[0])
	writeUsage(stderr)
	return errExitCode
}

func cmdRun(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("healthsnap run", flag.ContinueOnError)
	configPath := fs.String("config", "", "Pkl config file")
	stateDir := fs.String("state-dir", "", "override state directory")
	asJSON := fs.Bool("json", false, "print snapshot as JSON")
	markdown := fs.Bool("markdown", false, "print and write Markdown rendering")
	force := fs.Bool("force", false, "ignore per-check interval and run all checks")
	dryRun := fs.Bool("dry-run", false, "run checks without saving state, snapshots, or notifying")
	if code, done := parseFlags(fs, args, stdout, stderr); done {
		return code
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "healthsnap run: unexpected positional arguments")
		writeCommandUsage(fs, stderr)
		return errExitCode
	}

	cfg, paths, code := loadConfigAndPaths(context.Background(), stderr, env, *configPath, false)
	if code != 0 {
		return code
	}
	resolvedStateDir := resolveEffectiveStateDir(*stateDir, cfg.StateDir, paths.StateDir, env)
	out, err := healthsnap.Run(context.Background(), healthsnap.RunInput{
		Config:        cfg,
		StateDir:      resolvedStateDir,
		BaseEnv:       env,
		WriteMarkdown: *markdown,
		Force:         *force,
		DryRun:        *dryRun,
		Deps:          rt.deps,
		Notify:        rt.notify,
	})
	if err != nil {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return errExitCode
	}

	if err := writeSnapshotOutput(stdout, out.Snapshot, *asJSON, *markdown); err != nil {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return errExitCode
	}
	for _, a := range out.Snapshot.Alerts {
		if a.NotifyError != "" {
			fmt.Fprintf(stderr, "healthsnap: notify %s: %s\n", a.RuleID, a.NotifyError)
		}
	}
	if out.HasNotifyError() {
		return errExitCode
	}
	if out.Snapshot.State != healthsnap.SeverityOK {
		return 1
	}
	return 0
}

func cmdStatus(args []string, stdout, stderr io.Writer, env []string, _ appRuntime) int {
	fs := flag.NewFlagSet("healthsnap status", flag.ContinueOnError)
	configPath := fs.String("config", "", "Pkl config file")
	stateDir := fs.String("state-dir", "", "override state directory")
	asJSON := fs.Bool("json", false, "print snapshot as JSON")
	markdown := fs.Bool("markdown", false, "print snapshot as Markdown")
	if code, done := parseFlags(fs, args, stdout, stderr); done {
		return code
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "healthsnap status: unexpected positional arguments")
		writeCommandUsage(fs, stderr)
		return errExitCode
	}

	cfg, paths, code := loadConfigAndPaths(context.Background(), stderr, env, *configPath, *configPath == "")
	if code != 0 {
		return code
	}
	resolvedStateDir := resolveEffectiveStateDir(*stateDir, cfg.StateDir, paths.StateDir, env)
	snap, err := healthsnap.LoadLatestSnapshot(resolvedStateDir)
	if err != nil {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return errExitCode
	}
	if err := writeSnapshotOutput(stdout, snap, *asJSON, *markdown); err != nil {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return errExitCode
	}
	if snap.State != healthsnap.SeverityOK {
		return 1
	}
	return 0
}

func loadConfigAndPaths(
	ctx context.Context,
	stderr io.Writer,
	env []string,
	configPath string,
	optional bool,
) (healthsnap.Config, healthsnap.Paths, int) {
	paths, err := healthsnap.LocatePaths(env)
	if err != nil && (configPath == "" || optional) {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return healthsnap.Config{}, healthsnap.Paths{}, errExitCode
	}
	if configPath == "" {
		configPath = paths.ConfigFile
	}
	cfg, err := healthsnap.LoadConfig(ctx, configPath, optional)
	if err != nil {
		fmt.Fprintf(stderr, "healthsnap: %v\n", err)
		return healthsnap.Config{}, healthsnap.Paths{}, errExitCode
	}
	return cfg, paths, 0
}

func resolveEffectiveStateDir(flagDir, cfgDir, defaultDir string, env []string) string {
	if flagDir != "" {
		return healthsnap.ExpandHome(flagDir, env)
	}
	if cfgDir != "" {
		return healthsnap.ExpandHome(cfgDir, env)
	}
	return defaultDir
}

func writeSnapshotOutput(w io.Writer, snap healthsnap.Snapshot, asJSON, asMarkdown bool) error {
	switch {
	case asJSON && asMarkdown:
		return errors.New("--json and --markdown cannot be combined on stdout")
	case asJSON:
		enc := json.NewEncoder(w)
		enc.SetIndent("", "  ")
		if err := enc.Encode(snap); err != nil {
			return fmt.Errorf("encode json: %w", err)
		}
		return nil
	case asMarkdown:
		_, err := fmt.Fprint(w, healthsnap.RenderMarkdown(snap))
		if err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		return nil
	default:
		_, err := fmt.Fprint(w, healthsnap.FormatSnapshotText(snap))
		if err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		return nil
	}
}
