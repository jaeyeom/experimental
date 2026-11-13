// Command diskwatch checks free space and runs a configured action.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/diskwatch"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, os.Environ(), realRuntime()))
}

type runtime struct {
	stat     diskwatch.StatFS
	run      diskwatch.Runner
	now      func() time.Time
	hostname string
}

func realRuntime() runtime {
	host, err := os.Hostname()
	if err != nil {
		host = "unknown"
	}
	return runtime{stat: diskwatch.Stat, run: diskwatch.Exec, now: time.Now, hostname: host}
}

func run(args []string, stdout, stderr io.Writer, env []string, rt runtime) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: diskwatch check|status|hook")
		return 2
	}
	switch args[0] {
	case "check":
		return cmdCheck(args[1:], stdout, stderr, env, rt, false)
	case "hook":
		return cmdCheck(args[1:], stdout, stderr, env, rt, true)
	case "status":
		return cmdStatus(args[1:], stdout, stderr, env, rt)
	default:
		fmt.Fprintf(stderr, "diskwatch: unknown command %q\n", args[0])
		return 2
	}
}

func cmdCheck(args []string, stdout, stderr io.Writer, env []string, rt runtime, hook bool) int {
	fs := flag.NewFlagSet("diskwatch check", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dry := fs.Bool("dry-run", false, "print what would trigger without running the action")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if hook && *dry {
		fmt.Fprintln(stderr, "diskwatch: hook does not take --dry-run")
		return 2
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := diskwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return 1
	}
	report, next, err := diskwatch.Check(context.Background(), diskwatch.Input{
		Config:     cfg,
		State:      state,
		Now:        rt.now(),
		Stat:       rt.stat,
		Run:        rt.run,
		BaseEnv:    env,
		Hostname:   rt.hostname,
		SummaryDir: filepath.Dir(paths.StateFile),
		Home:       envValue(env, "HOME"),
		DryRun:     *dry,
		Hook:       hook,
	})
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return 1
	}
	if !*dry && !report.Skipped {
		if err := diskwatch.SaveState(paths.StateFile, next); err != nil {
			fmt.Fprintf(stderr, "diskwatch: %v\n", err)
			return 1
		}
	}
	return printReport(stdout, stderr, report)
}

func cmdStatus(args []string, stdout, stderr io.Writer, env []string, rt runtime) int {
	fs := flag.NewFlagSet("diskwatch status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	asJSON := fs.Bool("json", false, "print JSON")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := diskwatch.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return 1
	}
	rows := diskwatch.Status(cfg, state, rt.now(), rt.stat)
	text, err := diskwatch.FormatStatus(rows, *asJSON)
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return 1
	}
	fmt.Fprint(stdout, text)
	for _, row := range rows {
		if row.Error != "" {
			return 1
		}
	}
	return 0
}

func load(stderr io.Writer, env []string, configPath string) (diskwatch.Config, diskwatch.Paths, int) {
	paths, err := diskwatch.LocatePaths(env)
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return diskwatch.Config{}, diskwatch.Paths{}, 1
	}
	if configPath == "" {
		configPath = paths.ConfigFile
	}
	cfg, err := diskwatch.LoadConfig(configPath)
	if err != nil {
		fmt.Fprintf(stderr, "diskwatch: %v\n", err)
		return diskwatch.Config{}, diskwatch.Paths{}, 2
	}
	return cfg, paths, 0
}

func printReport(stdout, stderr io.Writer, report diskwatch.Report) int {
	code := 0
	if report.Skipped {
		fmt.Fprintln(stdout, "diskwatch: hook skipped")
		return 0
	}
	for _, mount := range report.Mounts {
		if mount.ReadError != "" {
			fmt.Fprintf(stderr, "diskwatch: %s: %s\n", mount.Path, mount.ReadError)
			code = 1
		}
		if mount.ActionError != "" {
			fmt.Fprintf(stderr, "diskwatch: %s: %s\n", mount.Path, mount.ActionError)
			code = 1
		}
		if !mount.Triggered {
			continue
		}
		prefix := "trigger"
		if mount.DryRun {
			prefix = "dry-run"
		}
		fmt.Fprintf(stdout, "%s %s %s free %.2f%%\n%s", prefix, mount.Path, mount.Kind, diskwatch.FreePct(mount.Free, mount.Total), mount.Summary.Text())
	}
	return code
}

func envValue(env []string, key string) string {
	prefix := key + "="
	for _, item := range env {
		if len(item) >= len(prefix) && item[:len(prefix)] == prefix {
			return item[len(prefix):]
		}
	}
	return ""
}
