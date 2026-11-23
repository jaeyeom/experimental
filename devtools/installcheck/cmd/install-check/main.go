// Command install-check messages an agent over agentbus when watched GitHub
// issues land on main. It is meant to run from a systemd user timer.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/installcheck"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, os.Environ(), installcheck.Exec, time.Now))
}

func run(args []string, stdout, stderr io.Writer, env []string, runner installcheck.Runner, now func() time.Time) int {
	if code, ok := newInstallCheckTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
	cmd := "run"
	if len(args) > 0 && len(args[0]) > 0 && args[0][0] != '-' {
		cmd, args = args[0], args[1:]
	}
	switch cmd {
	case "run":
		return cmdRun(args, stdout, stderr, env, runner, now)
	case "status":
		return cmdStatus(args, stdout, stderr, env, runner)
	default:
		fmt.Fprintf(stderr, "install-check: unknown command %q\nusage: install-check [run|status] [flags]\n", cmd)
		return 2
	}
}

func cmdRun(args []string, stdout, stderr io.Writer, env []string, runner installcheck.Runner, now func() time.Time) int {
	fs := flag.NewFlagSet("install-check run", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dry := fs.Bool("dry-run", false, "print what would be sent; change no state")
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := installcheck.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	res := installcheck.Check(context.Background(), installcheck.Input{
		Config: cfg, State: state, Now: now(), Run: runner, DryRun: *dry,
	})
	code = 0
	for _, e := range res.Errors {
		fmt.Fprintf(stderr, "install-check: %v\n", e)
		code = 1
	}
	if *dry {
		if res.Message != "" {
			fmt.Fprintf(stdout, "dry-run: would send to %s:\n%s", cfg.NotifyName, res.Message)
		}
		return code
	}
	if res.Sent {
		fmt.Fprintf(stderr, "install-check: sent message to %s\n", cfg.NotifyName)
	}
	if err := installcheck.SaveState(paths.StateFile, res.Next); err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	return code
}

func cmdStatus(args []string, stdout, stderr io.Writer, env []string, runner installcheck.Runner) int {
	fs := flag.NewFlagSet("install-check status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", "", "config file")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	cfg, paths, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := installcheck.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	text, errs := installcheck.Status(context.Background(), cfg, state, runner)
	fmt.Fprint(stdout, text)
	for _, e := range errs {
		fmt.Fprintf(stderr, "install-check: %v\n", e)
	}
	if len(errs) > 0 {
		return 1
	}
	return 0
}

func load(stderr io.Writer, env []string, configPath string) (installcheck.Config, installcheck.Paths, int) {
	paths, err := installcheck.LocatePaths(env)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, 1
	}
	if configPath == "" {
		configPath = paths.ConfigFile
	}
	cfg, err := loadConfig(configPath)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, 2
	}
	if cfg.StateFile != "" {
		paths.StateFile = cfg.StateFile
	}
	return cfg, paths, 0
}

func loadConfig(path string) (installcheck.Config, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own config path
	if err != nil {
		return installcheck.Config{}, fmt.Errorf("read config: %w", err)
	}
	cfg, err := installcheck.ParseConfig(data)
	if err != nil {
		return installcheck.Config{}, fmt.Errorf("%s: %w", path, err)
	}
	return cfg, nil
}
