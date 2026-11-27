// Command install-check watches GitHub repositories and delivers notifications
// when watched issues land on main. It is meant to run from a timer or cron job.
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/installcheck"
	agentbusadapter "github.com/jaeyeom/experimental/devtools/installcheck/adapter/agentbus"
	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

type appRuntime struct {
	run    installcheck.Runner
	notify notify.Runner
	now    func() time.Time
}

func realRuntime() appRuntime {
	return appRuntime{
		run:    installcheck.Exec,
		notify: notify.Exec,
		now:    time.Now,
	}
}

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, os.Environ(), realRuntime()))
}

func run(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	if len(args) > 0 {
		switch args[0] {
		case "help", "-h", "--help":
			writeUsage(stdout)
			return 0
		}
	}
	if code, ok := newInstallCheckTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
	cmd := "run"
	if len(args) > 0 && len(args[0]) > 0 && args[0][0] != '-' {
		cmd, args = args[0], args[1:]
	}
	switch cmd {
	case "run":
		return cmdRun(args, stdout, stderr, env, rt)
	case "status":
		return cmdStatus(args, stdout, stderr, env, rt)
	default:
		fmt.Fprintf(stderr, "install-check: unknown command %q\n", cmd)
		writeUsage(stderr)
		return 2
	}
}

func writeUsage(w io.Writer) {
	fmt.Fprint(w, `usage: install-check [command] [flags]

Commands:
  run       Check repos and notify on newly closed watched issues (default)
  status    Show saved heads and currently open watched issues
  doctor    Verify configuration and external tools
  version   Print version information
  update    Check for or install binary updates
  skill     Manage embedded agent skill documentation
  help      Show this help message
`)
}

type runJSONOutput struct {
	DryRun       bool                      `json:"dryRun"`
	Sent         bool                      `json:"sent"`
	NotifyMode   string                    `json:"notifyMode"`
	NotifyTarget string                    `json:"notifyTarget"`
	GHBin        string                    `json:"ghBin"`
	Message      string                    `json:"message,omitempty"`
	Repos        []installcheck.RepoResult `json:"repos"`
	Errors       []string                  `json:"errors,omitempty"`
}

func cmdRun(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("install-check run", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dry := fs.Bool("dry-run", false, "print what would be sent; change no state")
	configPath := fs.String("config", "", "config file")
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "install-check: run does not take positional arguments")
		return 2
	}
	cfg, paths, ghBin, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := installcheck.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	res := installcheck.Check(context.Background(), installcheck.Input{
		Config:   cfg,
		State:    state,
		Now:      rt.now(),
		Run:      rt.run,
		Notifier: buildNotifier(cfg, stdout, env, rt, *asJSON),
		GHBin:    ghBin,
		DryRun:   *dry,
	})
	code = 0
	if len(res.Errors) > 0 {
		code = 1
	}
	if !*dry {
		if err := installcheck.SaveState(paths.StateFile, res.Next); err != nil {
			res.Errors = append(res.Errors, err)
			code = 1
		}
	}
	for _, e := range res.Errors {
		fmt.Fprintf(stderr, "install-check: %v\n", e)
	}
	if *asJSON {
		if err := writeRunJSON(stdout, cfg, ghBin, *dry, res); err != nil {
			fmt.Fprintf(stderr, "install-check: %v\n", err)
			return 1
		}
		return code
	}
	if *dry {
		if res.Message != "" {
			fmt.Fprintf(stdout, "dry-run: would notify %s (%s):\n%s",
				cfg.NotifyTargetLabel(), cfg.EffectiveNotifyMode(), res.Message)
		}
		return code
	}
	if res.Sent && cfg.EffectiveNotifyMode() != installcheck.NotifyModeStdout {
		fmt.Fprintf(stderr, "install-check: notified %s (%s)\n",
			cfg.NotifyTargetLabel(), cfg.EffectiveNotifyMode())
	}
	return code
}

func writeRunJSON(stdout io.Writer, cfg installcheck.Config, ghBin string, dry bool, res installcheck.Result) error {
	out := runJSONOutput{
		DryRun:       dry,
		Sent:         res.Sent,
		NotifyMode:   cfg.EffectiveNotifyMode(),
		NotifyTarget: cfg.NotifyTargetLabel(),
		GHBin:        ghBin,
		Message:      res.Message,
		Repos:        res.Repos,
	}
	for _, e := range res.Errors {
		out.Errors = append(out.Errors, e.Error())
	}
	enc := json.NewEncoder(stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(out); err != nil {
		return fmt.Errorf("encode run output: %w", err)
	}
	return nil
}

func buildNotifier(cfg installcheck.Config, stdout io.Writer, env []string, rt appRuntime, asJSON bool) installcheck.Notifier {
	switch cfg.EffectiveNotifyMode() {
	case installcheck.NotifyModeStdout:
		out := stdout
		if asJSON {
			// Avoid corrupting stdout JSON; JSON payload carries the message.
			out = io.Discard
		}
		return installcheck.StdoutNotifier{Out: out}
	case installcheck.NotifyModeCommand:
		return installcheck.CommandNotifier{
			Template:  cfg.NotifyCommand,
			Recipient: cfg.NotifyName,
			BaseEnv:   env,
			Run:       rt.notify,
		}
	default:
		return agentbusadapter.Notifier{
			Recipient: cfg.NotifyName,
			Run:       rt.run,
		}
	}
}

func cmdStatus(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	fs := flag.NewFlagSet("install-check status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	configPath := fs.String("config", "", "config file")
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "install-check: status does not take positional arguments")
		return 2
	}
	cfg, paths, ghBin, code := load(stderr, env, *configPath)
	if code != 0 {
		return code
	}
	state, err := installcheck.LoadState(paths.StateFile)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	rep, errs := installcheck.InspectStatus(context.Background(), cfg, state, rt.run, ghBin)
	text, err := installcheck.FormatStatus(rep, *asJSON)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return 1
	}
	fmt.Fprint(stdout, text)
	for _, e := range errs {
		fmt.Fprintf(stderr, "install-check: %v\n", e)
	}
	if len(errs) > 0 {
		return 1
	}
	return 0
}

func load(stderr io.Writer, env []string, configPath string) (installcheck.Config, installcheck.Paths, string, int) {
	paths, err := installcheck.LocatePaths(env)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, "", 1
	}
	if configPath == "" {
		configPath = paths.ConfigFile
	}
	cfg, err := loadConfig(configPath)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, "", 2
	}
	stateFile, err := cfg.ResolveStateFile(paths.StateFile, env)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, "", 2
	}
	paths.StateFile = stateFile
	ghBin, err := cfg.ResolveGHBin(env)
	if err != nil {
		fmt.Fprintf(stderr, "install-check: %v\n", err)
		return installcheck.Config{}, installcheck.Paths{}, "", 2
	}
	return cfg, paths, ghBin, 0
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
