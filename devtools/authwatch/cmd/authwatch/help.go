package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"strings"
)

// command is one authwatch subcommand. The dispatch and the usage both
// read commands(), so a command cannot be added to one without the other.
type command struct {
	name    string
	args    string
	summary string
	run     func(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int
}

func commands() []command {
	return []command{
		{"check", "[--config PATH]", "run one pass over every profile and notify on a level change (for cron)", cmdCheck},
		{"run", "[--every 5m] [--config PATH]", "run a pass on an interval until stopped (for a service)", cmdRun},
		{"status", "[--profile P] [--json | --line] [--config PATH]", "print each profile's current level and expiry", cmdStatus},
		{"report-failure", "--profile P [--config PATH]", "record that a CLI reported an auth failure for a profile", cmdReportFailure},
	}
}

func findCommand(name string) (command, bool) {
	for _, c := range commands() {
		if c.name == name {
			return c, true
		}
	}
	return command{}, false
}

// writeUsage prints every command, the config location, and the README.
func writeUsage(w io.Writer) {
	fmt.Fprintln(w, "usage: authwatch <command> [flags]")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Watch agent CLI OAuth token expiry and notify when a level changes.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Commands:")
	for _, c := range commands() {
		fmt.Fprintf(w, "  %-15s %s\n", c.name, c.summary)
		fmt.Fprintf(w, "  %-15s   authwatch %s %s\n", "", c.name, c.args)
	}
	fmt.Fprintln(w, "  help [command]  print this help, or one command's flags")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Config: --config PATH, else $XDG_CONFIG_HOME/authwatch/config.toml")
	fmt.Fprintln(w, "(~/.config on Linux, ~/Library/Application Support on macOS).")
	fmt.Fprintln(w, "README: https://github.com/jaeyeom/experimental/tree/main/devtools/authwatch")
}

// runHelp is `authwatch help [command]`.
func runHelp(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	if len(args) == 0 {
		writeUsage(stdout)
		return 0
	}
	c, ok := findCommand(args[0])
	if !ok {
		fmt.Fprintf(stderr, "authwatch: unknown command %q\n\n", args[0])
		writeUsage(stderr)
		return errExitCode
	}
	return c.run([]string{"--help"}, stdout, stderr, env, rt)
}

// parseFlags parses a command's flags. -h or --help prints that
// command's usage and flags to stdout; done is then true with code 0.
// A bad flag prints the error and the usage to stderr.
func parseFlags(fs *flag.FlagSet, args []string, stdout, stderr io.Writer) (code int, done bool) {
	fs.SetOutput(stderr)
	fs.Usage = func() {}
	err := fs.Parse(args)
	switch {
	case errors.Is(err, flag.ErrHelp):
		writeCommandUsage(fs, stdout)
		return 0, true
	case err != nil:
		writeCommandUsage(fs, stderr)
		return errExitCode, true
	}
	return 0, false
}

func writeCommandUsage(fs *flag.FlagSet, w io.Writer) {
	name := strings.TrimPrefix(fs.Name(), "authwatch ")
	if c, ok := findCommand(name); ok {
		fmt.Fprintf(w, "usage: authwatch %s %s\n\n%s\n\nFlags:\n", c.name, c.args, c.summary)
	}
	fs.SetOutput(w)
	fs.PrintDefaults()
}
