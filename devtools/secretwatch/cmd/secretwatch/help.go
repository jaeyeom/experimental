package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"strings"
)

type command struct {
	name    string
	args    string
	summary string
	run     func(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int
}

func commands() []command {
	return []command{
		{"check", "[--since TIME] [--follow] [--json] [--config PATH]", "scan new journal entries and notify once per finding", cmdCheck},
		{"scan", "--since TIME [--until TIME] [--unit UNIT] [--json] [--config PATH]", "one-shot historical scan without changing state", cmdScan},
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

func writeUsage(w io.Writer) {
	fmt.Fprintln(w, "usage: secretwatch <command> [flags]")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Watch systemd journals for leaked secrets and alert without exposing them.")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Commands:")
	for _, c := range commands() {
		fmt.Fprintf(w, "  %-15s %s\n", c.name, c.summary)
		fmt.Fprintf(w, "  %-15s   secretwatch %s %s\n", "", c.name, c.args)
	}
	fmt.Fprintln(w, "  help [command]  print this help, or one command's flags")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Config: --config PATH, else $XDG_CONFIG_HOME/secretwatch/config.pkl")
	fmt.Fprintln(w, "(~/.config on Linux, ~/Library/Application Support on macOS).")
	fmt.Fprintln(w, "README: https://github.com/jaeyeom/experimental/tree/main/devtools/secretwatch")
}

func runHelp(args []string, stdout, stderr io.Writer, env []string, rt appRuntime) int {
	if len(args) == 0 {
		writeUsage(stdout)
		return 0
	}
	c, ok := findCommand(args[0])
	if !ok {
		fmt.Fprintf(stderr, "secretwatch: unknown command %q\n\n", args[0])
		writeUsage(stderr)
		return errExitCode
	}
	return c.run([]string{"--help"}, stdout, stderr, env, rt)
}

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
	name := strings.TrimPrefix(fs.Name(), "secretwatch ")
	if c, ok := findCommand(name); ok {
		fmt.Fprintf(w, "usage: secretwatch %s %s\n\n%s\n\nFlags:\n", c.name, c.args, c.summary)
	}
	fs.SetOutput(w)
	fs.PrintDefaults()
}
