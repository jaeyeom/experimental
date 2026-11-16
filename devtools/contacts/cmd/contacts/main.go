// Command contacts is a personal contact and relationship store that coding
// agents can query: who someone is, what happened with them before, and what
// was promised either way.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// Exit codes shared by every command.
const (
	exitOK        = 0
	exitError     = 1
	exitUsage     = 2 // also "no such person"
	exitAmbiguous = 3
)

func main() {
	a := &app{stdout: os.Stdout, stderr: os.Stderr, getenv: os.Getenv, now: time.Now}
	os.Exit(a.run(os.Args[1:]))
}

// app holds everything a command touches outside the store, so tests can
// fix the clock and the environment.
type app struct {
	stdout, stderr io.Writer
	getenv         func(string) string
	now            func() time.Time
	configFlag     string
}

// commands maps each command name to its handler.
var commands = map[string]func(a *app, args []string) int{
	"init":   (*app).cmdInit,
	"add":    (*app).cmdAdd,
	"edit":   (*app).cmdEdit,
	"merge":  (*app).cmdMerge,
	"delete": (*app).cmdDelete,
}

// usages holds one usage line per command, in help order.
var usages = []struct{ name, line string }{
	{"init", "contacts init"},
	{"add", "contacts add <name> [--alias A]... [--handle kind:value]... [--team T] [--role R] [--tz Z] [--channel C]"},
	{"edit", "contacts edit <person> [add flags] [--remove-handle kind:value]... [--remove-alias A]..."},
	{"merge", "contacts merge <keep> <drop>"},
	{"delete", "contacts delete <person> --yes"},
}

func usageOf(name string) string {
	for _, u := range usages {
		if u.name == name {
			return u.line
		}
	}
	return "contacts " + name
}

func (a *app) run(args []string) int {
	fs := flag.NewFlagSet("contacts", flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	fs.Usage = func() { a.usage() }
	fs.StringVar(&a.configFlag, "config", "", "config file (default $CONTACTS_CONFIG or $XDG_CONFIG_HOME/contacts/config.toml)")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	args = fs.Args()
	if len(args) == 0 {
		a.usage()
		return exitUsage
	}
	if args[0] == "help" {
		a.usage()
		return exitOK
	}
	cmd, ok := commands[args[0]]
	if !ok {
		fmt.Fprintf(a.stderr, "contacts: unknown command %q\n", args[0])
		a.usage()
		return exitUsage
	}
	return cmd(a, args[1:])
}

func (a *app) usage() {
	fmt.Fprintln(a.stderr, "Usage: contacts [--config path] <command> [args]")
	fmt.Fprintln(a.stderr, "\nCommands:")
	for _, u := range usages {
		fmt.Fprintf(a.stderr, "  %s\n", u.line)
	}
}

func (a *app) config() (contacts.Config, error) {
	path, err := contacts.ConfigPath(a.configFlag, a.getenv)
	if err != nil {
		return contacts.Config{}, fmt.Errorf("config path: %w", err)
	}
	cfg, err := contacts.LoadConfig(path, a.getenv)
	if err != nil {
		return cfg, fmt.Errorf("load config: %w", err)
	}
	return cfg, nil
}

func (a *app) errorf(format string, args ...any) {
	fmt.Fprintf(a.stderr, "contacts: "+format+"\n", args...)
}

// flags returns a flag set for one command that reports usage errors on
// stderr.
func (a *app) flags(name string) *flag.FlagSet {
	fs := flag.NewFlagSet("contacts "+name, flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	fs.Usage = func() { fmt.Fprintf(a.stderr, "Usage: %s\n", usageOf(name)) }
	return fs
}

// parse parses args with flags and positionals in any order, and checks the
// number of positionals is between minPos and maxPos. On failure it prints
// the problem and returns false; the caller exits with exitUsage.
func (a *app) parse(fs *flag.FlagSet, args []string, minPos, maxPos int) ([]string, bool) {
	var pos []string
	for {
		if err := fs.Parse(args); err != nil {
			return nil, false
		}
		args = fs.Args()
		if len(args) == 0 {
			break
		}
		pos = append(pos, args[0])
		args = args[1:]
	}
	if len(pos) < minPos || len(pos) > maxPos {
		fs.Usage()
		return nil, false
	}
	return pos, true
}

func (a *app) cmdInit(args []string) int {
	fs := a.flags("init")
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	cfg, err := a.config()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if err := contacts.Init(cfg.DataDir); err != nil {
		a.errorf("%v", err)
		return exitError
	}
	fmt.Fprintf(a.stdout, "initialized %s\n", cfg.DataDir)
	return exitOK
}
