// Command slackwatch tracks Slack posts made on a user's behalf and pushes
// replies to whoever owns each post. One binary is both the server
// (slackwatch serve) and the CLI.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("slackwatch", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() { usage(stderr) }
	configFlag := fs.String("config", "", "config file (default $SLACKWATCH_CONFIG or ~/.config/slackwatch/config.json)")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}
	args = fs.Args()
	if len(args) == 0 {
		usage(stderr)
		return 2
	}
	configPath := slackwatch.ConfigPath(*configFlag)
	switch args[0] {
	case "help", "-h", "--help":
		usage(stderr)
		return 0
	}
	cmd, ok := commands[args[0]]
	if !ok {
		fmt.Fprintf(stderr, "slackwatch: unknown command %q\n", args[0])
		usage(stderr)
		return 2
	}
	return cmd(configPath, args[1:], stdout, stderr)
}

var commands = map[string]func(configPath string, args []string, stdout, stderr io.Writer) int{
	"serve":   cmdServe,
	"add":     cmdAdd,
	"claim":   cmdClaim,
	"done":    cmdDone,
	"snooze":  cmdSnooze,
	"ack":     cmdAck,
	"ls":      cmdLs,
	"show":    cmdShow,
	"sub":     cmdSub,
	"unsub":   cmdUnsub,
	"subs":    cmdSubs,
	"status":  cmdStatus,
	"inbound": cmdInbound,
}

func usage(w io.Writer) {
	fmt.Fprint(w, `Usage:
  slackwatch [--config path] <command> [args]

Commands:
  slackwatch serve    run the watcher (Socket Mode, catch-up, sweep)
  slackwatch add <post> [--owner T] [--purpose S] [--ref R ...] [--until DATE] [--source outbox|hook|manual]
                      watch a post; a thread reply is watched on its parent thread
  slackwatch claim <post> --owner T [--purpose S] [--ref R ...]
                      take an unowned or orphaned watch
  slackwatch done <post> [--reason S]
  slackwatch snooze <post> --until DATE
  slackwatch ack <post>
                      mark every stored reply acknowledged
  slackwatch ls [--owner T] [--state S] [--unacked] [--json]
  slackwatch show <post> [--json]
  slackwatch sub <channel> --owner T [--purpose S] [--until DATE] [--include-self]
                      notify T of every new message in a channel
  slackwatch unsub <channel> [--owner T]
  slackwatch subs [--json]
  slackwatch status [--json]
                      server heartbeat; exits 1 when down, stale, or undelivered
  slackwatch inbound [--since DUR|DATE] [--json]
                      recorded DMs and mentions
  slackwatch help     show this help

<post> is a Slack permalink, a channel and ts, or a watch id.
<channel> is a channel id or #name.
DATE is YYYY-MM-DD (through the end of that day), RFC 3339, or a duration such as 72h.
`)
}

func fail(stderr io.Writer, err error) int {
	fmt.Fprintf(stderr, "slackwatch: %v\n", err)
	return 1
}
