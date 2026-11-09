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
	case "serve":
		return cmdServe(configPath, args[1:], stdout, stderr)
	case "help", "-h", "--help":
		usage(stderr)
		return 0
	default:
		fmt.Fprintf(stderr, "slackwatch: unknown command %q\n", args[0])
		usage(stderr)
		return 2
	}
}

func usage(w io.Writer) {
	fmt.Fprint(w, `Usage:
  slackwatch [--config path] <command> [args]

Commands:
  slackwatch serve    run the watcher (Socket Mode, catch-up, sweep)
  slackwatch help     show this help
`)
}

func cmdServe(configPath string, args []string, _, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintln(stderr, "slackwatch serve: takes no arguments")
		return 2
	}
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return fail(stderr, err)
	}
	store, err := slackwatch.OpenStore(cfg.DBPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	return fail(stderr, errors.New("serve: Slack connection is not implemented yet"))
}

func fail(stderr io.Writer, err error) int {
	fmt.Fprintf(stderr, "slackwatch: %v\n", err)
	return 1
}
