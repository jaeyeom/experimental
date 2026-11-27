package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

const cronUsage = "orgctl cron publish --role ROLE --file FILE [--state-dir DIR]"

func runCron(args []string, stdin io.Reader, stderr io.Writer, now func() time.Time) int {
	if len(args) < 2 || args[1] != "publish" {
		fmt.Fprintln(stderr, "usage: "+cronUsage)
		return 2
	}
	fs := flag.NewFlagSet("cron publish", flag.ContinueOnError)
	fs.SetOutput(stderr)
	role := fs.String("role", "", "role name publishing its live cron list")
	file := fs.String("file", "", "JSON file to read, or - for stdin")
	stateDir := fs.String("state-dir", "", "cron state directory (default $XDG_STATE_HOME/orgctl/crons)")
	if err := fs.Parse(args[2:]); err != nil {
		return 2
	}
	if *role == "" || *file == "" || fs.NArg() != 0 {
		fmt.Fprintln(stderr, "usage: "+cronUsage)
		return 2
	}
	var data []byte
	var err error
	if *file == "-" {
		data, err = io.ReadAll(stdin)
	} else {
		data, err = os.ReadFile(*file) //nolint:gosec // G304: the caller named the cron input file
	}
	if err != nil {
		fmt.Fprintf(stderr, "read crons: %v\n", err)
		return 1
	}
	crons, err := orgctl.ParsePublishedCrons(data)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if now == nil {
		now = time.Now
	}
	if _, err := orgctl.PublishCrons(*stateDir, *role, crons, now().UTC()); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}
