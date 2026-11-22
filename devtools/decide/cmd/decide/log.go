package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// cmdLog runs decide log subcommands. Only search exists, and it is
// read-only.
func cmdLog(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 || args[0] != "search" {
		return fail(stderr, fmt.Errorf("usage: decide log search [--class c,...] [--all] [--json] <words>"), 2)
	}
	fs := flag.NewFlagSet("log search", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print entries as JSON")
	all := fs.Bool("all", false, "include superseded items")
	classes := fs.String("class", "", "comma-separated classes to search (default decision,fact)")
	if code, stop := stopped(parseFlags(fs, args[1:])); stop {
		return code
	}
	opts := decide.SearchOptions{Words: fs.Args(), All: *all}
	if *classes != "" {
		opts.Classes = strings.Split(*classes, ",")
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	found, err := decide.SearchLog(items, opts)
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *jsonOut {
		if found == nil {
			found = []decide.Item{}
		}
		return writeJSON(stdout, stderr, found)
	}
	for i := range found {
		writeLogEntry(stdout, &found[i])
	}
	return 0
}

func writeLogEntry(w io.Writer, it *decide.Item) {
	fmt.Fprintf(w, "%s\t%s\t%s\n", it.ID, decide.LogTime(it).Local().Format(time.DateOnly), it.Question)
	if a := decide.AnswerText(it); a != "" {
		fmt.Fprintf(w, "\tanswer: %s\n", a)
	}
	if it.Outcome != "" {
		fmt.Fprintf(w, "\toutcome: %s\n", it.Outcome)
	}
	if it.Supersedes != "" {
		fmt.Fprintf(w, "\tsupersedes %s\n", it.Supersedes)
	}
	if it.SupersededBy != "" {
		fmt.Fprintf(w, "\tsuperseded by %s\n", it.SupersededBy)
	}
}

// cmdSupersede runs decide supersede <old> --by <new>.
func cmdSupersede(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("supersede", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print both items as JSON")
	by := fs.String("by", "", "id of the newer item that replaces <old>")
	// Accept the id before or after the flags.
	var id string
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		id, args = args[0], args[1:]
	}
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if id == "" && fs.NArg() == 1 {
		id = fs.Arg(0)
	} else if fs.NArg() != 0 {
		return fail(stderr, fmt.Errorf("decide supersede takes one <old> id"), 2)
	}
	if strings.TrimSpace(id) == "" || strings.TrimSpace(*by) == "" {
		return fail(stderr, fmt.Errorf("usage: decide supersede <old> --by <new>"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	old, newer, err := led.Supersede(id, *by, senderOf(context.Background()))
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, []decide.Item{old, newer})
	}
	fmt.Fprintf(stdout, "%s supersedes %s\n", newer.ID, old.ID)
	return 0
}
