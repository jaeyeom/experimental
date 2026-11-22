package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// backfillIn is where cmdBackfill reads the approval of the batch.
var backfillIn io.Reader

// cmdBackfill runs decide backfill [--yes]: it prints a proposed class for
// each existing decision and retags them only after the batch is approved.
func cmdBackfill(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("backfill", flag.ContinueOnError)
	fs.SetOutput(stderr)
	yes := fs.Bool("yes", false, "approve the batch without asking")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if fs.NArg() != 0 {
		return fail(stderr, fmt.Errorf("decide backfill takes no arguments"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	props := decide.ProposeBackfill(items)
	if len(props) == 0 {
		fmt.Fprintln(stdout, "nothing to retag")
		return 0
	}
	for _, p := range props {
		fmt.Fprintf(stdout, "%s\t%s -> %s\t%s\n", p.ID, p.From, p.To, p.Reason)
	}
	if !*yes {
		in := backfillIn
		if in == nil {
			in = os.Stdin
		}
		fmt.Fprintf(stdout, "Retag these %d items? [y/N] ", len(props))
		line, _ := bufio.NewReader(in).ReadString('\n')
		if a := strings.ToLower(strings.TrimSpace(line)); a != "y" && a != "yes" {
			fmt.Fprintln(stdout, "\nnot applied")
			return 0
		}
	}
	n, err := led.ApplyBackfill(props, senderOf(context.Background()))
	if err != nil {
		return fail(stderr, err, 1)
	}
	fmt.Fprintf(stdout, "retagged %d\n", n)
	return 0
}
