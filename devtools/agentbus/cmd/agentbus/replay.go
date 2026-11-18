package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

func cmdReleaseHeld(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("release-held", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 1 {
		fmt.Fprintln(stderr, "agentbus release-held: usage: agentbus release-held <name>")
		return 2
	}
	target := fs.Arg(0)
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "release-held", To: target}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus release-held: %s\n", resp.Error)
		return 1
	}
	fmt.Fprintf(stdout, "released %d held message(s) for %s\n", resp.Released, target)
	return 0
}

func cmdReplay(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("replay", flag.ContinueOnError)
	fs.SetOutput(stderr)
	to := fs.String("to", "", "filter by recipient name or handle")
	since := fs.String("since", "", "include messages sent at or after this time (30m, 6h, 2d, RFC3339, or YYYY-MM-DD)")
	reason := fs.String("reason", "", "filter by unprocessed reason: auth or no-turn")
	dryRun := fs.Bool("dry-run", false, "list messages that would be replayed without sending")
	force := fs.Bool("force", false, "replay messages even if already replayed")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "agentbus replay: unexpected arguments")
		return 2
	}
	if *reason != "" && *reason != "auth" && *reason != "no-turn" {
		fmt.Fprintf(stderr, "agentbus replay: invalid --reason %q (want auth or no-turn)\n", *reason)
		return 2
	}
	var sinceRFC string
	if *since != "" {
		ts, err := parseSince(*since, logNow())
		if err != nil {
			fmt.Fprintf(stderr, "agentbus replay: --since: %s\n", err)
			return 2
		}
		sinceRFC = ts.UTC().Format(time.RFC3339Nano)
	}
	resp, code := callDaemon(ctx, env, daemon.Request{
		Op:     "replay",
		To:     *to,
		Since:  sinceRFC,
		Reason: *reason,
		DryRun: *dryRun,
		Force:  *force,
	}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus replay: %s\n", resp.Error)
		return 1
	}
	for _, entry := range resp.Replayed {
		if *dryRun {
			fmt.Fprintf(stdout, "would replay %s to %s (%s)\n", entry.OriginalID, entry.To, entry.Reason)
		} else {
			fmt.Fprintf(stdout, "replayed %s as %s to %s (%s): %s\n", entry.OriginalID, entry.NewID, entry.To, entry.Reason, entry.State)
		}
	}
	if len(resp.Replayed) == 0 {
		fmt.Fprintln(stdout, "no unprocessed messages matched")
	}
	return 0
}
