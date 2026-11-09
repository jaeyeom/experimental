package main

import (
	"context"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

func cmdList(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintf(stderr, "agentbus list: unexpected arguments\n")
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "list"}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	printRows(stdout, resp.Rows)
	if resp.Text == "" {
		return 0
	}
	if len(resp.Rows) > 0 {
		fmt.Fprintln(stdout)
	}
	fmt.Fprint(stdout, resp.Text)
	return 0
}

func cmdSearch(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	for _, arg := range args {
		if strings.HasPrefix(arg, "-") {
			fmt.Fprintf(stderr, "agentbus search: unexpected flag %s\n", arg)
			return 2
		}
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "search", Query: strings.Join(args, " ")}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	printRows(stdout, resp.Rows)
	return 0
}

func printRows(stdout io.Writer, rows []daemon.AgentRow) {
	for i, row := range rows {
		if i > 0 {
			fmt.Fprintln(stdout)
		}
		printField(stdout, "name", row.Name)
		printField(stdout, "when", row.When)
		printField(stdout, "liveness", row.Liveness)
		printField(stdout, "transport", row.Transport)
		printField(stdout, "handle", row.Handle)
		printField(stdout, "label", row.Label)
		printField(stdout, "title", row.Title)
	}
}

func printField(stdout io.Writer, key, value string) {
	if value == "" {
		return
	}
	fmt.Fprintf(stdout, "%s: %s\n", key, value)
}
