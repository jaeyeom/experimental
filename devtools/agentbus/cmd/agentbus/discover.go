package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// listEntry is one object of `agentbus list --json`. The field names are
// stable.
type listEntry struct {
	Name     string `json:"name"`
	When     string `json:"when"`
	Liveness string `json:"liveness"`
	// LivenessAge is the seconds since liveness was observed; null when
	// it never was.
	LivenessAge *int64 `json:"liveness_age"` //nolint:tagliatelle // the stable list --json field name
	Transport   string `json:"transport"`
	Handle      string `json:"handle"`
}

// jsonLiveness folds the text liveness words into alive, dead,
// hibernated, or unknown. A blocked agent is alive: it is waiting on a
// prompt.
func jsonLiveness(word string) string {
	switch word {
	case "alive", "blocked":
		return "alive"
	case "hibernated":
		return "hibernated"
	case "gone":
		return "dead"
	default:
		return "unknown"
	}
}

func cmdList(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	var asJSON, fresh bool
	for _, arg := range args {
		switch {
		case arg == "--json" && !asJSON:
			asJSON = true
		case arg == "--fresh" && !fresh:
			fresh = true
		default:
			fmt.Fprintf(stderr, "agentbus list: unexpected arguments\n")
			return 2
		}
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "list", Fresh: fresh}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	if asJSON {
		entries := make([]listEntry, 0, len(resp.Rows))
		for _, row := range resp.Rows {
			entries = append(entries, listEntry{
				Name: row.Name, When: row.When, Liveness: jsonLiveness(row.Liveness),
				LivenessAge: row.LivenessAge, Transport: row.Transport, Handle: row.Handle,
			})
		}
		if err := json.NewEncoder(stdout).Encode(entries); err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
		return 0
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
	// Text is one warning per machine that could not be listed. The
	// search still succeeds with the agents it found.
	for _, line := range strings.Split(strings.TrimSuffix(resp.Text, "\n"), "\n") {
		if line != "" {
			fmt.Fprintf(stderr, "agentbus: %s\n", line)
		}
	}
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
