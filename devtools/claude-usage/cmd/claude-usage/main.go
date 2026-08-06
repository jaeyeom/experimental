// Command claude-usage prints Claude Code 5-hour and 7-day usage windows.
//
// Usage:
//
//	claude-usage          # human-readable table
//	claude-usage --json   # stable JSON contract
//
// Exit codes:
//
//	0  success
//	1  unexpected error (network, invalid flags, keychain failure, …)
//	2  no usage data (API-key user, expired creds, custom base URL, or empty API result)
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/usage"
)

const (
	exitOK     = 0
	exitError  = 1
	exitNoData = 2
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr, defaultFetcher()))
}

func defaultFetcher() usage.Fetcher {
	return usage.DefaultClient()
}

func run(args []string, stdout, stderr io.Writer, fetcher usage.Fetcher) int {
	fs := flag.NewFlagSet("claude-usage", flag.ContinueOnError)
	fs.SetOutput(stderr)
	wantJSON := fs.Bool("json", false, "print raw JSON instead of the human-readable table")
	fs.Usage = func() {
		fmt.Fprintf(stderr, "Usage: claude-usage [--json]\n\n")
		fmt.Fprintf(stderr, "Print Claude Code 5-hour and 7-day usage windows.\n\n")
		fs.PrintDefaults()
	}
	if err := fs.Parse(args); err != nil {
		return exitError
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(stderr, "error: unexpected arguments: %v\n", fs.Args())
		fs.Usage()
		return exitError
	}

	err := usage.Write(context.Background(), stdout, fetcher, *wantJSON)
	if err == nil {
		return exitOK
	}
	if errors.Is(err, usage.ErrNoData) || errors.Is(err, usage.ErrCustomEndpoint) {
		switch {
		case errors.Is(err, usage.ErrCustomEndpoint):
			fmt.Fprintf(stderr, "no usage data (custom ANTHROPIC_BASE_URL; oauth usage api not applicable)\n")
		default:
			fmt.Fprintf(stderr, "no usage data (API-key user, expired creds, or API unavailable)\n")
		}
		return exitNoData
	}
	if errors.Is(err, usage.ErrUnauthorized) {
		fmt.Fprintf(stderr, "no usage data (oauth token rejected; re-login with Claude Code)\n")
		return exitNoData
	}
	fmt.Fprintf(stderr, "error: %v\n", err)
	return exitError
}
