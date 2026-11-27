package main

import (
	"context"
	"errors"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/quotapace"
	"github.com/jaeyeom/experimental/devtools/quotapace/internal/collect/claude"
	"github.com/jaeyeom/experimental/devtools/quotapace/internal/collect/claude/credentials"
)

const exitCollectNoData = 2

type collectClaudeOpts struct {
	format    claude.OutputFormat
	profile   string
	autoStart bool
}

func runCollectClaude(args []string, store quotapace.Store, e env) (int, error) {
	opts, err := parseCollectClaudeOpts(args, e)
	if err != nil {
		return 0, err
	}
	data, code, err := fetchClaudeData(e)
	if err != nil || code != 0 {
		return code, err
	}
	if opts.profile != "" {
		code, err := recordClaudeReadings(store, data, opts, e)
		if err != nil || code != 0 {
			return code, err
		}
	}
	return writeClaudeOutput(data, opts.format, e)
}

func parseCollectClaudeOpts(args []string, e env) (collectClaudeOpts, error) {
	fs := newFlags("collect claude", e)
	wantJSON := fs.Bool("json", false, "print raw JSON instead of the human-readable summary")
	wantJSONL := fs.Bool("jsonl", false, "print JSON lines for quotapace record --stdin")
	wantQuotapace := fs.Bool("quotapace", false, "alias for --jsonl")
	profile := fs.String("profile", "", "record collected readings into this quotapace profile")
	recordFlag := fs.Bool("record", false, "record collected readings into --profile")
	autoStart := fs.Bool("auto-start", false, "start a fresh plan when the window resets (requires --profile)")
	if err := parse(fs, args); err != nil {
		return collectClaudeOpts{}, err
	}
	wantLines := *wantJSONL || *wantQuotapace
	if *wantJSON && wantLines {
		return collectClaudeOpts{}, usageErr("--json and --jsonl/--quotapace are mutually exclusive")
	}
	if (*recordFlag || *autoStart) && *profile == "" {
		return collectClaudeOpts{}, usageErr("--profile is required when recording")
	}
	if *profile != "" {
		if err := quotapace.CheckProfileName(*profile); err != nil {
			return collectClaudeOpts{}, usageErr("%v", err)
		}
	}
	format := claude.OutputHuman
	switch {
	case *wantJSON:
		format = claude.OutputJSON
	case wantLines:
		format = claude.OutputQuotapace
	}
	return collectClaudeOpts{
		format:    format,
		profile:   *profile,
		autoStart: *autoStart,
	}, nil
}

func fetchClaudeData(e env) (*claude.Data, int, error) {
	fetcher := newClaudeFetcher(e)
	ctx := e.ctx
	if ctx == nil {
		ctx = context.Background()
	}
	data, err := fetcher.Fetch(ctx)
	if err != nil {
		if code, handled := handleClaudeFetchError(err, e); handled {
			return nil, code, nil
		}
		return nil, 0, fmt.Errorf("collect claude: %w", err)
	}
	if data == nil {
		fmt.Fprintln(e.stderr, "no usage data (API-key user, expired creds, or API unavailable)")
		return nil, exitCollectNoData, nil
	}
	return data, 0, nil
}

func recordClaudeReadings(store quotapace.Store, data *claude.Data, opts collectClaudeOpts, e env) (int, error) {
	readings, err := claude.Readings(data, opts.profile, e.now())
	if err != nil {
		if errors.Is(err, claude.ErrNoData) {
			fmt.Fprintln(e.stderr, "no usage data (no window with both percent and reset time)")
			return exitCollectNoData, nil
		}
		return 0, fmt.Errorf("collect claude readings: %w", err)
	}
	recordOut := e.stdout
	if opts.format != claude.OutputHuman {
		recordOut = e.stderr
	}
	for _, r := range readings {
		if err := recordOne(store, r, opts.autoStart, recordOut); err != nil {
			return 0, err
		}
	}
	return 0, nil
}

func writeClaudeOutput(data *claude.Data, format claude.OutputFormat, e env) (int, error) {
	if err := claude.Format(e.stdout, data, format); err != nil {
		if errors.Is(err, claude.ErrNoData) {
			fmt.Fprintln(e.stderr, "no usage data (no window with both percent and reset time)")
			return exitCollectNoData, nil
		}
		return 0, fmt.Errorf("format claude usage: %w", err)
	}
	return 0, nil
}

func newClaudeFetcher(e env) claude.Fetcher {
	if e.collectClaude != nil {
		return e.collectClaude()
	}
	return &claude.Client{
		Tokens: &credentials.Resolver{
			Env: e.getenv,
			Now: e.now,
		},
		Env: e.getenv,
		Now: e.now,
	}
}

func handleClaudeFetchError(err error, e env) (int, bool) {
	switch {
	case errors.Is(err, claude.ErrCustomEndpoint):
		fmt.Fprintln(e.stderr, "no usage data (custom ANTHROPIC_BASE_URL; oauth usage api not applicable)")
		return exitCollectNoData, true
	case errors.Is(err, claude.ErrUnauthorized):
		fmt.Fprintln(e.stderr, "no usage data (oauth token rejected; re-login with Claude Code)")
		return exitCollectNoData, true
	case errors.Is(err, claude.ErrNoData):
		fmt.Fprintln(e.stderr, "no usage data (API-key user, expired creds, or API unavailable)")
		return exitCollectNoData, true
	default:
		return 0, false
	}
}
