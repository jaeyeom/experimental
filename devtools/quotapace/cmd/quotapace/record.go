package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func runRecord(args []string, store quotapace.Store, e env) error {
	fs := newFlags("record", e)
	profile := fs.String("profile", "", "profile (account or machine)")
	window := fs.String("window", "", "window, such as 5h or 7d")
	percent := fs.Float64("percent", -1, "percent used")
	resetsIn := fs.String("resets-in", "", "time until the reset, such as 6d5h")
	resetsAt := fs.String("resets-at", "", "reset time, RFC 3339")
	source := fs.String("source", "", "where the reading came from")
	atFlag := fs.String("at", "", "reading time, RFC 3339 (default now)")
	stdinFlag := fs.Bool("stdin", false, "read readings from standard input")
	format := fs.String("format", "jsonl", "stdin format: jsonl or claude-usage")
	autoStart := fs.Bool("auto-start", false, "start a fresh plan when the window resets")
	if err := parse(fs, args); err != nil {
		return err
	}
	if *percent >= 0 && *stdinFlag {
		return usageErr("pass --percent or --stdin, not both")
	}
	now := e.now()
	at, err := parseTime("at", *atFlag)
	if err != nil {
		return err
	}
	if at.IsZero() {
		at = now
	}
	var readings []quotapace.Reading
	if *percent >= 0 {
		resets, err := resetTime(*resetsAt, *resetsIn, at)
		if err != nil {
			return err
		}
		if *profile == "" || *window == "" || resets.IsZero() {
			return usageErr("record needs --profile, --window, and --resets-in or --resets-at")
		}
		readings = []quotapace.Reading{{At: at, Profile: *profile, Window: *window, Percent: *percent, ResetsAt: resets, Source: *source}}
	} else {
		readings, err = readStdin(e.stdin, *format, *profile, *source, at)
		if err != nil {
			return err
		}
	}
	for _, r := range readings {
		if err := recordOne(store, r, *autoStart, e.stdout); err != nil {
			return err
		}
	}
	return nil
}

func recordOne(store quotapace.Store, r quotapace.Reading, autoStart bool, stdout io.Writer) error {
	res, err := store.Record(r, autoStart)
	if err != nil {
		return fmt.Errorf("record %s %s: %w", r.Profile, r.Window, err)
	}
	if c := res.Closed; c != nil {
		fmt.Fprintf(stdout, "%s %s: window reset (%s); plan closed at %.0f%%\n", r.Profile, r.Window, c.Closed.Reason, c.Closed.Final)
		if res.Started == nil {
			fmt.Fprintf(stdout, "start a new plan: quotapace plan start --profile %s --window %s\n", r.Profile, r.Window)
		} else {
			fmt.Fprintf(stdout, "%s %s: started a new plan to the reset %s\n", r.Profile, r.Window, res.Started.ResetsAt.Format(time.RFC3339))
		}
	}
	return nil
}

func readStdin(r io.Reader, format, profile, source string, now time.Time) ([]quotapace.Reading, error) {
	switch format {
	case "claude-usage":
		if profile == "" {
			return nil, usageErr("--format claude-usage needs --profile")
		}
		return readClaudeUsage(r, profile, source, now)
	case "jsonl":
		return readJSONL(r, profile, source, now)
	default:
		return nil, usageErr("--format %q: want jsonl or claude-usage", format)
	}
}

func readJSONL(r io.Reader, profile, source string, now time.Time) ([]quotapace.Reading, error) {
	readings, err := quotapace.ParseReadingsJSONL(r, profile, source, now)
	if err != nil {
		if errors.Is(err, quotapace.ErrInvalidInput) {
			return nil, fmt.Errorf("%w: %w", errUsage, err)
		}
		return nil, fmt.Errorf("parse stdin jsonl: %w", err)
	}
	return readings, nil
}

// claudeUsage is the object `claude-usage --json` prints.
type claudeUsage struct {
	FiveHour        *float64   `json:"fiveHour"`
	SevenDay        *float64   `json:"sevenDay"`
	FiveHourResetAt *time.Time `json:"fiveHourResetAt"`
	SevenDayResetAt *time.Time `json:"sevenDayResetAt"`
	FetchedAt       *time.Time `json:"fetchedAt"`
	Source          string     `json:"source"`
}

func readClaudeUsage(r io.Reader, profile, source string, now time.Time) ([]quotapace.Reading, error) {
	var u claudeUsage
	if err := json.NewDecoder(r).Decode(&u); err != nil {
		return nil, usageErr("claude-usage input: %v", err)
	}
	at := now
	if u.FetchedAt != nil && !u.FetchedAt.IsZero() {
		at = u.FetchedAt.UTC()
	}
	if source == "" {
		source = u.Source
	}
	if source == "" {
		source = "claude-usage"
	}
	var out []quotapace.Reading
	add := func(window string, pct *float64, resets *time.Time) {
		if pct != nil && resets != nil {
			out = append(out, quotapace.Reading{At: at, Profile: profile, Window: window, Percent: *pct, ResetsAt: *resets, Source: source})
		}
	}
	add("5h", u.FiveHour, u.FiveHourResetAt)
	add("7d", u.SevenDay, u.SevenDayResetAt)
	if len(out) == 0 {
		return nil, errors.New("claude-usage input has no window with both a percent and a reset time")
	}
	return out, nil
}
