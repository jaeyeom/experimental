package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func cmdStats(args []string, stdout, stderr io.Writer) int {
	return cmdStatsAt(args, stdout, stderr, time.Now())
}

func cmdStatsAt(args []string, stdout, stderr io.Writer, now time.Time) int {
	if len(args) == 0 || args[0] != "answers" {
		return fail(stderr, fmt.Errorf("usage: decide stats answers [--since <duration or date>] [--until <duration or date>] [--agent <name>] [--json]"), 2)
	}
	fs := flag.NewFlagSet("stats answers", flag.ContinueOnError)
	fs.SetOutput(stderr)
	sinceRaw := fs.String("since", "", "include answers at or after this duration ago (30m, 6h, 2d), YYYY-MM-DD date, or RFC3339 time")
	untilRaw := fs.String("until", "", "include answers at or before this duration ago (30m, 6h, 2d), YYYY-MM-DD date, or RFC3339 time")
	agent := fs.String("agent", "", "filter to one asking agent")
	jsonOut := fs.Bool("json", false, "print rows as JSON with item IDs")
	if code, stop := stopped(parseFlags(fs, args[1:])); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide stats answers does not take arguments"), 2)
	}
	opts, err := parseStatsOptions(*sinceRaw, *untilRaw, *agent, now)
	if err != nil {
		return fail(stderr, err, 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	rows := decide.ComputeAnswerStats(items, opts)
	if *jsonOut {
		return writeJSON(stdout, stderr, rows)
	}
	if err := writeAnswerStatsTable(stdout, rows); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

func parseStatsOptions(sinceRaw, untilRaw, agent string, now time.Time) (decide.AnswerStatsOptions, error) {
	var opts decide.AnswerStatsOptions
	opts.Agent = strings.TrimSpace(agent)
	if strings.TrimSpace(sinceRaw) != "" {
		ts, err := parseStatsTime(strings.TrimSpace(sinceRaw), false, now)
		if err != nil {
			return decide.AnswerStatsOptions{}, fmt.Errorf("decide stats answers --since %q: %w", sinceRaw, err)
		}
		opts.Since = &ts
	}
	if strings.TrimSpace(untilRaw) != "" {
		ts, err := parseStatsTime(strings.TrimSpace(untilRaw), true, now)
		if err != nil {
			return decide.AnswerStatsOptions{}, fmt.Errorf("decide stats answers --until %q: %w", untilRaw, err)
		}
		opts.Until = &ts
	}
	if opts.Since != nil && opts.Until != nil && opts.Since.After(*opts.Until) {
		return decide.AnswerStatsOptions{}, errors.New("decide stats answers: --since must be before or equal to --until")
	}
	return opts, nil
}

func parseStatsTime(raw string, isUntil bool, now time.Time) (time.Time, error) {
	if ts, err := time.Parse(time.RFC3339Nano, raw); err == nil {
		return ts.UTC(), nil
	}
	if ts, err := time.ParseInLocation(time.DateOnly, raw, time.Local); err == nil {
		if isUntil {
			return ts.AddDate(0, 0, 1).Add(-time.Nanosecond).UTC(), nil
		}
		return ts.UTC(), nil
	}
	d, ok := parseStatsDuration(raw)
	if !ok || d <= 0 {
		return time.Time{}, errors.New("want a duration like 30m, 6h, 2d, a YYYY-MM-DD date, or an RFC3339 time")
	}
	return now.Add(-d).UTC(), nil
}

func parseStatsDuration(s string) (time.Duration, bool) {
	var days time.Duration
	if before, after, found := strings.Cut(s, "d"); found {
		n, err := strconv.ParseUint(before, 10, 16)
		if err != nil {
			return 0, false
		}
		days = time.Duration(n) * 24 * time.Hour
		if after == "" {
			return days, true
		}
		s = after
	}
	d, err := time.ParseDuration(s)
	if err != nil || d < 0 {
		return 0, false
	}
	return days + d, true
}

func writeAnswerStatsTable(w io.Writer, rows []decide.AnswerStatsRow) error {
	if len(rows) == 0 {
		return nil
	}
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	if _, err := fmt.Fprintln(tw, "AGENT\tOPTION\tFREE-OVER-OPTIONS\tFREE-ONLY\tUNKNOWN\tFREE-SHARE\tTOTAL"); err != nil {
		return fmt.Errorf("write stats header: %w", err)
	}
	for _, r := range rows {
		share := "-"
		if r.Option+r.FreeTextOverOptions > 0 {
			share = fmt.Sprintf("%.1f%%", r.FreeTextShare*100)
		}
		if _, err := fmt.Fprintf(tw, "%s\t%d\t%d\t%d\t%d\t%s\t%d\n",
			r.Agent, r.Option, r.FreeTextOverOptions, r.FreeTextOnly, r.Unknown, share, r.Total); err != nil {
			return fmt.Errorf("write stats row: %w", err)
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("flush stats table: %w", err)
	}
	return nil
}
