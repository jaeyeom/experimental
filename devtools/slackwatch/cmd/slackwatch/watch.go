package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

// catchUpper fetches new traffic for one watch from Slack.
type catchUpper interface {
	CatchUpWatch(ctx context.Context, w slackwatch.Watch) error
}

// newCatchUp builds the Slack-backed catch-up that add runs. Tests replace
// it with a fake.
var newCatchUp = func(cfg slackwatch.Config, store *slackwatch.Store, stderr io.Writer) (catchUpper, error) {
	return slackCatchUp(cfg, store, stderr)
}

var (
	states  = []string{slackwatch.StateOpen, slackwatch.StateUnowned, slackwatch.StateOrphaned, slackwatch.StateClosed}
	sources = []string{slackwatch.SourceOutbox, slackwatch.SourceHook, slackwatch.SourceManual}
)

// stringList is a repeatable string flag.
type stringList []string

func (l *stringList) String() string { return strings.Join(*l, ",") }

func (l *stringList) Set(v string) error {
	*l = append(*l, v)
	return nil
}

// optString is a string flag that records whether it was set.
type optString struct {
	v   string
	set bool
}

func (o *optString) String() string { return o.v }

func (o *optString) Set(v string) error {
	o.v, o.set = v, true
	return nil
}

func (o *optString) ptr() *string {
	if !o.set {
		return nil
	}
	return &o.v
}

// watchFlags are the flags add and claim share.
type watchFlags struct {
	owner, purpose optString
	refs           stringList
}

func (f *watchFlags) register(fs *flag.FlagSet) {
	fs.Var(&f.owner, "owner", "owner name, resolved through config targets")
	fs.Var(&f.purpose, "purpose", "what the post is for")
	fs.Var(&f.refs, "ref", "reference such as an issue or PR (repeatable)")
}

func (f *watchFlags) update() slackwatch.WatchUpdate {
	u := slackwatch.WatchUpdate{Owner: f.owner.ptr(), Purpose: f.purpose.ptr()}
	if len(f.refs) > 0 {
		u.Refs = f.refs
	}
	return u
}

// parseInterspersed parses fs from args, allowing flags after positional
// arguments, and returns the positional arguments.
func parseInterspersed(fs *flag.FlagSet, args []string) ([]string, error) {
	var pos []string
	for {
		if err := fs.Parse(args); err != nil {
			return nil, err //nolint:wrapcheck // The flag package already printed it.
		}
		args = fs.Args()
		if len(args) == 0 {
			return pos, nil
		}
		pos = append(pos, args[0])
		args = args[1:]
	}
}

// parseUntil accepts YYYY-MM-DD (through the end of that local day),
// RFC 3339, or a Go duration from now.
func parseUntil(s string, now time.Time) (time.Time, error) {
	if d, err := time.ParseInLocation(time.DateOnly, s, time.Local); err == nil {
		return d.AddDate(0, 0, 1), nil
	}
	if t, err := time.Parse(time.RFC3339, s); err == nil {
		return t, nil
	}
	if d, err := time.ParseDuration(s); err == nil {
		return now.Add(d), nil
	}
	return time.Time{}, fmt.Errorf("invalid date %q: want YYYY-MM-DD, RFC 3339, or a duration such as 72h", s)
}

func newFlagSet(name string, stderr io.Writer) *flag.FlagSet {
	fs := flag.NewFlagSet("slackwatch "+name, flag.ContinueOnError)
	fs.SetOutput(stderr)
	return fs
}

func openStore(configPath string) (slackwatch.Config, *slackwatch.Store, error) {
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return cfg, nil, err //nolint:wrapcheck // Already descriptive.
	}
	store, err := slackwatch.OpenStore(cfg.DBPath)
	return cfg, store, err //nolint:wrapcheck // Already descriptive.
}

// withWatch opens the store and resolves the <post> arguments to a watch.
func withWatch(configPath string, pos []string, stderr io.Writer, f func(*slackwatch.Store, slackwatch.Watch) error) int {
	p, err := slackwatch.ParsePost(pos)
	if err != nil {
		return usageErr(stderr, err)
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	w, err := store.FindWatch(p)
	if err != nil {
		return fail(stderr, err)
	}
	if err := f(store, w); err != nil {
		return fail(stderr, err)
	}
	return 0
}

func usageErr(stderr io.Writer, err error) int {
	fmt.Fprintf(stderr, "slackwatch: %v\n", err)
	return 2
}

func cmdAdd(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("add", stderr)
	var wf watchFlags
	wf.register(fs)
	until := fs.String("until", "", "keep the watch open until this date")
	var source optString
	fs.Var(&source, "source", "outbox, hook, or manual (default manual)")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	p, err := slackwatch.ParsePost(pos)
	if err != nil {
		return usageErr(stderr, err)
	}
	if p.WatchID != 0 {
		return usageErr(stderr, errors.New("add needs a permalink or channel and ts, not a watch id"))
	}
	u := wf.update()
	if source.set && !slices.Contains(sources, source.v) {
		return usageErr(stderr, fmt.Errorf("--source must be one of %s", strings.Join(sources, ", ")))
	}
	u.Source = source.ptr()
	now := time.Now()
	if *until != "" {
		t, err := parseUntil(*until, now)
		if err != nil {
			return usageErr(stderr, err)
		}
		u.Until = &t
	}
	cfg, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	w, created, err := store.AddWatch(p, u, now)
	if err != nil {
		return fail(stderr, err)
	}
	verb := "updated"
	if created {
		verb = "created"
	}
	fmt.Fprintf(stdout, "watch %d %s\n", w.ID, verb)
	catchUp(cfg, store, w, stderr)
	return 0
}

// catchUp runs an immediate catch-up for w. Without a token file it does
// nothing, so add works offline. A catch-up failure is a warning: the
// watch is already registered.
func catchUp(cfg slackwatch.Config, store *slackwatch.Store, w slackwatch.Watch, stderr io.Writer) {
	if cfg.TokenFile == "" {
		return
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		if !errors.Is(err, os.ErrNotExist) {
			fmt.Fprintf(stderr, "slackwatch: catch-up skipped: %v\n", err)
		}
		return
	}
	c, err := newCatchUp(cfg, store, stderr)
	if err == nil {
		err = c.CatchUpWatch(context.Background(), w)
	}
	if err != nil {
		fmt.Fprintf(stderr, "slackwatch: catch-up for watch %d failed: %v\n", w.ID, err)
	}
}

func cmdClaim(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("claim", stderr)
	var wf watchFlags
	wf.register(fs)
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if wf.owner.v == "" {
		return usageErr(stderr, errors.New("claim needs --owner"))
	}
	u := wf.update()
	u.Owner = nil
	return withWatch(configPath, pos, stderr, func(s *slackwatch.Store, w slackwatch.Watch) error {
		w, err := s.ClaimWatch(w.ID, wf.owner.v, u)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		fmt.Fprintf(stdout, "watch %d claimed by %s\n", w.ID, w.Owner)
		return nil
	})
}

func cmdDone(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("done", stderr)
	reason := fs.String("reason", "", "why the watch is closed")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	return withWatch(configPath, pos, stderr, func(s *slackwatch.Store, w slackwatch.Watch) error {
		if _, err := s.CloseWatch(w.ID, *reason, time.Now()); err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		fmt.Fprintf(stdout, "watch %d closed\n", w.ID)
		return nil
	})
}

func cmdSnooze(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("snooze", stderr)
	untilFlag := fs.String("until", "", "keep the watch open until this date (required)")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if *untilFlag == "" {
		return usageErr(stderr, errors.New("snooze needs --until"))
	}
	until, err := parseUntil(*untilFlag, time.Now())
	if err != nil {
		return usageErr(stderr, err)
	}
	return withWatch(configPath, pos, stderr, func(s *slackwatch.Store, w slackwatch.Watch) error {
		if _, err := s.SnoozeWatch(w.ID, until); err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		fmt.Fprintf(stdout, "watch %d snoozed until %s\n", w.ID, until.Format(time.RFC3339))
		return nil
	})
}

func cmdAck(configPath string, args []string, stdout, stderr io.Writer) int {
	pos, err := parseInterspersed(newFlagSet("ack", stderr), args)
	if err != nil {
		return 2
	}
	return withWatch(configPath, pos, stderr, func(s *slackwatch.Store, w slackwatch.Watch) error {
		n, err := s.AckWatch(w.ID)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		fmt.Fprintf(stdout, "watch %d: acked %d replies\n", w.ID, n)
		return nil
	})
}

func cmdLs(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("ls", stderr)
	var f slackwatch.WatchFilter
	fs.StringVar(&f.Owner, "owner", "", "only watches with this owner")
	fs.StringVar(&f.State, "state", "", "only watches in this state: "+strings.Join(states, ", "))
	fs.BoolVar(&f.Unacked, "unacked", false, "only watches with unacknowledged replies")
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 0 {
		return usageErr(stderr, errors.New("ls takes no arguments"))
	}
	if f.State != "" && !slices.Contains(states, f.State) {
		return usageErr(stderr, fmt.Errorf("--state must be one of %s", strings.Join(states, ", ")))
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	rows, err := store.ListWatches(f)
	if err != nil {
		return fail(stderr, err)
	}
	if *asJSON {
		return writeJSON(stdout, stderr, rows)
	}
	tw := tabwriter.NewWriter(stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tSTATE\tOWNER\tPURPOSE\tREFS\tCHANNEL\tREPLIES\tLAST REPLY\tUNACKED\tUNDELIVERED")
	for _, r := range rows {
		last := ""
		if r.LastReplyTS != "" {
			last = r.LastReplyAuthor + " " + tsTime(r.LastReplyTS)
		}
		undelivered := ""
		if r.Undelivered {
			undelivered = "yes"
		}
		fmt.Fprintf(tw, "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%s\t%d\t%s\n", r.ID, r.State, r.Owner, r.Purpose,
			strings.Join(r.Refs, ","), r.Channel, r.Replies, last, r.Unacked, undelivered)
	}
	if err := tw.Flush(); err != nil {
		return fail(stderr, err)
	}
	return 0
}

func cmdShow(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("show", stderr)
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	var d slackwatch.WatchDetail
	code := withWatch(configPath, pos, stderr, func(s *slackwatch.Store, w slackwatch.Watch) error {
		d, err = s.ShowWatch(w.ID)
		return err //nolint:wrapcheck // Already descriptive.
	})
	if code != 0 {
		return code
	}
	if *asJSON {
		return writeJSON(stdout, stderr, d)
	}
	printDetail(stdout, d)
	return 0
}

func printDetail(w io.Writer, d slackwatch.WatchDetail) {
	fmt.Fprintf(w, "watch %d  %s  %s %s  thread %s  since %s\n", d.ID, d.State, d.Kind, d.Channel, d.ThreadTS, d.SinceTS)
	if d.Permalink != "" {
		fmt.Fprintf(w, "link: %s\n", d.Permalink)
	}
	fmt.Fprintf(w, "owner: %s\npurpose: %s\nrefs: %s\nsource: %s\ncreated: %s\n",
		d.Owner, d.Purpose, strings.Join(d.Refs, ", "), d.Source, d.CreatedAt.Local().Format(time.RFC3339))
	if !d.Until.IsZero() {
		fmt.Fprintf(w, "until: %s\n", d.Until.Local().Format(time.RFC3339))
	}
	if !d.ClosedAt.IsZero() {
		fmt.Fprintf(w, "closed: %s %s\n", d.ClosedAt.Local().Format(time.RFC3339), d.CloseReason)
	}
	fmt.Fprintf(w, "posts: %s\n", strings.Join(d.Posts, ", "))
	fmt.Fprintf(w, "replies (%d):\n", len(d.Replies))
	for _, r := range d.Replies {
		var marks []string
		if r.IsSelf {
			marks = append(marks, "self")
		}
		if r.Acked {
			marks = append(marks, "acked")
		}
		if r.Deleted {
			marks = append(marks, "deleted")
		}
		link := ""
		if r.Permalink != "" {
			link = " " + r.Permalink
		}
		fmt.Fprintf(w, "  %s %s %s [%s]%s: %s\n", r.TS, tsTime(r.TS), r.Author, strings.Join(marks, ","), link, r.Text)
	}
	fmt.Fprintf(w, "reactions (%d):\n", len(d.Reactions))
	for _, r := range d.Reactions {
		fmt.Fprintf(w, "  %s :%s: by %s\n", r.TS, r.Name, r.User)
	}
}

// tsTime formats the seconds part of a Slack ts as local time.
func tsTime(ts string) string {
	sec, _, _ := strings.Cut(ts, ".")
	n, err := strconv.ParseInt(sec, 10, 64)
	if err != nil {
		return ts
	}
	return time.Unix(n, 0).Local().Format("2006-01-02 15:04")
}

func writeJSON(stdout, stderr io.Writer, v any) int {
	enc := json.NewEncoder(stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		return fail(stderr, err)
	}
	return 0
}
