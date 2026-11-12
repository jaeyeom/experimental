package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"strings"
	"sync"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack/socketmode"
)

// heartbeat keeps the status row current while serve runs. Every change
// rewrites the row, and run rewrites it every interval. It is safe for
// concurrent use.
type heartbeat struct {
	mu       sync.Mutex
	store    *slackwatch.Store
	st       slackwatch.Status
	interval time.Duration
	now      func() time.Time
	logger   *slog.Logger
}

// newHeartbeat writes a fresh status row for this process, reconnecting
// until Socket Mode connects.
func newHeartbeat(store *slackwatch.Store, interval time.Duration, logger *slog.Logger, now func() time.Time) *heartbeat {
	h := &heartbeat{store: store, interval: interval, now: now, logger: logger}
	h.update(func(st *slackwatch.Status) {
		*st = slackwatch.Status{
			PID: os.Getpid(), StartedAt: now(),
			HeartbeatInterval: slackwatch.Duration{Duration: interval},
			Connection:        slackwatch.ConnReconnecting,
		}
	})
	return h
}

// update applies f and writes the row with a new heartbeat time. A write
// failure is logged; the next update retries.
func (h *heartbeat) update(f func(*slackwatch.Status)) {
	h.mu.Lock()
	defer h.mu.Unlock()
	f(&h.st)
	h.st.HeartbeatAt = h.now()
	if err := h.store.WriteStatus(h.st); err != nil {
		h.logger.Warn("slackwatch serve: write status", "error", err)
	}
}

// run rewrites the heartbeat every interval until ctx ends.
func (h *heartbeat) run(ctx context.Context) {
	tick := time.NewTicker(h.interval)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			h.update(func(*slackwatch.Status) {})
		}
	}
}

// socketEvent records a connection change or the arrival of an event.
func (h *heartbeat) socketEvent(t socketmode.EventType) {
	switch t {
	case socketmode.EventTypeConnected:
		h.update(func(st *slackwatch.Status) { st.Connection = slackwatch.ConnConnected })
	case socketmode.EventTypeConnecting, socketmode.EventTypeConnectionError,
		socketmode.EventTypeDisconnect, socketmode.EventTypeInvalidAuth:
		h.update(func(st *slackwatch.Status) { st.Connection = slackwatch.ConnReconnecting })
	case socketmode.EventTypeEventsAPI:
		h.update(func(st *slackwatch.Status) { st.LastEventAt = h.now() })
	}
}

// swept records the result of a sweep: a catch-up after a connect or a
// periodic sweep.
func (h *heartbeat) swept(catchUp bool, err error) {
	msg := ""
	if err != nil {
		msg = err.Error()
	}
	h.update(func(st *slackwatch.Status) {
		if catchUp {
			st.LastCatchUpAt, st.LastCatchUpError = h.now(), msg
		} else {
			st.LastSweepAt, st.LastSweepError = h.now(), msg
		}
	})
}

// outboxPolled records the result of an outbox poll.
func (h *heartbeat) outboxPolled(err error) {
	h.update(func(st *slackwatch.Status) { st.OutboxPoll.Record(h.now(), err) })
}

// stop marks the server down.
func (h *heartbeat) stop() {
	h.update(func(st *slackwatch.Status) { st.Connection = slackwatch.ConnDown })
}

func cmdStatus(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("status", stderr)
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 0 {
		return usageErr(stderr, errors.New("status takes no arguments"))
	}
	cfg, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	st, err := store.ReadStatus(time.Now())
	if err != nil {
		return fail(stderr, err)
	}
	if st.Outbox, err = store.OutboxCounts(); err != nil {
		return fail(stderr, err)
	}
	st.PostMode = cfg.Post.Mode
	if st.PostMode == slackwatch.PostModeDirect {
		if st.PostingDisabled, err = store.PostingDisabled(); err != nil {
			return fail(stderr, err)
		}
	}
	if *asJSON {
		if code := writeJSON(stdout, stderr, st); code != 0 {
			return code
		}
	} else {
		printStatus(stdout, st)
	}
	if !st.Healthy() {
		return 1
	}
	return 0
}

func printStatus(w io.Writer, st slackwatch.Status) {
	fmt.Fprintf(w, "server: %s\n", st.State)
	if st.PID != 0 {
		fmt.Fprintf(w, "pid: %d\n", st.PID)
	}
	fmt.Fprintf(w, "started: %s\n", localTime(st.StartedAt))
	fmt.Fprintf(w, "heartbeat: %s (every %s)\n", localTime(st.HeartbeatAt), st.HeartbeatInterval)
	fmt.Fprintf(w, "last event: %s\n", localTime(st.LastEventAt))
	fmt.Fprintf(w, "last catch-up: %s\n", sweepResult(st.LastCatchUpAt, st.LastCatchUpError))
	fmt.Fprintf(w, "last sweep: %s\n", sweepResult(st.LastSweepAt, st.LastSweepError))
	fmt.Fprintf(w, "undelivered: %d\n", st.Undelivered)
	posting := st.PostMode
	switch {
	case posting == "":
		posting = "off"
	case st.PostingDisabled != "":
		posting += " (disabled: " + st.PostingDisabled + ")"
	}
	fmt.Fprintf(w, "posting: %s\n", posting)
	fmt.Fprintf(w, "outbox: %d pending, %d awaiting-handoff, %d needs-redraft\n",
		st.Outbox.Pending, st.Outbox.AwaitingHandoff, st.Outbox.NeedsRedraft)
	switch p := st.OutboxPoll; {
	case p.Failing():
		fmt.Fprintf(w, "outbox poll: failing since %s: %s\n", localTime(p.FailingSince), p.LastError)
	case p.OK:
		fmt.Fprintf(w, "outbox poll: ok %s\n", localTime(p.LastOK))
	}
}

func localTime(t time.Time) string {
	if t.IsZero() {
		return "never"
	}
	return t.Local().Format(time.RFC3339)
}

func sweepResult(t time.Time, errText string) string {
	switch {
	case t.IsZero():
		return "never"
	case errText != "":
		return localTime(t) + " failed: " + errText
	default:
		return localTime(t) + " ok"
	}
}

// parseSince accepts YYYY-MM-DD (from the start of that local day),
// RFC 3339, or a Go duration before now.
func parseSince(s string, now time.Time) (time.Time, error) {
	if d, err := time.ParseInLocation(time.DateOnly, s, time.Local); err == nil {
		return d, nil
	}
	if t, err := time.Parse(time.RFC3339, s); err == nil {
		return t, nil
	}
	if d, err := time.ParseDuration(s); err == nil {
		return now.Add(-d), nil
	}
	return time.Time{}, fmt.Errorf("invalid --since %q: want YYYY-MM-DD, RFC 3339, or a duration such as 24h", s)
}

func cmdInbound(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("inbound", stderr)
	sinceFlag := fs.String("since", "", "only messages at or after this time")
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 0 {
		return usageErr(stderr, errors.New("inbound takes no arguments"))
	}
	var since time.Time
	if *sinceFlag != "" {
		if since, err = parseSince(*sinceFlag, time.Now()); err != nil {
			return usageErr(stderr, err)
		}
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	rows, err := store.ListInbound(since)
	if err != nil {
		return fail(stderr, err)
	}
	if *asJSON {
		return writeJSON(stdout, stderr, rows)
	}
	tw := tabwriter.NewWriter(stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "TIME\tKIND\tCHANNEL\tTS\tAUTHOR\tTEXT")
	for _, in := range rows {
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\t%s\t%s\n", tsTime(in.TS), in.Kind, in.Channel, in.TS, in.Author, strings.Join(strings.Fields(in.Text), " "))
	}
	if err := tw.Flush(); err != nil {
		return fail(stderr, err)
	}
	return 0
}
