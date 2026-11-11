// Package eventlog is the append-only JSONL log the daemon writes.
package eventlog

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
)

// DefaultRetentionDays is how long daily files stay when retention is unset.
const DefaultRetentionDays = 30

// Event names are the v1 contract. There is no name-expiry event.
const (
	EventRegister       = "register"
	EventDescribe       = "describe"
	EventRelease        = "release"
	EventHandoff        = "handoff"
	EventClaimRefused   = "claim_refused"
	EventSend           = "send"
	EventDeliverAttempt = "deliver_attempt"
	EventDelivered      = "delivered"
	EventHeld           = "held"
	EventRetry          = "retry"
	EventRequeued       = "requeued"
	EventExpired        = "expired"
	EventRejected       = "rejected"
	EventAdapterError   = "adapter_error"
	EventBypass         = "bypass"
	EventStart          = "start"
	EventStop           = "stop"
	EventLeaseClaim     = "lease_claim"
	EventLeaseGrant     = "lease_grant"
	EventLeaseExtend    = "lease_extend"
	EventLeaseLeave     = "lease_leave"
	EventLeaseRelease   = "lease_release"
	EventLeaseSkip      = "lease_skip"
	// EventLeaseCheckIn is a check-in sent to a holder whose bid ended.
	EventLeaseCheckIn = "lease_checkin"
	// EventLeaseAutoExtend is a silent extend of a holder still working.
	EventLeaseAutoExtend = "lease_auto_extend"
	// EventLeaseTakeover is a takeover of an idle and silent, or gone,
	// holder.
	EventLeaseTakeover = "lease_takeover"
	// EventLeaseAdopt is an adopt that saved a taken-over checkout's
	// leftovers and reset it.
	EventLeaseAdopt = "lease_adopt"
	// EventLeaseWouldTakeOver is a takeover the daemon decided but did
	// not carry out because lease_takeover is "log".
	EventLeaseWouldTakeOver = "lease_would_take_over"
	// EventLeasePause is the user taking the checkout, at once or queued.
	EventLeasePause = "lease_pause"
	// EventLeaseResume is the user giving the checkout back, or withdrawing
	// a queued pause.
	EventLeaseResume = "lease_resume"
	// EventLeaseWith shares the user's pause with an agent pane.
	EventLeaseWith = "lease_with"
	// EventLeaseUnshare takes an agent pane off the user's pause.
	EventLeaseUnshare = "lease_unshare"
	// EventHibernated is an idle agent stopped with its pane kept.
	EventHibernated = "agent_hibernated"
	// EventHibernateRefused is an agent hibernate would not stop, or one
	// that did not exit in time.
	EventHibernateRefused = "agent_hibernate_refused"
	// EventWake is a hibernated agent relaunched in its pane and ready.
	EventWake = "agent_wake"
	// EventWakeFailed is a relaunch that failed or timed out.
	EventWakeFailed = "agent_wake_failed"
	// EventWakeSkipped is a wake not tried because the pane runs
	// something other than its shell; the record was dropped.
	EventWakeSkipped = "agent_wake_skipped"
)

// timeLayout is RFC 3339 with millisecond precision.
const timeLayout = "2006-01-02T15:04:05.000Z07:00"

// Event is one log record. Empty optional fields are left out of the line.
// Resolved is the bus name a bypass target maps to.
type Event struct {
	Time        time.Time
	Event       string
	MsgID       string
	Actor       string
	Name        string
	Resolved    string
	Detail      string
	Reason      string
	TargetState string
}

// Filter selects lines. Zero values match everything.
type Filter struct {
	Since time.Time
	Name  string
	MsgID string
	Event string
}

// Log writes and reads events-YYYY-MM-DD.jsonl files in Dir.
// The daemon is the only writer. Readers open the same directory.
type Log struct {
	Dir           string
	RetentionDays int
	now           func() time.Time
	mu            sync.Mutex
}

type record struct {
	TS          string `json:"ts"`
	Event       string `json:"event"`
	MsgID       string `json:"msg_id,omitempty"` //nolint:tagliatelle // event log contract
	Actor       string `json:"actor"`
	Name        string `json:"name,omitempty"`
	Resolved    string `json:"resolved,omitempty"`
	Detail      string `json:"detail,omitempty"`
	Reason      string `json:"reason,omitempty"`
	TargetState string `json:"target_state,omitempty"` //nolint:tagliatelle // event log contract
}

// Open returns a log stored in dir. RetentionDays zero means DefaultRetentionDays.
func Open(dir string) *Log {
	return &Log{Dir: dir}
}

// Append validates e, drops daily files older than the retention window, and
// writes one JSON line. A zero Time uses the log clock.
func (l *Log) Append(e Event) error {
	l.mu.Lock()
	defer l.mu.Unlock()
	if err := validate(e); err != nil {
		return err
	}
	if err := l.prune(); err != nil {
		return err
	}
	line, ts, err := encode(e, l.clock())
	if err != nil {
		return err
	}
	if err := os.MkdirAll(l.Dir, 0o755); err != nil {
		return fmt.Errorf("create log dir: %w", err)
	}
	path := filepath.Join(l.Dir, fileName(ts))
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("open log: %w", err)
	}
	_, werr := f.Write(append(line, '\n'))
	cerr := f.Close()
	if werr != nil {
		return fmt.Errorf("append log: %w", werr)
	}
	if cerr != nil {
		return fmt.Errorf("close log: %w", cerr)
	}
	return nil
}

// Read returns matching lines in file order. It does not start a daemon.
// A missing directory is an empty log.
func (l *Log) Read(f Filter) ([]string, error) {
	var out []string
	err := l.readFrom(map[string]int64{}, f, func(line string) error {
		out = append(out, line)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Follow emits matching lines, including ones already on disk, until ctx is
// cancelled. Cancellation is a clean stop.
func (l *Log) Follow(ctx context.Context, f Filter, emit func(string) error) error {
	off := map[string]int64{}
	for {
		if err := l.readFrom(off, f, emit); err != nil {
			return err
		}
		timer := time.NewTimer(20 * time.Millisecond)
		select {
		case <-ctx.Done():
			timer.Stop()
			return nil
		case <-timer.C:
		}
	}
}

// Parse decodes one JSON line.
func Parse(line string) (Event, error) {
	var rec record
	if err := json.Unmarshal([]byte(line), &rec); err != nil {
		return Event{}, fmt.Errorf("parse log line: %w", err)
	}
	ts, err := time.Parse(timeLayout, rec.TS)
	if err != nil {
		return Event{}, fmt.Errorf("parse log time: %w", err)
	}
	return Event{
		Time:        ts.UTC(),
		Event:       rec.Event,
		MsgID:       rec.MsgID,
		Actor:       rec.Actor,
		Name:        rec.Name,
		Resolved:    rec.Resolved,
		Detail:      rec.Detail,
		Reason:      rec.Reason,
		TargetState: rec.TargetState,
	}, nil
}

// Format renders one JSON line for a person. The file line stays the contract.
func Format(line string) (string, error) {
	ev, err := Parse(line)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString(ev.Time.Format(timeLayout))
	b.WriteByte(' ')
	b.WriteString(ev.Event)
	writeField(&b, "actor", ev.Actor)
	writeField(&b, "name", ev.Name)
	writeField(&b, "resolved", ev.Resolved)
	writeField(&b, "msg", ev.MsgID)
	writeField(&b, "detail", ev.Detail)
	writeField(&b, "reason", ev.Reason)
	writeField(&b, "target_state", ev.TargetState)
	return b.String(), nil
}

func writeField(b *strings.Builder, key, val string) {
	if val == "" {
		return
	}
	b.WriteByte(' ')
	b.WriteString(key)
	b.WriteByte('=')
	b.WriteString(strconv.Quote(val))
}

func (l *Log) readFrom(off map[string]int64, f Filter, emit func(string) error) error {
	names, err := l.dayFiles()
	if err != nil {
		return err
	}
	for _, name := range names {
		if err := l.readFile(name, off, f, emit); err != nil {
			return err
		}
	}
	return nil
}

func (l *Log) readFile(name string, off map[string]int64, f Filter, emit func(string) error) error {
	fh, err := os.Open(filepath.Join(l.Dir, name))
	if err != nil {
		return fmt.Errorf("open %s: %w", name, err)
	}
	start := off[name]
	_, seekErr := fh.Seek(start, io.SeekStart)
	data, readErr := io.ReadAll(fh)
	closeErr := fh.Close()
	if seekErr != nil {
		return fmt.Errorf("seek %s: %w", name, seekErr)
	}
	if readErr != nil {
		return fmt.Errorf("read %s: %w", name, readErr)
	}
	if closeErr != nil {
		return fmt.Errorf("close %s: %w", name, closeErr)
	}
	rest := data
	consumed := int64(0)
	for {
		i := bytes.IndexByte(rest, '\n')
		if i < 0 {
			break
		}
		raw := rest[:i]
		rest = rest[i+1:]
		consumed += int64(i + 1)
		if len(raw) == 0 {
			continue
		}
		line := string(raw)
		ok, err := matches(line, f)
		if err != nil {
			return err
		}
		if !ok {
			continue
		}
		if err := emit(line); err != nil {
			return err
		}
	}
	off[name] = start + consumed
	return nil
}

func matches(line string, f Filter) (bool, error) {
	ev, err := Parse(line)
	if err != nil {
		return false, err
	}
	if !f.Since.IsZero() && ev.Time.Before(f.Since) {
		return false, nil
	}
	if f.Name != "" && ev.Name != f.Name {
		return false, nil
	}
	if f.MsgID != "" && ev.MsgID != f.MsgID {
		return false, nil
	}
	if f.Event != "" && ev.Event != f.Event {
		return false, nil
	}
	return true, nil
}

func (l *Log) prune() error {
	names, err := l.dayFiles()
	if err != nil {
		return err
	}
	now := l.clock().UTC()
	today := time.Date(now.Year(), now.Month(), now.Day(), 0, 0, 0, 0, time.UTC)
	cutoff := today.AddDate(0, 0, -l.days())
	for _, name := range names {
		day, ok := parseDay(name)
		if !ok || !day.Before(cutoff) {
			continue
		}
		if err := os.Remove(filepath.Join(l.Dir, name)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("remove old log %s: %w", name, err)
		}
	}
	return nil
}

func (l *Log) dayFiles() ([]string, error) {
	entries, err := os.ReadDir(l.Dir)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("read log dir: %w", err)
	}
	names := make([]string, 0, len(entries))
	for _, entry := range entries {
		if _, ok := parseDay(entry.Name()); ok {
			names = append(names, entry.Name())
		}
	}
	sort.Strings(names)
	return names, nil
}

func (l *Log) days() int {
	if l.RetentionDays > 0 {
		return l.RetentionDays
	}
	return DefaultRetentionDays
}

func (l *Log) clock() time.Time {
	if l.now != nil {
		return l.now()
	}
	return time.Now()
}

func validate(e Event) error {
	if !knownEvent(e.Event) {
		return fmt.Errorf("unknown event %q", e.Event)
	}
	if e.Actor == "" {
		return errors.New("actor is required")
	}
	if e.Detail == "" && e.Reason == "" {
		return errors.New("detail or reason is required")
	}
	if e.Event == EventHeld && (e.Reason == "" || e.TargetState == "") {
		return errors.New("held requires a reason and a target state")
	}
	return nil
}

func knownEvent(name string) bool {
	switch name {
	case EventRegister, EventDescribe, EventRelease, EventHandoff, EventClaimRefused,
		EventSend, EventDeliverAttempt, EventDelivered, EventHeld, EventRetry,
		EventRequeued, EventExpired, EventRejected, EventAdapterError, EventBypass, EventStart, EventStop,
		EventLeaseClaim, EventLeaseGrant, EventLeaseExtend, EventLeaseLeave,
		EventLeaseRelease, EventLeaseSkip, EventLeaseCheckIn, EventLeaseAutoExtend,
		EventLeaseTakeover, EventLeaseAdopt, EventLeaseWouldTakeOver,
		EventLeasePause, EventLeaseResume, EventLeaseWith, EventLeaseUnshare,
		EventHibernated, EventHibernateRefused, EventWake, EventWakeFailed, EventWakeSkipped:
		return true
	default:
		return false
	}
}

func encode(e Event, now time.Time) ([]byte, time.Time, error) {
	ts := e.Time
	if ts.IsZero() {
		ts = now
	}
	ts = ts.UTC()
	line, err := json.Marshal(record{
		TS:          ts.Format(timeLayout),
		Event:       e.Event,
		MsgID:       e.MsgID,
		Actor:       e.Actor,
		Name:        e.Name,
		Resolved:    e.Resolved,
		Detail:      e.Detail,
		Reason:      e.Reason,
		TargetState: e.TargetState,
	})
	if err != nil {
		return nil, time.Time{}, fmt.Errorf("encode log line: %w", err)
	}
	return line, ts, nil
}

func fileName(ts time.Time) string {
	return "events-" + ts.UTC().Format("2006-01-02") + ".jsonl"
}

func parseDay(name string) (time.Time, bool) {
	day, ok := strings.CutPrefix(name, "events-")
	if !ok {
		return time.Time{}, false
	}
	day, ok = strings.CutSuffix(day, ".jsonl")
	if !ok {
		return time.Time{}, false
	}
	parsed, err := time.Parse("2006-01-02", day)
	if err != nil {
		return time.Time{}, false
	}
	return parsed, true
}
