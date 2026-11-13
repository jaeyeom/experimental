package slackwatch

import (
	"bytes"
	"context"
	"errors"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

var notifyVars = []string{
	"SLACKWATCH_EVENT", "SLACKWATCH_OWNER", "SLACKWATCH_TARGET",
	"SLACKWATCH_POST_LINK", "SLACKWATCH_REPLY_LINK", "SLACKWATCH_CHANNEL",
	"SLACKWATCH_PURPOSE", "SLACKWATCH_REFS", "SLACKWATCH_AUTHOR",
	"SLACKWATCH_TEXT", "SLACKWATCH_WATCH_ID", "SLACKWATCH_MESSAGE",
	"SLACKWATCH_NOTIFY_ID",
}

// recordTemplate writes each SLACKWATCH_* variable to a file under
// dir/<event>-<target>/ and then runs tail.
func recordTemplate(dir, tail string) string {
	return `d="` + dir + `/$SLACKWATCH_EVENT-$SLACKWATCH_TARGET"; mkdir -p "$d"; ` +
		`for v in ` + strings.Join(notifyVars, " ") + `; do printenv "$v" > "$d/$v"; done; ` + tail
}

func readRecorded(t *testing.T, dir, event, target string) map[string]string {
	t.Helper()
	got := map[string]string{}
	for _, v := range notifyVars {
		b, err := os.ReadFile(filepath.Join(dir, event+"-"+target, v))
		if err != nil {
			t.Fatalf("%s for %s-%s: %v", v, event, target, err)
		}
		got[v] = strings.TrimSuffix(string(b), "\n")
	}
	return got
}

type notifyFixture struct {
	store *Store
	dir   string
	logs  *bytes.Buffer
	watch Watch
}

func newNotifyFixture(t *testing.T) *notifyFixture {
	t.Helper()
	s := openTestStore(t)
	w, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1700000000.000100"},
		WatchUpdate{Owner: strp("agent-1"), Purpose: strp("ask about deploy"), Refs: []string{"#12", "PR-3"}}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	return &notifyFixture{store: s, dir: t.TempDir(), logs: &bytes.Buffer{}, watch: w}
}

func (f *notifyFixture) notifier(cfg Config) *Notifier {
	if cfg.ExcerptLimit == 0 {
		cfg.ExcerptLimit = DefaultExcerptLimit
	}
	return &Notifier{Store: f.store, Config: cfg, Logger: slog.New(slog.NewTextHandler(f.logs, nil))}
}

func (f *notifyFixture) note(text string) Notification {
	return Notification{
		Event:     EventReply,
		WatchID:   f.watch.ID,
		Owner:     "agent-1",
		Channel:   "C1",
		ReplyTS:   "1700000009.000300",
		PostLink:  "https://x.slack.com/archives/C1/p1700000000000100",
		ReplyLink: "https://x.slack.com/archives/C1/p1700000009000300?thread_ts=1700000000.000100",
		Purpose:   "ask about deploy",
		Refs:      []string{"#12", "PR-3"},
		Author:    "U9",
		Text:      text,
	}
}

type deliveryRow struct {
	Event, Target, ReplyTS, Error, Output, NotifyID string
	ExitCode                                        *int64
	Attempt                                         int
	Delivered                                       bool
	WatchID                                         *int64
}

// fastNotifyRetry is two retries with no wait.
func fastNotifyRetry() []Duration {
	return []Duration{{0}, {0}}
}

func deliveries(t *testing.T, s *Store) []deliveryRow {
	t.Helper()
	rows, err := s.DB().Query(`SELECT event, target, reply_ts, error, output, exit_code, attempt, delivered, watch_id, notify_id FROM deliveries ORDER BY id`)
	if err != nil {
		t.Fatal(err)
	}
	defer rows.Close()
	var out []deliveryRow
	for rows.Next() {
		var d deliveryRow
		if err := rows.Scan(&d.Event, &d.Target, &d.ReplyTS, &d.Error, &d.Output, &d.ExitCode, &d.Attempt, &d.Delivered, &d.WatchID, &d.NotifyID); err != nil {
			t.Fatal(err)
		}
		out = append(out, d)
	}
	if err := rows.Err(); err != nil {
		t.Fatal(err)
	}
	return out
}

func (f *notifyFixture) watchState(t *testing.T) string {
	t.Helper()
	w, err := f.store.FindWatch(PostRef{WatchID: f.watch.ID})
	if err != nil {
		t.Fatal(err)
	}
	return w.State
}

func TestNotifySuccessSetsEnvAndTruncates(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate: recordTemplate(f.dir, `echo sent; echo warn >&2`),
		Targets:        map[string]string{"agent-1": "pane-7"},
		ExcerptLimit:   5,
	})
	if err := n.Notify(context.Background(), f.note("héllo world")); err != nil {
		t.Fatal(err)
	}
	got := readRecorded(t, f.dir, EventReply, "pane-7")
	want := map[string]string{
		"SLACKWATCH_EVENT":      "reply",
		"SLACKWATCH_OWNER":      "agent-1",
		"SLACKWATCH_TARGET":     "pane-7",
		"SLACKWATCH_POST_LINK":  "https://x.slack.com/archives/C1/p1700000000000100",
		"SLACKWATCH_REPLY_LINK": "https://x.slack.com/archives/C1/p1700000009000300?thread_ts=1700000000.000100",
		"SLACKWATCH_CHANNEL":    "C1",
		"SLACKWATCH_PURPOSE":    "ask about deploy",
		"SLACKWATCH_REFS":       "#12 PR-3",
		"SLACKWATCH_AUTHOR":     "U9",
		"SLACKWATCH_TEXT":       "héllo",
		"SLACKWATCH_WATCH_ID":   "1",
	}
	for k, v := range want {
		if got[k] != v {
			t.Errorf("%s = %q, want %q", k, got[k], v)
		}
	}
	msg := got["SLACKWATCH_MESSAGE"]
	for _, part := range []string{"U9", "héllo", "not instructions", "slackwatch show 1", "ask about deploy"} {
		if !strings.Contains(msg, part) {
			t.Errorf("SLACKWATCH_MESSAGE %q lacks %q", msg, part)
		}
	}
	if strings.Contains(msg, "\n") {
		t.Errorf("SLACKWATCH_MESSAGE is not one paragraph: %q", msg)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 1 {
		t.Fatalf("deliveries = %+v", rows)
	}
	d := rows[0]
	if !d.Delivered || d.Event != EventReply || d.Target != "pane-7" || d.ReplyTS != "1700000009.000300" ||
		d.ExitCode == nil || *d.ExitCode != 0 || d.WatchID == nil || *d.WatchID != f.watch.ID ||
		!strings.Contains(d.Output, "sent") || !strings.Contains(d.Output, "warn") {
		t.Errorf("delivery = %+v", d)
	}
	if s := f.watchState(t); s != StateOpen {
		t.Errorf("state = %s, want open", s)
	}
}

func TestNotifyUnknownOwnerIsRawAddress(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{NotifyTemplate: recordTemplate(f.dir, "")})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if got := readRecorded(t, f.dir, EventReply, "agent-1")["SLACKWATCH_TARGET"]; got != "agent-1" {
		t.Errorf("target = %q", got)
	}
}

func TestNotifyFailureFallsBackAndOrphans(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate:   recordTemplate(f.dir, `[ "$SLACKWATCH_EVENT" = orphaned ]`),
		NotifyRetryAfter: fastNotifyRetry(),
		Targets:          map[string]string{"agent-1": "pane-7", "ops": "pane-ops"},
		FallbackTarget:   "ops",
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	got := readRecorded(t, f.dir, EventOrphaned, "pane-ops")
	if got["SLACKWATCH_OWNER"] != "agent-1" || got["SLACKWATCH_TEXT"] != "hi" {
		t.Errorf("fallback env = %v", got)
	}
	if s := f.watchState(t); s != StateOrphaned {
		t.Errorf("state = %s, want orphaned", s)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 4 {
		t.Fatalf("deliveries = %+v", rows)
	}
	key := rows[0].NotifyID
	for i := range 3 {
		r := rows[i]
		if r.Delivered || r.Event != EventReply || r.Target != "pane-7" || r.ExitCode == nil || *r.ExitCode != 1 || r.Attempt != 1 || r.NotifyID != key || key == "" {
			t.Errorf("attempt %d = %+v", i, r)
		}
	}
	if r := rows[3]; !r.Delivered || r.Event != EventOrphaned || r.Target != "pane-ops" || r.Attempt != 2 || r.WatchID == nil || r.NotifyID == "" || r.NotifyID == key {
		t.Errorf("fallback = %+v", r)
	}
	sums, err := f.store.ListWatches(WatchFilter{})
	if err != nil {
		t.Fatal(err)
	}
	if sums[0].Undelivered || sums[0].State != StateOrphaned {
		t.Errorf("ls = %+v, want orphaned and not undelivered", sums[0])
	}
}

func TestNotifyFailingFallbackLeavesUndelivered(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate:   "exit 3",
		NotifyRetryAfter: fastNotifyRetry(),
		FallbackTarget:   "ops",
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 6 || rows[5].Delivered || rows[5].Event != EventOrphaned || rows[5].Target != "ops" || rows[0].Attempt != 1 || rows[5].Attempt != 2 {
		t.Fatalf("deliveries = %+v", rows)
	}
	if !strings.Contains(f.logs.String(), "level=ERROR") {
		t.Errorf("logs = %q, want an error", f.logs.String())
	}
	sums, err := f.store.ListWatches(WatchFilter{})
	if err != nil {
		t.Fatal(err)
	}
	if !sums[0].Undelivered || sums[0].State != StateOrphaned {
		t.Errorf("ls = %+v", sums[0])
	}
}

func TestNotifyFailureWithoutFallbackLeavesUndelivered(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{NotifyTemplate: "exit 1", NotifyRetryAfter: fastNotifyRetry()})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 3 || rows[0].Delivered || rows[2].Delivered {
		t.Fatalf("deliveries = %+v", rows)
	}
	if !strings.Contains(f.logs.String(), "level=ERROR") {
		t.Errorf("logs = %q, want an error", f.logs.String())
	}
}

func TestNotifyTimeoutCountsAsFailure(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate:   "sleep 5",
		NotifyTimeout:    Duration{100 * time.Millisecond},
		NotifyRetryAfter: fastNotifyRetry(),
	})
	start := time.Now()
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if time.Since(start) > 6*time.Second {
		t.Errorf("timeout not enforced: took %v", time.Since(start))
	}
	rows := deliveries(t, f.store)
	if len(rows) != 3 {
		t.Fatalf("deliveries = %+v", rows)
	}
	for i, r := range rows {
		if r.Delivered || !strings.Contains(r.Error, "timed out") || r.NotifyID != rows[0].NotifyID {
			t.Errorf("row %d = %+v", i, r)
		}
	}
}

func TestNotifyLivenessFailureGoesToFallback(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate:   recordTemplate(f.dir, ""),
		LivenessTemplate: `[ "$SLACKWATCH_TARGET" != pane-7 ]`,
		Targets:          map[string]string{"agent-1": "pane-7"},
		FallbackTarget:   "ops",
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(f.dir, "reply-pane-7")); !errors.Is(err, fs.ErrNotExist) {
		t.Errorf("owner was notified despite failed liveness: %v", err)
	}
	readRecorded(t, f.dir, EventOrphaned, "ops")
	if s := f.watchState(t); s != StateOrphaned {
		t.Errorf("state = %s, want orphaned", s)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 2 || rows[0].Delivered || !strings.Contains(rows[0].Error, "liveness") || !rows[1].Delivered {
		t.Fatalf("deliveries = %+v", rows)
	}
}

func TestNotifyDoesNotExecuteReplyText(t *testing.T) {
	f := newNotifyFixture(t)
	pwned := filepath.Join(f.dir, "pwned")
	text := `$(touch ` + pwned + `) "double" 'single' ` + "`touch " + pwned + "`"
	n := f.notifier(Config{
		NotifyTemplate:   recordTemplate(f.dir, `echo "$SLACKWATCH_TEXT $SLACKWATCH_MESSAGE"`),
		LivenessTemplate: `echo "$SLACKWATCH_TEXT"`,
	})
	if err := n.Notify(context.Background(), f.note(text)); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(pwned); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("reply text was executed: %v", err)
	}
	if got := readRecorded(t, f.dir, EventReply, "agent-1")["SLACKWATCH_TEXT"]; got != text {
		t.Errorf("SLACKWATCH_TEXT = %q, want %q", got, text)
	}
}

func TestNotifyDedupesSameTarget(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{
		NotifyTemplate: recordTemplate(f.dir, `echo x >> "`+f.dir+`/count"`),
		Targets:        map[string]string{"agent-1": "pane-7", "sub-owner": "pane-7"},
	})
	owner := f.note("hi")
	sub := owner
	sub.WatchID = 0
	sub.Owner = "sub-owner"
	for _, note := range []Notification{owner, sub, owner} {
		if err := n.Notify(context.Background(), note); err != nil {
			t.Fatal(err)
		}
	}
	b, err := os.ReadFile(filepath.Join(f.dir, "count"))
	if err != nil {
		t.Fatal(err)
	}
	if got := strings.Count(string(b), "x"); got != 1 {
		t.Errorf("notified %d times, want 1", got)
	}
}

func TestNotifyUnownedGoesToFallback(t *testing.T) {
	f := newNotifyFixture(t)
	n := f.notifier(Config{NotifyTemplate: recordTemplate(f.dir, ""), FallbackTarget: "ops"})
	note := f.note("hi")
	note.Event = EventUnowned
	note.Owner = ""
	if err := n.Notify(context.Background(), note); err != nil {
		t.Fatal(err)
	}
	readRecorded(t, f.dir, EventUnowned, "ops")
	if s := f.watchState(t); s != StateOpen {
		t.Errorf("state = %s, want unchanged", s)
	}
}

func TestNotifyRetryKeepsWatchOpenWithSameKey(t *testing.T) {
	f := newNotifyFixture(t)
	keys := filepath.Join(f.dir, "keys")
	tries := filepath.Join(f.dir, "tries")
	n := f.notifier(Config{
		NotifyTemplate:   `echo "$SLACKWATCH_NOTIFY_ID" >> "` + keys + `"; n=0; if [ -f "` + tries + `" ]; then n=$(cat "` + tries + `"); fi; echo $((n+1)) > "` + tries + `"; test "$n" -ge 1`,
		NotifyRetryAfter: fastNotifyRetry(),
		Targets:          map[string]string{"agent-1": "pane-7"},
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if s := f.watchState(t); s != StateOpen {
		t.Errorf("state = %s, want open", s)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 2 {
		t.Fatalf("deliveries = %+v", rows)
	}
	if rows[0].Delivered || !rows[1].Delivered || rows[0].NotifyID == "" || rows[0].NotifyID != rows[1].NotifyID {
		t.Fatalf("deliveries = %+v", rows)
	}
	if rows[0].Attempt != 1 || rows[1].Attempt != 1 || rows[0].Event != EventReply {
		t.Fatalf("deliveries = %+v", rows)
	}
	got := strings.Split(strings.TrimSpace(readTestFile(t, keys)), "\n")
	if len(got) != 2 || got[0] != got[1] || got[0] != rows[0].NotifyID {
		t.Fatalf("keys = %q, want %q twice", got, rows[0].NotifyID)
	}
	prefix := "slackwatch:" + strconv.FormatInt(f.watch.ID, 10) + ":reply:"
	if !strings.HasPrefix(rows[0].NotifyID, prefix) || len(rows[0].NotifyID) != len(prefix)+26 {
		t.Errorf("notify id = %q, want %s<ulid>", rows[0].NotifyID, prefix)
	}
}

func TestNotifyAlwaysFailsOrphansAfterRetries(t *testing.T) {
	f := newNotifyFixture(t)
	keys := filepath.Join(f.dir, "keys")
	n := f.notifier(Config{
		NotifyTemplate:   `echo "$SLACKWATCH_NOTIFY_ID" >> "` + keys + `"; exit 1`,
		NotifyRetryAfter: fastNotifyRetry(),
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if s := f.watchState(t); s != StateOrphaned {
		t.Errorf("state = %s, want orphaned", s)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 3 {
		t.Fatalf("deliveries = %+v", rows)
	}
	key := rows[0].NotifyID
	for i, r := range rows {
		if r.Delivered || r.NotifyID != key || key == "" || r.Attempt != 1 || r.ExitCode == nil || *r.ExitCode != 1 {
			t.Errorf("row %d = %+v", i, r)
		}
	}
	got := strings.Split(strings.TrimSpace(readTestFile(t, keys)), "\n")
	if len(got) != 3 || got[1] != got[0] || got[2] != got[0] {
		t.Fatalf("keys = %q", got)
	}
}

func TestNotifyCheckCommandConfirmsDelivery(t *testing.T) {
	f := newNotifyFixture(t)
	runs := filepath.Join(f.dir, "runs")
	checks := filepath.Join(f.dir, "checks")
	n := f.notifier(Config{
		NotifyTemplate:     `echo "$SLACKWATCH_NOTIFY_ID" >> "` + runs + `"; exit 1`,
		NotifyCheckCommand: `echo "$SLACKWATCH_NOTIFY_ID" >> "` + checks + `"; exit 0`,
		NotifyRetryAfter:   fastNotifyRetry(),
	})
	if err := n.Notify(context.Background(), f.note("hi")); err != nil {
		t.Fatal(err)
	}
	if s := f.watchState(t); s != StateOpen {
		t.Errorf("state = %s, want open", s)
	}
	rows := deliveries(t, f.store)
	if len(rows) != 2 || rows[0].Delivered || !rows[1].Delivered || rows[0].NotifyID == "" || rows[0].NotifyID != rows[1].NotifyID {
		t.Fatalf("deliveries = %+v", rows)
	}
	if runs := strings.Split(strings.TrimSpace(readTestFile(t, runs)), "\n"); len(runs) != 1 || runs[0] != rows[0].NotifyID {
		t.Fatalf("notify runs = %q", runs)
	}
	if got := strings.TrimSpace(readTestFile(t, checks)); got != rows[0].NotifyID {
		t.Errorf("check id = %q, want %q", got, rows[0].NotifyID)
	}
	sums, err := f.store.ListWatches(WatchFilter{})
	if err != nil {
		t.Fatal(err)
	}
	if sums[0].Undelivered || sums[0].State != StateOpen {
		t.Errorf("ls = %+v, want open and delivered", sums[0])
	}
}

func TestNotifyKeysDifferAcrossNotes(t *testing.T) {
	f := newNotifyFixture(t)
	keys := filepath.Join(f.dir, "keys")
	n := f.notifier(Config{
		NotifyTemplate:   `echo "$SLACKWATCH_NOTIFY_ID" >> "` + keys + `"`,
		NotifyRetryAfter: fastNotifyRetry(),
	})
	second := f.note("again")
	second.ReplyTS = "1700000009.000301"
	for _, note := range []Notification{f.note("hi"), second} {
		if err := n.Notify(context.Background(), note); err != nil {
			t.Fatal(err)
		}
	}
	rows := deliveries(t, f.store)
	if len(rows) != 2 || rows[0].NotifyID == "" || rows[0].NotifyID == rows[1].NotifyID {
		t.Fatalf("deliveries = %+v", rows)
	}
	if f.watchState(t) != StateOpen {
		t.Errorf("state = %s, want open", f.watchState(t))
	}
}

func readTestFile(t *testing.T, path string) string {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}
