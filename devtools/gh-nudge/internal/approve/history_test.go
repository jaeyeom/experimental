package approve

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func historyNow() time.Time { return time.Date(2026, 11, 13, 12, 0, 0, 0, time.UTC) }

func historyLine(t *testing.T, when time.Time, pr, author, title, action, rule string) string {
	t.Helper()
	entry := loggedDecision{Time: when, PR: pr, Title: title, Author: author, Action: action}
	if rule != "" {
		entry.Rule = &rule
	}
	body, err := json.Marshal(entry)
	if err != nil {
		t.Fatal(err)
	}
	return string(body)
}

func writeHistoryLog(t *testing.T, lines ...string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "approve-log.jsonl")
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

type historyHit struct {
	calls int
	refs  []github.PullRef
	snaps map[string]github.PRSnapshot
	err   error
}

func (h *historyHit) lookup(refs []github.PullRef) (map[string]github.PRSnapshot, error) {
	h.calls++
	h.refs = append([]github.PullRef(nil), refs...)
	if h.err != nil {
		return nil, h.err
	}
	return h.snaps, nil
}

func historyOpts(logPath, cachePath string, hit *historyHit) (HistoryOptions, *bytes.Buffer, *bytes.Buffer) {
	var out, errBuf bytes.Buffer
	return HistoryOptions{
		LogPath:   logPath,
		CachePath: cachePath,
		Days:      7,
		Action:    ActionApproved,
		Now:       historyNow,
		Out:       &out,
		Err:       &errBuf,
		Lookup:    hit.lookup,
	}, &out, &errBuf
}

func TestHistoryUsesAuthorFromTheLog(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	logPath := writeHistoryLog(t, historyLine(t, when, "acme/widgets#12", "alice", "Tidy", ActionApproved, "dashboard"))
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#12": {State: "OPEN", Author: "someone-else", Title: "other"},
	}}
	opts, out, errBuf := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 1 {
		t.Fatalf("lookups = %d, want 1", hit.calls)
	}
	if !strings.Contains(out.String(), "alice") || strings.Contains(out.String(), "someone-else") {
		t.Fatalf("out = %s", out.String())
	}
	if errBuf.Len() != 0 {
		t.Fatalf("stderr = %q", errBuf.String())
	}
}

func TestHistoryBatchesUncachedPullRequests(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	logPath := writeHistoryLog(t,
		historyLine(t, when, "acme/widgets#1", "alice", "One", ActionApproved, "dashboard"),
		historyLine(t, when, "acme/gadgets#2", "bob", "Two", ActionApproved, "dashboard"),
		historyLine(t, when.Add(time.Minute), "acme/widgets#1", "alice", "One", ActionApproved, "dashboard"),
	)
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#1": {State: "OPEN", Author: "alice", Title: "One"},
		"acme/gadgets#2": {State: "CLOSED", Author: "bob", Title: "Two"},
	}}
	opts, out, _ := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 1 || len(hit.refs) != 2 {
		t.Fatalf("calls = %d refs = %+v", hit.calls, hit.refs)
	}
	if !strings.Contains(out.String(), "OPEN") || !strings.Contains(out.String(), "CLOSED") {
		t.Fatalf("out = %s", out.String())
	}
}

func TestHistoryRefetchesClosedAndReusesMerged(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	dir := t.TempDir()
	logPath := writeHistoryLog(t,
		historyLine(t, when, "acme/widgets#1", "alice", "Merged", ActionApproved, "dashboard"),
		historyLine(t, when, "acme/gadgets#2", "bob", "Closed", ActionApproved, "dashboard"),
	)
	cachePath := filepath.Join(dir, "pr-state.json")
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#1": {State: "MERGED", Author: "alice", Title: "Merged"},
		"acme/gadgets#2": {State: "CLOSED", Author: "bob", Title: "Closed"},
	}}
	opts, _, _ := historyOpts(logPath, cachePath, hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 1 {
		t.Fatalf("first lookups = %d", hit.calls)
	}
	body, err := os.ReadFile(cachePath)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(body), "gadgets") || !strings.Contains(string(body), "MERGED") {
		t.Fatalf("cache = %s", body)
	}
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 2 || len(hit.refs) != 1 || hit.refs[0].Raw != "acme/gadgets#2" {
		t.Fatalf("second calls = %d refs = %+v", hit.calls, hit.refs)
	}
}

func TestHistoryNoStateMakesNoCalls(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	logPath := writeHistoryLog(t, historyLine(t, when, "acme/widgets#1", "", "From log", ActionApproved, "dashboard"))
	cachePath := filepath.Join(t.TempDir(), "pr-state.json")
	if err := os.WriteFile(cachePath, []byte(`{"pulls":{"acme/widgets#1":{"state":"MERGED","author":"alice","title":"Cached"}}}`+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	hit := &historyHit{err: os.ErrInvalid}
	opts, out, _ := historyOpts(logPath, cachePath, hit)
	opts.NoState = true
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 0 {
		t.Fatalf("lookups = %d, want 0", hit.calls)
	}
	if strings.Contains(out.String(), "STATE") || strings.Contains(out.String(), "MERGED") || !strings.Contains(out.String(), "alice") {
		t.Fatalf("out = %s", out.String())
	}
}

func TestHistoryLooksUpAuthorMissingFromOldLines(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	old := `{"time":"` + when.Format(time.RFC3339) + `","pr":"acme/widgets#7","title":"Old","action":"approved","rule":"dashboard"}`
	logPath := writeHistoryLog(t, old)
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#7": {State: "OPEN", Author: "carol", Title: "Old"},
	}}
	opts, out, _ := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if hit.calls != 1 || !strings.Contains(out.String(), "carol") {
		t.Fatalf("calls = %d out = %s", hit.calls, out.String())
	}
}

func TestHistoryUnreadablePullRequestWarns(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	logPath := writeHistoryLog(t, historyLine(t, when, "acme/widgets#9", "alice", "Gone", ActionApproved, "dashboard"))
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#9": {Unreadable: true},
	}}
	opts, out, errBuf := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "alice") || !strings.Contains(out.String(), "?") || !strings.Contains(out.String(), "Gone") {
		t.Fatalf("out = %s", out.String())
	}
	if errBuf.String() != "cannot read acme/widgets#9\n" {
		t.Fatalf("stderr = %q", errBuf.String())
	}
}

func TestHistoryGraphQLErrorExits(t *testing.T) {
	when := historyNow().Add(-time.Hour)
	logPath := writeHistoryLog(t, historyLine(t, when, "acme/widgets#9", "alice", "Gone", ActionApproved, "dashboard"))
	hit := &historyHit{err: os.ErrClosed}
	opts, out, _ := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	err := History(opts)
	if err == nil || !strings.Contains(err.Error(), os.ErrClosed.Error()) {
		t.Fatalf("err = %v", err)
	}
	if out.Len() != 0 {
		t.Fatalf("out = %q, want empty", out.String())
	}
}

func TestHistoryFiltersActionRuleAndDays(t *testing.T) {
	logPath := writeHistoryLog(t,
		historyLine(t, historyNow().Add(-48*time.Hour), "acme/widgets#1", "alice", "Recent approve", ActionApproved, "dashboard"),
		historyLine(t, historyNow().Add(-48*time.Hour), "acme/widgets#2", "bob", "Would", ActionWouldApprove, "dashboard"),
		historyLine(t, historyNow().Add(-48*time.Hour), "acme/widgets#3", "cara", "Hand", ActionHandoff, "other"),
		historyLine(t, historyNow().Add(-10*24*time.Hour), "acme/widgets#4", "dan", "Old", ActionApproved, "dashboard"),
	)
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#1": {State: "OPEN", Author: "alice", Title: "Recent approve"},
		"acme/widgets#2": {State: "OPEN", Author: "bob", Title: "Would"},
		"acme/widgets#3": {State: "OPEN", Author: "cara", Title: "Hand"},
		"acme/widgets#4": {State: "OPEN", Author: "dan", Title: "Old"},
	}}
	opts, out, _ := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(out.String(), "Would") || strings.Contains(out.String(), "Hand") || strings.Contains(out.String(), "Old") || !strings.Contains(out.String(), "Recent approve") {
		t.Fatalf("approved filter out = %s", out.String())
	}
	opts.Action = "all"
	opts.Rule = "other"
	out.Reset()
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "Hand") || strings.Contains(out.String(), "Recent") {
		t.Fatalf("rule filter out = %s", out.String())
	}
}

func TestHistoryJSONIsStable(t *testing.T) {
	when := time.Date(2026, 11, 12, 15, 4, 5, 0, time.UTC)
	logPath := writeHistoryLog(t, historyLine(t, when, "acme/widgets#1", "alice", "Tidy", ActionApproved, "dashboard"))
	hit := &historyHit{snaps: map[string]github.PRSnapshot{
		"acme/widgets#1": {State: "MERGED", Author: "alice", Title: "Tidy"},
	}}
	opts, out, _ := historyOpts(logPath, filepath.Join(t.TempDir(), "pr-state.json"), hit)
	opts.JSON = true
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	const want = `[{"time":"2026-11-12T15:04:05Z","pr":"acme/widgets#1","author":"alice","state":"MERGED","action":"approved","rule":"dashboard","title":"Tidy"}]` + "\n"
	if out.String() != want {
		t.Fatalf("json = %s\nwant %s", out.String(), want)
	}
	opts.NoState = true
	opts.JSON = true
	out.Reset()
	hit.calls = 0
	if err := History(opts); err != nil {
		t.Fatal(err)
	}
	const wantNoState = `[{"time":"2026-11-12T15:04:05Z","pr":"acme/widgets#1","author":"alice","action":"approved","rule":"dashboard","title":"Tidy"}]` + "\n"
	if out.String() != wantNoState {
		t.Fatalf("json = %s\nwant %s", out.String(), wantNoState)
	}
	if hit.calls != 0 {
		t.Fatalf("no-state lookups = %d", hit.calls)
	}
}

func TestHistoryRejectsBadFlags(t *testing.T) {
	opts, _, _ := historyOpts(filepath.Join(t.TempDir(), "missing.jsonl"), filepath.Join(t.TempDir(), "cache.json"), &historyHit{})
	opts.Days = -1
	if err := History(opts); err == nil || !strings.Contains(err.Error(), "non-negative") {
		t.Fatalf("days err = %v", err)
	}
	opts.Days = 7
	opts.Action = "would_approve"
	if err := History(opts); err == nil || !strings.Contains(err.Error(), "would_approve") {
		t.Fatalf("action err = %v", err)
	}
}
