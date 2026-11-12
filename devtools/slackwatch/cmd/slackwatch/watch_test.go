package main

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

const (
	topLink   = "https://acme.slack.com/archives/C0123/p1700000000000100"
	replyLink = "https://acme.slack.com/archives/C0123/p1700000005000200?thread_ts=1700000000.000100&cid=C0123"
)

// fakeCatchUp records the watches it was asked to catch up.
type fakeCatchUp struct {
	ids []int64
}

func (f *fakeCatchUp) CatchUpWatch(_ context.Context, w slackwatch.Watch) error {
	f.ids = append(f.ids, w.ID)
	return nil
}

func useFakeCatchUp(t *testing.T) *fakeCatchUp {
	t.Helper()
	f := &fakeCatchUp{}
	old := newCatchUp
	newCatchUp = func(slackwatch.Config, *slackwatch.Store, io.Writer) (catchUpper, error) { return f, nil }
	t.Cleanup(func() { newCatchUp = old })
	return f
}

// noTokenConfig writes a config without a tokenFile.
func noTokenConfig(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.json")
	body := `{"dbPath": "` + filepath.Join(dir, "slackwatch.db") + `"}`
	if err := os.WriteFile(cfg, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return cfg
}

// sw runs slackwatch with cfg and fails the test on a non-zero exit.
func sw(t *testing.T, cfg string, args ...string) string {
	t.Helper()
	var stdout, stderr bytes.Buffer
	if code := run(append([]string{"--config", cfg}, args...), &stdout, &stderr); code != 0 {
		t.Fatalf("slackwatch %q = %d, stderr %q", args, code, stderr.String())
	}
	return stdout.String()
}

func lsJSON(t *testing.T, cfg string, args ...string) []slackwatch.WatchSummary {
	t.Helper()
	var rows []slackwatch.WatchSummary
	if err := json.Unmarshal([]byte(sw(t, cfg, append([]string{"ls", "--json"}, args...)...)), &rows); err != nil {
		t.Fatal(err)
	}
	return rows
}

func TestAddTopThenReplySharesWatch(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", topLink, "--owner", "agent-1", "--purpose", "ask")
	sw(t, cfg, "add", replyLink)
	rows := lsJSON(t, cfg)
	if len(rows) != 1 {
		t.Fatalf("ls = %+v, want one watch", rows)
	}
	var d slackwatch.WatchDetail
	if err := json.Unmarshal([]byte(sw(t, cfg, "show", replyLink, "--json")), &d); err != nil {
		t.Fatal(err)
	}
	if len(d.Posts) != 2 || d.ThreadTS != "1700000000.000100" {
		t.Errorf("show = %+v, want two posts on thread 1700000000.000100", d)
	}
}

func TestAddRepeatUpdatesPurpose(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", "C0123", "1700000000.000100", "--purpose", "old", "--ref", "PR#1", "--ref", "PR#2")
	sw(t, cfg, "add", topLink, "--purpose", "new")
	rows := lsJSON(t, cfg)
	if len(rows) != 1 {
		t.Fatalf("ls = %+v, want one watch", rows)
	}
	if rows[0].Purpose != "new" || strings.Join(rows[0].Refs, ",") != "PR#1,PR#2" {
		t.Errorf("row = %+v", rows[0])
	}
}

func TestClaimDoneAndFilters(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", topLink)
	sw(t, cfg, "add", "C0999", "1700000000.000100", "--owner", "b", "--until", "2030-01-02")
	if rows := lsJSON(t, cfg, "--state", "unowned"); len(rows) != 1 || rows[0].Channel != "C0123" {
		t.Fatalf("ls --state unowned = %+v", rows)
	}
	sw(t, cfg, "claim", topLink, "--owner", "a")
	if rows := lsJSON(t, cfg, "--owner", "a", "--state", "open"); len(rows) != 1 || rows[0].Channel != "C0123" {
		t.Errorf("ls --owner a --state open = %+v", rows)
	}
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "claim", topLink, "--owner", "c"}, &stdout, &stderr); code == 0 {
		t.Error("claim of an open watch succeeded")
	}
	sw(t, cfg, "done", topLink, "--reason", "answered")
	rows := lsJSON(t, cfg, "--state", "closed")
	if len(rows) != 1 || rows[0].CloseReason != "answered" {
		t.Errorf("ls --state closed = %+v", rows)
	}
	sw(t, cfg, "snooze", "2", "--until", "2031-05-06T07:08:09Z")
	rows = lsJSON(t, cfg, "--owner", "b")
	if len(rows) != 1 || !rows[0].Until.Equal(time.Date(2031, 5, 6, 7, 8, 9, 0, time.UTC)) {
		t.Errorf("after snooze = %+v", rows)
	}
}

func TestLsJSONStable(t *testing.T) {
	cfg := noTokenConfig(t)
	if got := strings.TrimSpace(sw(t, cfg, "ls", "--json")); got != "[]" {
		t.Errorf("empty ls --json = %q, want []", got)
	}
	sw(t, cfg, "add", topLink, "--owner", "a")
	sw(t, cfg, "add", "C0999", "1700000000.000100")
	first := sw(t, cfg, "ls", "--json")
	if second := sw(t, cfg, "ls", "--json"); first != second {
		t.Errorf("ls --json changed between runs:\n%s\n%s", first, second)
	}
	var raw []map[string]any
	if err := json.Unmarshal([]byte(first), &raw); err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{"id", "state", "owner", "channel", "replies", "unacked", "undelivered"} {
		if _, ok := raw[0][key]; !ok {
			t.Errorf("ls --json row lacks %q: %s", key, first)
		}
	}
}

func TestAckClearsUnacked(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", topLink, "--owner", "a")
	cfgData, err := slackwatch.LoadConfig(cfg)
	if err != nil {
		t.Fatal(err)
	}
	store, err := slackwatch.OpenStore(cfgData.DBPath)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.AddReply(slackwatch.Reply{Channel: "C0123", TS: "1700000001.000000", ThreadTS: "1700000000.000100", WatchID: 1, Author: "U2", Text: "hi"}); err != nil {
		t.Fatal(err)
	}
	store.Close()
	if rows := lsJSON(t, cfg, "--unacked"); len(rows) != 1 || rows[0].Unacked != 1 {
		t.Fatalf("ls --unacked = %+v", rows)
	}
	if out := sw(t, cfg, "ls"); !strings.Contains(out, "U2") {
		t.Errorf("ls text lacks last reply author: %q", out)
	}
	sw(t, cfg, "ack", topLink)
	if rows := lsJSON(t, cfg, "--unacked"); len(rows) != 0 {
		t.Errorf("ls --unacked after ack = %+v", rows)
	}
	if out := sw(t, cfg, "show", "1"); !strings.Contains(out, "hi") {
		t.Errorf("show lacks reply text: %q", out)
	}
}

// openCfgStore opens the store that cfg names. The caller closes it.
func openCfgStore(t *testing.T, cfg string) *slackwatch.Store {
	t.Helper()
	c, err := slackwatch.LoadConfig(cfg)
	if err != nil {
		t.Fatal(err)
	}
	store, err := slackwatch.OpenStore(c.DBPath)
	if err != nil {
		t.Fatal(err)
	}
	return store
}

func TestShowPrintsPermalinks(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", "C0123", "1700000000.000100", "--owner", "a")
	sw(t, cfg, "add", "C0999", "1700000000.000100")
	store := openCfgStore(t, cfg)
	if err := store.SetTeamURL("https://acme.slack.com/"); err != nil {
		t.Fatal(err)
	}
	if _, err := store.AddReply(slackwatch.Reply{Channel: "C0123", TS: "1700000005.000200", ThreadTS: "1700000000.000100", WatchID: 1, Author: "U2", Text: "hi"}); err != nil {
		t.Fatal(err)
	}
	store.Close()

	out := sw(t, cfg, "show", "1")
	if !strings.Contains(out, "link: "+topLink+"\n") {
		t.Errorf("show lacks the post link %q:\n%s", topLink, out)
	}
	if !strings.Contains(out, replyLink) || !strings.Contains(out, "U2") {
		t.Errorf("show lacks the reply link %q:\n%s", replyLink, out)
	}

	var d slackwatch.WatchDetail
	if err := json.Unmarshal([]byte(sw(t, cfg, "show", "1", "--json")), &d); err != nil {
		t.Fatal(err)
	}
	if d.Permalink != topLink || len(d.Replies) != 1 || d.Replies[0].Permalink != replyLink {
		t.Errorf("show --json = %+v", d)
	}

	var raw []map[string]any
	if err := json.Unmarshal([]byte(sw(t, cfg, "ls", "--json")), &raw); err != nil {
		t.Fatal(err)
	}
	if len(raw) != 2 {
		t.Fatalf("ls --json = %v, want two watches", raw)
	}
	for _, row := range raw {
		if link, _ := row["permalink"].(string); !strings.HasPrefix(link, "https://acme.slack.com/archives/") {
			t.Errorf("ls --json row lacks permalink: %v", row)
		}
	}
	if out := sw(t, cfg, "ls"); strings.Contains(out, "https://") {
		t.Errorf("plain ls shows links:\n%s", out)
	}
}

func TestShowWithoutTeamURLHasNoLinks(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", "C0123", "1700000000.000100")
	if out := sw(t, cfg, "show", "1"); strings.Contains(out, "https://") || strings.Contains(out, "link:") {
		t.Errorf("show without a team URL prints a link:\n%s", out)
	}
	if out := sw(t, cfg, "show", "1", "--json"); strings.Contains(out, "permalink") {
		t.Errorf("show --json without a team URL has a permalink:\n%s", out)
	}
}

func TestShowPrefersAddedPermalink(t *testing.T) {
	cfg := noTokenConfig(t)
	sw(t, cfg, "add", replyLink)
	store := openCfgStore(t, cfg)
	if err := store.SetTeamURL("https://other.slack.com/"); err != nil {
		t.Fatal(err)
	}
	store.Close()
	if out := sw(t, cfg, "show", "1"); !strings.Contains(out, "link: "+replyLink+"\n") {
		t.Errorf("show lacks the added permalink %q:\n%s", replyLink, out)
	}
	if rows := lsJSON(t, cfg); len(rows) != 1 || rows[0].Permalink != replyLink {
		t.Errorf("ls --json = %+v, want permalink %q", rows, replyLink)
	}
}

func TestAddWithoutTokenSkipsCatchUp(t *testing.T) {
	f := useFakeCatchUp(t)
	cfg := noTokenConfig(t)
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "add", topLink}, &stdout, &stderr); code != 0 {
		t.Fatalf("add = %d, stderr %q", code, stderr.String())
	}
	if len(f.ids) != 0 {
		t.Errorf("catch-up ran without a token: %v", f.ids)
	}
	if stderr.Len() != 0 {
		t.Errorf("add without a token wrote to stderr: %q", stderr.String())
	}
}

func TestAddWithTokenRunsCatchUp(t *testing.T) {
	f := useFakeCatchUp(t)
	cfg := writeConfig(t, t.TempDir(), 0o600)
	sw(t, cfg, "add", topLink)
	sw(t, cfg, "add", replyLink)
	if len(f.ids) != 2 || f.ids[0] != 1 || f.ids[1] != 1 {
		t.Errorf("catch-up watches = %v, want [1 1]", f.ids)
	}
}

func TestCommandsRejectUnknownPost(t *testing.T) {
	cfg := noTokenConfig(t)
	for _, args := range [][]string{
		{"done", "7"},
		{"ack", topLink},
		{"show", "C0123", "1700000000.000100"},
		{"snooze", "7", "--until", "2030-01-01"},
		{"add", "not-a-post"},
		{"snooze", topLink},
		{"claim", topLink},
		{"ls", "--state", "bogus"},
	} {
		var stdout, stderr bytes.Buffer
		if code := run(append([]string{"--config", cfg}, args...), &stdout, &stderr); code == 0 {
			t.Errorf("slackwatch %q succeeded", args)
		}
	}
}
