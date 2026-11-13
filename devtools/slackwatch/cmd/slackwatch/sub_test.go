package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

func useFakeResolver(t *testing.T, ids map[string]string) {
	t.Helper()
	old := resolveChannel
	resolveChannel = func(_ slackwatch.Config, _ *slackwatch.Store, name string, _ io.Writer) (string, error) {
		if id, ok := ids[name]; ok {
			return id, nil
		}
		return "", errors.New("channel_not_found")
	}
	t.Cleanup(func() { resolveChannel = old })
}

func subsJSON(t *testing.T, cfg string) []slackwatch.Sub {
	t.Helper()
	var subs []slackwatch.Sub
	if err := json.Unmarshal([]byte(sw(t, cfg, "subs", "--json")), &subs); err != nil {
		t.Fatal(err)
	}
	return subs
}

func TestSubResolvesNameAndLists(t *testing.T) {
	useFakeResolver(t, map[string]string{"deploys": "C0DEPLOY"})
	cfg := noTokenConfig(t)
	if got := strings.TrimSpace(sw(t, cfg, "subs", "--json")); got != "[]" {
		t.Errorf("empty subs --json = %q, want []", got)
	}
	sw(t, cfg, "sub", "#deploys", "--owner", "agent-1", "--purpose", "triage", "--until", "2031-05-06T07:08:09Z", "--include-self")
	sw(t, cfg, "sub", "C0OTHER", "--owner", "agent-2")
	subs := subsJSON(t, cfg)
	want := []slackwatch.Sub{
		{ID: 1, Channel: "C0DEPLOY", Owner: "agent-1", Purpose: "triage", Until: time.Date(2031, 5, 6, 7, 8, 9, 0, time.UTC), IncludeSelf: true},
		{ID: 2, Channel: "C0OTHER", Owner: "agent-2"},
	}
	if len(subs) != len(want) {
		t.Fatalf("subs = %+v, want %+v", subs, want)
	}
	for i, w := range want {
		g := subs[i]
		untilOK := g.Until.Equal(w.Until)
		g.Until, w.Until = time.Time{}, time.Time{}
		if !untilOK || g != w {
			t.Errorf("sub %d = %+v, want %+v", i, subs[i], want[i])
		}
	}
	if out := sw(t, cfg, "subs"); !strings.Contains(out, "C0DEPLOY") || !strings.Contains(out, "triage") {
		t.Errorf("subs text = %q", out)
	}
}

func TestSubNeedsOwner(t *testing.T) {
	cfg := noTokenConfig(t)
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "sub", "C0123"}, &stdout, &stderr); code != 2 {
		t.Errorf("sub without --owner = %d, want 2", code)
	}
}

func TestSubUnknownNameFails(t *testing.T) {
	useFakeResolver(t, nil)
	cfg := noTokenConfig(t)
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "sub", "#nope", "--owner", "a"}, &stdout, &stderr); code != 1 {
		t.Errorf("sub of an unknown name = %d, want 1", code)
	}
}

func TestUnsub(t *testing.T) {
	useFakeResolver(t, map[string]string{"deploys": "C0DEPLOY"})
	cfg := noTokenConfig(t)
	sw(t, cfg, "sub", "C0DEPLOY", "--owner", "a")
	sw(t, cfg, "sub", "C0DEPLOY", "--owner", "b")
	sw(t, cfg, "unsub", "#deploys", "--owner", "a")
	if subs := subsJSON(t, cfg); len(subs) != 1 || subs[0].Owner != "b" {
		t.Errorf("after unsub --owner a: %+v", subs)
	}
	sw(t, cfg, "unsub", "C0DEPLOY")
	if subs := subsJSON(t, cfg); len(subs) != 0 {
		t.Errorf("after unsub: %+v", subs)
	}
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "unsub", "C0DEPLOY"}, &stdout, &stderr); code != 1 {
		t.Errorf("unsub with nothing to remove = %d, want 1", code)
	}
}
