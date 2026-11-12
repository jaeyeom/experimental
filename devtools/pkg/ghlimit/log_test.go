package ghlimit

import (
	"bytes"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestAppendLogAndStats(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state", "log.jsonl")
	now := time.Unix(1_700_000_000, 0)
	for _, e := range []Entry{
		{Time: now.Add(-2 * time.Hour), Caller: "old", Reason: "slot", WaitMS: 9000},
		{Time: now.Add(-30 * time.Minute), PID: 1, Caller: "prsync", Args: "pr merge _", Reason: "slot", WaitMS: 1500},
		{Time: now.Add(-20 * time.Minute), PID: 2, Caller: "prsync", Args: "pr merge _", Reason: "mutation_gap", WaitMS: 800},
		{Time: now.Add(-10 * time.Minute), PID: 3, Caller: "gh-merge", Args: "pr view _", Reason: "backoff", WaitMS: 60000},
	} {
		if err := AppendLog(path, e); err != nil {
			t.Fatal(err)
		}
	}
	var out bytes.Buffer
	if err := WriteStats(&out, path, now, time.Hour); err != nil {
		t.Fatal(err)
	}
	got := out.String()
	for _, want := range []string{
		"3 waits in the last 1h0m0s, 62.3s total",
		"backoff",
		"mutation_gap",
		"prsync",
		"gh-merge",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("stats missing %q:\n%s", want, got)
		}
	}
	if strings.Contains(got, "old") {
		t.Errorf("stats include an entry older than the window:\n%s", got)
	}
}

func TestStatsWithoutLog(t *testing.T) {
	var out bytes.Buffer
	if err := WriteStats(&out, filepath.Join(t.TempDir(), "none.jsonl"), time.Now(), time.Hour); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "0 waits") {
		t.Fatalf("stats = %q", out.String())
	}
}
