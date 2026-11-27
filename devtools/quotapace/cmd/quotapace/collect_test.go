package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
	"github.com/jaeyeom/experimental/devtools/quotapace/internal/collect/claude"
	"github.com/jaeyeom/experimental/devtools/quotapace/internal/collect/claude/credentials"
)

func (h *harness) runWithFetcher(fetcher claude.Fetcher, args ...string) int {
	h.stdout.Reset()
	h.stderr.Reset()
	return run(args, env{
		getenv: func(k string) string {
			if k == "XDG_DATA_HOME" {
				return h.data
			}
			return ""
		},
		stdin:  strings.NewReader(""),
		stdout: &h.stdout,
		stderr: &h.stderr,
		now:    func() time.Time { return h.now },
		collectClaude: func() claude.Fetcher {
			return fetcher
		},
	})
}

func TestCollectClaudeHuman(t *testing.T) {
	h := newHarness(t)
	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 0, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 0, time.UTC)
	fetcher := claude.FetcherFunc(func(context.Context) (*claude.Data, error) {
		return &claude.Data{
			PlanName:        "Team",
			FiveHour:        claude.Float(2.4),
			SevenDay:        claude.Float(12),
			FiveHourResetAt: claude.Time(reset5),
			SevenDayResetAt: claude.Time(reset7),
		}, nil
	})
	code := h.runWithFetcher(fetcher, "collect", "claude")
	if code != 0 {
		t.Fatalf("exit = %d, stderr=%q", code, h.stderr.String())
	}
	want := strings.Join([]string{
		"plan: Team",
		"  5-hour    2%   resets 2026-07-24 03:29 UTC",
		"  7-day    12%   resets 2026-07-27 22:59 UTC",
		"",
	}, "\n")
	if h.stdout.String() != want {
		t.Fatalf("stdout = %q, want %q", h.stdout.String(), want)
	}
}

func TestCollectClaudeJSON(t *testing.T) {
	h := newHarness(t)
	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	fetchedAt := time.Date(2026, 7, 24, 1, 0, 0, 0, time.UTC)
	fetcher := claude.FetcherFunc(func(context.Context) (*claude.Data, error) {
		return &claude.Data{
			PlanName:        "Team",
			FiveHour:        claude.Float(33.3),
			SevenDay:        claude.Float(12),
			FiveHourResetAt: claude.Time(reset5),
			FetchedAt:       claude.Time(fetchedAt),
			Source:          "keychain:/Users/alice/.claude",
		}, nil
	})
	code := h.runWithFetcher(fetcher, "collect", "claude", "--json")
	if code != 0 {
		t.Fatalf("exit = %d, stderr=%q", code, h.stderr.String())
	}
	var got map[string]any
	if err := json.Unmarshal(h.stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, h.stdout.String())
	}
	if got["planName"] != "Team" || got["fiveHour"] != 33.3 || got["sevenDay"] != float64(12) {
		t.Fatalf("unexpected json: %s", h.stdout.String())
	}
	if got["fetchedAt"] != "2026-07-24T01:00:00Z" || got["source"] != "keychain:/Users/alice/.claude" {
		t.Fatalf("unexpected metadata: %s", h.stdout.String())
	}
}

func TestCollectClaudeJSONLAndProfileRecording(t *testing.T) {
	h := newHarness(t)
	configDir := filepath.Join(t.TempDir(), ".claude-work")
	if err := os.MkdirAll(configDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(configDir, ".credentials.json"), []byte(`{
		"claudeAiOauth": {
			"accessToken": "test-oauth-token",
			"subscriptionType": "max",
			"expiresAt": 1893456000000
		}
	}`), 0o600); err != nil {
		t.Fatal(err)
	}

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = w.Write([]byte(`{
			"five_hour": {"utilization": 14.5, "resets_at": "2026-11-17T03:00:00Z"},
			"seven_day": {"utilization": 33.3, "resets_at": "2026-11-22T00:00:00Z"}
		}`))
	}))
	t.Cleanup(srv.Close)

	fetchedAt := time.Date(2026, 11, 17, 0, 0, 0, 0, time.UTC)
	client := &claude.Client{
		Tokens: &credentials.Resolver{
			ConfigDir: configDir,
			HomeDir:   t.TempDir(),
			GOOS:      "linux",
			Now:       func() time.Time { return fetchedAt },
		},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
		Now:        func() time.Time { return fetchedAt },
	}

	// --jsonl emits lines accepted by quotapace.ParseReadingsJSONL.
	if code := h.runWithFetcher(client, "collect", "claude", "--jsonl"); code != 0 {
		t.Fatalf("collect --jsonl exit = %d, stderr=%q", code, h.stderr.String())
	}
	parsed, err := quotapace.ParseReadingsJSONL(bytes.NewReader(h.stdout.Bytes()), "remote", "", fetchedAt)
	if err != nil {
		t.Fatalf("ParseReadingsJSONL: %v\n%s", err, h.stdout.String())
	}
	wantSource := "credentials-file:" + configDir
	if len(parsed) != 2 || parsed[0].Window != "5h" || parsed[0].Percent != 14.5 || parsed[1].Window != "7d" || parsed[1].Percent != 33.3 || parsed[1].Source != wantSource {
		t.Fatalf("parsed = %+v", parsed)
	}

	// Direct recording into --profile work (plus --auto-start).
	if code := h.runWithFetcher(client, "collect", "claude", "--profile", "work", "--auto-start"); code != 0 {
		t.Fatalf("collect --profile work exit = %d, stderr=%q", code, h.stderr.String())
	}
	if !strings.Contains(h.stdout.String(), "plan: Max") {
		t.Fatalf("expected human summary on stdout, got %q", h.stdout.String())
	}

	var snap quotapace.Snapshot
	if err := json.Unmarshal([]byte(h.must(2, "", "status", "--profile", "work", "--json")), &snap); err != nil {
		t.Fatal(err)
	}
	ws := snap.Profiles[0].Windows
	if len(ws) != 2 || ws[0].Window != "5h" || ws[0].Latest.Percent != 14.5 || ws[1].Window != "7d" || ws[1].Latest.Percent != 33.3 || ws[1].Latest.Source != wantSource {
		t.Fatalf("recorded windows = %+v", ws)
	}

	// Recording alongside --json keeps stdout pure JSON.
	if code := h.runWithFetcher(client, "collect", "claude", "--profile", "work", "--json"); code != 0 {
		t.Fatalf("collect --profile work --json exit = %d, stderr=%q", code, h.stderr.String())
	}
	var jsonOut map[string]any
	if err := json.Unmarshal(h.stdout.Bytes(), &jsonOut); err != nil {
		t.Fatalf("stdout with --profile --json is not valid JSON: %v\n%s", err, h.stdout.String())
	}
}

func TestCollectClaudeExitCodesAndErrors(t *testing.T) {
	h := newHarness(t)

	for _, tt := range []struct {
		name    string
		err     error
		want    int
		wantMsg string
	}{
		{name: "no data", err: claude.ErrNoData, want: exitCollectNoData, wantMsg: "no usage data"},
		{name: "custom endpoint", err: claude.ErrCustomEndpoint, want: exitCollectNoData, wantMsg: "ANTHROPIC_BASE_URL"},
		{name: "unauthorized", err: claude.ErrUnauthorized, want: exitCollectNoData, wantMsg: "re-login"},
		{name: "unexpected", err: errors.New("boom"), want: exitSoftware, wantMsg: "boom"},
	} {
		fetcher := claude.FetcherFunc(func(context.Context) (*claude.Data, error) {
			return nil, tt.err
		})
		code := h.runWithFetcher(fetcher, "collect", "claude")
		if code != tt.want {
			t.Errorf("%s: exit = %d, want %d (stderr=%q)", tt.name, code, tt.want, h.stderr.String())
		}
		if !strings.Contains(h.stderr.String(), tt.wantMsg) {
			t.Errorf("%s: stderr = %q, want %q", tt.name, h.stderr.String(), tt.wantMsg)
		}
	}

	// Nil data without error exits 2.
	nilFetcher := claude.FetcherFunc(func(context.Context) (*claude.Data, error) {
		return nil, nil
	})
	if code := h.runWithFetcher(nilFetcher, "collect", "claude"); code != exitCollectNoData {
		t.Errorf("nil data: exit = %d, want %d", code, exitCollectNoData)
	}

	// Usage errors exit 64.
	for _, args := range [][]string{
		{"collect"},
		{"collect", "unknown"},
		{"collect", "claude", "extra"},
		{"collect", "claude", "--json", "--jsonl"},
		{"collect", "claude", "--json", "--quotapace"},
		{"collect", "claude", "--record"},
		{"collect", "claude", "--auto-start"},
		{"collect", "claude", "--profile", "a/b"},
	} {
		if code := h.run("", args...); code != exitUsage {
			t.Errorf("%v: exit = %d, want %d (stderr=%q)", args, code, exitUsage, h.stderr.String())
		}
	}
}
