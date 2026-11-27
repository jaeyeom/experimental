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

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/credentials"
	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/usage"
	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func TestRunHuman(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 0, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 0, time.UTC)
	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return &usage.Data{
			PlanName:        "Team",
			FiveHour:        usage.Float(2.4),
			SevenDay:        usage.Float(12),
			FiveHourResetAt: usage.Time(reset5),
			SevenDayResetAt: usage.Time(reset7),
		}, nil
	})

	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	want := strings.Join([]string{
		"plan: Team",
		"  5-hour    2%   resets 2026-07-24 03:29 UTC",
		"  7-day    12%   resets 2026-07-27 22:59 UTC",
		"",
	}, "\n")
	if got := stdout.String(); got != want {
		t.Fatalf("stdout mismatch\ngot:\n%q\nwant:\n%q", got, want)
	}
	if stderr.Len() != 0 {
		t.Fatalf("unexpected stderr: %q", stderr.String())
	}
}

func TestRunJSON(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	fetchedAt := time.Date(2026, 7, 24, 1, 0, 0, 0, time.UTC)
	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return &usage.Data{
			PlanName:        "Team",
			FiveHour:        usage.Float(33.3),
			SevenDay:        usage.Float(12),
			FiveHourResetAt: usage.Time(reset5),
			FetchedAt:       usage.Time(fetchedAt),
			Source:          "keychain:/Users/alice/.claude",
		}, nil
	})

	var stdout, stderr bytes.Buffer
	code := run([]string{"--json"}, &stdout, &stderr, fetcher)
	if code != exitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}

	var got map[string]any
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if got["planName"] != "Team" || got["fiveHour"] != 33.3 || got["sevenDay"] != float64(12) {
		t.Fatalf("unexpected json: %s", stdout.String())
	}
	if got["fiveHourResetAt"] != "2026-07-24T03:29:59.312Z" {
		t.Fatalf("fiveHourResetAt = %v", got["fiveHourResetAt"])
	}
	if got["sevenDayResetAt"] != nil {
		t.Fatalf("sevenDayResetAt want null, got %v", got["sevenDayResetAt"])
	}
	if got["fetchedAt"] != "2026-07-24T01:00:00Z" {
		t.Fatalf("fetchedAt = %v", got["fetchedAt"])
	}
	if got["source"] != "keychain:/Users/alice/.claude" {
		t.Fatalf("source = %v", got["source"])
	}
}

func TestRunQuotapaceAcceptedByQuotapaceParser(t *testing.T) {
	t.Parallel()

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
			"five_hour": {"utilization": 14.5, "resets_at": "2026-07-24T05:00:00Z"},
			"seven_day": {"utilization": 33.3, "resets_at": "2026-07-29T15:00:00Z"}
		}`))
	}))
	t.Cleanup(srv.Close)

	fetchedAt := time.Date(2026, 7, 24, 2, 0, 0, 0, time.UTC)
	client := &usage.Client{
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

	var stdout, stderr bytes.Buffer
	code := run([]string{"--quotapace"}, &stdout, &stderr, client)
	if code != exitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("unexpected stderr: %q", stderr.String())
	}

	fallbackNow := time.Date(2026, 7, 24, 2, 5, 0, 0, time.UTC)
	readings, err := quotapace.ParseReadingsJSONL(bytes.NewReader(stdout.Bytes()), "remote", "", fallbackNow)
	if err != nil {
		t.Fatalf("quotapace.ParseReadingsJSONL rejected --quotapace output: %v\noutput:\n%s", err, stdout.String())
	}
	if len(readings) != 2 {
		t.Fatalf("got %d readings, want 2", len(readings))
	}
	wantSource := "credentials-file:" + configDir
	if readings[0].Window != "5h" || readings[0].Percent != 14.5 || readings[0].Source != wantSource || !readings[0].At.Equal(fetchedAt) || readings[0].Profile != "remote" {
		t.Fatalf("5h reading = %+v", readings[0])
	}
	if readings[1].Window != "7d" || readings[1].Percent != 33.3 || readings[1].Source != wantSource || !readings[1].At.Equal(fetchedAt) || readings[1].Profile != "remote" {
		t.Fatalf("7d reading = %+v", readings[1])
	}

	store := quotapace.Store{Dir: t.TempDir()}
	for _, r := range readings {
		if _, err := store.Record(r, false); err != nil {
			t.Fatalf("store.Record(%+v): %v", r, err)
		}
	}
	stored, err := store.Readings("remote")
	if err != nil {
		t.Fatalf("store.Readings: %v", err)
	}
	if len(stored) != 2 || stored[1].Percent != 33.3 || stored[1].Source != wantSource {
		t.Fatalf("stored readings = %+v", stored)
	}
}

func TestRunMutuallyExclusiveFlags(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		t.Fatal("fetcher should not be called")
		return nil, nil
	})
	var stdout, stderr bytes.Buffer
	code := run([]string{"--json", "--quotapace"}, &stdout, &stderr, fetcher)
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if !strings.Contains(stderr.String(), "mutually exclusive") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunNoData(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return nil, usage.ErrNoData
	})
	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitNoData {
		t.Fatalf("exit = %d, want %d", code, exitNoData)
	}
	if stdout.Len() != 0 {
		t.Fatalf("stdout should be empty, got %q", stdout.String())
	}
	if !strings.Contains(stderr.String(), "no usage data") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunNilData(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return nil, nil
	})
	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitNoData {
		t.Fatalf("exit = %d, want %d; stderr=%q", code, exitNoData, stderr.String())
	}
}

func TestRunFetchError(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return nil, errors.New("boom")
	})
	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
	if !strings.Contains(stderr.String(), "boom") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunUnexpectedArgs(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		t.Fatal("fetcher should not be called")
		return nil, nil
	})
	var stdout, stderr bytes.Buffer
	code := run([]string{"extra"}, &stdout, &stderr, fetcher)
	if code != exitError {
		t.Fatalf("exit = %d, want %d", code, exitError)
	}
}

func TestRunCustomEndpoint(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return nil, usage.ErrCustomEndpoint
	})
	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitNoData {
		t.Fatalf("exit = %d, want %d; stderr=%q", code, exitNoData, stderr.String())
	}
	if !strings.Contains(stderr.String(), "ANTHROPIC_BASE_URL") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunUnauthorized(t *testing.T) {
	t.Parallel()

	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return nil, usage.ErrUnauthorized
	})
	var stdout, stderr bytes.Buffer
	code := run(nil, &stdout, &stderr, fetcher)
	if code != exitNoData {
		t.Fatalf("exit = %d, want %d; stderr=%q", code, exitNoData, stderr.String())
	}
	if !strings.Contains(stderr.String(), "re-login") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
