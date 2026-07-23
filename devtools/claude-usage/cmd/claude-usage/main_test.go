package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/usage"
)

func TestRunHuman(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 0, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 0, time.UTC)
	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return &usage.Data{
			PlanName:        "Team",
			FiveHour:        usage.Int(2),
			SevenDay:        usage.Int(12),
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
	fetcher := usage.FetcherFunc(func(context.Context) (*usage.Data, error) {
		return &usage.Data{
			PlanName:        "Team",
			FiveHour:        usage.Int(2),
			SevenDay:        usage.Int(12),
			FiveHourResetAt: usage.Time(reset5),
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
	if got["planName"] != "Team" || got["fiveHour"] != float64(2) || got["sevenDay"] != float64(12) {
		t.Fatalf("unexpected json: %s", stdout.String())
	}
	if got["fiveHourResetAt"] != "2026-07-24T03:29:59.312Z" {
		t.Fatalf("fiveHourResetAt = %v", got["fiveHourResetAt"])
	}
	if got["sevenDayResetAt"] != nil {
		t.Fatalf("sevenDayResetAt want null, got %v", got["sevenDayResetAt"])
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
