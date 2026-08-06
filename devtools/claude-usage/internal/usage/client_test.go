package usage

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"math"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/credentials"
)

type staticTokens struct {
	creds *credentials.Creds
	err   error
}

func (s staticTokens) Resolve(context.Context) (*credentials.Creds, error) {
	return s.creds, s.err
}

func TestClientFetchSuccess(t *testing.T) {
	t.Parallel()

	var gotAuth, gotBeta, gotUA string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotAuth = r.Header.Get("Authorization")
		gotBeta = r.Header.Get("anthropic-beta")
		gotUA = r.Header.Get("User-Agent")
		if r.URL.Path != UsageAPIPath {
			t.Errorf("path = %q", r.URL.Path)
		}
		_ = json.NewEncoder(w).Encode(map[string]any{
			"five_hour": map[string]any{
				"utilization": 2.4,
				"resets_at":   "2026-07-24T03:29:59.312Z",
			},
			"seven_day": map[string]any{
				"utilization": 12,
				"resets_at":   "2026-07-27T22:59:59.312Z",
			},
		})
	}))
	t.Cleanup(srv.Close)

	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken:      "test-token",
			SubscriptionType: "team",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}

	data, err := c.Fetch(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if data.PlanName != "Team" {
		t.Errorf("PlanName = %q", data.PlanName)
	}
	if data.FiveHour == nil || *data.FiveHour != 2 {
		t.Errorf("FiveHour = %v", data.FiveHour)
	}
	if data.SevenDay == nil || *data.SevenDay != 12 {
		t.Errorf("SevenDay = %v", data.SevenDay)
	}
	want5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	if data.FiveHourResetAt == nil || !data.FiveHourResetAt.Equal(want5) {
		t.Errorf("FiveHourResetAt = %v", data.FiveHourResetAt)
	}
	if gotAuth != "Bearer test-token" {
		t.Errorf("Authorization = %q", gotAuth)
	}
	if gotBeta != oauthBetaHeader {
		t.Errorf("anthropic-beta = %q", gotBeta)
	}
	if gotUA != DefaultUserAgent {
		t.Errorf("User-Agent = %q", gotUA)
	}
}

func TestClientFetchUnauthorized(t *testing.T) {
	t.Parallel()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		_, _ = io.WriteString(w, `{"error":"unauthorized"}`)
	}))
	t.Cleanup(srv.Close)

	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "bad", SubscriptionType: "pro",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrUnauthorized) {
		t.Fatalf("got %v, want ErrUnauthorized", err)
	}
}

func TestClientFetchRateLimited(t *testing.T) {
	t.Parallel()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusTooManyRequests)
	}))
	t.Cleanup(srv.Close)

	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "tok", SubscriptionType: "team",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrRateLimited) {
		t.Fatalf("got %v, want ErrRateLimited", err)
	}
}

func TestClientFetchNoCredentials(t *testing.T) {
	t.Parallel()
	c := &Client{
		Tokens: staticTokens{err: credentials.ErrNotFound},
		Env:    func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v, want ErrNoData", err)
	}
}

func TestClientFetchExpiredCredentials(t *testing.T) {
	t.Parallel()
	c := &Client{
		Tokens: staticTokens{err: credentials.ErrExpired},
		Env:    func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v, want ErrNoData", err)
	}
}

func TestClientFetchAPISubscription(t *testing.T) {
	t.Parallel()
	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "tok", SubscriptionType: "api",
		}},
		Env: func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v, want ErrNoData", err)
	}
}

func TestClientFetchCustomEndpoint(t *testing.T) {
	t.Parallel()
	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "tok", SubscriptionType: "team",
		}},
		Env: func(k string) string {
			if k == "ANTHROPIC_BASE_URL" {
				return "https://example.com/v1"
			}
			return ""
		},
	}
	_, err := c.Fetch(context.Background())
	if !errors.Is(err, ErrCustomEndpoint) {
		t.Fatalf("got %v, want ErrCustomEndpoint", err)
	}
}

func TestClientFetchEmptyBody(t *testing.T) {
	t.Parallel()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
	}))
	t.Cleanup(srv.Close)

	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "tok", SubscriptionType: "team",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if err == nil || !strings.Contains(err.Error(), "empty body") {
		t.Fatalf("got %v, want empty body error", err)
	}
}

func TestClientFetchNullWindows(t *testing.T) {
	t.Parallel()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		_, _ = io.WriteString(w, `{"five_hour":null,"seven_day":null}`)
	}))
	t.Cleanup(srv.Close)

	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: "tok", SubscriptionType: "pro",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}
	data, err := c.Fetch(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if data.PlanName != "Pro" {
		t.Fatalf("PlanName = %q", data.PlanName)
	}
	if data.FiveHour != nil || data.SevenDay != nil {
		t.Fatalf("expected nil windows, got five=%v seven=%v", data.FiveHour, data.SevenDay)
	}
}

func TestUsingCustomAPIEndpoint(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		env  map[string]string
		want bool
	}{
		{name: "unset", env: nil, want: false},
		{name: "default anthropic", env: map[string]string{"ANTHROPIC_BASE_URL": "https://api.anthropic.com"}, want: false},
		{name: "default with path", env: map[string]string{"ANTHROPIC_BASE_URL": "https://api.anthropic.com/v1"}, want: false},
		{name: "custom", env: map[string]string{"ANTHROPIC_BASE_URL": "https://proxy.example/v1"}, want: true},
		{name: "alt env", env: map[string]string{"ANTHROPIC_API_BASE_URL": "http://localhost:8080"}, want: true},
		{name: "garbage", env: map[string]string{"ANTHROPIC_BASE_URL": "not a url"}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got := UsingCustomAPIEndpoint(func(k string) string {
				if tt.env == nil {
					return ""
				}
				return tt.env[k]
			})
			if got != tt.want {
				t.Fatalf("got %v, want %v", got, tt.want)
			}
		})
	}
}

func TestParseUtilizationClamp(t *testing.T) {
	t.Parallel()
	v := 150.7
	got := parseUtilization(&v)
	if got == nil || *got != 100 {
		t.Fatalf("got %v, want 100", got)
	}
	neg := -3.2
	got = parseUtilization(&neg)
	if got == nil || *got != 0 {
		t.Fatalf("got %v, want 0", got)
	}
	nan := math.NaN()
	if parseUtilization(&nan) != nil {
		t.Fatal("NaN should be nil")
	}
}

func TestClientDoesNotLeakTokenOnHTTPError(t *testing.T) {
	t.Parallel()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		_, _ = io.WriteString(w, "fail")
	}))
	t.Cleanup(srv.Close)

	// Distinctive token-like value for leak detection (not a real credential).
	secret := "test-token-must-not-appear-in-errors"
	c := &Client{
		Tokens: staticTokens{creds: &credentials.Creds{
			AccessToken: secret, SubscriptionType: "team",
		}},
		HTTPClient: srv.Client(),
		BaseURL:    srv.URL,
		Env:        func(string) string { return "" },
	}
	_, err := c.Fetch(context.Background())
	if err == nil {
		t.Fatal("expected error")
	}
	if strings.Contains(err.Error(), secret) {
		t.Fatalf("token leaked in error: %v", err)
	}
}
