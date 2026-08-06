package usage

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/credentials"
)

const (
	// DefaultUsageAPIBase is the Anthropic API origin used by Claude Code OAuth usage.
	DefaultUsageAPIBase = "https://api.anthropic.com"
	// UsageAPIPath is the OAuth usage endpoint path.
	UsageAPIPath = "/api/oauth/usage"
	// DefaultUserAgent matches claude-hud / Claude Code usage requests.
	DefaultUserAgent = "claude-code/2.1"
	// DefaultTimeout is the HTTP client timeout when none is configured.
	DefaultTimeout = 15 * time.Second
	// oauthBetaHeader is required by the OAuth usage endpoint.
	oauthBetaHeader = "oauth-2025-04-20"
)

// Sentinel / classified errors for the Anthropic usage API.
var (
	// ErrCustomEndpoint means ANTHROPIC_BASE_URL points away from api.anthropic.com,
	// so the OAuth usage API is not applicable.
	ErrCustomEndpoint = errors.New("custom anthropic base url; oauth usage api not applicable")
	// ErrUnauthorized means the access token was rejected (401/403).
	ErrUnauthorized = errors.New("usage api unauthorized")
	// ErrRateLimited means the usage API returned HTTP 429.
	ErrRateLimited = errors.New("usage api rate limited")
)

// TokenSource supplies an OAuth access token and optional subscription type.
// Implementations must not log the token.
type TokenSource interface {
	Resolve(ctx context.Context) (*credentials.Creds, error)
}

// Client fetches usage from Anthropic's OAuth usage endpoint.
type Client struct {
	// Tokens resolves Claude Code OAuth credentials.
	Tokens TokenSource
	// HTTPClient performs requests. If nil, a client with DefaultTimeout is used.
	HTTPClient *http.Client
	// BaseURL overrides the API origin (default: DefaultUsageAPIBase). Tests inject httptest.
	BaseURL string
	// UserAgent overrides the User-Agent header (default: DefaultUserAgent).
	UserAgent string
	// Env looks up environment variables (default: os.Getenv).
	Env func(string) string
}

// Fetch implements Fetcher.
func (c *Client) Fetch(ctx context.Context) (*Data, error) {
	if c == nil {
		return nil, fmt.Errorf("usage client is nil")
	}
	if c.Tokens == nil {
		return nil, fmt.Errorf("usage client token source is nil")
	}
	env := c.Env
	if env == nil {
		env = os.Getenv
	}
	if UsingCustomAPIEndpoint(env) {
		return nil, ErrCustomEndpoint
	}

	creds, planName, err := c.resolvePlan(ctx)
	if err != nil {
		return nil, err
	}
	body, err := c.getUsage(ctx, creds.AccessToken)
	if err != nil {
		return nil, err
	}
	return mapAPIResponse(body, planName)
}

func (c *Client) resolvePlan(ctx context.Context) (*credentials.Creds, string, error) {
	creds, err := c.Tokens.Resolve(ctx)
	if err != nil {
		if errors.Is(err, credentials.ErrNotFound) || errors.Is(err, credentials.ErrExpired) {
			return nil, "", fmt.Errorf("%w: %v", ErrNoData, err)
		}
		return nil, "", fmt.Errorf("resolve credentials: %w", err)
	}
	if creds == nil || creds.AccessToken == "" {
		return nil, "", ErrNoData
	}
	planName := credentials.PlanName(creds.SubscriptionType)
	if planName == "" {
		// API-key style subscription or missing type: usage windows do not apply.
		return nil, "", ErrNoData
	}
	return creds, planName, nil
}

func (c *Client) getUsage(ctx context.Context, accessToken string) ([]byte, error) {
	httpClient := c.HTTPClient
	if httpClient == nil {
		httpClient = &http.Client{Timeout: DefaultTimeout}
	}

	base := strings.TrimRight(c.BaseURL, "/")
	if base == "" {
		base = DefaultUsageAPIBase
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, base+UsageAPIPath, nil)
	if err != nil {
		return nil, fmt.Errorf("build usage request: %w", err)
	}
	req.Header.Set("Authorization", "Bearer "+accessToken)
	req.Header.Set("anthropic-beta", oauthBetaHeader)
	ua := c.UserAgent
	if ua == "" {
		ua = DefaultUserAgent
	}
	req.Header.Set("User-Agent", ua)

	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("usage api request: %w", err)
	}
	defer resp.Body.Close() //nolint:errcheck

	body, err := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	if err != nil {
		return nil, fmt.Errorf("read usage api response: %w", err)
	}
	if err := classifyHTTPStatus(resp.StatusCode); err != nil {
		return nil, err
	}
	return body, nil
}

func classifyHTTPStatus(code int) error {
	switch code {
	case http.StatusOK:
		return nil
	case http.StatusUnauthorized, http.StatusForbidden:
		return fmt.Errorf("%w: http %d", ErrUnauthorized, code)
	case http.StatusTooManyRequests:
		return fmt.Errorf("%w: http 429", ErrRateLimited)
	default:
		return fmt.Errorf("usage api http %d", code)
	}
}

// apiResponse is the Anthropic OAuth usage JSON shape (snake_case wire format).
type apiResponse struct {
	FiveHour *apiWindow `json:"five_hour"` //nolint:tagliatelle // Anthropic API wire format
	SevenDay *apiWindow `json:"seven_day"` //nolint:tagliatelle // Anthropic API wire format
}

type apiWindow struct {
	Utilization *float64 `json:"utilization"`
	ResetsAt    *string  `json:"resets_at"` //nolint:tagliatelle // Anthropic API wire format
}

func mapAPIResponse(body []byte, planName string) (*Data, error) {
	if len(strings.TrimSpace(string(body))) == 0 {
		return nil, fmt.Errorf("usage api returned empty body")
	}
	var raw apiResponse
	if err := json.Unmarshal(body, &raw); err != nil {
		return nil, fmt.Errorf("parse usage api json: %w", err)
	}
	d := &Data{PlanName: planName}
	if raw.FiveHour != nil {
		d.FiveHour = parseUtilization(raw.FiveHour.Utilization)
		t, err := parseAPITime(raw.FiveHour.ResetsAt)
		if err != nil {
			return nil, err
		}
		d.FiveHourResetAt = t
	}
	if raw.SevenDay != nil {
		d.SevenDay = parseUtilization(raw.SevenDay.Utilization)
		t, err := parseAPITime(raw.SevenDay.ResetsAt)
		if err != nil {
			return nil, err
		}
		d.SevenDayResetAt = t
	}
	return d, nil
}

func parseUtilization(v *float64) *int {
	if v == nil {
		return nil
	}
	if math.IsNaN(*v) || math.IsInf(*v, 0) {
		return nil
	}
	// API returns 0–100 utilization percent; clamp and round like claude-hud.
	n := int(math.Round(math.Max(0, math.Min(100, *v))))
	return &n
}

func parseAPITime(s *string) (*time.Time, error) {
	if s == nil || *s == "" {
		return nil, nil
	}
	t, err := time.Parse(time.RFC3339Nano, *s)
	if err != nil {
		t, err = time.Parse(time.RFC3339, *s)
		if err != nil {
			// Invalid date: ignore rather than fail the whole snapshot (claude-hud returns null).
			return nil, nil
		}
	}
	return &t, nil
}

// UsingCustomAPIEndpoint reports whether env configures a non-Anthropic API base.
// When true, the OAuth usage endpoint is not applicable.
func UsingCustomAPIEndpoint(env func(string) string) bool {
	if env == nil {
		env = os.Getenv
	}
	base := strings.TrimSpace(env("ANTHROPIC_BASE_URL"))
	if base == "" {
		base = strings.TrimSpace(env("ANTHROPIC_API_BASE_URL"))
	}
	if base == "" {
		return false
	}
	u, err := url.Parse(base)
	if err != nil || u.Scheme == "" || u.Host == "" {
		// Unparseable custom value: treat as custom (safer than calling Anthropic).
		return true
	}
	origin := strings.ToLower(u.Scheme) + "://" + strings.ToLower(u.Host)
	return origin != "https://api.anthropic.com"
}

// DefaultClient returns a Client wired to the real Keychain/file resolver.
func DefaultClient() *Client {
	return &Client{
		Tokens: &credentials.Resolver{},
	}
}
