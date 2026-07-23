// Package usage defines Claude Code rate-limit usage data and fetching.
package usage

import (
	"context"
	"errors"
	"fmt"
	"io"
	"time"
)

// ErrNoData indicates usage data is unavailable (API-key user, expired
// credentials, or the usage API returned nothing usable).
var ErrNoData = errors.New("no usage data")

// Data is the stable usage snapshot exposed by the CLI (human + JSON).
type Data struct {
	PlanName        string     `json:"planName"`
	FiveHour        *int       `json:"fiveHour"`
	SevenDay        *int       `json:"sevenDay"`
	FiveHourResetAt *time.Time `json:"fiveHourResetAt"`
	SevenDayResetAt *time.Time `json:"sevenDayResetAt"`
}

// Fetcher loads current usage. Implementations may call the Anthropic OAuth
// usage API, bridge to claude-hud, or return fixtures in tests.
type Fetcher interface {
	Fetch(ctx context.Context) (*Data, error)
}

// FetcherFunc adapts a function to the Fetcher interface.
type FetcherFunc func(ctx context.Context) (*Data, error)

// Fetch implements Fetcher.
func (f FetcherFunc) Fetch(ctx context.Context) (*Data, error) {
	return f(ctx)
}

// Write fetches usage and writes human or JSON output.
// Returns ErrNoData when the fetcher reports no usable data (including a nil
// *Data with a nil error).
func Write(ctx context.Context, w io.Writer, f Fetcher, asJSON bool) error {
	if f == nil {
		return fmt.Errorf("usage fetcher is nil")
	}
	data, err := f.Fetch(ctx)
	if err != nil {
		return fmt.Errorf("fetch usage: %w", err)
	}
	if data == nil {
		return ErrNoData
	}
	if asJSON {
		return FormatJSON(w, data)
	}
	return FormatHuman(w, data)
}
