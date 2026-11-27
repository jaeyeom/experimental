// Package claude fetches and formats Claude Code subscription rate-limit usage.
package claude

import (
	"context"
	"errors"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

// ErrNoData indicates usage data is unavailable (API-key user, expired
// credentials, or the usage API returned nothing usable).
var ErrNoData = errors.New("no usage data")

// Data is the stable usage snapshot exposed by the CLI (human + JSON).
type Data struct {
	PlanName        string     `json:"planName"`
	FiveHour        *float64   `json:"fiveHour"`
	SevenDay        *float64   `json:"sevenDay"`
	FiveHourResetAt *time.Time `json:"fiveHourResetAt"`
	SevenDayResetAt *time.Time `json:"sevenDayResetAt"`
	FetchedAt       *time.Time `json:"fetchedAt,omitempty"`
	Source          string     `json:"source,omitempty"`
}

// OutputFormat selects the output rendering for Write.
type OutputFormat int

// Supported output formats for Write.
const (
	OutputHuman OutputFormat = iota
	OutputJSON
	OutputQuotapace
)

// Fetcher loads current Claude Code usage.
type Fetcher interface {
	Fetch(ctx context.Context) (*Data, error)
}

// FetcherFunc adapts a function to the Fetcher interface.
type FetcherFunc func(ctx context.Context) (*Data, error)

// Fetch implements Fetcher.
func (f FetcherFunc) Fetch(ctx context.Context) (*Data, error) {
	return f(ctx)
}

// Write fetches usage and writes human, JSON, or quotapace JSONL output.
// Returns ErrNoData when the fetcher reports no usable data (including a nil
// *Data with a nil error).
func Write(ctx context.Context, w io.Writer, f Fetcher, format OutputFormat) error {
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
	return Format(w, data, format)
}

// Format renders an already-fetched usage snapshot in the requested format.
func Format(w io.Writer, data *Data, format OutputFormat) error {
	if data == nil {
		return ErrNoData
	}
	switch format {
	case OutputHuman:
		return FormatHuman(w, data)
	case OutputJSON:
		return FormatJSON(w, data)
	case OutputQuotapace:
		return FormatQuotapace(w, data)
	default:
		return fmt.Errorf("unknown output format %d", format)
	}
}

// Readings converts non-empty usage windows in data into quotapace readings
// for profile. Returns ErrNoData when neither window has both utilization and
// a reset time.
func Readings(data *Data, profile string, fallbackNow time.Time) ([]quotapace.Reading, error) {
	if data == nil {
		return nil, ErrNoData
	}
	at := fallbackNow.UTC()
	if data.FetchedAt != nil && !data.FetchedAt.IsZero() {
		at = data.FetchedAt.UTC()
	}
	var out []quotapace.Reading
	for _, win := range completeWindows(data) {
		out = append(out, quotapace.Reading{
			At:       at,
			Profile:  profile,
			Window:   win.name,
			Percent:  win.percent,
			ResetsAt: win.resetsAt,
			Source:   data.Source,
		})
	}
	if len(out) == 0 {
		return nil, ErrNoData
	}
	return out, nil
}
