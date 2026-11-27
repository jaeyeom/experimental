package usage

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestWriteHuman(t *testing.T) {
	t.Parallel()

	reset := time.Date(2026, 7, 24, 3, 29, 0, 0, time.UTC)
	f := FetcherFunc(func(context.Context) (*Data, error) {
		return &Data{
			PlanName:        "Team",
			FiveHour:        Float(2),
			SevenDay:        Float(12),
			FiveHourResetAt: Time(reset),
			SevenDayResetAt: Time(reset),
		}, nil
	})
	var buf bytes.Buffer
	if err := Write(context.Background(), &buf, f, OutputHuman); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), "plan: Team") {
		t.Fatalf("got %q", buf.String())
	}
}

func TestWriteQuotapace(t *testing.T) {
	t.Parallel()

	reset := time.Date(2026, 7, 24, 3, 29, 0, 0, time.UTC)
	fetched := time.Date(2026, 7, 24, 1, 0, 0, 0, time.UTC)
	f := FetcherFunc(func(context.Context) (*Data, error) {
		return &Data{
			PlanName:        "Team",
			FiveHour:        Float(2.5),
			SevenDay:        Float(33.3),
			FiveHourResetAt: Time(reset),
			SevenDayResetAt: Time(reset),
			FetchedAt:       Time(fetched),
			Source:          "keychain:/Users/alice/.claude",
		}, nil
	})
	var buf bytes.Buffer
	if err := Write(context.Background(), &buf, f, OutputQuotapace); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), `"percent":33.3`) {
		t.Fatalf("got %q", buf.String())
	}
}

func TestWriteNoData(t *testing.T) {
	t.Parallel()

	f := FetcherFunc(func(context.Context) (*Data, error) {
		return nil, ErrNoData
	})
	var buf bytes.Buffer
	err := Write(context.Background(), &buf, f, OutputHuman)
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v", err)
	}
}

func TestWriteNilData(t *testing.T) {
	t.Parallel()

	f := FetcherFunc(func(context.Context) (*Data, error) {
		return nil, nil
	})
	err := Write(context.Background(), &bytes.Buffer{}, f, OutputHuman)
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v", err)
	}
}

func TestWriteNilFetcher(t *testing.T) {
	t.Parallel()
	err := Write(context.Background(), &bytes.Buffer{}, nil, OutputHuman)
	if err == nil {
		t.Fatal("expected error")
	}
}
