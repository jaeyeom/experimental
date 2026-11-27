package claude

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

func TestReadings(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 5, 0, 0, 0, time.UTC)
	reset7 := time.Date(2026, 7, 29, 15, 0, 0, 0, time.UTC)
	fetchedAt := time.Date(2026, 7, 24, 2, 0, 0, 0, time.UTC)
	fallback := time.Date(2026, 7, 24, 2, 5, 0, 0, time.UTC)

	rs, err := Readings(&Data{
		PlanName:        "Max",
		FiveHour:        Float(14.5),
		SevenDay:        Float(33.3),
		FiveHourResetAt: Time(reset5),
		SevenDayResetAt: Time(reset7),
		FetchedAt:       Time(fetchedAt),
		Source:          "keychain:/Users/alice/.claude",
	}, "work", fallback)
	if err != nil {
		t.Fatalf("Readings: %v", err)
	}
	if len(rs) != 2 {
		t.Fatalf("got %d readings, want 2", len(rs))
	}
	if rs[0].Window != "5h" || rs[0].Percent != 14.5 || !rs[0].At.Equal(fetchedAt) || rs[0].Profile != "work" || rs[0].Source != "keychain:/Users/alice/.claude" {
		t.Fatalf("5h reading = %+v", rs[0])
	}
	if rs[1].Window != "7d" || rs[1].Percent != 33.3 || !rs[1].At.Equal(fetchedAt) || rs[1].Profile != "work" {
		t.Fatalf("7d reading = %+v", rs[1])
	}

	// Fallback timestamp when FetchedAt is omitted.
	rsFallback, err := Readings(&Data{
		FiveHour:        Float(5),
		FiveHourResetAt: Time(reset5),
	}, "work", fallback)
	if err != nil {
		t.Fatalf("Readings fallback: %v", err)
	}
	if len(rsFallback) != 1 || !rsFallback[0].At.Equal(fallback) {
		t.Fatalf("fallback reading = %+v", rsFallback)
	}

	// Empty windows return ErrNoData.
	if _, err := Readings(&Data{PlanName: "Pro"}, "work", fallback); !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v, want ErrNoData", err)
	}
}
