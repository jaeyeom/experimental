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
			FiveHour:        Int(2),
			SevenDay:        Int(12),
			FiveHourResetAt: Time(reset),
			SevenDayResetAt: Time(reset),
		}, nil
	})
	var buf bytes.Buffer
	if err := Write(context.Background(), &buf, f, false); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), "plan: Team") {
		t.Fatalf("got %q", buf.String())
	}
}

func TestWriteNoData(t *testing.T) {
	t.Parallel()

	f := FetcherFunc(func(context.Context) (*Data, error) {
		return nil, ErrNoData
	})
	var buf bytes.Buffer
	err := Write(context.Background(), &buf, f, false)
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v", err)
	}
}

func TestWriteNilData(t *testing.T) {
	t.Parallel()

	f := FetcherFunc(func(context.Context) (*Data, error) {
		return nil, nil
	})
	err := Write(context.Background(), &bytes.Buffer{}, f, false)
	if !errors.Is(err, ErrNoData) {
		t.Fatalf("got %v", err)
	}
}

func TestWriteNilFetcher(t *testing.T) {
	t.Parallel()
	err := Write(context.Background(), &bytes.Buffer{}, nil, false)
	if err == nil {
		t.Fatal("expected error")
	}
}
