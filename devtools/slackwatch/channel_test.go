package slackwatch

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

type listFunc func(context.Context, string) (ChannelPage, error)

func (f listFunc) ListChannels(ctx context.Context, cursor string) (ChannelPage, error) {
	return f(ctx, cursor)
}

func TestChannelIDCacheHitSkipsList(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	if err := s.PutChannel("deploys", "C9", now); err != nil {
		t.Fatal(err)
	}
	var calls int
	id, err := (ChannelLookup{
		Store: s, Now: func() time.Time { return now }, TTL: DefaultChannelCacheTTL,
		List: listFunc(func(context.Context, string) (ChannelPage, error) {
			calls++
			return ChannelPage{}, errors.New("list called")
		}),
	}).ChannelID(context.Background(), "deploys")
	if err != nil || id != "C9" || calls != 0 {
		t.Fatalf("ChannelID = %q, %v, calls %d", id, err, calls)
	}
}

func TestChannelIDCacheMissStoresPages(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	var cursors []string
	id, err := (ChannelLookup{
		Store: s, Now: func() time.Time { return now }, TTL: DefaultChannelCacheTTL, MaxWait: DefaultChannelLookupWait,
		List: listFunc(func(_ context.Context, cursor string) (ChannelPage, error) {
			cursors = append(cursors, cursor)
			if cursor == "" {
				return ChannelPage{Channels: []NamedChannel{{Name: "general", ID: "C1"}}, Next: "p2"}, nil
			}
			return ChannelPage{Channels: []NamedChannel{{Name: "deploys", ID: "C9"}}}, nil
		}),
	}).ChannelID(context.Background(), "deploys")
	if err != nil || id != "C9" {
		t.Fatalf("ChannelID = %q, %v", id, err)
	}
	if strings.Join(cursors, ",") != ",p2" {
		t.Errorf("cursors = %q", cursors)
	}
	got, seen, ok, err := s.ChannelByName("deploys")
	if err != nil || !ok || got != "C9" || !seen.Equal(now) {
		t.Errorf("cached deploys = %q, %v, %v, %v", got, seen, ok, err)
	}
	if got, _, ok, err := s.ChannelByName("general"); err != nil || !ok || got != "C1" {
		t.Errorf("cached general = %q, %v, %v", got, ok, err)
	}
}

func TestChannelIDRateLimitResumesCursor(t *testing.T) {
	s := openTestStore(t)
	var cursors []string
	var waits []time.Duration
	id, err := (ChannelLookup{
		Store: s, TTL: DefaultChannelCacheTTL, MaxWait: DefaultChannelLookupWait,
		Sleep:  func(context.Context, time.Duration) error { return nil },
		OnWait: func(d time.Duration) { waits = append(waits, d) },
		List: listFunc(func(_ context.Context, cursor string) (ChannelPage, error) {
			cursors = append(cursors, cursor)
			switch len(cursors) {
			case 1:
				return ChannelPage{Channels: []NamedChannel{{Name: "general", ID: "C1"}}, Next: "p2"}, nil
			case 2:
				return ChannelPage{}, &RateLimitError{RetryAfter: 30 * time.Second}
			default:
				return ChannelPage{Channels: []NamedChannel{{Name: "deploys", ID: "C9"}}}, nil
			}
		}),
	}).ChannelID(context.Background(), "deploys")
	if err != nil || id != "C9" {
		t.Fatalf("ChannelID = %q, %v", id, err)
	}
	if strings.Join(cursors, ",") != ",p2,p2" {
		t.Errorf("cursors = %q, want a retry of p2", cursors)
	}
	if len(waits) != 1 || waits[0] != 30*time.Second {
		t.Errorf("waits = %v", waits)
	}
}

func TestChannelIDRateLimitPastBound(t *testing.T) {
	var slept bool
	_, err := (ChannelLookup{
		TTL: DefaultChannelCacheTTL, MaxWait: 0,
		Sleep: func(context.Context, time.Duration) error { slept = true; return nil },
		List: listFunc(func(context.Context, string) (ChannelPage, error) {
			return ChannelPage{}, &RateLimitError{RetryAfter: 30 * time.Second}
		}),
	}).ChannelID(context.Background(), "deploys")
	if err == nil || !strings.Contains(err.Error(), "could not resolve #deploys (rate limit wait exceeded)") || !strings.Contains(err.Error(), "pass the channel id (C…) to --to") {
		t.Fatalf("err = %v", err)
	}
	if slept {
		t.Error("slept past the bound")
	}
}

func TestEventAndWatchRefreshCachedChannel(t *testing.T) {
	s := openTestStore(t)
	old := testNow().Add(-time.Hour)
	if err := s.PutChannel("general", "C1", old); err != nil {
		t.Fatal(err)
	}
	now := testNow()
	if _, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1.000100"}, WatchUpdate{}, now); err != nil {
		t.Fatal(err)
	}
	if _, seen, ok, err := s.ChannelByName("general"); err != nil || !ok || !seen.Equal(now) {
		t.Fatalf("after watch, seen = %v, %v, %v", seen, ok, err)
	}
	later := now.Add(time.Minute)
	p := &Processor{Store: s, Notifier: &recordingNotifier{}, Now: func() time.Time { return later }}
	if err := p.HandleMessage(context.Background(), Message{Channel: "C1", TS: "2.000100", User: "U2", Text: "hi"}); err != nil {
		t.Fatal(err)
	}
	if _, seen, ok, err := s.ChannelByName("general"); err != nil || !ok || !seen.Equal(later) {
		t.Fatalf("after event, seen = %v, %v, %v", seen, ok, err)
	}
}
