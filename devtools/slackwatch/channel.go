package slackwatch

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"
)

// DefaultChannelCacheTTL is how long a cached channel id is trusted.
const DefaultChannelCacheTTL = 7 * 24 * time.Hour

// DefaultChannelLookupWait is the longest a name lookup waits out rate limits.
const DefaultChannelLookupWait = 60 * time.Second

// NamedChannel is a Slack channel name and id.
type NamedChannel struct {
	Name string
	ID   string
}

// ChannelPage is one page of conversations.list.
type ChannelPage struct {
	Channels []NamedChannel
	Next     string
}

// ChannelLister lists conversations one page at a time. An empty cursor
// is the first page. A rate-limited page returns a *RateLimitError.
type ChannelLister interface {
	ListChannels(ctx context.Context, cursor string) (ChannelPage, error)
}

// ChannelLookup resolves a channel name to an id. It reads and writes
// the channel cache in Store. List is not called on a fresh cache hit.
type ChannelLookup struct {
	Store *Store
	List  ChannelLister
	Now   func() time.Time
	// TTL is how long a cached id is trusted. Zero trusts nothing.
	TTL time.Duration
	// MaxWait bounds the total RetryAfter slept during one lookup.
	// Zero fails on the first rate limit.
	MaxWait time.Duration
	// Sleep waits out a rate limit. Nil uses Wait.
	Sleep func(ctx context.Context, d time.Duration) error
	// OnWait is called with RetryAfter before each sleep. Nil is ignored.
	OnWait func(d time.Duration)
}

// Wait sleeps for d, or returns early when ctx ends.
func Wait(ctx context.Context, d time.Duration) error {
	if d <= 0 {
		return nil
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("wait out rate limit: %w", ctx.Err())
	case <-timer.C:
		return nil
	}
}

// ChannelID returns the id for name. A cached id younger than TTL makes
// no List call.
func (l ChannelLookup) ChannelID(ctx context.Context, name string) (string, error) {
	if id, ok, err := l.cached(name); err != nil || ok {
		return id, err
	}
	return l.Resolve(ctx, name)
}

// Resolve lists channels for name, skipping the cache, and stores every
// channel it sees. The pagination cursor is kept across rate-limit retries.
func (l ChannelLookup) Resolve(ctx context.Context, name string) (string, error) {
	var cursor string
	var waited time.Duration
	for {
		page, err := l.List.ListChannels(ctx, cursor)
		if err != nil {
			waited, err = l.limitWait(ctx, name, err, waited)
			if err != nil {
				return "", err
			}
			continue
		}
		id, found, err := l.remember(name, page)
		if err != nil {
			return "", err
		}
		if found {
			return id, nil
		}
		if page.Next == "" {
			return "", resolveFailure(name, "not found")
		}
		cursor = page.Next
	}
}

func (l ChannelLookup) cached(name string) (string, bool, error) {
	if l.Store == nil || l.TTL <= 0 {
		return "", false, nil
	}
	id, seen, ok, err := l.Store.ChannelByName(name)
	if err != nil || !ok {
		return "", false, err
	}
	now := l.now()
	if now.Before(seen) || now.Sub(seen) > l.TTL {
		return "", false, nil
	}
	return id, true, nil
}

func (l ChannelLookup) limitWait(ctx context.Context, name string, err error, waited time.Duration) (time.Duration, error) {
	var rl *RateLimitError
	if !errors.As(err, &rl) {
		return waited, resolveFailure(name, err.Error())
	}
	if rl.RetryAfter <= 0 || waited+rl.RetryAfter > l.MaxWait {
		return waited, resolveFailure(name, "rate limit wait exceeded")
	}
	if l.OnWait != nil {
		l.OnWait(rl.RetryAfter)
	}
	if err := l.sleep(ctx, rl.RetryAfter); err != nil {
		return waited, err
	}
	return waited + rl.RetryAfter, nil
}

func (l ChannelLookup) remember(want string, page ChannelPage) (string, bool, error) {
	var found string
	now := l.now()
	for _, c := range page.Channels {
		if c.Name == "" || c.ID == "" {
			continue
		}
		if l.Store != nil {
			if err := l.Store.PutChannel(c.Name, c.ID, now); err != nil {
				return "", false, err
			}
		}
		if c.Name == want {
			found = c.ID
		}
	}
	if found == "" {
		return "", false, nil
	}
	return found, true, nil
}

func (l ChannelLookup) now() time.Time {
	if l.Now != nil {
		return l.Now()
	}
	return time.Now()
}

func (l ChannelLookup) sleep(ctx context.Context, d time.Duration) error {
	if l.Sleep != nil {
		return l.Sleep(ctx, d)
	}
	return Wait(ctx, d)
}

func resolveFailure(name, reason string) error {
	return fmt.Errorf("could not resolve #%s (%s); pass the channel id (C…) to --to", name, reason)
}

func channelNotFound(err error) bool {
	return err != nil && strings.Contains(err.Error(), "channel_not_found")
}

// PutChannel stores name → id as seen at seen.
func (s *Store) PutChannel(name, id string, seen time.Time) error {
	if name == "" || id == "" {
		return errors.New("channel cache needs a name and an id")
	}
	_, err := s.db.Exec(`
INSERT INTO channels (name, id, seen_at) VALUES (?, ?, ?)
ON CONFLICT(name) DO UPDATE SET id = excluded.id, seen_at = excluded.seen_at`,
		name, id, seen.Unix())
	if err != nil {
		return fmt.Errorf("store channel #%s: %w", name, err)
	}
	return nil
}

// ChannelByName returns the cached id and the time it was seen.
func (s *Store) ChannelByName(name string) (id string, seen time.Time, ok bool, err error) {
	var sec int64
	err = s.db.QueryRow(`SELECT id, seen_at FROM channels WHERE name = ?`, name).Scan(&id, &sec)
	if errors.Is(err, sql.ErrNoRows) {
		return "", time.Time{}, false, nil
	}
	if err != nil {
		return "", time.Time{}, false, fmt.Errorf("read channel #%s: %w", name, err)
	}
	return id, time.Unix(sec, 0), true, nil
}

// ChannelName returns the cached name for id.
func (s *Store) ChannelName(id string) (string, bool, error) {
	var name string
	err := s.db.QueryRow(`SELECT name FROM channels WHERE id = ?`, id).Scan(&name)
	if errors.Is(err, sql.ErrNoRows) {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("read channel %s: %w", id, err)
	}
	return name, true, nil
}

// ForgetChannel drops the cached name.
func (s *Store) ForgetChannel(name string) error {
	if _, err := s.db.Exec(`DELETE FROM channels WHERE name = ?`, name); err != nil {
		return fmt.Errorf("drop channel #%s: %w", name, err)
	}
	return nil
}

// ForgetChannelID drops every cached name for id.
func (s *Store) ForgetChannelID(id string) error {
	if id == "" {
		return nil
	}
	if _, err := s.db.Exec(`DELETE FROM channels WHERE id = ?`, id); err != nil {
		return fmt.Errorf("drop channel %s: %w", id, err)
	}
	return nil
}

// SetOutboxChannel stores a new destination channel on an outbox record.
func (s *Store) SetOutboxChannel(id int64, channel string, now time.Time) error {
	if _, err := s.db.Exec(`UPDATE outbox SET channel = ?, updated_at = ? WHERE id = ?`, channel, now.Unix(), id); err != nil {
		return fmt.Errorf("set outbox %d channel: %w", id, err)
	}
	return nil
}

// TouchChannel refreshes seen_at for a cached id. An unknown id is left alone.
func (s *Store) TouchChannel(id string, seen time.Time) error {
	if id == "" {
		return nil
	}
	if _, err := s.db.Exec(`UPDATE channels SET seen_at = ? WHERE id = ?`, seen.Unix(), id); err != nil {
		return fmt.Errorf("refresh channel %s: %w", id, err)
	}
	return nil
}
