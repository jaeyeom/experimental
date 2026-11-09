package slackwatch

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"strconv"
	"time"
)

// Fetched is a message read from the Slack Web API with its reactions.
// Each reaction's TS is the message ts. LatestReply is the ts of a thread
// parent's latest reply, as conversations.history reports it.
type Fetched struct {
	Message
	Reactions   []Reaction
	LatestReply string
}

// SlackReader is the read-only Slack Web API that catch-up uses. Each
// call returns every page. A rate-limited call returns a *RateLimitError.
type SlackReader interface {
	// Replies calls conversations.replies for messages after oldest.
	Replies(ctx context.Context, channel, threadTS, oldest string) ([]Fetched, error)
	// History calls conversations.history for messages after oldest.
	History(ctx context.Context, channel, oldest string) ([]Fetched, error)
	// Search calls search.messages.
	Search(ctx context.Context, query string) ([]Fetched, error)
}

// RateLimitError is a Slack 429 response.
type RateLimitError struct {
	RetryAfter time.Duration
}

func (e *RateLimitError) Error() string {
	return fmt.Sprintf("slack rate limited, retry after %s", e.RetryAfter)
}

const (
	// maxRateLimitRetries bounds how often one call waits out a 429.
	maxRateLimitRetries = 5
	// searchLookback is how far back the first search looks.
	searchLookback = 24 * time.Hour
	// searchScope is the cursor scope of the search catch-up.
	searchScope = "search"
)

// CatchUp fetches traffic missed while no events arrived and runs it
// through the same Processor as live events. Dedupe on (channel, ts) makes
// overlaps with events harmless.
type CatchUp struct {
	Slack SlackReader
	Proc  *Processor
	// Sleep waits out a rate limit. Nil waits on a timer.
	Sleep func(ctx context.Context, d time.Duration) error
}

func watchScope(id int64) string { return "watch:" + strconv.FormatInt(id, 10) }

// CatchUpWatch fetches messages after the watch's cursor (initially its
// since_ts): thread replies for a thread watch, channel history for a dm
// watch.
func (c *CatchUp) CatchUpWatch(ctx context.Context, w Watch) error {
	scope := watchScope(w.ID)
	cursor, err := c.Proc.Store.cursor(scope, w.SinceTS)
	if err != nil {
		return err
	}
	var msgs []Fetched
	err = c.retry(ctx, func() (err error) {
		if w.Kind == KindDM {
			msgs, err = c.Slack.History(ctx, w.Channel, cursor)
		} else {
			msgs, err = c.Slack.Replies(ctx, w.Channel, w.ThreadTS, cursor)
		}
		return err //nolint:wrapcheck // Wrapped below; retry needs the *RateLimitError.
	})
	if err == nil {
		err = c.handle(ctx, scope, cursor, msgs)
	}
	if err != nil {
		return fmt.Errorf("catch up watch %d: %w", w.ID, err)
	}
	return nil
}

// searchQueries find the token owner's own posts (for footers) and
// messages sent to or mentioning them (for inbound).
func searchQueries(self, after string) []string {
	return []string{
		"from:me after:" + after,
		"to:me after:" + after,
		"<@" + self + "> after:" + after,
	}
}

// catchUpSearch finds footer posts and inbound messages after the search
// cursor.
func (c *CatchUp) catchUpSearch(ctx context.Context) error {
	cursor, err := c.Proc.Store.cursor(searchScope, "")
	if err != nil {
		return err
	}
	if cursor == "" {
		cursor = unixTS(c.Proc.Now().Add(-searchLookback))
	}
	sec, err := tsSeconds(cursor)
	if err != nil {
		return fmt.Errorf("search cursor: %w", err)
	}
	// after: is a whole day in the searcher's time zone and excludes that
	// day. Go back two days and filter on ts.
	after := time.Unix(sec, 0).UTC().AddDate(0, 0, -2).Format(time.DateOnly)
	var all []Fetched
	for _, q := range searchQueries(c.Proc.Self, after) {
		var msgs []Fetched
		if err := c.retry(ctx, func() (err error) {
			msgs, err = c.Slack.Search(ctx, q)
			return err //nolint:wrapcheck // Wrapped below; retry needs the *RateLimitError.
		}); err != nil {
			return fmt.Errorf("catch up search %q: %w", q, err)
		}
		all = append(all, msgs...)
	}
	if err := c.handle(ctx, searchScope, cursor, all); err != nil {
		return fmt.Errorf("catch up search: %w", err)
	}
	return nil
}

// handle processes msgs newer than cursor in ts order, stores reactions
// on every fetched message, and then advances the cursor to the latest
// ts. On error the cursor stays, so the next run fetches the batch again.
func (c *CatchUp) handle(ctx context.Context, scope, cursor string, msgs []Fetched) error {
	slices.SortStableFunc(msgs, func(a, b Fetched) int { return CompareTS(a.TS, b.TS) })
	latest := cursor
	for _, m := range msgs {
		if CompareTS(m.TS, cursor) > 0 {
			if err := c.Proc.HandleMessage(ctx, m.Message); err != nil {
				return err
			}
		}
		for _, r := range m.Reactions {
			if err := c.Proc.HandleReaction(m.Channel, r); err != nil {
				return err
			}
		}
		if CompareTS(m.TS, latest) > 0 {
			latest = m.TS
		}
	}
	if latest == cursor {
		return nil
	}
	return c.Proc.Store.setCursor(scope, latest)
}

// retry runs call and waits out up to maxRateLimitRetries rate limits.
func (c *CatchUp) retry(ctx context.Context, call func() error) error {
	sleep := c.Sleep
	if sleep == nil {
		sleep = sleepContext
	}
	for i := 0; ; i++ {
		err := call()
		var rl *RateLimitError
		if !errors.As(err, &rl) || i == maxRateLimitRetries {
			return err
		}
		if err := sleep(ctx, rl.RetryAfter); err != nil {
			return err
		}
	}
}

func sleepContext(ctx context.Context, d time.Duration) error {
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("wait for rate limit: %w", ctx.Err())
	case <-t.C:
		return nil
	}
}

// Run catches up every watch that can still get a reply and every active
// subscription, then runs the search. A failing watch or subscription does
// not stop the others. The errors are joined.
func (c *CatchUp) Run(ctx context.Context) error {
	now := c.Proc.Now()
	watches, err := c.Proc.Store.catchUpWatches(now, c.Proc.Config.ReopenWindow.Duration)
	if err != nil {
		return err
	}
	subs, err := c.Proc.Store.ListSubs()
	if err != nil {
		return err
	}
	var errs []error
	for _, w := range watches {
		if err := ctx.Err(); err != nil {
			return fmt.Errorf("catch up: %w", err)
		}
		errs = append(errs, c.CatchUpWatch(ctx, w))
	}
	for _, s := range subs {
		if err := ctx.Err(); err != nil {
			return fmt.Errorf("catch up: %w", err)
		}
		if s.active(now) {
			errs = append(errs, c.CatchUpSub(ctx, s))
		}
	}
	errs = append(errs, c.catchUpSearch(ctx))
	return errors.Join(errs...)
}

// Sweep runs a catch-up and then closes quiet watches.
func (c *CatchUp) Sweep(ctx context.Context) error {
	err := c.Run(ctx)
	_, expErr := c.Proc.Store.ExpireWatches(c.Proc.Now(), c.Proc.Config.QuietExpiry.Duration)
	return errors.Join(err, expErr)
}

// catchUpWatches returns the watches that are not closed or closed within
// reopenWindow of now, since a reply to those reopens them.
func (s *Store) catchUpWatches(now time.Time, reopenWindow time.Duration) ([]Watch, error) {
	var out []Watch
	err := s.queryEach(func(rows *sql.Rows) error {
		var r watchRow
		if err := rows.Scan(r.dest()...); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		out = append(out, r.watch())
		return nil
	}, `SELECT `+watchCols+` FROM watches WHERE state <> ? OR closed_at >= ? ORDER BY id`,
		StateClosed, now.Add(-reopenWindow).Unix())
	if err != nil {
		return nil, fmt.Errorf("list watches to catch up: %w", err)
	}
	return out, nil
}

// ExpireWatches closes watches past their deadline and returns how many
// it closed. The deadline is until when set. Otherwise it is quiet after
// the later of creation and the latest reply by someone other than the
// token owner. Self replies update last_activity but do not keep a watch
// open. Closing never notifies.
func (s *Store) ExpireWatches(now time.Time, quiet time.Duration) (int64, error) {
	res, err := s.db.Exec(`
UPDATE watches SET state = ?1, closed_at = ?2,
	close_reason = CASE WHEN until IS NULL THEN 'quiet' ELSE 'until passed' END
WHERE state <> ?1 AND ?2 > COALESCE(until, ?3 + max(created_at, COALESCE(
	(SELECT MAX(CAST(ts AS REAL)) FROM replies r WHERE r.watch_id = watches.id AND r.is_self = 0), 0)))`,
		StateClosed, now.Unix(), int64(quiet/time.Second))
	if err != nil {
		return 0, fmt.Errorf("expire watches: %w", err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("expire watches: %w", err)
	}
	return n, nil
}

// cursor returns the last handled ts for scope, or def when unset.
func (s *Store) cursor(scope, def string) (string, error) {
	var ts string
	err := s.db.QueryRow(`SELECT ts FROM cursors WHERE scope = ?`, scope).Scan(&ts)
	if errors.Is(err, sql.ErrNoRows) {
		return def, nil
	}
	if err != nil {
		return "", fmt.Errorf("read cursor %s: %w", scope, err)
	}
	return ts, nil
}

func (s *Store) setCursor(scope, ts string) error {
	if _, err := s.db.Exec(`INSERT INTO cursors (scope, ts) VALUES (?, ?)
ON CONFLICT (scope) DO UPDATE SET ts = excluded.ts`, scope, ts); err != nil {
		return fmt.Errorf("set cursor %s: %w", scope, err)
	}
	return nil
}
