package slackwatch

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"
)

// Sub is a channel subscription: every new message in Channel notifies
// Owner. A zero Until never ends.
type Sub struct {
	ID          int64     `json:"id"`
	Channel     string    `json:"channel"`
	Owner       string    `json:"owner"`
	Purpose     string    `json:"purpose"`
	Until       time.Time `json:"until,omitzero"`
	IncludeSelf bool      `json:"includeSelf"`
}

func (s Sub) active(now time.Time) bool { return s.Until.IsZero() || now.Before(s.Until) }

// subThreadLookback is how far before a subscription's cursor catch-up
// looks for thread parents whose replies may be new.
const subThreadLookback = 7 * 24 * time.Hour

func subScope(id int64) string { return "sub:" + strconv.FormatInt(id, 10) }

// unixTS is the Slack ts at the start of t's second.
func unixTS(t time.Time) string { return strconv.FormatInt(t.Unix(), 10) + ".000000" }

const subCols = `id, channel, owner, purpose, until, include_self`

func scanSub(rows *sql.Rows) (Sub, error) {
	var s Sub
	var until sql.NullInt64
	if err := rows.Scan(&s.ID, &s.Channel, &s.Owner, &s.Purpose, &until, &s.IncludeSelf); err != nil {
		return Sub{}, err //nolint:wrapcheck // Wrapped by queryEach.
	}
	s.Until = unixTime(until)
	return s, nil
}

// AddSub subscribes s.Owner to s.Channel. An existing subscription for the
// same channel and owner is replaced by s and AddSub reports created
// false. A new subscription catches up from now.
func (s *Store) AddSub(sub Sub, now time.Time) (Sub, bool, error) {
	if sub.Channel == "" || sub.Owner == "" {
		return Sub{}, false, errors.New("a subscription needs a channel and an owner")
	}
	var until any
	if !sub.Until.IsZero() {
		until = sub.Until.Unix()
	}
	tx, err := s.db.Begin()
	if err != nil {
		return Sub{}, false, fmt.Errorf("begin sub: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // No-op after Commit.
	err = tx.QueryRow(`SELECT id FROM subs WHERE channel = ? AND owner = ?`, sub.Channel, sub.Owner).Scan(&sub.ID)
	created := errors.Is(err, sql.ErrNoRows)
	switch {
	case created:
		res, err := tx.Exec(`INSERT INTO subs (channel, owner, purpose, until, include_self) VALUES (?, ?, ?, ?, ?)`,
			sub.Channel, sub.Owner, sub.Purpose, until, sub.IncludeSelf)
		if err != nil {
			return Sub{}, false, fmt.Errorf("insert sub: %w", err)
		}
		if sub.ID, err = res.LastInsertId(); err != nil {
			return Sub{}, false, fmt.Errorf("insert sub: %w", err)
		}
		if _, err := tx.Exec(`INSERT INTO cursors (scope, ts) VALUES (?, ?)`, subScope(sub.ID), unixTS(now)); err != nil {
			return Sub{}, false, fmt.Errorf("set cursor of sub %d: %w", sub.ID, err)
		}
	case err != nil:
		return Sub{}, false, fmt.Errorf("find sub: %w", err)
	default:
		if _, err := tx.Exec(`UPDATE subs SET purpose = ?, until = ?, include_self = ? WHERE id = ?`,
			sub.Purpose, until, sub.IncludeSelf, sub.ID); err != nil {
			return Sub{}, false, fmt.Errorf("update sub %d: %w", sub.ID, err)
		}
	}
	if err := tx.Commit(); err != nil {
		return Sub{}, false, fmt.Errorf("commit sub: %w", err)
	}
	return sub, created, nil
}

// RemoveSubs removes the subscriptions to channel, only owner's when owner
// is set, and returns how many it removed.
func (s *Store) RemoveSubs(channel, owner string) (int64, error) {
	tx, err := s.db.Begin()
	if err != nil {
		return 0, fmt.Errorf("begin unsub: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // No-op after Commit.
	const match = `channel = ?1 AND (?2 = '' OR owner = ?2)`
	if _, err := tx.Exec(`DELETE FROM cursors WHERE scope IN (SELECT 'sub:' || id FROM subs WHERE `+match+`)`,
		channel, owner); err != nil {
		return 0, fmt.Errorf("remove sub cursors: %w", err)
	}
	res, err := tx.Exec(`DELETE FROM subs WHERE `+match, channel, owner)
	if err != nil {
		return 0, fmt.Errorf("remove subs: %w", err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("remove subs: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return 0, fmt.Errorf("commit unsub: %w", err)
	}
	return n, nil
}

// ListSubs returns every subscription ordered by id.
func (s *Store) ListSubs() ([]Sub, error) {
	return s.subs(`SELECT ` + subCols + ` FROM subs ORDER BY id`)
}

func (s *Store) channelSubs(channel string) ([]Sub, error) {
	return s.subs(`SELECT `+subCols+` FROM subs WHERE channel = ? ORDER BY id`, channel)
}

func (s *Store) subs(query string, args ...any) ([]Sub, error) {
	out := []Sub{}
	err := s.queryEach(func(rows *sql.Rows) error {
		sub, err := scanSub(rows)
		if err != nil {
			return err
		}
		out = append(out, sub)
		return nil
	}, query, args...)
	if err != nil {
		return nil, fmt.Errorf("list subs: %w", err)
	}
	return out, nil
}

// CatchUpSub fetches channel messages after the subscription's cursor:
// conversations.history for top-level messages, and conversations.replies
// for threads whose latest reply is after the cursor. Thread parents are
// looked for up to subThreadLookback before the cursor.
func (c *CatchUp) CatchUpSub(ctx context.Context, s Sub) error {
	scope := subScope(s.ID)
	cursor, err := c.Proc.Store.cursor(scope, unixTS(c.Proc.Now()))
	if err != nil {
		return err
	}
	if err := c.catchUpSub(ctx, s, scope, cursor); err != nil {
		return fmt.Errorf("catch up sub %d: %w", s.ID, err)
	}
	return nil
}

func (c *CatchUp) catchUpSub(ctx context.Context, s Sub, scope, cursor string) error {
	sec, err := tsSeconds(cursor)
	if err != nil {
		return err
	}
	oldest := unixTS(time.Unix(sec, 0).Add(-subThreadLookback))
	var history []Fetched
	if err := c.retry(ctx, func() (err error) {
		history, err = c.Slack.History(ctx, s.Channel, oldest)
		return err //nolint:wrapcheck // Wrapped by CatchUpSub; retry needs the *RateLimitError.
	}); err != nil {
		return err
	}
	all := history
	for _, h := range history {
		if h.LatestReply == "" || CompareTS(h.LatestReply, cursor) <= 0 {
			continue
		}
		var replies []Fetched
		if err := c.retry(ctx, func() (err error) {
			replies, err = c.Slack.Replies(ctx, s.Channel, h.TS, cursor)
			return err //nolint:wrapcheck // Wrapped by CatchUpSub; retry needs the *RateLimitError.
		}); err != nil {
			return err
		}
		all = append(all, replies...)
	}
	return c.handle(ctx, scope, cursor, all)
}

// tsSeconds returns the whole seconds of a Slack ts.
func tsSeconds(ts string) (int64, error) {
	whole, _, _ := strings.Cut(ts, ".")
	sec, err := strconv.ParseInt(whole, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("slack ts %q: %w", ts, err)
	}
	return sec, nil
}
