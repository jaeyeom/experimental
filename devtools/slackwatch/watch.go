package slackwatch

import (
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"
)

// Watch kinds, states, and sources as stored in the watches table.
const (
	KindThread = "thread"
	KindDM     = "dm"

	StateOpen     = "open"
	StateUnowned  = "unowned"
	StateOrphaned = "orphaned"
	StateClosed   = "closed"

	SourceOutbox = "outbox"
	SourceHook   = "hook"
	SourceManual = "manual"
	SourceFooter = "footer"
)

// ErrNoWatch means no watch matches the given post or id.
var ErrNoWatch = errors.New("no such watch")

// Watch is one watched thread or DM. Zero times mean unset.
type Watch struct {
	ID           int64     `json:"id"`
	Channel      string    `json:"channel"`
	ThreadTS     string    `json:"threadTs"`
	Kind         string    `json:"kind"`
	SinceTS      string    `json:"sinceTs"`
	State        string    `json:"state"`
	Owner        string    `json:"owner"`
	Purpose      string    `json:"purpose"`
	Refs         []string  `json:"refs"`
	Source       string    `json:"source"`
	Until        time.Time `json:"until,omitzero"`
	LastActivity time.Time `json:"lastActivity,omitzero"`
	CreatedAt    time.Time `json:"createdAt"`
	ClosedAt     time.Time `json:"closedAt,omitzero"`
	CloseReason  string    `json:"closeReason,omitempty"`
}

// WatchUpdate holds the fields a command sets on a watch. Nil fields are
// left unchanged.
type WatchUpdate struct {
	Owner   *string
	Purpose *string
	Refs    []string
	Source  *string
	Until   *time.Time
}

// WatchSummary is one row of ls.
type WatchSummary struct {
	Watch
	Replies         int    `json:"replies"`
	LastReplyAuthor string `json:"lastReplyAuthor,omitempty"`
	LastReplyTS     string `json:"lastReplyTs,omitempty"`
	Unacked         int    `json:"unacked"`
	Undelivered     bool   `json:"undelivered"`
}

// WatchFilter selects rows for ls. Empty fields match everything.
type WatchFilter struct {
	Owner   string
	State   string
	Unacked bool
}

// WatchDetail is a watch with everything stored under it.
type WatchDetail struct {
	Watch
	Posts     []string   `json:"posts"`
	Replies   []Reply    `json:"replies"`
	Reactions []Reaction `json:"reactions"`
}

// Reaction is one emoji reaction on a watched post or reply.
type Reaction struct {
	TS   string `json:"ts"`
	User string `json:"user"`
	Name string `json:"name"`
}

const watchCols = `id, channel, thread_ts, kind, since_ts, state, owner, purpose, refs,
source, until, last_activity, created_at, closed_at, close_reason`

// Refs are stored newline-separated.
const refsSep = "\n"

// tsOrder and tsOrderDesc sort Slack ts text numerically.
const (
	tsOrder     = `length(ts), ts`
	tsOrderDesc = `length(ts) DESC, ts DESC`
)

type watchRow struct {
	w                   Watch
	refs                string
	until, last, closed sql.NullInt64
	created             int64
}

func (r *watchRow) dest(extra ...any) []any {
	return append([]any{
		&r.w.ID, &r.w.Channel, &r.w.ThreadTS, &r.w.Kind, &r.w.SinceTS, &r.w.State,
		&r.w.Owner, &r.w.Purpose, &r.refs, &r.w.Source, &r.until, &r.last,
		&r.created, &r.closed, &r.w.CloseReason,
	}, extra...)
}

func (r *watchRow) watch() Watch {
	w := r.w
	w.Refs = []string{}
	if r.refs != "" {
		w.Refs = strings.Split(r.refs, refsSep)
	}
	w.Until = unixTime(r.until)
	w.LastActivity = unixTime(r.last)
	w.CreatedAt = time.Unix(r.created, 0).UTC()
	w.ClosedAt = unixTime(r.closed)
	return w
}

func unixTime(n sql.NullInt64) time.Time {
	if !n.Valid {
		return time.Time{}
	}
	return time.Unix(n.Int64, 0).UTC()
}

type rowQuerier interface {
	QueryRow(query string, args ...any) *sql.Row
}

func getWatch(q rowQuerier, id int64) (Watch, error) {
	var r watchRow
	err := q.QueryRow(`SELECT `+watchCols+` FROM watches WHERE id = ?`, id).Scan(r.dest()...)
	if errors.Is(err, sql.ErrNoRows) {
		return Watch{}, fmt.Errorf("watch %d: %w", id, ErrNoWatch)
	}
	if err != nil {
		return Watch{}, fmt.Errorf("read watch %d: %w", id, err)
	}
	return r.watch(), nil
}

// findWatchID returns the watch for p: by id, by thread, or by a
// registered post.
func findWatchID(q rowQuerier, p PostRef) (int64, error) {
	if p.WatchID != 0 {
		if _, err := getWatch(q, p.WatchID); err != nil {
			return 0, err
		}
		return p.WatchID, nil
	}
	thread := p.ThreadTS
	if thread == "" {
		thread = p.TS
	}
	var id int64
	err := q.QueryRow(`
SELECT id FROM watches WHERE channel = ?1 AND thread_ts = ?2
UNION
SELECT w.id FROM watch_posts p JOIN watches w ON w.id = p.watch_id
WHERE w.channel = ?1 AND p.ts = ?3
LIMIT 1`, p.Channel, thread, p.TS).Scan(&id)
	if errors.Is(err, sql.ErrNoRows) {
		return 0, fmt.Errorf("post %s %s: %w", p.Channel, p.TS, ErrNoWatch)
	}
	if err != nil {
		return 0, fmt.Errorf("find watch for %s %s: %w", p.Channel, p.TS, err)
	}
	return id, nil
}

// FindWatch returns the watch named by p.
func (s *Store) FindWatch(p PostRef) (Watch, error) {
	id, err := findWatchID(s.db, p)
	if err != nil {
		return Watch{}, err
	}
	return getWatch(s.db, id)
}

// AddWatch registers the post p and returns its watch. A thread reply is
// watched on its parent thread. Adding a post that is already watched
// updates the fields set in u and reports created false. The watch's
// since_ts is its earliest registered post.
func (s *Store) AddWatch(p PostRef, u WatchUpdate, now time.Time) (Watch, bool, error) {
	if p.Channel == "" || p.TS == "" {
		return Watch{}, false, errors.New("add needs a Slack post, not a watch id")
	}
	tx, err := s.db.Begin()
	if err != nil {
		return Watch{}, false, fmt.Errorf("begin add: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // No-op after Commit.

	id, err := findWatchID(tx, p)
	created := errors.Is(err, ErrNoWatch)
	switch {
	case created:
		id, err = insertWatch(tx, p, u, now)
		if err != nil {
			return Watch{}, false, err
		}
	case err != nil:
		return Watch{}, false, err
	default:
		if _, err := tx.Exec(`UPDATE watches SET since_ts = ?1
WHERE id = ?2 AND (length(?1) < length(since_ts) OR (length(?1) = length(since_ts) AND ?1 < since_ts))`, p.TS, id); err != nil {
			return Watch{}, false, fmt.Errorf("update since_ts: %w", err)
		}
	}
	if err := addUpdate(tx, id, u); err != nil {
		return Watch{}, false, err
	}
	if _, err := tx.Exec(`INSERT INTO watch_posts (watch_id, ts) VALUES (?, ?) ON CONFLICT DO NOTHING`, id, p.TS); err != nil {
		return Watch{}, false, fmt.Errorf("add watch post: %w", err)
	}
	w, err := getWatch(tx, id)
	if err != nil {
		return Watch{}, false, err
	}
	if err := tx.Commit(); err != nil {
		return Watch{}, false, fmt.Errorf("commit add: %w", err)
	}
	return w, created, nil
}

// addUpdate applies u for add. An owner on an unowned watch claims it,
// so a footer-rule watch that add names an owner for ends up open.
func addUpdate(tx *sql.Tx, id int64, u WatchUpdate) error {
	if u.Owner != nil && *u.Owner != "" {
		var state string
		if err := tx.QueryRow(`SELECT state FROM watches WHERE id = ?`, id).Scan(&state); err != nil {
			return fmt.Errorf("read watch %d: %w", id, err)
		}
		if state == StateUnowned {
			return claimWatch(tx, id, *u.Owner, u)
		}
	}
	return updateWatch(tx, id, u)
}

func insertWatch(tx *sql.Tx, p PostRef, u WatchUpdate, now time.Time) (int64, error) {
	thread := p.ThreadTS
	if thread == "" {
		thread = p.TS
	}
	kind := KindThread
	if strings.HasPrefix(p.Channel, "D") {
		kind = KindDM
	}
	state := StateUnowned
	if u.Owner != nil && *u.Owner != "" {
		state = StateOpen
	}
	res, err := tx.Exec(`INSERT INTO watches (channel, thread_ts, kind, since_ts, state, source, created_at)
VALUES (?, ?, ?, ?, ?, ?, ?)`, p.Channel, thread, kind, p.TS, state, SourceManual, now.Unix())
	if err != nil {
		return 0, fmt.Errorf("insert watch: %w", err)
	}
	id, err := res.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("insert watch: %w", err)
	}
	return id, nil
}

func updateWatch(tx *sql.Tx, id int64, u WatchUpdate) error {
	var refs, until any
	if u.Refs != nil {
		refs = strings.Join(u.Refs, refsSep)
	}
	if u.Until != nil {
		until = u.Until.Unix()
	}
	_, err := tx.Exec(`UPDATE watches SET
owner = COALESCE(?, owner), purpose = COALESCE(?, purpose), refs = COALESCE(?, refs),
source = COALESCE(?, source), until = COALESCE(?, until)
WHERE id = ?`, u.Owner, u.Purpose, refs, u.Source, until, id)
	if err != nil {
		return fmt.Errorf("update watch %d: %w", id, err)
	}
	return nil
}

// claimWatch sets owner and the fields in u on watch id and opens it.
// The caller checks the state inside the same transaction.
func claimWatch(tx *sql.Tx, id int64, owner string, u WatchUpdate) error {
	u.Owner = &owner
	if err := updateWatch(tx, id, u); err != nil {
		return err
	}
	if _, err := tx.Exec(`UPDATE watches SET state = ? WHERE id = ?`, StateOpen, id); err != nil {
		return fmt.Errorf("claim watch %d: %w", id, err)
	}
	return nil
}

// ClaimWatch gives an unowned or orphaned watch to owner and opens it.
func (s *Store) ClaimWatch(id int64, owner string, u WatchUpdate) (Watch, error) {
	tx, err := s.db.Begin()
	if err != nil {
		return Watch{}, fmt.Errorf("begin claim: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // No-op after Commit.
	w, err := getWatch(tx, id)
	if err != nil {
		return Watch{}, err
	}
	if w.State != StateUnowned && w.State != StateOrphaned {
		return Watch{}, fmt.Errorf("watch %d is %s (owner %q); only unowned or orphaned watches can be claimed", id, w.State, w.Owner)
	}
	if err := claimWatch(tx, id, owner, u); err != nil {
		return Watch{}, err
	}
	if w, err = getWatch(tx, id); err != nil {
		return Watch{}, err
	}
	if err := tx.Commit(); err != nil {
		return Watch{}, fmt.Errorf("commit claim: %w", err)
	}
	return w, nil
}

// execWatch runs a single-watch UPDATE and returns the updated watch.
func (s *Store) execWatch(id int64, query string, args ...any) (Watch, error) {
	res, err := s.db.Exec(query, append(args, id)...)
	if err != nil {
		return Watch{}, fmt.Errorf("update watch %d: %w", id, err)
	}
	if n, err := res.RowsAffected(); err != nil {
		return Watch{}, fmt.Errorf("update watch %d: %w", id, err)
	} else if n == 0 {
		return Watch{}, fmt.Errorf("watch %d: %w", id, ErrNoWatch)
	}
	return getWatch(s.db, id)
}

// CloseWatch closes a watch with an optional reason.
func (s *Store) CloseWatch(id int64, reason string, now time.Time) (Watch, error) {
	return s.execWatch(id, `UPDATE watches SET state = ?, closed_at = ?, close_reason = ? WHERE id = ?`,
		StateClosed, now.Unix(), reason)
}

// SnoozeWatch sets the time until which a watch stays open.
func (s *Store) SnoozeWatch(id int64, until time.Time) (Watch, error) {
	return s.execWatch(id, `UPDATE watches SET until = ? WHERE id = ?`, until.Unix())
}

// AckWatch marks every stored reply on the watch acknowledged and returns
// how many changed.
func (s *Store) AckWatch(id int64) (int64, error) {
	if _, err := getWatch(s.db, id); err != nil {
		return 0, err
	}
	res, err := s.db.Exec(`UPDATE replies SET acked = 1 WHERE watch_id = ? AND acked = 0`, id)
	if err != nil {
		return 0, fmt.Errorf("ack watch %d: %w", id, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("ack watch %d: %w", id, err)
	}
	return n, nil
}

// ListWatches returns one summary per watch matching f, ordered by id.
// Unacked counts replies by others that are not yet acknowledged.
// Undelivered is set when a failed delivery has no later successful one
// for the same reply, such as a fallback.
func (s *Store) ListWatches(f WatchFilter) ([]WatchSummary, error) {
	rows, err := s.db.Query(`
SELECT `+watchCols+`,
	(SELECT COUNT(*) FROM replies r WHERE r.watch_id = w.id),
	(SELECT COUNT(*) FROM replies r WHERE r.watch_id = w.id AND r.acked = 0 AND r.is_self = 0),
	COALESCE((SELECT author FROM replies r WHERE r.watch_id = w.id ORDER BY `+tsOrderDesc+` LIMIT 1), ''),
	COALESCE((SELECT ts FROM replies r WHERE r.watch_id = w.id ORDER BY `+tsOrderDesc+` LIMIT 1), ''),
	EXISTS (SELECT 1 FROM deliveries d WHERE d.watch_id = w.id AND d.delivered = 0
		AND NOT EXISTS (SELECT 1 FROM deliveries l WHERE l.watch_id = d.watch_id
			AND l.reply_ts = d.reply_ts AND l.delivered = 1 AND l.id > d.id))
FROM watches w
WHERE (?1 = '' OR owner = ?1) AND (?2 = '' OR state = ?2)
ORDER BY id`, f.Owner, f.State)
	if err != nil {
		return nil, fmt.Errorf("list watches: %w", err)
	}
	defer rows.Close()
	out := []WatchSummary{}
	for rows.Next() {
		var r watchRow
		var sum WatchSummary
		if err := rows.Scan(r.dest(&sum.Replies, &sum.Unacked, &sum.LastReplyAuthor, &sum.LastReplyTS, &sum.Undelivered)...); err != nil {
			return nil, fmt.Errorf("list watches: %w", err)
		}
		if f.Unacked && sum.Unacked == 0 {
			continue
		}
		sum.Watch = r.watch()
		out = append(out, sum)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("list watches: %w", err)
	}
	return out, nil
}

// ShowWatch returns the watch with its posts, replies, and reactions.
func (s *Store) ShowWatch(id int64) (WatchDetail, error) {
	w, err := getWatch(s.db, id)
	if err != nil {
		return WatchDetail{}, err
	}
	d := WatchDetail{Watch: w, Posts: []string{}, Replies: []Reply{}, Reactions: []Reaction{}}
	err = s.queryEach(func(rows *sql.Rows) error {
		var ts string
		if err := rows.Scan(&ts); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		d.Posts = append(d.Posts, ts)
		return nil
	}, `SELECT ts FROM watch_posts WHERE watch_id = ? ORDER BY `+tsOrder, id)
	if err != nil {
		return WatchDetail{}, err
	}
	err = s.queryEach(func(rows *sql.Rows) error {
		var r Reply
		var watchID sql.NullInt64
		if err := rows.Scan(&r.Channel, &r.TS, &watchID, &r.Author, &r.Text, &r.ThreadTS, &r.EditedAt, &r.IsSelf, &r.Acked, &r.Deleted); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		r.WatchID = watchID.Int64
		d.Replies = append(d.Replies, r)
		return nil
	}, `SELECT channel, ts, watch_id, author, text, thread_ts, edited_at, is_self, acked, deleted
FROM replies WHERE watch_id = ? ORDER BY `+tsOrder, id)
	if err != nil {
		return WatchDetail{}, err
	}
	err = s.queryEach(func(rows *sql.Rows) error {
		var r Reaction
		if err := rows.Scan(&r.TS, &r.User, &r.Name); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		d.Reactions = append(d.Reactions, r)
		return nil
	}, `SELECT ts, user, name FROM reactions
WHERE channel = ?1 AND ts IN (
	SELECT ts FROM watch_posts WHERE watch_id = ?2
	UNION SELECT ts FROM replies WHERE watch_id = ?2)
ORDER BY `+tsOrder+`, user, name`, w.Channel, id)
	if err != nil {
		return WatchDetail{}, err
	}
	return d, nil
}

func (s *Store) queryEach(scan func(*sql.Rows) error, query string, args ...any) error {
	rows, err := s.db.Query(query, args...)
	if err != nil {
		return fmt.Errorf("query: %w", err)
	}
	defer rows.Close()
	for rows.Next() {
		if err := scan(rows); err != nil {
			return fmt.Errorf("scan: %w", err)
		}
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("query: %w", err)
	}
	return nil
}
