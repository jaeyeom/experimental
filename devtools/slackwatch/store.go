package slackwatch

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

// migrations are applied in order. PRAGMA user_version records how many
// have run, so append new steps and never edit old ones.
//
// Slack timestamps (ts) are TEXT. Local times (*_at, until, last_activity)
// are Unix seconds. Booleans are 0 or 1.
var migrations = []string{
	`
CREATE TABLE watches (
	id            INTEGER PRIMARY KEY,
	channel       TEXT NOT NULL,
	thread_ts     TEXT NOT NULL,
	kind          TEXT NOT NULL CHECK (kind IN ('thread', 'dm')),
	since_ts      TEXT NOT NULL DEFAULT '',
	state         TEXT NOT NULL DEFAULT 'open'
	              CHECK (state IN ('open', 'unowned', 'orphaned', 'closed')),
	owner         TEXT NOT NULL DEFAULT '',
	purpose       TEXT NOT NULL DEFAULT '',
	refs          TEXT NOT NULL DEFAULT '',
	source        TEXT NOT NULL CHECK (source IN ('outbox', 'hook', 'manual', 'footer')),
	until         INTEGER,
	last_activity INTEGER,
	created_at    INTEGER NOT NULL,
	closed_at     INTEGER,
	close_reason  TEXT NOT NULL DEFAULT '',
	UNIQUE (channel, thread_ts)
);
CREATE INDEX watches_state ON watches (state);

CREATE TABLE watch_posts (
	watch_id INTEGER NOT NULL REFERENCES watches (id) ON DELETE CASCADE,
	ts       TEXT NOT NULL,
	PRIMARY KEY (watch_id, ts)
);

CREATE TABLE replies (
	id        INTEGER PRIMARY KEY,
	channel   TEXT NOT NULL,
	ts        TEXT NOT NULL,
	watch_id  INTEGER REFERENCES watches (id) ON DELETE SET NULL,
	author    TEXT NOT NULL DEFAULT '',
	text      TEXT NOT NULL DEFAULT '',
	thread_ts TEXT NOT NULL DEFAULT '',
	edited_at TEXT NOT NULL DEFAULT '',
	is_self   INTEGER NOT NULL DEFAULT 0,
	acked     INTEGER NOT NULL DEFAULT 0,
	UNIQUE (channel, ts)
);
CREATE INDEX replies_watch ON replies (watch_id);

CREATE TABLE reactions (
	channel TEXT NOT NULL,
	ts      TEXT NOT NULL,
	user    TEXT NOT NULL,
	name    TEXT NOT NULL,
	PRIMARY KEY (channel, ts, user, name)
);

CREATE TABLE inbound (
	id      INTEGER PRIMARY KEY,
	channel TEXT NOT NULL,
	ts      TEXT NOT NULL,
	author  TEXT NOT NULL DEFAULT '',
	text    TEXT NOT NULL DEFAULT '',
	kind    TEXT NOT NULL CHECK (kind IN ('dm', 'mention')),
	UNIQUE (channel, ts)
);

CREATE TABLE subs (
	id           INTEGER PRIMARY KEY,
	channel      TEXT NOT NULL,
	owner        TEXT NOT NULL,
	purpose      TEXT NOT NULL DEFAULT '',
	until        INTEGER,
	include_self INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX subs_channel ON subs (channel);

CREATE TABLE cursors (
	scope TEXT PRIMARY KEY,
	ts    TEXT NOT NULL
);

CREATE TABLE deliveries (
	id        INTEGER PRIMARY KEY,
	event     TEXT NOT NULL,
	target    TEXT NOT NULL,
	exit_code INTEGER,
	attempt   INTEGER NOT NULL DEFAULT 1,
	delivered INTEGER NOT NULL DEFAULT 0,
	error     TEXT NOT NULL DEFAULT '',
	at        INTEGER NOT NULL
);
CREATE INDEX deliveries_undelivered ON deliveries (delivered, at);

CREATE TABLE status (
	id           INTEGER PRIMARY KEY CHECK (id = 1),
	pid          INTEGER,
	started_at   INTEGER,
	heartbeat_at INTEGER
);
INSERT INTO status (id) VALUES (1);
`,
	// A delivery with delivered = 0 flags its watch in ls.
	`
ALTER TABLE deliveries ADD COLUMN watch_id INTEGER REFERENCES watches (id) ON DELETE SET NULL;
CREATE INDEX deliveries_watch ON deliveries (watch_id, delivered);
`,
	// reply_ts dedupes notifications per (event, reply_ts, target). output
	// holds the command's truncated stdout and stderr.
	`
ALTER TABLE deliveries ADD COLUMN reply_ts TEXT NOT NULL DEFAULT '';
ALTER TABLE deliveries ADD COLUMN output TEXT NOT NULL DEFAULT '';
CREATE INDEX deliveries_dedupe ON deliveries (event, reply_ts, target, delivered);
`,
	// A deleted reply keeps its row so a replayed event stays deduped.
	`
ALTER TABLE replies ADD COLUMN deleted INTEGER NOT NULL DEFAULT 0;
`,
	// The serve heartbeat. heartbeat_interval is in seconds, so status
	// can tell a stale row without knowing the server's config.
	`
ALTER TABLE status ADD COLUMN heartbeat_interval INTEGER;
ALTER TABLE status ADD COLUMN connection TEXT NOT NULL DEFAULT '';
ALTER TABLE status ADD COLUMN last_event_at INTEGER;
ALTER TABLE status ADD COLUMN last_catchup_at INTEGER;
ALTER TABLE status ADD COLUMN last_catchup_error TEXT NOT NULL DEFAULT '';
ALTER TABLE status ADD COLUMN last_sweep_at INTEGER;
ALTER TABLE status ADD COLUMN last_sweep_error TEXT NOT NULL DEFAULT '';
ALTER TABLE status ADD COLUMN undelivered INTEGER NOT NULL DEFAULT 0;
`,
	// team_url is the workspace URL from auth.test, for building
	// permalinks. A watch added from a permalink keeps that link.
	`
CREATE TABLE workspace (
	id       INTEGER PRIMARY KEY CHECK (id = 1),
	team_url TEXT NOT NULL DEFAULT ''
);
INSERT INTO workspace (id) VALUES (1);
ALTER TABLE watches ADD COLUMN permalink TEXT NOT NULL DEFAULT '';
`,
}

// Store is the slackwatch SQLite database. The server and the CLI each
// open it directly. WAL mode lets them share the file.
type Store struct {
	db *sql.DB
}

// OpenStore opens or creates the database at path and applies pending
// migrations. The parent directory is created with mode 0700.
func OpenStore(path string) (*Store, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return nil, fmt.Errorf("create slackwatch db directory: %w", err)
	}
	// Immediate transactions take the write lock up front, so a read
	// followed by a write waits on busy_timeout instead of failing.
	db, err := sqlitedb.Open(path, "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		return nil, fmt.Errorf("open slackwatch db: %w", err)
	}
	s := &Store{db: db}
	if err := s.migrate(); err != nil {
		db.Close()
		return nil, err
	}
	return s, nil
}

func (s *Store) migrate() error {
	tx, err := s.db.Begin()
	if err != nil {
		return fmt.Errorf("begin migration: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // No-op after Commit.
	var version int
	if err := tx.QueryRow(`PRAGMA user_version`).Scan(&version); err != nil {
		return fmt.Errorf("read schema version: %w", err)
	}
	if version > len(migrations) {
		return fmt.Errorf("slackwatch db schema version %d is newer than this binary (%d)", version, len(migrations))
	}
	for i := version; i < len(migrations); i++ {
		if _, err := tx.Exec(migrations[i]); err != nil {
			return fmt.Errorf("apply migration %d: %w", i+1, err)
		}
	}
	if _, err := tx.Exec(fmt.Sprintf(`PRAGMA user_version = %d`, len(migrations))); err != nil {
		return fmt.Errorf("set schema version: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit migration: %w", err)
	}
	return nil
}

// TeamURL returns the stored workspace URL, or "" when not known yet.
func (s *Store) TeamURL() (string, error) {
	var u string
	if err := s.db.QueryRow(`SELECT team_url FROM workspace WHERE id = 1`).Scan(&u); err != nil {
		return "", fmt.Errorf("read team URL: %w", err)
	}
	return u, nil
}

// SetTeamURL stores the workspace URL that auth.test reports.
func (s *Store) SetTeamURL(u string) error {
	if _, err := s.db.Exec(`UPDATE workspace SET team_url = ? WHERE id = 1`, u); err != nil {
		return fmt.Errorf("store team URL: %w", err)
	}
	return nil
}

// DB returns the underlying handle.
func (s *Store) DB() *sql.DB {
	return s.db
}

// Close closes the database.
func (s *Store) Close() error {
	if err := s.db.Close(); err != nil {
		return fmt.Errorf("close slackwatch db: %w", err)
	}
	return nil
}

// Reply is one message in a watched thread or DM. WatchID 0 means none.
// Permalink is set by ShowWatch when the team URL is known.
type Reply struct {
	Channel   string `json:"channel"`
	TS        string `json:"ts"`
	WatchID   int64  `json:"watchId"`
	Author    string `json:"author"`
	Text      string `json:"text"`
	ThreadTS  string `json:"threadTs"`
	EditedAt  string `json:"editedAt,omitempty"`
	IsSelf    bool   `json:"isSelf"`
	Acked     bool   `json:"acked"`
	Deleted   bool   `json:"deleted,omitempty"`
	Permalink string `json:"permalink,omitempty"`
}

// AddReply stores r. A reply already stored under the same (channel, ts)
// is left unchanged and AddReply reports false.
func (s *Store) AddReply(r Reply) (bool, error) {
	watchID := sql.NullInt64{Int64: r.WatchID, Valid: r.WatchID != 0}
	res, err := s.db.Exec(`
INSERT INTO replies (channel, ts, watch_id, author, text, thread_ts, edited_at, is_self, acked)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (channel, ts) DO NOTHING`,
		r.Channel, r.TS, watchID, r.Author, r.Text, r.ThreadTS, r.EditedAt, r.IsSelf, r.Acked)
	if err != nil {
		return false, fmt.Errorf("add reply %s/%s: %w", r.Channel, r.TS, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("add reply %s/%s: %w", r.Channel, r.TS, err)
	}
	return n == 1, nil
}
