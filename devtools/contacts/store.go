package contacts

import (
	"database/sql"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

// DBName is the database file name inside the data directory.
const DBName = "contacts.db"

// ErrNotInitialized means the data directory has no database yet.
var ErrNotInitialized = errors.New("contacts is not initialized; run contacts init")

// migrations are applied in order; migration i brings the database to
// PRAGMA user_version i+1. They are forward-only: never edit or remove one,
// only append.
var migrations = []string{
	`CREATE TABLE people (
		id         TEXT PRIMARY KEY,
		name       TEXT NOT NULL,
		team       TEXT NOT NULL DEFAULT '',
		role       TEXT NOT NULL DEFAULT '',
		tz         TEXT NOT NULL DEFAULT '',
		channel    TEXT NOT NULL DEFAULT '',
		created_at TEXT NOT NULL
	);
	CREATE TABLE aliases (
		person_id TEXT NOT NULL REFERENCES people(id) ON DELETE CASCADE,
		alias     TEXT NOT NULL COLLATE NOCASE,
		PRIMARY KEY (person_id, alias)
	);
	CREATE INDEX aliases_alias ON aliases(alias);
	CREATE TABLE handles (
		person_id TEXT NOT NULL REFERENCES people(id) ON DELETE CASCADE,
		kind      TEXT NOT NULL,
		value     TEXT NOT NULL COLLATE NOCASE,
		UNIQUE (kind, value)
	);
	CREATE INDEX handles_value ON handles(value);
	CREATE INDEX handles_person ON handles(person_id);
	CREATE TABLE events (
		id         INTEGER PRIMARY KEY,
		person_id  TEXT NOT NULL REFERENCES people(id) ON DELETE CASCADE,
		date       TEXT NOT NULL,
		channel    TEXT NOT NULL,
		link       TEXT NOT NULL,
		summary    TEXT NOT NULL,
		reporter   TEXT NOT NULL,
		created_at TEXT NOT NULL
	);
	CREATE INDEX events_person_date ON events(person_id, date);
	CREATE TABLE commitments (
		id         INTEGER PRIMARY KEY,
		person_id  TEXT NOT NULL REFERENCES people(id) ON DELETE CASCADE,
		direction  TEXT NOT NULL CHECK (direction IN ('mine', 'theirs')),
		text       TEXT NOT NULL,
		due        TEXT NOT NULL DEFAULT '',
		status     TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'done', 'dropped')),
		link       TEXT NOT NULL DEFAULT '',
		reporter   TEXT NOT NULL,
		note       TEXT NOT NULL DEFAULT '',
		created_at TEXT NOT NULL,
		closed_at  TEXT NOT NULL DEFAULT ''
	);
	CREATE INDEX commitments_person_status ON commitments(person_id, status);
	CREATE TABLE access_log (
		id        INTEGER PRIMARY KEY,
		time      TEXT NOT NULL,
		caller    TEXT NOT NULL,
		command   TEXT NOT NULL,
		person_id TEXT NOT NULL DEFAULT '',
		reason    TEXT NOT NULL DEFAULT ''
	);
	CREATE INDEX access_log_time ON access_log(time);`,
	// compacted is how many events a contacts age summary stands for; 0
	// for an ordinary event.
	`ALTER TABLE events ADD COLUMN compacted INTEGER NOT NULL DEFAULT 0;`,
}

// Store is an open contacts database plus its data directory.
type Store struct {
	db  *sql.DB
	dir string
	now func() time.Time
}

// Init creates dir (0700), dir/people (0700) and the database (0600), and
// migrates the database to the latest version. Running it again is safe.
func Init(dir string) error {
	if err := CheckDataDir(dir); err != nil {
		return err
	}
	for _, d := range []string{dir, filepath.Join(dir, "people")} {
		if err := os.MkdirAll(d, 0o700); err != nil {
			return fmt.Errorf("create %s: %w", d, err)
		}
		if err := os.Chmod(d, 0o700); err != nil {
			return fmt.Errorf("chmod %s: %w", d, err)
		}
	}
	path := filepath.Join(dir, DBName)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("create %s: %w", path, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("create %s: %w", path, err)
	}
	if err := os.Chmod(path, 0o600); err != nil {
		return fmt.Errorf("chmod %s: %w", path, err)
	}
	s, err := Open(dir, time.Now)
	if err != nil {
		return err
	}
	return s.Close()
}

// Open opens the database in dir and applies pending migrations. now is the
// clock used for timestamps and dates.
func Open(dir string, now func() time.Time) (*Store, error) {
	if err := CheckDataDir(dir); err != nil {
		return nil, err
	}
	path := filepath.Join(dir, DBName)
	if _, err := os.Stat(path); errors.Is(err, fs.ErrNotExist) {
		return nil, ErrNotInitialized
	}
	db, err := sqlitedb.Open(path, "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		return nil, fmt.Errorf("open store: %w", err)
	}
	if err := migrate(db, migrations); err != nil {
		db.Close()
		return nil, err
	}
	return &Store{db: db, dir: dir, now: now}, nil
}

// Close closes the database.
func (s *Store) Close() error {
	if err := s.db.Close(); err != nil {
		return fmt.Errorf("close store: %w", err)
	}
	return nil
}

// migrate brings db to len(steps). The version check and every step run in
// one immediate transaction, so two processes opening the database at once
// apply each step exactly once.
func migrate(db *sql.DB, steps []string) error {
	tx, err := db.Begin()
	if err != nil {
		return fmt.Errorf("migrate: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit
	var version int
	if err := tx.QueryRow(`PRAGMA user_version`).Scan(&version); err != nil {
		return fmt.Errorf("migrate: read version: %w", err)
	}
	if version > len(steps) {
		return fmt.Errorf("database version %d is newer than this contacts (%d); upgrade contacts", version, len(steps))
	}
	if version == len(steps) {
		return nil
	}
	for i := version; i < len(steps); i++ {
		if _, err := tx.Exec(steps[i]); err != nil {
			return fmt.Errorf("migrate to version %d: %w", i+1, err)
		}
	}
	if _, err := tx.Exec(fmt.Sprintf(`PRAGMA user_version = %d`, len(steps))); err != nil {
		return fmt.Errorf("migrate: set version: %w", err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("migrate: %w", err)
	}
	return nil
}

func (s *Store) timestamp() string { return s.now().UTC().Format(timeLayout) }

const (
	dateLayout = "2006-01-02"
	// timeLayout is fixed-width UTC, so timestamps sort as strings.
	timeLayout = "2006-01-02T15:04:05Z"
)
