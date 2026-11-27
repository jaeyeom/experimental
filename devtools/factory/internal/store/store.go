// Package store is the local SQLite mirror of work items plus factory's own
// claims, sync cursors, and run history.
package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

// Store is the local mirror and factory's own state.
type Store struct {
	db      *sql.DB
	claimMu sync.Mutex
}

type queryer interface {
	ExecContext(ctx context.Context, query string, args ...any) (sql.Result, error)
	QueryContext(ctx context.Context, query string, args ...any) (*sql.Rows, error)
	QueryRowContext(ctx context.Context, query string, args ...any) *sql.Row
}

// Open creates the parent directory with mode 0700, opens path with WAL,
// a 5s busy timeout, and foreign keys, and applies any missing migrations.
func Open(ctx context.Context, path string) (*Store, error) {
	if path == "" {
		return nil, errors.New("store path: expected a non-empty path, found empty")
	}
	abs, err := filepath.Abs(path)
	if err != nil {
		return nil, fmt.Errorf("store path %s: %w", path, err)
	}
	info, err := os.Stat(abs)
	if err == nil && info.IsDir() {
		return nil, fmt.Errorf("store path %s: expected a file, found a directory", abs)
	}
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("store path %s: %w", abs, err)
	}
	dir := filepath.Dir(abs)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, fmt.Errorf("create store directory %s: %w", dir, err)
	}
	db, err := sqlitedb.Open(abs, "_pragma=foreign_keys(1)")
	if err != nil {
		return nil, fmt.Errorf("open store %s: %w", abs, err)
	}
	s := &Store{db: db}
	if err := s.migrate(ctx); err != nil {
		cerr := db.Close()
		if cerr != nil {
			return nil, errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
		return nil, err
	}
	return s, nil
}

// Close closes the database.
func (s *Store) Close() error {
	if err := s.db.Close(); err != nil {
		return fmt.Errorf("close store: %w", err)
	}
	return nil
}

// DefaultPath returns $XDG_STATE_HOME/factory/factory.db.
// When XDG_STATE_HOME is unset, it returns ~/.local/state/factory/factory.db.
// An empty XDG_STATE_HOME, a nil LookupEnv or HomeDir, or an empty home panics.
// The planned signature returns a string, so an unusable env fails loud
// instead of inventing a path.
func DefaultPath(env config.Env) string {
	if env.LookupEnv == nil {
		panic("store path: expected LookupEnv, found nil")
	}
	if dir, ok := env.LookupEnv("XDG_STATE_HOME"); ok {
		if dir == "" {
			panic("XDG_STATE_HOME: expected a non-empty path, found empty")
		}
		return filepath.Join(dir, "factory", "factory.db")
	}
	if env.HomeDir == nil {
		panic("store path: expected HomeDir, found nil")
	}
	home, err := env.HomeDir()
	if err != nil {
		panic(fmt.Sprintf("home directory: expected a path, found error %v", err))
	}
	if home == "" {
		panic("home directory: expected a non-empty path, found empty")
	}
	return filepath.Join(home, ".local", "state", "factory", "factory.db")
}

// closeRows closes rows and keeps err when the close fails and err is still nil.
func closeRows(rows *sql.Rows, err error) error {
	if cerr := rows.Close(); cerr != nil && err == nil {
		return fmt.Errorf("close rows: %w", cerr)
	}
	return err
}

func (s *Store) withTx(ctx context.Context, fn func(*sql.Tx) error) (err error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin transaction: %w", err)
	}
	defer func() {
		if err == nil {
			return
		}
		if rbErr := tx.Rollback(); rbErr != nil && !errors.Is(rbErr, sql.ErrTxDone) {
			err = errors.Join(err, fmt.Errorf("rollback: %w", rbErr))
		}
	}()
	if err = fn(tx); err != nil {
		return err
	}
	if err = tx.Commit(); err != nil {
		return fmt.Errorf("commit: %w", err)
	}
	return nil
}

// timeLayout is RFC3339Nano with a fixed 9-digit fraction so text order
// matches time order. SQLite has no time type; every timestamp is this text.
const timeLayout = "2006-01-02T15:04:05.000000000Z07:00"

func formatTime(t time.Time) string {
	return t.UTC().Format(timeLayout)
}

func parseTime(column, value string) (time.Time, error) {
	if value == "" {
		return time.Time{}, nil
	}
	parsed, err := time.Parse(time.RFC3339Nano, value)
	if err != nil {
		return time.Time{}, fmt.Errorf("%s: expected RFC3339Nano, found %q", column, value)
	}
	if parsed.IsZero() {
		return time.Time{}, nil
	}
	return parsed, nil
}

// dateLayout is the items.due column. SQLite has no date type.
const dateLayout = "2006-01-02"

// formatDate stores a due date as YYYY-MM-DD in UTC, or NULL when t is nil.
func formatDate(t *time.Time) any {
	if t == nil {
		return nil
	}
	return t.UTC().Format(dateLayout)
}

// parseDate reads items.due. NULL is a missing date. Any other text must be YYYY-MM-DD.
func parseDate(column string, value sql.NullString) (*time.Time, error) {
	if !value.Valid {
		return nil, nil
	}
	parsed, err := time.Parse(dateLayout, value.String)
	if err != nil {
		return nil, fmt.Errorf("%s: expected YYYY-MM-DD, found %q", column, value.String)
	}
	due := time.Date(parsed.Year(), parsed.Month(), parsed.Day(), 0, 0, 0, 0, time.UTC)
	return &due, nil
}
