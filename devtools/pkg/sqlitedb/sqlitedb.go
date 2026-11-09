// Package sqlitedb opens SQLite databases with the pure-Go modernc.org/sqlite
// driver, so it works in cgo-free builds such as Bazel's pure=true mode.
package sqlitedb

import (
	"database/sql"
	"fmt"
	"net/url"

	_ "modernc.org/sqlite" // registers the "sqlite" driver
)

// Open opens the SQLite database at path in WAL mode with a 5 second busy
// timeout. The pragmas are part of the DSN, so every pooled connection gets
// them.
func Open(path string) (*sql.DB, error) {
	dsn := "file:" + (&url.URL{Path: path}).EscapedPath() +
		"?_pragma=journal_mode(WAL)&_pragma=busy_timeout(5000)"
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("open sqlite %s: %w", path, err)
	}
	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("open sqlite %s: %w", path, err)
	}
	return db, nil
}
