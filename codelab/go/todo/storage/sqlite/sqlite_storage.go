// Package sqlite provides a SQLite-based storage implementation for todo lists.
package sqlite

import (
	"database/sql"
	"fmt"
	"sync"

	"github.com/jaeyeom/experimental/codelab/go/todo/core"
	_ "modernc.org/sqlite"
)

// Storage implements todo list storage using SQLite.
type Storage struct {
	db *sql.DB
	mu sync.RWMutex
}

// New creates a new SQLite storage instance. It creates the database file if it
// doesn't exist and sets up the necessary tables.
func New(path string) (*Storage, error) {
	db, err := sql.Open("sqlite", path)
	if err != nil {
		return nil, fmt.Errorf("open database: %v", err)
	}

	// Create tables if they don't exist
	if err := createTables(db); err != nil {
		db.Close()
		return nil, fmt.Errorf("create tables: %v", err)
	}

	return &Storage{
		db: db,
	}, nil
}

// createTables creates the necessary tables in the database if they don't exist.
func createTables(db *sql.DB) error {
	_, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS items (
			id TEXT PRIMARY KEY,
			description TEXT NOT NULL,
			state INTEGER NOT NULL
		)
	`)
	if err != nil {
		return fmt.Errorf("create items table: %v", err)
	}
	return nil
}

// Save persists a todo list to the SQLite database. It acquires a write lock
// to ensure thread safety.
func (s *Storage) Save(list *core.List) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	tx, err := s.db.Begin()
	if err != nil {
		return fmt.Errorf("begin transaction: %v", err)
	}
	defer func() {
		if err != nil {
			if rbErr := tx.Rollback(); rbErr != nil {
				err = fmt.Errorf("rollback failed: %v (original error: %v)", rbErr, err)
			}
		}
	}()

	// Clear existing items
	if _, err := tx.Exec("DELETE FROM items"); err != nil {
		return fmt.Errorf("clear items: %v", err)
	}

	// Insert all items
	stmt, err := tx.Prepare("INSERT INTO items (id, description, state) VALUES (?, ?, ?)")
	if err != nil {
		return fmt.Errorf("prepare insert statement: %v", err)
	}
	defer stmt.Close()

	for _, item := range list.Items {
		if _, err := stmt.Exec(item.ID, item.Description, item.State); err != nil {
			return fmt.Errorf("insert item: %v", err)
		}
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit transaction: %v", err)
	}
	return nil
}

// Load reads a todo list from the SQLite database. It acquires a read lock to
// ensure thread safety.
func (s *Storage) Load() (*core.List, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	rows, err := s.db.Query("SELECT id, description, state FROM items")
	if err != nil {
		return nil, fmt.Errorf("query items: %v", err)
	}
	defer rows.Close()

	list := core.NewList()
	for rows.Next() {
		var item core.Item
		if err := rows.Scan(&item.ID, &item.Description, &item.State); err != nil {
			return nil, fmt.Errorf("scan item: %v", err)
		}
		list.Items = append(list.Items, item)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate rows: %v", err)
	}

	return list, nil
}

// Close closes the database connection.
func (s *Storage) Close() error {
	return s.db.Close()
}
