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
			state INTEGER NOT NULL,
			parent_id TEXT,
			FOREIGN KEY(parent_id) REFERENCES items(id)
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
	stmt, err := tx.Prepare("INSERT INTO items (id, description, state, parent_id) VALUES (?, ?, ?, ?)")
	if err != nil {
		return fmt.Errorf("prepare insert statement: %v", err)
	}
	defer stmt.Close()

	// Helper function to save an item and its subtasks recursively
	var saveItem func(item core.Item, parentID string) error
	saveItem = func(item core.Item, parentID string) error {
		var parent interface{}
		if parentID != "" {
			parent = parentID
		}
		if _, err := stmt.Exec(item.ID, item.Description, item.State, parent); err != nil {
			return fmt.Errorf("insert item: %v", err)
		}
		for _, subtask := range item.Subtasks {
			subtask.ParentID = item.ID
			if err := saveItem(subtask, item.ID); err != nil {
				return err
			}
		}
		return nil
	}

	// Save all top-level items and their subtasks
	for _, item := range list.Items {
		if err := saveItem(item, ""); err != nil {
			return err
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

	// Load all items into a map
	rows, err := s.db.Query("SELECT id, description, state, parent_id FROM items")
	if err != nil {
		return nil, fmt.Errorf("query items: %v", err)
	}
	defer rows.Close()

	// Map to store all items by their ID
	itemMap := make(map[string]*core.Item)
	var rootItems []core.Item

	// First pass: create all items
	for rows.Next() {
		var item core.Item
		var parentID sql.NullString
		if err := rows.Scan(&item.ID, &item.Description, &item.State, &parentID); err != nil {
			return nil, fmt.Errorf("scan item: %v", err)
		}

		if parentID.Valid {
			item.ParentID = parentID.String
		}

		// Store a pointer to the item in the map
		itemCopy := item
		itemMap[item.ID] = &itemCopy

		// If it's a root item (no parent), add it to rootItems
		if !parentID.Valid {
			rootItems = append(rootItems, item)
		}
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate rows: %v", err)
	}

	// Second pass: build the subtask hierarchy in topological order
	// First, build a map of children for each parent
	childrenMap := make(map[string][]*core.Item)
	for _, item := range itemMap {
		if item.ParentID != "" {
			childrenMap[item.ParentID] = append(childrenMap[item.ParentID], item)
		}
	}

	// Helper function to recursively build subtask hierarchy
	var buildSubtasks func(item *core.Item)
	buildSubtasks = func(item *core.Item) {
		if children, ok := childrenMap[item.ID]; ok {
			for _, child := range children {
				// Create a deep copy of the child
				childCopy := *child
				item.Subtasks = append(item.Subtasks, childCopy)
				// Recursively build subtasks for this child
				buildSubtasks(&item.Subtasks[len(item.Subtasks)-1])
			}
		}
	}

	// Create a new list with root items
	list := core.NewList()
	for _, rootItem := range rootItems {
		// Get the updated root item with subtasks from the map
		if updatedRoot, ok := itemMap[rootItem.ID]; ok {
			rootCopy := *updatedRoot
			buildSubtasks(&rootCopy)
			list.Items = append(list.Items, rootCopy)
		}
	}

	return list, nil
}

// Close closes the database connection.
func (s *Storage) Close() error {
	return s.db.Close()
}
