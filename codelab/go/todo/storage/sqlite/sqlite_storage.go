// Package sqlite provides a SQLite-based storage implementation for todo lists.
package sqlite

import (
	"database/sql"
	"fmt"
	"log/slog"
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
	slog.Debug("Opening SQLite database", "path", path)
	db, err := sql.Open("sqlite", path)
	if err != nil {
		return nil, fmt.Errorf("open database: %v", err)
	}

	// Create tables if they don't exist
	if err := createTables(db); err != nil {
		db.Close()
		return nil, fmt.Errorf("create tables: %v", err)
	}
	slog.Info("SQLite storage initialized", "path", path)

	return &Storage{
		db: db,
	}, nil
}

// createTables creates the necessary tables in the database if they don't exist.
func createTables(db *sql.DB) error {
	slog.Debug("Creating tables if they don't exist")
	_, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS items (
			id TEXT PRIMARY KEY,
			description TEXT NOT NULL,
			state INTEGER NOT NULL,
			parent_id TEXT,
			sibling_order INTEGER NOT NULL,
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

	slog.Debug("Starting transaction for saving todo list")
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
	slog.Debug("Clearing existing items")
	if _, err := tx.Exec("DELETE FROM items"); err != nil {
		return fmt.Errorf("clear items: %v", err)
	}

	// Insert all items
	stmt, err := tx.Prepare("INSERT INTO items (id, description, state, parent_id, sibling_order) VALUES (?, ?, ?, ?, ?)")
	if err != nil {
		return fmt.Errorf("prepare insert statement: %v", err)
	}
	defer stmt.Close()

	// Helper function to save an item and its subtasks recursively
	var saveItem func(item core.Item, parentID string, order int) error
	saveItem = func(item core.Item, parentID string, order int) error {
		var parent interface{}
		if parentID != "" {
			parent = parentID
		}
		slog.Debug("Saving item",
			"id", item.ID,
			"description", item.Description,
			"parent_id", parent,
			"order", order)
		if _, err := stmt.Exec(item.ID, item.Description, item.State, parent, order); err != nil {
			return fmt.Errorf("insert item: %v", err)
		}
		for i, subtask := range item.Subtasks {
			subtask.ParentID = item.ID
			if err := saveItem(subtask, item.ID, i); err != nil {
				return err
			}
		}
		return nil
	}

	// Save all top-level items and their subtasks
	slog.Debug("Saving root items and subtasks", "root_item_count", len(list.Items))
	for i, item := range list.Items {
		if err := saveItem(item, "", i); err != nil {
			return err
		}
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit transaction: %v", err)
	}
	slog.Info("Successfully saved todo list to SQLite")
	return nil
}

// Load reads a todo list from the SQLite database. It acquires a read lock to
// ensure thread safety.
func (s *Storage) Load() (*core.List, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	slog.Debug("Loading todo list from SQLite")
	// Load all items ordered by sibling_order
	rows, err := s.db.Query(`
		SELECT id, description, state, parent_id
		FROM items
		ORDER BY CASE WHEN parent_id IS NULL THEN 0 ELSE 1 END, parent_id, sibling_order
	`)
	if err != nil {
		return nil, fmt.Errorf("query items: %v", err)
	}
	defer rows.Close()

	// Map to store all items by their ID for quick lookup
	itemMap := make(map[string]*core.Item)
	// Slice to maintain order of root items
	var rootItems []core.Item
	// Map from parent ID to ordered slice of child items
	childrenMap := make(map[string][]*core.Item)

	// First pass: create all items
	itemCount := 0
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
		} else {
			// Add to childrenMap in order
			childrenMap[parentID.String] = append(childrenMap[parentID.String], &itemCopy)
		}
		itemCount++
	}

	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate rows: %v", err)
	}

	slog.Debug("Loaded items from database",
		"total_items", itemCount,
		"root_items", len(rootItems))

	// Helper function to recursively build subtask hierarchy
	var buildSubtasks func(item *core.Item)
	buildSubtasks = func(item *core.Item) {
		if children, ok := childrenMap[item.ID]; ok {
			slog.Debug("Building subtasks",
				"parent_id", item.ID,
				"subtask_count", len(children))
			// Children are already in the correct order from the SQL query
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
	// Root items are already in order from the SQL query
	for _, rootItem := range rootItems {
		// Get the updated root item with subtasks from the map
		if updatedRoot, ok := itemMap[rootItem.ID]; ok {
			rootCopy := *updatedRoot
			buildSubtasks(&rootCopy)
			list.Items = append(list.Items, rootCopy)
		}
	}

	slog.Info("Successfully loaded todo list from SQLite",
		"total_items", itemCount,
		"root_items", len(rootItems))
	return list, nil
}

// Close closes the database connection.
func (s *Storage) Close() error {
	if err := s.db.Close(); err != nil {
		return fmt.Errorf("close database connection: %w", err)
	}
	return nil
}
