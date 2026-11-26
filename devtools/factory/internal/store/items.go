package store

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const itemColumns = `source, id, title, url, type, state, done, parent, labels_json, assignee, priority, created, updated, due, not_before, author, author_association, locked, content_hash`

// UpsertItems writes items for one source in a single transaction.
// Each item replaces its previous blocked-by links, so a link removed in
// the backend disappears from the mirror.
func (s *Store) UpsertItems(ctx context.Context, sourceName string, items []source.Item) error {
	if sourceName == "" {
		return errors.New("upsert items: expected a non-empty source, found empty")
	}
	return s.withTx(ctx, func(tx *sql.Tx) error {
		for i := range items {
			if err := upsertItem(ctx, tx, sourceName, &items[i]); err != nil {
				return err
			}
		}
		return nil
	})
}

func upsertItem(ctx context.Context, tx *sql.Tx, sourceName string, it *source.Item) error {
	if it.ID == "" {
		return errors.New("upsert item: expected a non-empty id, found empty")
	}
	if it.Source != sourceName {
		return fmt.Errorf("upsert item %q: expected source %q, found %q", it.ID, sourceName, it.Source)
	}
	labels, err := marshalLabels(it.Labels)
	if err != nil {
		return fmt.Errorf("upsert item %s/%s: %w", sourceName, it.ID, err)
	}
	done := boolToInt(it.Done)
	locked := boolToInt(it.Locked)
	becameDone, err := becomesDone(ctx, tx, sourceName, it)
	if err != nil {
		return err
	}
	_, err = tx.ExecContext(ctx, `
		INSERT INTO items(
			source, id, title, url, type, state, done, parent, labels_json,
			assignee, priority, created, updated, due, not_before,
			author, author_association, locked, content_hash
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(source, id) DO UPDATE SET
			title = excluded.title,
			url = excluded.url,
			type = excluded.type,
			state = excluded.state,
			done = excluded.done,
			parent = excluded.parent,
			labels_json = excluded.labels_json,
			assignee = excluded.assignee,
			priority = excluded.priority,
			created = excluded.created,
			updated = excluded.updated,
			due = excluded.due,
			not_before = excluded.not_before,
			author = excluded.author,
			author_association = excluded.author_association,
			locked = excluded.locked,
			content_hash = excluded.content_hash`,
		sourceName, it.ID, it.Title, it.URL, it.Type, it.State, done, it.Parent,
		labels, it.Assignee, it.Priority, formatTime(it.Created), formatTime(it.Updated),
		formatDate(it.Due),
		formatDate(it.NotBefore),
		it.Author, it.AuthorAssociation, locked, it.ContentHash,
	)
	if err != nil {
		return fmt.Errorf("upsert item %s/%s: %w", sourceName, it.ID, err)
	}
	if becameDone {
		// The item's updated time is when the backend recorded the change.
		if err := insertFinished(ctx, tx, sourceName, it.ID, it.Updated, nil); err != nil {
			return err
		}
	}
	if _, err := tx.ExecContext(ctx, `DELETE FROM links WHERE source = ? AND blocked_id = ?`, sourceName, it.ID); err != nil {
		return fmt.Errorf("delete links for %s/%s: %w", sourceName, it.ID, err)
	}
	for i := range it.BlockedBy {
		ref := it.BlockedBy[i]
		if ref.ID == "" {
			return fmt.Errorf("upsert item %s/%s: expected a non-empty blocker id, found empty", sourceName, it.ID)
		}
		_, err := tx.ExecContext(ctx, `
			INSERT INTO links(source, blocker_id, blocked_id, link_id) VALUES (?, ?, ?, ?)`,
			sourceName, ref.ID, it.ID, ref.LinkID,
		)
		if err != nil {
			return fmt.Errorf("insert link %s %s -> %s: %w", sourceName, ref.ID, it.ID, err)
		}
	}
	return nil
}

// becomesDone reports whether it is done and its mirror row exists and is
// not done. An item first mirrored as done is not a finish this sync saw.
func becomesDone(ctx context.Context, tx *sql.Tx, sourceName string, it *source.Item) (bool, error) {
	if !it.Done {
		return false, nil
	}
	var was int
	err := tx.QueryRowContext(ctx, `SELECT done FROM items WHERE source = ? AND id = ?`, sourceName, it.ID).Scan(&was)
	if errors.Is(err, sql.ErrNoRows) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("upsert item %s/%s: %w", sourceName, it.ID, err)
	}
	return was == 0, nil
}

// DeleteItemsNotIn deletes items of source whose IDs are not set in keep.
// A false or missing value does not keep the ID. Links that mention a
// deleted item, in either direction, are deleted too. Other sources are
// left alone. The result is the number of items deleted.
func (s *Store) DeleteItemsNotIn(ctx context.Context, sourceName string, keep map[string]bool) (int, error) {
	if sourceName == "" {
		return 0, errors.New("delete items: expected a non-empty source, found empty")
	}
	var deleted int
	err := s.withTx(ctx, func(tx *sql.Tx) error {
		if err := fillKeepIDs(ctx, tx, keep); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `
			DELETE FROM links WHERE source = ? AND (
				blocker_id IN (SELECT id FROM items WHERE source = ? AND id NOT IN (SELECT id FROM keep_ids))
				OR blocked_id IN (SELECT id FROM items WHERE source = ? AND id NOT IN (SELECT id FROM keep_ids))
			)`, sourceName, sourceName, sourceName); err != nil {
			return fmt.Errorf("delete links for %s: %w", sourceName, err)
		}
		res, err := tx.ExecContext(ctx, `
			DELETE FROM items WHERE source = ? AND id NOT IN (SELECT id FROM keep_ids)`, sourceName)
		if err != nil {
			return fmt.Errorf("delete items for %s: %w", sourceName, err)
		}
		n, err := res.RowsAffected()
		if err != nil {
			return fmt.Errorf("deleted item count: %w", err)
		}
		deleted = int(n)
		return nil
	})
	if err != nil {
		return 0, err
	}
	return deleted, nil
}

// DeleteItem deletes one item of source and every link that mentions it,
// in either direction. It reports whether a row was deleted.
func (s *Store) DeleteItem(ctx context.Context, sourceName, id string) (bool, error) {
	if sourceName == "" {
		return false, errors.New("delete item: expected a non-empty source, found empty")
	}
	if id == "" {
		return false, errors.New("delete item: expected a non-empty id, found empty")
	}
	var deleted bool
	err := s.withTx(ctx, func(tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `
			DELETE FROM links WHERE source = ? AND (blocker_id = ? OR blocked_id = ?)`,
			sourceName, id, id); err != nil {
			return fmt.Errorf("delete links for %s/%s: %w", sourceName, id, err)
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM items WHERE source = ? AND id = ?`, sourceName, id)
		if err != nil {
			return fmt.Errorf("delete item %s/%s: %w", sourceName, id, err)
		}
		n, err := res.RowsAffected()
		if err != nil {
			return fmt.Errorf("deleted item count: %w", err)
		}
		deleted = n > 0
		return nil
	})
	if err != nil {
		return false, err
	}
	return deleted, nil
}

// ItemCount returns how many items of source the mirror holds.
func (s *Store) ItemCount(ctx context.Context, sourceName string) (int, error) {
	if sourceName == "" {
		return 0, errors.New("item count: expected a non-empty source, found empty")
	}
	var n int
	if err := s.db.QueryRowContext(ctx, `SELECT COUNT(*) FROM items WHERE source = ?`, sourceName).Scan(&n); err != nil {
		return 0, fmt.Errorf("item count %s: %w", sourceName, err)
	}
	return n, nil
}

func fillKeepIDs(ctx context.Context, tx *sql.Tx, keep map[string]bool) error {
	if _, err := tx.ExecContext(ctx, `CREATE TEMP TABLE IF NOT EXISTS keep_ids (id TEXT PRIMARY KEY)`); err != nil {
		return fmt.Errorf("keep ids: %w", err)
	}
	if _, err := tx.ExecContext(ctx, `DELETE FROM keep_ids`); err != nil {
		return fmt.Errorf("clear keep ids: %w", err)
	}
	for id, yes := range keep {
		if !yes {
			continue
		}
		if id == "" {
			return errors.New("delete items: expected a non-empty keep id, found empty")
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO keep_ids(id) VALUES (?)`, id); err != nil {
			return fmt.Errorf("keep id %q: %w", id, err)
		}
	}
	return nil
}

// Item reads one item. A missing item returns source.ErrNotFound.
func (s *Store) Item(ctx context.Context, sourceName, id string) (source.Item, error) {
	if sourceName == "" {
		return source.Item{}, errors.New("item: expected a non-empty source, found empty")
	}
	if id == "" {
		return source.Item{}, errors.New("item: expected a non-empty id, found empty")
	}
	row := s.db.QueryRowContext(ctx, `SELECT `+itemColumns+` FROM items WHERE source = ? AND id = ?`, sourceName, id)
	it, err := scanItem(row)
	if errors.Is(err, sql.ErrNoRows) {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", sourceName, id, source.ErrNotFound)
	}
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", sourceName, id, err)
	}
	if err := attachOne(ctx, s.db, &it); err != nil {
		return source.Item{}, err
	}
	return it, nil
}

// Items returns items from one source, or every source when source is empty.
// Rows are ordered by source and ID.
func (s *Store) Items(ctx context.Context, sourceName string) ([]source.Item, error) {
	var (
		items []source.Item
		err   error
	)
	if sourceName == "" {
		items, err = queryItems(ctx, s.db, `SELECT `+itemColumns+` FROM items ORDER BY source, id`)
	} else {
		items, err = queryItems(ctx, s.db, `SELECT `+itemColumns+` FROM items WHERE source = ? ORDER BY source, id`, sourceName)
	}
	if err != nil {
		return nil, fmt.Errorf("items: %w", err)
	}
	if err := attachMany(ctx, s.db, sourceName, items); err != nil {
		return nil, err
	}
	return items, nil
}

// FindItem returns every item with id, across sources, ordered by source.
func (s *Store) FindItem(ctx context.Context, id string) ([]source.Item, error) {
	if id == "" {
		return nil, errors.New("find item: expected a non-empty id, found empty")
	}
	items, err := queryItems(ctx, s.db, `SELECT `+itemColumns+` FROM items WHERE id = ? ORDER BY source`, id)
	if err != nil {
		return nil, fmt.Errorf("find item %s: %w", id, err)
	}
	if err := attachMany(ctx, s.db, "", items); err != nil {
		return nil, err
	}
	return items, nil
}

// Blocks returns the IDs of items that id blocks, ordered by ID.
func (s *Store) Blocks(ctx context.Context, sourceName, id string) (out []string, err error) {
	if sourceName == "" {
		return nil, errors.New("blocks: expected a non-empty source, found empty")
	}
	if id == "" {
		return nil, errors.New("blocks: expected a non-empty id, found empty")
	}
	rows, err := s.db.QueryContext(ctx, `
		SELECT blocked_id FROM links
		WHERE source = ? AND blocker_id = ?
		ORDER BY blocked_id`, sourceName, id)
	if err != nil {
		return nil, fmt.Errorf("blocks %s/%s: %w", sourceName, id, err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		var blocked string
		if err := rows.Scan(&blocked); err != nil {
			return nil, fmt.Errorf("blocks %s/%s: %w", sourceName, id, err)
		}
		out = append(out, blocked)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("blocks %s/%s: %w", sourceName, id, err)
	}
	return out, nil
}

type scanner interface {
	Scan(dest ...any) error
}

func queryItems(ctx context.Context, q queryer, query string, args ...any) (out []source.Item, err error) {
	rows, err := q.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		it, err := scanItem(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, it)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate items: %w", err)
	}
	return out, nil
}

func scanItem(sc scanner) (source.Item, error) {
	var (
		it               source.Item
		done, locked     int
		labels           string
		created, updated string
		due, notBefore   sql.NullString
	)
	if err := sc.Scan(
		&it.Source, &it.ID, &it.Title, &it.URL, &it.Type, &it.State, &done, &it.Parent,
		&labels, &it.Assignee, &it.Priority, &created, &updated, &due, &notBefore,
		&it.Author, &it.AuthorAssociation, &locked, &it.ContentHash,
	); err != nil {
		return source.Item{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	var err error
	it.Done, err = intToBool("items.done", it.Source, it.ID, done)
	if err != nil {
		return source.Item{}, err
	}
	it.Locked, err = intToBool("items.locked", it.Source, it.ID, locked)
	if err != nil {
		return source.Item{}, err
	}
	parsedLabels, err := unmarshalLabels(labels)
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", it.Source, it.ID, err)
	}
	it.Labels = parsedLabels
	it.Created, err = parseTime("items.created", created)
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", it.Source, it.ID, err)
	}
	it.Updated, err = parseTime("items.updated", updated)
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", it.Source, it.ID, err)
	}
	it.Due, err = parseDate("items.due", due)
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", it.Source, it.ID, err)
	}
	it.NotBefore, err = parseDate("items.not_before", notBefore)
	if err != nil {
		return source.Item{}, fmt.Errorf("item %s/%s: %w", it.Source, it.ID, err)
	}
	return it, nil
}

func attachOne(ctx context.Context, q queryer, it *source.Item) (err error) {
	rows, err := q.QueryContext(ctx, `
		SELECT blocker_id, link_id FROM links
		WHERE source = ? AND blocked_id = ?
		ORDER BY rowid`, it.Source, it.ID)
	if err != nil {
		return fmt.Errorf("links for %s/%s: %w", it.Source, it.ID, err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		var ref source.BlockRef
		if err := rows.Scan(&ref.ID, &ref.LinkID); err != nil {
			return fmt.Errorf("links for %s/%s: %w", it.Source, it.ID, err)
		}
		it.BlockedBy = append(it.BlockedBy, ref)
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("links for %s/%s: %w", it.Source, it.ID, err)
	}
	return nil
}

func attachMany(ctx context.Context, q queryer, sourceName string, items []source.Item) (err error) {
	if len(items) == 0 {
		return nil
	}
	index := make(map[string][]int, len(items))
	for i := range items {
		items[i].BlockedBy = nil
		key := items[i].Source + "\x00" + items[i].ID
		index[key] = append(index[key], i)
	}
	var rows *sql.Rows
	if sourceName == "" {
		rows, err = q.QueryContext(ctx, `SELECT source, blocked_id, blocker_id, link_id FROM links ORDER BY rowid`)
	} else {
		rows, err = q.QueryContext(ctx, `
			SELECT source, blocked_id, blocker_id, link_id FROM links
			WHERE source = ? ORDER BY rowid`, sourceName)
	}
	if err != nil {
		return fmt.Errorf("links: %w", err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		var linkSource, blocked, blocker, linkID string
		if err := rows.Scan(&linkSource, &blocked, &blocker, &linkID); err != nil {
			return fmt.Errorf("links: %w", err)
		}
		for _, i := range index[linkSource+"\x00"+blocked] {
			items[i].BlockedBy = append(items[i].BlockedBy, source.BlockRef{ID: blocker, LinkID: linkID})
		}
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("links: %w", err)
	}
	return nil
}

func marshalLabels(labels []string) (string, error) {
	if labels == nil {
		return "null", nil
	}
	raw, err := json.Marshal(labels)
	if err != nil {
		return "", fmt.Errorf("labels: %w", err)
	}
	return string(raw), nil
}

func unmarshalLabels(raw string) ([]string, error) {
	if raw == "" || raw == "null" {
		return nil, nil
	}
	var labels []string
	if err := json.Unmarshal([]byte(raw), &labels); err != nil {
		return nil, fmt.Errorf("labels_json: expected a JSON array of strings, found %q", raw)
	}
	return labels, nil
}

func boolToInt(v bool) int {
	if v {
		return 1
	}
	return 0
}

func intToBool(col, sourceName, id string, v int) (bool, error) {
	switch v {
	case 0:
		return false, nil
	case 1:
		return true, nil
	default:
		return false, fmt.Errorf("%s for %s/%s: expected 0 or 1, found %d", col, sourceName, id, v)
	}
}
