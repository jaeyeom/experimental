package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// Finished is one item that reached done. A row is written when a claim
// moves to done, or when a sync sees a mirrored item change from not done to
// done. Deleting the item from items later does not touch this row.
type Finished struct {
	Source     string
	ID         string
	Title      string
	Parent     string
	Labels     []string
	FinishedAt time.Time
	// ClaimedAt is the claim's claimed_at. Zero means no claim was recorded.
	ClaimedAt time.Time
}

const finishedColumns = `source, id, title, parent, labels_json, finished_at, claimed_at`

// insertFinished records sourceName/id as finished at finishedAt. Title,
// parent, and labels come from the mirror row, or stay empty when the item
// is not mirrored. claimedAt is the claim's text timestamp, or nil to use
// the latest claim that was not released or failed.
// The first finish wins: a later call only fills a missing claimed_at.
func insertFinished(ctx context.Context, tx *sql.Tx, sourceName, id string, finishedAt time.Time, claimedAt any) error {
	if finishedAt.IsZero() {
		return fmt.Errorf("finished %s/%s: expected a finish time, found zero", sourceName, id)
	}
	var title, parent string
	labels := "null"
	err := tx.QueryRowContext(ctx, `SELECT title, parent, labels_json FROM items WHERE source = ? AND id = ?`,
		sourceName, id).Scan(&title, &parent, &labels)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("finished %s/%s: %w", sourceName, id, err)
	}
	if claimedAt == nil {
		claimedAt, err = latestLiveClaim(ctx, tx, sourceName, id)
		if err != nil {
			return err
		}
	}
	_, err = tx.ExecContext(ctx, `
		INSERT INTO finished(`+finishedColumns+`) VALUES (?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(source, id) DO UPDATE SET
			claimed_at = COALESCE(finished.claimed_at, excluded.claimed_at)`,
		sourceName, id, title, parent, labels, formatTime(finishedAt), claimedAt,
	)
	if err != nil {
		return fmt.Errorf("finished %s/%s: %w", sourceName, id, err)
	}
	return nil
}

func latestLiveClaim(ctx context.Context, tx *sql.Tx, sourceName, id string) (any, error) {
	var raw sql.NullString
	err := tx.QueryRowContext(ctx, `
		SELECT MAX(claimed_at) FROM claims
		WHERE source = ? AND id = ? AND state NOT IN (?, ?)`,
		sourceName, id, string(Released), string(Failed)).Scan(&raw)
	if err != nil {
		return nil, fmt.Errorf("finished %s/%s: claim: %w", sourceName, id, err)
	}
	if !raw.Valid || raw.String == "" {
		return nil, nil
	}
	return raw.String, nil
}

// FinishedItems returns every finished row, ordered by finished_at, source, and ID.
func (s *Store) FinishedItems(ctx context.Context) (out []Finished, err error) {
	rows, err := s.db.QueryContext(ctx, `SELECT `+finishedColumns+` FROM finished ORDER BY finished_at, source, id`)
	if err != nil {
		return nil, fmt.Errorf("finished: %w", err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		f, err := scanFinished(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, f)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("finished: %w", err)
	}
	return out, nil
}

func scanFinished(sc scanner) (Finished, error) {
	var (
		f          Finished
		labels     string
		finishedAt string
		claimedAt  sql.NullString
	)
	if err := sc.Scan(&f.Source, &f.ID, &f.Title, &f.Parent, &labels, &finishedAt, &claimedAt); err != nil {
		return Finished{}, fmt.Errorf("finished: %w", err)
	}
	var err error
	f.Labels, err = unmarshalLabels(labels)
	if err != nil {
		return Finished{}, fmt.Errorf("finished %s/%s: %w", f.Source, f.ID, err)
	}
	f.FinishedAt, err = parseTime("finished.finished_at", finishedAt)
	if err != nil {
		return Finished{}, fmt.Errorf("finished %s/%s: %w", f.Source, f.ID, err)
	}
	if f.FinishedAt.IsZero() {
		return Finished{}, fmt.Errorf("finished %s/%s: expected finished_at, found empty", f.Source, f.ID)
	}
	f.ClaimedAt, err = parseTime("finished.claimed_at", claimedAt.String)
	if err != nil {
		return Finished{}, fmt.Errorf("finished %s/%s: %w", f.Source, f.ID, err)
	}
	return f, nil
}
