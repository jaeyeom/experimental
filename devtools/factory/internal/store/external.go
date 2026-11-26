package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// ExternalStatus is the result of fetching a blocker that is not in the mirror.
type ExternalStatus string

const (
	// ExternalFound means the source returned the blocker.
	ExternalFound ExternalStatus = "found"
	// ExternalNotFound means the source has no such item, or it is not visible.
	ExternalNotFound ExternalStatus = "not-found"
	// ExternalUnchecked means the blocker was not fetched: the lookup bound
	// was reached, or the fetch failed. Reason says which.
	ExternalUnchecked ExternalStatus = "unchecked"
)

// ExternalBlocker is a blocker of a mirrored item that is not itself in the
// mirror. It only answers whether that blocker is done; it is never a work
// item.
type ExternalBlocker struct {
	ID        string
	Status    ExternalStatus
	Done      bool
	State     string
	Assignee  string
	Title     string
	Reason    string
	FetchedAt time.Time
}

const externalColumns = `id, status, done, state, assignee, title, reason, fetched_at`

// ExternalBlockers returns source's external blocker records keyed by id.
func (s *Store) ExternalBlockers(ctx context.Context, source string) (out map[string]ExternalBlocker, err error) {
	if source == "" {
		return nil, errors.New("external blockers: expected a non-empty source, found empty")
	}
	rows, err := s.db.QueryContext(ctx, `SELECT `+externalColumns+` FROM external_blockers WHERE source = ?`, source)
	if err != nil {
		return nil, fmt.Errorf("external blockers %s: %w", source, err)
	}
	defer func() { err = closeRows(rows, err) }()
	out = make(map[string]ExternalBlocker)
	for rows.Next() {
		var (
			rec     ExternalBlocker
			status  string
			done    int
			fetched string
		)
		if err := rows.Scan(&rec.ID, &status, &done, &rec.State, &rec.Assignee, &rec.Title, &rec.Reason, &fetched); err != nil {
			return nil, fmt.Errorf("external blockers %s: %w", source, err)
		}
		rec.Status = ExternalStatus(status)
		rec.Done = done != 0
		at, err := parseTime("external_blockers.fetched_at", fetched)
		if err != nil {
			return nil, fmt.Errorf("external blockers %s: %w", source, err)
		}
		rec.FetchedAt = at
		out[rec.ID] = rec
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("external blockers %s: %w", source, err)
	}
	return out, nil
}

// ReplaceExternalBlockers makes recs the whole set of source's external
// blocker records: a record not in recs is deleted.
func (s *Store) ReplaceExternalBlockers(ctx context.Context, source string, recs []ExternalBlocker) error {
	if source == "" {
		return errors.New("replace external blockers: expected a non-empty source, found empty")
	}
	for i, rec := range recs {
		if rec.ID == "" {
			return fmt.Errorf("replace external blockers %s: expected an id, found empty at position %d", source, i)
		}
		switch rec.Status {
		case ExternalFound, ExternalNotFound, ExternalUnchecked:
		default:
			return fmt.Errorf("replace external blockers %s: %s: expected status found, not-found, or unchecked, found %q", source, rec.ID, rec.Status)
		}
	}
	return s.withTx(ctx, func(tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `DELETE FROM external_blockers WHERE source = ?`, source); err != nil {
			return fmt.Errorf("replace external blockers %s: %w", source, err)
		}
		for _, rec := range recs {
			done := 0
			if rec.Done {
				done = 1
			}
			if _, err := tx.ExecContext(ctx, `INSERT INTO external_blockers(source, `+externalColumns+`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
				source, rec.ID, string(rec.Status), done, rec.State, rec.Assignee, rec.Title, rec.Reason, formatTime(rec.FetchedAt)); err != nil {
				return fmt.Errorf("replace external blockers %s: %s: %w", source, rec.ID, err)
			}
		}
		return nil
	})
}
