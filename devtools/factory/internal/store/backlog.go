package store

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"time"
)

// BacklogSnapshot is one source's backlog counts at TakenAt.
type BacklogSnapshot struct {
	Source     string
	TakenAt    time.Time
	Ready      int
	Blocked    int
	InFlight   int
	NeedsHuman int
	ByStatus   map[string]int // Open items by raw status name.
}

const backlogColumns = `source, taken_at, ready, blocked, in_flight, needs_human, by_status_json`

// RecordBacklogSnapshot inserts snap unless the source already has a
// snapshot taken less than every before snap.TakenAt. It then deletes every
// snapshot, of any source, taken before snap.TakenAt minus keep.
// The result reports whether snap was inserted.
func (s *Store) RecordBacklogSnapshot(ctx context.Context, snap BacklogSnapshot, every, keep time.Duration) (bool, error) {
	if snap.Source == "" {
		return false, errors.New("backlog snapshot: expected a non-empty source, found empty")
	}
	if snap.TakenAt.IsZero() {
		return false, fmt.Errorf("backlog snapshot %s: expected taken_at, found zero", snap.Source)
	}
	if keep <= 0 {
		return false, fmt.Errorf("backlog snapshot %s: expected a positive retention, found %s", snap.Source, keep)
	}
	byStatus, err := json.Marshal(snap.ByStatus)
	if err != nil {
		return false, fmt.Errorf("backlog snapshot %s: %w", snap.Source, err)
	}
	inserted := false
	err = s.withTx(ctx, func(tx *sql.Tx) error {
		var last sql.NullString
		if err := tx.QueryRowContext(ctx, `SELECT MAX(taken_at) FROM backlog_snapshots WHERE source = ?`, snap.Source).Scan(&last); err != nil {
			return fmt.Errorf("backlog snapshot %s: %w", snap.Source, err)
		}
		lastAt, err := parseTime("backlog_snapshots.taken_at", last.String)
		if err != nil {
			return fmt.Errorf("backlog snapshot %s: %w", snap.Source, err)
		}
		if lastAt.IsZero() || snap.TakenAt.Sub(lastAt) >= every {
			if _, err := tx.ExecContext(ctx, `INSERT INTO backlog_snapshots(`+backlogColumns+`) VALUES (?, ?, ?, ?, ?, ?, ?)`,
				snap.Source, formatTime(snap.TakenAt), snap.Ready, snap.Blocked, snap.InFlight, snap.NeedsHuman, string(byStatus)); err != nil {
				return fmt.Errorf("backlog snapshot %s: %w", snap.Source, err)
			}
			inserted = true
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM backlog_snapshots WHERE taken_at < ?`, formatTime(snap.TakenAt.Add(-keep))); err != nil {
			return fmt.Errorf("prune backlog snapshots: %w", err)
		}
		return nil
	})
	if err != nil {
		return false, err
	}
	return inserted, nil
}

// BacklogSnapshots returns every snapshot, ordered by taken_at and source.
func (s *Store) BacklogSnapshots(ctx context.Context) (out []BacklogSnapshot, err error) {
	rows, err := s.db.QueryContext(ctx, `SELECT `+backlogColumns+` FROM backlog_snapshots ORDER BY taken_at, source`)
	if err != nil {
		return nil, fmt.Errorf("backlog snapshots: %w", err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		var (
			snap     BacklogSnapshot
			takenAt  string
			byStatus string
		)
		if err := rows.Scan(&snap.Source, &takenAt, &snap.Ready, &snap.Blocked, &snap.InFlight, &snap.NeedsHuman, &byStatus); err != nil {
			return nil, fmt.Errorf("backlog snapshots: %w", err)
		}
		snap.TakenAt, err = parseTime("backlog_snapshots.taken_at", takenAt)
		if err != nil {
			return nil, err
		}
		if err := json.Unmarshal([]byte(byStatus), &snap.ByStatus); err != nil {
			return nil, fmt.Errorf("backlog_snapshots.by_status_json for %s: expected a JSON object of counts, found %q", snap.Source, byStatus)
		}
		out = append(out, snap)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("backlog snapshots: %w", err)
	}
	return out, nil
}
