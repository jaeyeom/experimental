package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// Run is one audit-log row.
type Run struct {
	StartedAt  time.Time
	Command    string
	Outcome    string
	DetailJSON string
}

// AppendRun inserts one run row.
func (s *Store) AppendRun(ctx context.Context, run Run) error {
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO runs(started_at, command, outcome, detail_json) VALUES (?, ?, ?, ?)`,
		formatTime(run.StartedAt), run.Command, run.Outcome, run.DetailJSON,
	)
	if err != nil {
		return fmt.Errorf("append run: %w", err)
	}
	return nil
}

// Runs returns the newest runs first. limit must be at least 1.
func (s *Store) Runs(ctx context.Context, limit int) (out []Run, err error) {
	if limit < 1 {
		return nil, fmt.Errorf("runs: expected limit >= 1, found %d", limit)
	}
	rows, err := s.db.QueryContext(ctx, `
		SELECT started_at, command, outcome, detail_json
		FROM runs
		ORDER BY started_at DESC, rowid DESC
		LIMIT ?`, limit)
	if err != nil {
		return nil, fmt.Errorf("runs: %w", err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		run, err := scanRun(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, run)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("runs: %w", err)
	}
	return out, nil
}

// LastRunAt returns the latest started_at for command.
// A command that has never run returns the zero time and a nil error.
func (s *Store) LastRunAt(ctx context.Context, command string) (time.Time, error) {
	if command == "" {
		return time.Time{}, errors.New("last run: expected a non-empty command, found empty")
	}
	var raw sql.NullString
	err := s.db.QueryRowContext(ctx, `
		SELECT started_at FROM runs
		WHERE command = ?
		ORDER BY started_at DESC, rowid DESC
		LIMIT 1`, command).Scan(&raw)
	if errors.Is(err, sql.ErrNoRows) {
		return time.Time{}, nil
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("last run %s: %w", command, err)
	}
	if !raw.Valid || raw.String == "" {
		return time.Time{}, fmt.Errorf("last run %s: expected started_at, found empty", command)
	}
	at, err := parseTime("runs.started_at", raw.String)
	if err != nil {
		return time.Time{}, fmt.Errorf("last run %s: %w", command, err)
	}
	return at, nil
}

func scanRun(sc scanner) (Run, error) {
	var (
		run       Run
		startedAt string
	)
	if err := sc.Scan(&startedAt, &run.Command, &run.Outcome, &run.DetailJSON); err != nil {
		return Run{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	var err error
	run.StartedAt, err = parseTime("runs.started_at", startedAt)
	if err != nil {
		return Run{}, err
	}
	return run, nil
}

// SyncState returns the last updated timestamp seen and the last full sync.
// A source that has never synced returns two zero times and a nil error.
func (s *Store) SyncState(ctx context.Context, sourceName string) (time.Time, time.Time, error) {
	if sourceName == "" {
		return time.Time{}, time.Time{}, errors.New("sync state: expected a non-empty source, found empty")
	}
	var seenRaw, fullRaw sql.NullString
	err := s.db.QueryRowContext(ctx, `
		SELECT last_updated_seen, last_full_sync FROM sync_state WHERE source = ?`, sourceName).
		Scan(&seenRaw, &fullRaw)
	if errors.Is(err, sql.ErrNoRows) {
		return time.Time{}, time.Time{}, nil
	}
	if err != nil {
		return time.Time{}, time.Time{}, fmt.Errorf("sync state %s: %w", sourceName, err)
	}
	seen, err := parseTime("sync_state.last_updated_seen", seenRaw.String)
	if err != nil {
		return time.Time{}, time.Time{}, fmt.Errorf("sync state %s: %w", sourceName, err)
	}
	full, err := parseTime("sync_state.last_full_sync", fullRaw.String)
	if err != nil {
		return time.Time{}, time.Time{}, fmt.Errorf("sync state %s: %w", sourceName, err)
	}
	return seen, full, nil
}

// SetSyncState records the sync cursor for one source.
func (s *Store) SetSyncState(ctx context.Context, sourceName string, lastSeen, lastFull time.Time) error {
	if sourceName == "" {
		return errors.New("sync state: expected a non-empty source, found empty")
	}
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO sync_state(source, last_updated_seen, last_full_sync) VALUES (?, ?, ?)
		ON CONFLICT(source) DO UPDATE SET
			last_updated_seen = excluded.last_updated_seen,
			last_full_sync = excluded.last_full_sync`,
		sourceName, formatTime(lastSeen), formatTime(lastFull),
	)
	if err != nil {
		return fmt.Errorf("set sync state %s: %w", sourceName, err)
	}
	return nil
}

// SetMe records the authenticated account for a source that already has a sync row.
// An empty account is rejected. A missing sync_state row is an error.
func (s *Store) SetMe(ctx context.Context, sourceName, me string) error {
	if sourceName == "" {
		return errors.New("sync me: expected a non-empty source, found empty")
	}
	if me == "" {
		return errors.New("sync me: expected a non-empty account, found empty")
	}
	res, err := s.db.ExecContext(ctx, `UPDATE sync_state SET me = ? WHERE source = ?`, me, sourceName)
	if err != nil {
		return fmt.Errorf("set me %s: %w", sourceName, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("set me %s: %w", sourceName, err)
	}
	if n == 0 {
		return fmt.Errorf("set me %s: expected a sync_state row, found none", sourceName)
	}
	return nil
}

// Me returns the account stored for source.
// A source that has never synced, or whose sync did not record an account,
// returns an empty string and a nil error.
func (s *Store) Me(ctx context.Context, sourceName string) (string, error) {
	if sourceName == "" {
		return "", errors.New("sync me: expected a non-empty source, found empty")
	}
	var me sql.NullString
	err := s.db.QueryRowContext(ctx, `SELECT me FROM sync_state WHERE source = ?`, sourceName).Scan(&me)
	if errors.Is(err, sql.ErrNoRows) {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("me %s: %w", sourceName, err)
	}
	if !me.Valid {
		return "", nil
	}
	return me.String, nil
}

// LastSync returns when source last synced successfully.
// A source that has never synced returns the zero time and a nil error.
func (s *Store) LastSync(ctx context.Context, sourceName string) (time.Time, error) {
	if sourceName == "" {
		return time.Time{}, errors.New("last sync: expected a non-empty source, found empty")
	}
	var raw sql.NullString
	err := s.db.QueryRowContext(ctx, `SELECT last_sync FROM sync_state WHERE source = ?`, sourceName).Scan(&raw)
	if errors.Is(err, sql.ErrNoRows) {
		return time.Time{}, nil
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("last sync %s: %w", sourceName, err)
	}
	if !raw.Valid || raw.String == "" {
		return time.Time{}, nil
	}
	at, err := parseTime("sync_state.last_sync", raw.String)
	if err != nil {
		return time.Time{}, fmt.Errorf("last sync %s: %w", sourceName, err)
	}
	return at, nil
}

// SetLastSync records a successful sync time for a source that already has a sync row.
// A zero time is rejected. A missing sync_state row is an error.
// SetSyncState does not clear this column.
func (s *Store) SetLastSync(ctx context.Context, sourceName string, at time.Time) error {
	if sourceName == "" {
		return errors.New("last sync: expected a non-empty source, found empty")
	}
	if at.IsZero() {
		return errors.New("last sync: expected a non-zero time, found zero")
	}
	res, err := s.db.ExecContext(ctx, `UPDATE sync_state SET last_sync = ? WHERE source = ?`, formatTime(at), sourceName)
	if err != nil {
		return fmt.Errorf("set last sync %s: %w", sourceName, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("set last sync %s: %w", sourceName, err)
	}
	if n == 0 {
		return fmt.Errorf("set last sync %s: expected a sync_state row, found none", sourceName)
	}
	return nil
}
