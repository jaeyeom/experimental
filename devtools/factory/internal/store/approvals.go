package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// Approval is a local content-hash approval recorded for one work item.
type Approval struct {
	Source      string
	ItemID      string
	ContentHash string
	ApprovedAt  time.Time
}

const approvalColumns = `source, item_id, content_hash, approved_at`

// SetApproval inserts or updates the content-hash approval for (a.Source, a.ItemID).
func (s *Store) SetApproval(ctx context.Context, a Approval) error {
	if a.Source == "" {
		return errors.New("set approval: expected a non-empty source, found empty")
	}
	if a.ItemID == "" {
		return errors.New("set approval: expected a non-empty item id, found empty")
	}
	if a.ContentHash == "" {
		return errors.New("set approval: expected a non-empty content hash, found empty")
	}
	if a.ApprovedAt.IsZero() {
		return errors.New("set approval: expected a non-zero approved_at, found zero")
	}
	_, err := s.db.ExecContext(ctx, `
		INSERT INTO approvals(`+approvalColumns+`) VALUES (?, ?, ?, ?)
		ON CONFLICT(source, item_id) DO UPDATE SET
			content_hash = excluded.content_hash,
			approved_at = excluded.approved_at`,
		a.Source, a.ItemID, a.ContentHash, formatTime(a.ApprovedAt),
	)
	if err != nil {
		return fmt.Errorf("set approval %s/%s: %w", a.Source, a.ItemID, err)
	}
	return nil
}

// Approval returns the stored approval for (sourceName, itemID), or
// source.ErrNotFound when none exists.
func (s *Store) Approval(ctx context.Context, sourceName, itemID string) (Approval, error) {
	if sourceName == "" {
		return Approval{}, errors.New("approval: expected a non-empty source, found empty")
	}
	if itemID == "" {
		return Approval{}, errors.New("approval: expected a non-empty item id, found empty")
	}
	row := s.db.QueryRowContext(ctx, `
		SELECT `+approvalColumns+` FROM approvals
		WHERE source = ? AND item_id = ?`, sourceName, itemID)
	a, err := scanApproval(row)
	if errors.Is(err, sql.ErrNoRows) {
		return Approval{}, fmt.Errorf("approval %s/%s: %w", sourceName, itemID, source.ErrNotFound)
	}
	if err != nil {
		return Approval{}, fmt.Errorf("approval %s/%s: %w", sourceName, itemID, err)
	}
	return a, nil
}

// Approvals returns all stored approvals for sourceName, keyed by item_id.
func (s *Store) Approvals(ctx context.Context, sourceName string) (out map[string]Approval, err error) {
	if sourceName == "" {
		return nil, errors.New("approvals: expected a non-empty source, found empty")
	}
	rows, err := s.db.QueryContext(ctx, `
		SELECT `+approvalColumns+` FROM approvals
		WHERE source = ? ORDER BY item_id`, sourceName)
	if err != nil {
		return nil, fmt.Errorf("approvals %s: %w", sourceName, err)
	}
	defer func() { err = closeRows(rows, err) }()
	out = make(map[string]Approval)
	for rows.Next() {
		a, err := scanApproval(rows)
		if err != nil {
			return nil, fmt.Errorf("approvals %s: %w", sourceName, err)
		}
		out[a.ItemID] = a
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("approvals %s: %w", sourceName, err)
	}
	return out, nil
}

// DeleteApproval removes any stored approval for (sourceName, itemID) and
// reports whether a row was deleted.
func (s *Store) DeleteApproval(ctx context.Context, sourceName, itemID string) (bool, error) {
	if sourceName == "" {
		return false, errors.New("delete approval: expected a non-empty source, found empty")
	}
	if itemID == "" {
		return false, errors.New("delete approval: expected a non-empty item id, found empty")
	}
	res, err := s.db.ExecContext(ctx, `DELETE FROM approvals WHERE source = ? AND item_id = ?`, sourceName, itemID)
	if err != nil {
		return false, fmt.Errorf("delete approval %s/%s: %w", sourceName, itemID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("delete approval %s/%s count: %w", sourceName, itemID, err)
	}
	return n > 0, nil
}

func scanApproval(sc scanner) (Approval, error) {
	var (
		a          Approval
		approvedAt string
	)
	if err := sc.Scan(&a.Source, &a.ItemID, &a.ContentHash, &approvedAt); err != nil {
		return Approval{}, err //nolint:wrapcheck // caller adds context
	}
	parsed, err := parseTime("approvals.approved_at", approvedAt)
	if err != nil {
		return Approval{}, err
	}
	a.ApprovedAt = parsed
	return a, nil
}
