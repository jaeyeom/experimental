package memstore

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"time"
)

// RecordRevision appends a compact revision record inside tx, marshaling
// diffObj to compact JSON.
func RecordRevision(ctx context.Context, tx *sql.Tx, table, rowID, op string, diffObj any, actor, reason string) error {
	return recordRevisionAt(ctx, tx, table, rowID, op, diffObj, actor, reason, time.Now().UTC())
}

// recordRevisionAt is RecordRevision with an explicit timestamp, for callers
// that run under an injectable clock.
func recordRevisionAt(ctx context.Context, tx *sql.Tx, table, rowID, op string, diffObj any, actor, reason string, at time.Time) error {
	diffJSON := "{}"
	if diffObj != nil {
		b, err := json.Marshal(diffObj)
		if err != nil {
			return fmt.Errorf("marshal revision diff: %w", err)
		}
		diffJSON = string(b)
	}
	createdAt := at.UTC().Unix()
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO revisions (table_name, row_id, op, diff_json, actor, reason, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?)`,
		table, rowID, op, diffJSON, actor, reason, createdAt,
	); err != nil {
		return fmt.Errorf("insert revision: %w", err)
	}
	return nil
}
