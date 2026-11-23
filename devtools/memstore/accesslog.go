package memstore

import (
	"context"
	"fmt"
	"time"
)

// AccessLogEntry is one content-free access-log row.
// It never stores entity names, fact values, or query strings.
type AccessLogEntry struct {
	ID       int64     `json:"id"`
	Time     time.Time `json:"time"`
	Caller   string    `json:"caller"`
	Profile  string    `json:"profile"`
	Command  string    `json:"command"`
	EntityID string    `json:"entityId"`
	Reason   string    `json:"reason"`
}

// AccessLogFilter selects access-log rows. Zero-value fields match everything.
type AccessLogFilter struct {
	Caller   string
	EntityID string
	Since    time.Time
	Limit    int
}

// LogAccess appends one content-free access-log row stamped with the vault's
// clock.
func (v *Vault) LogAccess(ctx context.Context, caller, profile, command, entityID, reason string) error {
	ts := v.now().UTC().Unix()
	if _, err := v.db.ExecContext(ctx,
		`INSERT INTO access_log (ts, caller, profile, command, entity_id, reason)
		VALUES (?, ?, ?, ?, ?, ?)`,
		ts, caller, profile, command, entityID, reason,
	); err != nil {
		return fmt.Errorf("access log: %w", err)
	}
	return nil
}

// QueryAccessLog returns matching access-log rows, newest first.
func (v *Vault) QueryAccessLog(ctx context.Context, filter AccessLogFilter) ([]AccessLogEntry, error) {
	var sinceUnix int64
	hasSince := 0
	if !filter.Since.IsZero() {
		sinceUnix = filter.Since.Unix()
		hasSince = 1
	}
	limit := filter.Limit
	if limit <= 0 {
		limit = -1
	}

	rows, err := v.db.QueryContext(ctx,
		`SELECT id, ts, caller, profile, command, entity_id, reason
		FROM access_log
		WHERE (?1 = '' OR caller = ?1)
		  AND (?2 = '' OR entity_id = ?2)
		  AND (?3 = 0 OR ts >= ?4)
		ORDER BY ts DESC, id DESC
		LIMIT ?5`,
		filter.Caller, filter.EntityID, hasSince, sinceUnix, limit,
	)
	if err != nil {
		return nil, fmt.Errorf("query access log: %w", err)
	}
	defer rows.Close()

	out := []AccessLogEntry{}
	for rows.Next() {
		var e AccessLogEntry
		var ts int64
		if err := rows.Scan(&e.ID, &ts, &e.Caller, &e.Profile, &e.Command, &e.EntityID, &e.Reason); err != nil {
			return nil, fmt.Errorf("scan access log: %w", err)
		}
		e.Time = time.Unix(ts, 0).UTC()
		out = append(out, e)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate access log: %w", err)
	}
	return out, nil
}
