package memstore

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"
)

// ErrAlreadyRan is returned by BeginRoutineRun when a run with the same
// (routine_id, idempotency_key) has already been recorded.
var ErrAlreadyRan = errors.New("routine already ran for idempotency key")

// Routine run status values stored in routine_runs.status.
const (
	RoutineRunRunning = "running"
	RoutineRunOK      = "ok"
	RoutineRunFailed  = "failed"
	RoutineRunSkipped = "skipped"
)

// Routine outbox status values stored in routine_outbox.status.
const (
	OutboxPendingApproval = "pending_approval"
	OutboxApproved        = "approved"
	OutboxExecuted        = "executed"
	OutboxRejected        = "rejected"
	OutboxFailed          = "failed"
)

// RoutineRun represents a row in the routine_runs table.
type RoutineRun struct {
	ID             string     `json:"id"`
	RoutineID      string     `json:"routineId"`
	IdempotencyKey string     `json:"idempotencyKey"`
	Trigger        string     `json:"trigger,omitempty"`
	StartedAt      time.Time  `json:"startedAt"`
	FinishedAt     *time.Time `json:"finishedAt,omitempty"`
	Status         string     `json:"status"`
	ExitCode       *int       `json:"exitCode,omitempty"`
	MetricsJSON    string     `json:"metricsJson,omitempty"`
	ErrorSummary   string     `json:"errorSummary,omitempty"`
}

// RoutineRunFilter selects routine_runs rows in ListRoutineRuns.
type RoutineRunFilter struct {
	RoutineID string
	Status    string
	Limit     int
}

// RoutineOutboxItem represents a row in the routine_outbox table.
type RoutineOutboxItem struct {
	ID          string     `json:"id"`
	RoutineID   string     `json:"routineId"`
	ActionKind  string     `json:"actionKind"`
	PayloadJSON string     `json:"payloadJson"`
	Status      string     `json:"status"`
	DecideID    string     `json:"decideId,omitempty"`
	CreatedAt   time.Time  `json:"createdAt"`
	ExecutedAt  *time.Time `json:"executedAt,omitempty"`
}

// RoutineOutboxFilter selects routine_outbox rows in ListRoutineOutbox.
type RoutineOutboxFilter struct {
	RoutineID string
	Status    string
	Limit     int
}

// GetRoutineState returns the JSON value for (routineID, key) if present.
func (v *Vault) GetRoutineState(ctx context.Context, routineID, key string) (string, bool, error) {
	if routineID == "" || key == "" {
		return "", false, fmt.Errorf("routine_id and key are required")
	}
	var valueJSON string
	err := v.db.QueryRowContext(ctx,
		`SELECT value_json FROM routine_state WHERE routine_id = ? AND key = ?`,
		routineID, key,
	).Scan(&valueJSON)
	if errors.Is(err, sql.ErrNoRows) {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("get routine state %s/%s: %w", routineID, key, err)
	}
	return valueJSON, true, nil
}

// SetRoutineState upserts the JSON value for (routineID, key).
func (v *Vault) SetRoutineState(ctx context.Context, routineID, key, valueJSON string) error {
	if routineID == "" || key == "" {
		return fmt.Errorf("routine_id and key are required")
	}
	if !json.Valid([]byte(valueJSON)) {
		return fmt.Errorf("value_json must be valid JSON")
	}
	nowUnix := v.now().UTC().Unix()
	if _, err := v.db.ExecContext(ctx,
		`INSERT INTO routine_state (routine_id, key, value_json, updated_at)
		VALUES (?, ?, ?, ?)
		ON CONFLICT(routine_id, key) DO UPDATE SET
			value_json = excluded.value_json,
			updated_at = excluded.updated_at`,
		routineID, key, valueJSON, nowUnix,
	); err != nil {
		return fmt.Errorf("set routine state %s/%s: %w", routineID, key, err)
	}
	return nil
}

// AllRoutineState returns all key->value_json entries for routineID.
func (v *Vault) AllRoutineState(ctx context.Context, routineID string) (map[string]string, error) {
	if routineID == "" {
		return nil, fmt.Errorf("routine_id is required")
	}
	rows, err := v.db.QueryContext(ctx,
		`SELECT key, value_json FROM routine_state WHERE routine_id = ? ORDER BY key ASC`,
		routineID,
	)
	if err != nil {
		return nil, fmt.Errorf("list routine state %s: %w", routineID, err)
	}
	defer rows.Close()

	out := make(map[string]string)
	for rows.Next() {
		var k, val string
		if err := rows.Scan(&k, &val); err != nil {
			return nil, fmt.Errorf("scan routine state: %w", err)
		}
		out[k] = val
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate routine state: %w", err)
	}
	return out, nil
}

// BeginRoutineRun records a new running row in routine_runs, enforcing
// UNIQUE(routine_id, idempotency_key) and returning ErrAlreadyRan on conflict.
func (v *Vault) BeginRoutineRun(ctx context.Context, routineID, idempotencyKey, trigger string) (RoutineRun, error) {
	if routineID == "" || idempotencyKey == "" {
		return RoutineRun{}, fmt.Errorf("routine_id and idempotency_key are required")
	}
	id, err := newID("run")
	if err != nil {
		return RoutineRun{}, err
	}
	now := v.now().UTC()
	nowUnix := now.Unix()

	if _, err := v.db.ExecContext(ctx,
		`INSERT INTO routine_runs (id, routine_id, idempotency_key, trigger, started_at, status)
		VALUES (?, ?, ?, ?, ?, ?)`,
		id, routineID, idempotencyKey, trigger, nowUnix, RoutineRunRunning,
	); err != nil {
		if isUniqueConstraintError(err) {
			return RoutineRun{}, fmt.Errorf("%w: %s/%s", ErrAlreadyRan, routineID, idempotencyKey)
		}
		return RoutineRun{}, fmt.Errorf("begin routine run %s: %w", routineID, err)
	}

	return RoutineRun{
		ID:             id,
		RoutineID:      routineID,
		IdempotencyKey: idempotencyKey,
		Trigger:        trigger,
		StartedAt:      now,
		Status:         RoutineRunRunning,
	}, nil
}

// FinishRoutineRun updates a routine_runs row with completion status, optional
// exit code, metrics JSON, and error summary.
func (v *Vault) FinishRoutineRun(ctx context.Context, runID, status string, exitCode *int, metricsJSON, errorSummary string) error {
	if runID == "" {
		return fmt.Errorf("run_id is required")
	}
	if !isValidRoutineRunFinishStatus(status) {
		return fmt.Errorf("invalid routine run finish status %q", status)
	}
	if metricsJSON != "" && !json.Valid([]byte(metricsJSON)) {
		return fmt.Errorf("metrics_json must be valid JSON")
	}
	nowUnix := v.now().UTC().Unix()
	var code sql.NullInt64
	if exitCode != nil {
		code = sql.NullInt64{Int64: int64(*exitCode), Valid: true}
	}

	res, err := v.db.ExecContext(ctx,
		`UPDATE routine_runs
		SET finished_at = ?, status = ?, exit_code = ?, metrics_json = ?, error_summary = ?
		WHERE id = ?`,
		nowUnix, status, code, metricsJSON, errorSummary, runID,
	)
	if err != nil {
		return fmt.Errorf("finish routine run %s: %w", runID, err)
	}
	affected, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("finish routine run %s rows affected: %w", runID, err)
	}
	if affected == 0 {
		return fmt.Errorf("%w: routine run %q", ErrNotFound, runID)
	}
	return nil
}

// ListRoutineRuns returns matching routine_runs rows, newest first.
func (v *Vault) ListRoutineRuns(ctx context.Context, filter RoutineRunFilter) ([]RoutineRun, error) {
	limit := filter.Limit
	if limit <= 0 {
		limit = -1
	}
	rows, err := v.db.QueryContext(ctx,
		`SELECT id, routine_id, idempotency_key, trigger, started_at, finished_at, status, exit_code, metrics_json, error_summary
		FROM routine_runs
		WHERE (?1 = '' OR routine_id = ?1)
		  AND (?2 = '' OR status = ?2)
		ORDER BY started_at DESC, id DESC
		LIMIT ?3`,
		filter.RoutineID, filter.Status, limit,
	)
	if err != nil {
		return nil, fmt.Errorf("list routine runs: %w", err)
	}
	defer rows.Close()

	out := []RoutineRun{}
	for rows.Next() {
		r, err := scanRoutineRun(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, r)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate routine runs: %w", err)
	}
	return out, nil
}

// AddRoutineOutbox appends a pending_approval item to routine_outbox.
func (v *Vault) AddRoutineOutbox(ctx context.Context, routineID, actionKind, payloadJSON, decideID string) (RoutineOutboxItem, error) {
	if routineID == "" || actionKind == "" || payloadJSON == "" {
		return RoutineOutboxItem{}, fmt.Errorf("routine_id, action_kind, and payload_json are required")
	}
	if !json.Valid([]byte(payloadJSON)) {
		return RoutineOutboxItem{}, fmt.Errorf("payload_json must be valid JSON")
	}
	id, err := newID("out")
	if err != nil {
		return RoutineOutboxItem{}, err
	}
	now := v.now().UTC()
	nowUnix := now.Unix()

	if _, err := v.db.ExecContext(ctx,
		`INSERT INTO routine_outbox (id, routine_id, action_kind, payload_json, status, decide_id, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?)`,
		id, routineID, actionKind, payloadJSON, OutboxPendingApproval, decideID, nowUnix,
	); err != nil {
		return RoutineOutboxItem{}, fmt.Errorf("add routine outbox: %w", err)
	}

	return RoutineOutboxItem{
		ID:          id,
		RoutineID:   routineID,
		ActionKind:  actionKind,
		PayloadJSON: payloadJSON,
		Status:      OutboxPendingApproval,
		DecideID:    decideID,
		CreatedAt:   now,
	}, nil
}

// ListRoutineOutbox returns matching routine_outbox rows, oldest first.
func (v *Vault) ListRoutineOutbox(ctx context.Context, filter RoutineOutboxFilter) ([]RoutineOutboxItem, error) {
	limit := filter.Limit
	if limit <= 0 {
		limit = -1
	}
	rows, err := v.db.QueryContext(ctx,
		`SELECT id, routine_id, action_kind, payload_json, status, decide_id, created_at, executed_at
		FROM routine_outbox
		WHERE (?1 = '' OR routine_id = ?1)
		  AND (?2 = '' OR status = ?2)
		ORDER BY created_at ASC, id ASC
		LIMIT ?3`,
		filter.RoutineID, filter.Status, limit,
	)
	if err != nil {
		return nil, fmt.Errorf("list routine outbox: %w", err)
	}
	defer rows.Close()

	out := []RoutineOutboxItem{}
	for rows.Next() {
		item, err := scanRoutineOutbox(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, item)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate routine outbox: %w", err)
	}
	return out, nil
}

// UpdateRoutineOutbox updates status and optionally decideID and executedAt for
// an outbox item.
func (v *Vault) UpdateRoutineOutbox(ctx context.Context, id, status, decideID string, executedAt *time.Time) error {
	if id == "" {
		return fmt.Errorf("outbox id is required")
	}
	if !isValidOutboxStatus(status) {
		return fmt.Errorf("invalid routine outbox status %q", status)
	}
	var execUnix sql.NullInt64
	if executedAt != nil {
		execUnix = sql.NullInt64{Int64: executedAt.UTC().Unix(), Valid: true}
	} else if status == OutboxExecuted {
		execUnix = sql.NullInt64{Int64: v.now().UTC().Unix(), Valid: true}
	}

	res, err := v.db.ExecContext(ctx,
		`UPDATE routine_outbox
		SET status = ?,
		    decide_id = COALESCE(NULLIF(?, ''), decide_id),
		    executed_at = COALESCE(?, executed_at)
		WHERE id = ?`,
		status, decideID, execUnix, id,
	)
	if err != nil {
		return fmt.Errorf("update routine outbox %s: %w", id, err)
	}
	affected, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("update routine outbox %s rows affected: %w", id, err)
	}
	if affected == 0 {
		return fmt.Errorf("%w: routine outbox %q", ErrNotFound, id)
	}
	return nil
}

func scanRoutineRun(rows *sql.Rows) (RoutineRun, error) {
	var r RoutineRun
	var startedUnix int64
	var finishedUnix, exitCode sql.NullInt64
	if err := rows.Scan(
		&r.ID, &r.RoutineID, &r.IdempotencyKey, &r.Trigger,
		&startedUnix, &finishedUnix, &r.Status, &exitCode, &r.MetricsJSON, &r.ErrorSummary,
	); err != nil {
		return RoutineRun{}, fmt.Errorf("scan routine run: %w", err)
	}
	r.StartedAt = time.Unix(startedUnix, 0).UTC()
	if finishedUnix.Valid {
		t := time.Unix(finishedUnix.Int64, 0).UTC()
		r.FinishedAt = &t
	}
	if exitCode.Valid {
		c := int(exitCode.Int64)
		r.ExitCode = &c
	}
	return r, nil
}

func scanRoutineOutbox(rows *sql.Rows) (RoutineOutboxItem, error) {
	var item RoutineOutboxItem
	var createdUnix int64
	var execUnix sql.NullInt64
	if err := rows.Scan(
		&item.ID, &item.RoutineID, &item.ActionKind, &item.PayloadJSON,
		&item.Status, &item.DecideID, &createdUnix, &execUnix,
	); err != nil {
		return RoutineOutboxItem{}, fmt.Errorf("scan routine outbox: %w", err)
	}
	item.CreatedAt = time.Unix(createdUnix, 0).UTC()
	if execUnix.Valid {
		t := time.Unix(execUnix.Int64, 0).UTC()
		item.ExecutedAt = &t
	}
	return item, nil
}

func isValidRoutineRunFinishStatus(s string) bool {
	switch s {
	case RoutineRunOK, RoutineRunFailed, RoutineRunSkipped:
		return true
	default:
		return false
	}
}

func isValidOutboxStatus(s string) bool {
	switch s {
	case OutboxPendingApproval, OutboxApproved, OutboxExecuted, OutboxRejected, OutboxFailed:
		return true
	default:
		return false
	}
}

func isUniqueConstraintError(err error) bool {
	return err != nil && strings.Contains(err.Error(), "UNIQUE constraint failed")
}
