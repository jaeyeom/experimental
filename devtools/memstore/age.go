package memstore

import (
	"context"
	"crypto/sha256"
	"database/sql"
	"encoding/hex"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	"golang.org/x/sys/unix"
)

// AgeReporter is the reporter recorded on the summary events Age writes.
const AgeReporter = "memstore age"

// ageActor is the actor recorded on the revisions Age writes.
const ageActor = "memstore age"

// ErrAgeLocked means another age run holds the vault's lock.
var ErrAgeLocked = errors.New("another memstore age run is in progress")

var errDryRun = errors.New("dry run")

// AgeOptions controls Vault.Age.
type AgeOptions struct {
	// RetentionDays is the age, in days, past which rows are aged.
	RetentionDays int
	// Drop deletes old events instead of compacting them.
	Drop bool
	// DryRun computes the changes and makes none.
	DryRun bool
}

// AgeResult counts what Age changed, or would change on a dry run.
type AgeResult struct {
	ExpiredFacts      int `json:"expiredFacts"`
	CompactedEvents   int `json:"compactedEvents"`
	Summaries         int `json:"summaries"`
	DroppedEvents     int `json:"droppedEvents"`
	DeletedRevisions  int `json:"deletedRevisions"`
	DeletedAccessRows int `json:"deletedAccessRows"`
}

// Changed reports whether r counts any change.
func (r AgeResult) Changed() bool { return r != AgeResult{} }

func ageLockPath(vc VaultConfig) string { return vc.Path + ".age.lock" }

// Age applies retention using the vault's clock. Active facts whose
// valid_until has passed are marked expired and leave the search index.
// Events dated before the cutoff are rolled up into one summary event per
// (month, entity, compartment, sensitivity) that keeps the newest source_ref
// and the originals (linked via compacted_into_id), or are deleted with Drop.
// Revisions and access-log rows older than the cutoff are deleted. Every
// fact expiry and event compaction or drop is recorded as a revision; Age
// writes no access-log row, so a second run changes nothing. Age holds a
// lock file next to the vault and returns ErrAgeLocked when another run
// holds it.
func (v *Vault) Age(ctx context.Context, opts AgeOptions) (AgeResult, error) {
	var r AgeResult
	if opts.RetentionDays <= 0 {
		return r, errors.New("retention days must be positive")
	}
	unlock, err := lockFile(ageLockPath(v.config))
	if err != nil {
		return r, err
	}
	defer unlock()

	now := v.now().UTC()
	cutoff := now.AddDate(0, 0, -opts.RetentionDays)
	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return r, fmt.Errorf("age: begin: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit
	if err := ageTx(ctx, tx, now, cutoff, opts, &r); err != nil {
		return AgeResult{}, err
	}
	if opts.DryRun {
		return r, nil
	}
	if err := tx.Commit(); err != nil {
		return AgeResult{}, fmt.Errorf("age: commit: %w", err)
	}
	return r, nil
}

func ageTx(ctx context.Context, tx *sql.Tx, now, cutoff time.Time, opts AgeOptions, r *AgeResult) error {
	var err error
	if r.DeletedRevisions, err = execCount(ctx, tx, `DELETE FROM revisions WHERE created_at < ?`, cutoff.Unix()); err != nil {
		return fmt.Errorf("age: revisions: %w", err)
	}
	if r.DeletedAccessRows, err = execCount(ctx, tx, `DELETE FROM access_log WHERE ts < ?`, cutoff.Unix()); err != nil {
		return fmt.Errorf("age: access log: %w", err)
	}
	if r.ExpiredFacts, err = expireFacts(ctx, tx, now); err != nil {
		return err
	}
	cutoffDate := cutoff.Format("2006-01-02")
	if opts.Drop {
		r.DroppedEvents, err = dropEvents(ctx, tx, now, cutoffDate)
		return err
	}
	r.CompactedEvents, r.Summaries, err = compactEvents(ctx, tx, now, cutoffDate)
	return err
}

func execCount(ctx context.Context, tx *sql.Tx, q string, args ...any) (int, error) {
	res, err := tx.ExecContext(ctx, q, args...)
	if err != nil {
		return 0, fmt.Errorf("exec: %w", err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("rows affected: %w", err)
	}
	return int(n), nil
}

// queryStrings runs q and returns the first column of every row.
func queryStrings(ctx context.Context, tx *sql.Tx, q string, args ...any) ([]string, error) {
	rows, err := tx.QueryContext(ctx, q, args...)
	if err != nil {
		return nil, fmt.Errorf("age: %w", err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var s string
		if err := rows.Scan(&s); err != nil {
			return nil, fmt.Errorf("age: %w", err)
		}
		out = append(out, s)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("age: %w", err)
	}
	return out, nil
}

func expireFacts(ctx context.Context, tx *sql.Tx, now time.Time) (int, error) {
	ids, err := queryStrings(ctx, tx, `SELECT id FROM facts
		WHERE valid_until IS NOT NULL AND valid_until < ? AND expired_at IS NULL ORDER BY id`, now.Unix())
	if err != nil {
		return 0, err
	}
	for _, id := range ids {
		if _, err := tx.ExecContext(ctx, `UPDATE facts SET expired_at = ?1, updated_at = ?1 WHERE id = ?2`, now.Unix(), id); err != nil {
			return 0, fmt.Errorf("age: expire fact: %w", err)
		}
		if err := deleteFTSDocTx(ctx, tx, id, DocKindFact); err != nil {
			return 0, err
		}
		diff := map[string]any{"expired_at": now.Unix()}
		if err := recordRevisionAt(ctx, tx, "facts", id, "update", diff, ageActor, "valid_until passed", now); err != nil {
			return 0, err
		}
	}
	return len(ids), nil
}

func dropEvents(ctx context.Context, tx *sql.Tx, now time.Time, cutoffDate string) (int, error) {
	ids, err := queryStrings(ctx, tx, `SELECT id FROM episodic_events WHERE event_date < ? ORDER BY id`, cutoffDate)
	if err != nil {
		return 0, err
	}
	for _, id := range ids {
		if err := deleteFTSDocTx(ctx, tx, id, DocKindEvent); err != nil {
			return 0, err
		}
		if err := recordRevisionAt(ctx, tx, "episodic_events", id, "delete", map[string]any{"dropped": true}, ageActor, "retention drop", now); err != nil {
			return 0, err
		}
	}
	// One statement, so originals and the summaries pointing at them go together.
	if _, err := tx.ExecContext(ctx, `DELETE FROM episodic_events WHERE event_date < ?`, cutoffDate); err != nil {
		return 0, fmt.Errorf("age: drop events: %w", err)
	}
	return len(ids), nil
}

type ageEvent struct {
	id, date, entity, compartment, sensitivity string
	channel, sourceRef, summary, reporter      string
	occurredAt                                 int64
}

func (e ageEvent) isSummary() bool { return e.reporter == AgeReporter }

// groupKey is the (month, entity, compartment, sensitivity) rollup key.
func (e ageEvent) groupKey() string {
	return strings.Join([]string{e.date[:7], e.entity, e.compartment, e.sensitivity}, "\x00")
}

// weight is how many original events e stands for.
func (e ageEvent) weight() int {
	var n int
	if e.isSummary() {
		if _, err := fmt.Sscanf(e.summary, "%d events", &n); err == nil && n > 0 {
			return n
		}
	}
	return 1
}

func loadOldEvents(ctx context.Context, tx *sql.Tx, cutoffDate string) ([][]ageEvent, error) {
	rows, err := tx.QueryContext(ctx, `SELECT id, event_date, occurred_at, IFNULL(entity_id, ''), compartment, sensitivity,
			channel, source_ref, summary, reporter
		FROM episodic_events WHERE event_date < ? AND compacted_into_id IS NULL
		ORDER BY substr(event_date, 1, 7), IFNULL(entity_id, ''), compartment, sensitivity, event_date, occurred_at, id`, cutoffDate)
	if err != nil {
		return nil, fmt.Errorf("age: %w", err)
	}
	defer rows.Close()
	var groups [][]ageEvent
	for rows.Next() {
		var e ageEvent
		if err := rows.Scan(&e.id, &e.date, &e.occurredAt, &e.entity, &e.compartment, &e.sensitivity,
			&e.channel, &e.sourceRef, &e.summary, &e.reporter); err != nil {
			return nil, fmt.Errorf("age: %w", err)
		}
		if n := len(groups); n > 0 && groups[n-1][0].groupKey() == e.groupKey() {
			groups[n-1] = append(groups[n-1], e)
			continue
		}
		groups = append(groups, []ageEvent{e})
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("age: %w", err)
	}
	return groups, nil
}

func compactEvents(ctx context.Context, tx *sql.Tx, now time.Time, cutoffDate string) (compacted, summaries int, err error) {
	groups, err := loadOldEvents(ctx, tx, cutoffDate)
	if err != nil {
		return 0, 0, err
	}
	for _, g := range groups {
		if len(g) == 1 && g[0].isSummary() {
			continue
		}
		if err := compactGroup(ctx, tx, now, g); err != nil {
			return 0, 0, err
		}
		compacted += len(g)
		summaries++
	}
	return compacted, summaries, nil
}

func compactGroup(ctx context.Context, tx *sql.Tx, now time.Time, g []ageEvent) error {
	total := 0
	var channels, ids []string
	for _, e := range g {
		total += e.weight()
		for c := range strings.SplitSeq(e.channel, ",") {
			if c != "" {
				channels = append(channels, c)
			}
		}
		ids = append(ids, e.id)
	}
	slices.Sort(channels)
	channels = slices.Compact(channels)
	newest := g[len(g)-1]
	sum := sha256.Sum256([]byte(g[0].groupKey() + "\x00" + strings.Join(ids, "\x00")))
	id := "evt_age_" + hex.EncodeToString(sum[:12])
	text := fmt.Sprintf("%d events, channels: %s", total, strings.Join(channels, ", "))
	if _, err := tx.ExecContext(ctx, `INSERT INTO episodic_events
		(id, event_date, occurred_at, entity_id, compartment, sensitivity, channel, source_ref, summary, reporter, created_at)
		VALUES (?, ?, ?, NULLIF(?, ''), ?, ?, ?, ?, ?, ?, ?)`,
		id, newest.date, newest.occurredAt, newest.entity, newest.compartment, newest.sensitivity,
		strings.Join(channels, ","), newest.sourceRef, text, AgeReporter, now.Unix()); err != nil {
		return fmt.Errorf("age: insert summary: %w", err)
	}
	for _, e := range g {
		if e.isSummary() {
			// Keep the chain one level deep: re-point what the old summary rolled up.
			if _, err := tx.ExecContext(ctx, `UPDATE episodic_events SET compacted_into_id = ? WHERE compacted_into_id = ?`, id, e.id); err != nil {
				return fmt.Errorf("age: repoint: %w", err)
			}
		}
		if _, err := tx.ExecContext(ctx, `UPDATE episodic_events SET compacted_into_id = ? WHERE id = ?`, id, e.id); err != nil {
			return fmt.Errorf("age: mark compacted: %w", err)
		}
		if err := deleteFTSDocTx(ctx, tx, e.id, DocKindEvent); err != nil {
			return err
		}
	}
	if err := upsertFTSDocTx(ctx, tx, id, DocKindEvent, newest.entity, newest.compartment, Sensitivity(newest.sensitivity), text, ""); err != nil {
		return err
	}
	diff := map[string]any{"compacted": ids, "count": total}
	return recordRevisionAt(ctx, tx, "episodic_events", id, "insert", diff, ageActor, "retention compaction", now)
}

// lockFile takes a non-blocking exclusive flock on path and returns ErrAgeLocked
// when another process holds it.
func lockFile(path string) (unlock func(), err error) {
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, fmt.Errorf("age lock: %w", err)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, unix.EWOULDBLOCK) {
			return nil, ErrAgeLocked
		}
		return nil, fmt.Errorf("age lock: %w", err)
	}
	return func() { f.Close() }, nil
}
