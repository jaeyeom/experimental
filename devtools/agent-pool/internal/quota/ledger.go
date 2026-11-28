package quota

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/chorong-sh/chorong/pkg/sqlitedb"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	recentRunsLimit = 20
	ledgerSchemaSQL = `
CREATE TABLE IF NOT EXISTS job_runs (
	id                    INTEGER PRIMARY KEY AUTOINCREMENT,
	job_id                TEXT    NOT NULL,
	repo                  TEXT    NOT NULL,
	skill                 TEXT    NOT NULL,
	status                TEXT    NOT NULL,
	provider              TEXT    NOT NULL,
	model                 TEXT    NOT NULL,
	input_tokens          INTEGER NOT NULL DEFAULT 0,
	cache_read_tokens     INTEGER NOT NULL DEFAULT 0,
	cache_creation_tokens INTEGER NOT NULL DEFAULT 0,
	output_tokens         INTEGER NOT NULL DEFAULT 0,
	effective_units       INTEGER NOT NULL DEFAULT 0,
	turns_used            INTEGER NOT NULL DEFAULT 0,
	duration_ms           INTEGER NOT NULL DEFAULT 0,
	pre_five_hour_pct     REAL,
	post_five_hour_pct    REAL,
	pre_seven_day_pct     REAL,
	post_seven_day_pct    REAL,
	finished_at           INTEGER NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_job_runs_finished_at ON job_runs (finished_at DESC);
`
)

// RunRecord represents one completed job execution stored in the local
// consumer donation audit ledger.
type RunRecord struct {
	JobID               string             `json:"jobId"`
	Repo                string             `json:"repo"`
	Skill               string             `json:"skill"`
	Status              protocol.JobStatus `json:"status"`
	Provider            string             `json:"provider"`
	Model               string             `json:"model"`
	InputTokens         int64              `json:"inputTokens"`
	CacheReadTokens     int64              `json:"cacheReadTokens"`
	CacheCreationTokens int64              `json:"cacheCreationTokens"`
	OutputTokens        int64              `json:"outputTokens"`
	EffectiveUnits      int64              `json:"effectiveUnits"`
	TurnsUsed           int                `json:"turnsUsed"`
	DurationMs          int64              `json:"durationMs"`
	PreFiveHourPct      *float64           `json:"preFiveHourPct,omitempty"`
	PostFiveHourPct     *float64           `json:"postFiveHourPct,omitempty"`
	PreSevenDayPct      *float64           `json:"preSevenDayPct,omitempty"`
	PostSevenDayPct     *float64           `json:"postSevenDayPct,omitempty"`
	FinishedAt          time.Time          `json:"finishedAt"`
}

// LedgerStats summarizes recorded job runs since a cutoff timestamp.
type LedgerStats struct {
	TotalJobs           int                        `json:"totalJobs"`
	JobsByStatus        map[protocol.JobStatus]int `json:"jobsByStatus"`
	InputTokens         int64                      `json:"inputTokens"`
	CacheReadTokens     int64                      `json:"cacheReadTokens"`
	CacheCreationTokens int64                      `json:"cacheCreationTokens"`
	OutputTokens        int64                      `json:"outputTokens"`
	TotalTokens         int64                      `json:"totalTokens"`
	EffectiveUnits      int64                      `json:"effectiveUnits"`
	TotalDurationMs     int64                      `json:"totalDurationMs"`
	RecentRuns          []RunRecord                `json:"recentRuns"`
}

// Ledger is the local SQLite audit log of donated job runs.
type Ledger struct {
	db *sql.DB
}

// OpenLedger opens or initializes the local usage ledger SQLite database at
// path.
func OpenLedger(path string) (*Ledger, error) {
	db, err := sqlitedb.Open(path, "_txlock=immediate")
	if err != nil {
		return nil, fmt.Errorf("open usage ledger: %w", err)
	}
	db.SetMaxOpenConns(1)

	if _, err := db.Exec(ledgerSchemaSQL); err != nil {
		_ = db.Close()
		return nil, fmt.Errorf("apply usage ledger schema: %w", err)
	}
	return &Ledger{db: db}, nil
}

// Close closes the underlying SQLite database.
func (l *Ledger) Close() error {
	if err := l.db.Close(); err != nil {
		return fmt.Errorf("close usage ledger: %w", err)
	}
	return nil
}

// RecordRun inserts a completed job run into the ledger.
func (l *Ledger) RecordRun(ctx context.Context, run RunRecord) error {
	finishedAt := run.FinishedAt
	if finishedAt.IsZero() {
		finishedAt = time.Now().UTC()
	}

	const query = `
		INSERT INTO job_runs (
			job_id, repo, skill, status, provider, model,
			input_tokens, cache_read_tokens, cache_creation_tokens, output_tokens,
			effective_units, turns_used, duration_ms,
			pre_five_hour_pct, post_five_hour_pct, pre_seven_day_pct, post_seven_day_pct,
			finished_at
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`

	_, err := l.db.ExecContext(
		ctx, query,
		run.JobID, run.Repo, run.Skill, string(run.Status), run.Provider, run.Model,
		run.InputTokens, run.CacheReadTokens, run.CacheCreationTokens, run.OutputTokens,
		run.EffectiveUnits, run.TurnsUsed, run.DurationMs,
		toNullFloat64(run.PreFiveHourPct), toNullFloat64(run.PostFiveHourPct),
		toNullFloat64(run.PreSevenDayPct), toNullFloat64(run.PostSevenDayPct),
		finishedAt.UTC().UnixNano(),
	)
	if err != nil {
		return fmt.Errorf("record job run %s: %w", run.JobID, err)
	}
	return nil
}

// Stats aggregates job counts, token totals, effective units, duration, and up
// to the 20 most recent runs finished at or after since. A zero since includes
// all recorded runs.
func (l *Ledger) Stats(ctx context.Context, since time.Time) (LedgerStats, error) {
	sinceNs := int64(0)
	if !since.IsZero() {
		sinceNs = since.UTC().UnixNano()
	}

	stats := LedgerStats{
		JobsByStatus: map[protocol.JobStatus]int{},
	}

	if err := l.queryTotals(ctx, sinceNs, &stats); err != nil {
		return LedgerStats{}, err
	}
	if err := l.queryStatusCounts(ctx, sinceNs, &stats); err != nil {
		return LedgerStats{}, err
	}
	recent, err := l.queryRecentRuns(ctx, sinceNs)
	if err != nil {
		return LedgerStats{}, err
	}
	stats.RecentRuns = recent
	return stats, nil
}

func (l *Ledger) queryTotals(ctx context.Context, sinceNs int64, stats *LedgerStats) error {
	const query = `
		SELECT
			COUNT(*),
			COALESCE(SUM(input_tokens), 0),
			COALESCE(SUM(cache_read_tokens), 0),
			COALESCE(SUM(cache_creation_tokens), 0),
			COALESCE(SUM(output_tokens), 0),
			COALESCE(SUM(effective_units), 0),
			COALESCE(SUM(duration_ms), 0)
		FROM job_runs
		WHERE finished_at >= ?`

	err := l.db.QueryRowContext(ctx, query, sinceNs).Scan(
		&stats.TotalJobs,
		&stats.InputTokens,
		&stats.CacheReadTokens,
		&stats.CacheCreationTokens,
		&stats.OutputTokens,
		&stats.EffectiveUnits,
		&stats.TotalDurationMs,
	)
	if err != nil {
		return fmt.Errorf("query ledger totals: %w", err)
	}
	stats.TotalTokens = stats.InputTokens + stats.CacheReadTokens + stats.CacheCreationTokens + stats.OutputTokens
	return nil
}

func (l *Ledger) queryStatusCounts(ctx context.Context, sinceNs int64, stats *LedgerStats) error {
	const query = `
		SELECT status, COUNT(*)
		FROM job_runs
		WHERE finished_at >= ?
		GROUP BY status`

	rows, err := l.db.QueryContext(ctx, query, sinceNs)
	if err != nil {
		return fmt.Errorf("query ledger status counts: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var (
			status string
			count  int
		)
		if err := rows.Scan(&status, &count); err != nil {
			return fmt.Errorf("scan ledger status count: %w", err)
		}
		stats.JobsByStatus[protocol.JobStatus(status)] = count
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("iterate ledger status counts: %w", err)
	}
	return nil
}

func (l *Ledger) queryRecentRuns(ctx context.Context, sinceNs int64) ([]RunRecord, error) {
	const query = `
		SELECT
			job_id, repo, skill, status, provider, model,
			input_tokens, cache_read_tokens, cache_creation_tokens, output_tokens,
			effective_units, turns_used, duration_ms,
			pre_five_hour_pct, post_five_hour_pct, pre_seven_day_pct, post_seven_day_pct,
			finished_at
		FROM job_runs
		WHERE finished_at >= ?
		ORDER BY finished_at DESC, id DESC
		LIMIT ?`

	rows, err := l.db.QueryContext(ctx, query, sinceNs, recentRunsLimit)
	if err != nil {
		return nil, fmt.Errorf("query recent ledger runs: %w", err)
	}
	defer rows.Close()

	var runs []RunRecord
	for rows.Next() {
		rec, err := scanRunRecord(rows)
		if err != nil {
			return nil, err
		}
		runs = append(runs, rec)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate recent ledger runs: %w", err)
	}
	return runs, nil
}

func scanRunRecord(rows *sql.Rows) (RunRecord, error) {
	var (
		rec        RunRecord
		statusStr  string
		preFive    sql.NullFloat64
		postFive   sql.NullFloat64
		preSeven   sql.NullFloat64
		postSeven  sql.NullFloat64
		finishedNs int64
	)
	err := rows.Scan(
		&rec.JobID, &rec.Repo, &rec.Skill, &statusStr, &rec.Provider, &rec.Model,
		&rec.InputTokens, &rec.CacheReadTokens, &rec.CacheCreationTokens, &rec.OutputTokens,
		&rec.EffectiveUnits, &rec.TurnsUsed, &rec.DurationMs,
		&preFive, &postFive, &preSeven, &postSeven,
		&finishedNs,
	)
	if err != nil {
		return RunRecord{}, fmt.Errorf("scan ledger run row: %w", err)
	}
	rec.Status = protocol.JobStatus(statusStr)
	rec.PreFiveHourPct = fromNullFloat64(preFive)
	rec.PostFiveHourPct = fromNullFloat64(postFive)
	rec.PreSevenDayPct = fromNullFloat64(preSeven)
	rec.PostSevenDayPct = fromNullFloat64(postSeven)
	rec.FinishedAt = time.Unix(0, finishedNs).UTC()
	return rec, nil
}

func toNullFloat64(v *float64) sql.NullFloat64 {
	if v == nil {
		return sql.NullFloat64{}
	}
	return sql.NullFloat64{Float64: *v, Valid: true}
}

func fromNullFloat64(v sql.NullFloat64) *float64 {
	if !v.Valid {
		return nil
	}
	f := v.Float64
	return &f
}
