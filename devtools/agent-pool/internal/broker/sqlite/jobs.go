package sqlite

import (
	"context"
	"crypto/subtle"
	"database/sql"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const jobSelectColumns = `
	job_id, requester_id, idempotency_key, repo, default_branch, catalog_sha,
	skill, args_json, status, attempts, max_attempts, worker_id, worker_alias,
	lease_token, lease_expires_at, cancel_requested, deliverable_json,
	usage_json, progress_json, error_code, error_message, created_at, updated_at`

type rowScanner interface {
	Scan(dest ...any) error
}

// CountPendingJobs counts QUEUED jobs across all repos, or restricted to repos
// when non-empty.
func (s *Store) CountPendingJobs(ctx context.Context, repos []string) (int, error) {
	var count int
	if len(repos) == 0 {
		const query = `SELECT COUNT(*) FROM jobs WHERE status = ?`
		if err := s.db.QueryRowContext(ctx, query, string(protocol.JobStatusQueued)).Scan(&count); err != nil {
			return 0, fmt.Errorf("count pending jobs: %w", err)
		}
		return count, nil
	}

	reposJSON, err := marshalStringSlice(repos)
	if err != nil {
		return 0, fmt.Errorf("marshal pending job repos: %w", err)
	}
	const query = `SELECT COUNT(*) FROM jobs WHERE status = ? AND repo IN (SELECT value FROM json_each(?))`
	if err := s.db.QueryRowContext(ctx, query, string(protocol.JobStatusQueued), reposJSON).Scan(&count); err != nil {
		return 0, fmt.Errorf("count pending jobs for repos: %w", err)
	}
	return count, nil
}

// EnqueueJob inserts a new QUEUED job, deduplicating by (requesterID,
// idempotencyKey) and enforcing maxConcurrentPerRequester on non-terminal jobs.
func (s *Store) EnqueueJob(ctx context.Context, job broker.JobRecord, maxConcurrentPerRequester int) (broker.JobRecord, bool, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return broker.JobRecord{}, false, fmt.Errorf("begin enqueue tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	if job.IdempotencyKey != "" {
		existing, found, err := findByIdempotencyKey(ctx, tx, job.RequesterID, job.IdempotencyKey)
		if err != nil {
			return broker.JobRecord{}, false, err
		}
		if found {
			return existing, true, nil
		}
	}

	if err := checkRequesterConcurrency(ctx, tx, job.RequesterID, maxConcurrentPerRequester); err != nil {
		return broker.JobRecord{}, false, err
	}

	prepared, err := prepareNewJob(job)
	if err != nil {
		return broker.JobRecord{}, false, err
	}

	if err := insertJob(ctx, tx, prepared); err != nil {
		return broker.JobRecord{}, false, err
	}
	if err := tx.Commit(); err != nil {
		return broker.JobRecord{}, false, fmt.Errorf("commit enqueue tx: %w", err)
	}
	return prepared, false, nil
}

// GetJob retrieves a job by ID.
func (s *Store) GetJob(ctx context.Context, jobID string) (broker.JobRecord, error) {
	const query = `SELECT ` + jobSelectColumns + ` FROM jobs WHERE job_id = ?`
	rec, err := scanJob(s.db.QueryRowContext(ctx, query, jobID))
	if errors.Is(err, sql.ErrNoRows) {
		return broker.JobRecord{}, fmt.Errorf("job %s: %w", jobID, broker.ErrNotFound)
	}
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("get job %s: %w", jobID, err)
	}
	return rec, nil
}

// ClaimLease atomically claims the oldest QUEUED job eligible for the worker's
// supported repositories and optional per-repo skill restrictions.
func (s *Store) ClaimLease(
	ctx context.Context,
	workerID, workerAlias string,
	supportedRepos []string,
	supportedSkills map[string][]string,
	leaseTTL time.Duration,
	now time.Time,
) (broker.JobRecord, error) {
	if len(supportedRepos) == 0 {
		return broker.JobRecord{}, fmt.Errorf("no supported repos: %w", broker.ErrNoJobAvailable)
	}

	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("begin claim lease tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	candidate, err := selectNextQueuedJob(ctx, tx, supportedRepos, supportedSkills)
	if err != nil {
		return broker.JobRecord{}, err
	}

	rawLease, err := randomBytes(tokenByteLen)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("generate lease token: %w", err)
	}
	leaseToken := base64.RawURLEncoding.EncodeToString(rawLease)
	nowUTC := now.UTC()
	expiresAt := nowUTC.Add(leaseTTL)

	const updateQuery = `
		UPDATE jobs
		SET status = ?,
		    attempts = attempts + 1,
		    worker_id = ?,
		    worker_alias = ?,
		    lease_token = ?,
		    lease_expires_at = ?,
		    updated_at = ?
		WHERE job_id = ? AND status = ?`
	res, err := tx.ExecContext(
		ctx, updateQuery,
		string(protocol.JobStatusLeased), workerID, workerAlias,
		leaseToken, expiresAt.UnixNano(), nowUTC.UnixNano(),
		candidate.JobID, string(protocol.JobStatusQueued),
	)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("update claimed job %s: %w", candidate.JobID, err)
	}
	affected, err := res.RowsAffected()
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("claimed job %s rows affected: %w", candidate.JobID, err)
	}
	if affected == 0 {
		return broker.JobRecord{}, fmt.Errorf("job %s already claimed: %w", candidate.JobID, broker.ErrNoJobAvailable)
	}

	if err := tx.Commit(); err != nil {
		return broker.JobRecord{}, fmt.Errorf("commit claim lease tx: %w", err)
	}

	candidate.Status = protocol.JobStatusLeased
	candidate.Attempts++
	candidate.WorkerID = workerID
	candidate.WorkerAlias = workerAlias
	candidate.LeaseToken = leaseToken
	candidate.LeaseExpiresAt = &expiresAt
	candidate.UpdatedAt = nowUTC
	return candidate, nil
}

// RenewLease verifies an active lease, transitions LEASED to RUNNING, extends
// expiry, persists progress, and returns whether cancellation was requested.
func (s *Store) RenewLease(
	ctx context.Context,
	jobID, leaseToken string,
	progress protocol.JobProgress,
	leaseTTL time.Duration,
	now time.Time,
) (time.Time, bool, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return time.Time{}, false, fmt.Errorf("begin renew lease tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	job, err := getJobTx(ctx, tx, jobID)
	if err != nil {
		return time.Time{}, false, err
	}
	if !tokensMatch(job.LeaseToken, leaseToken) {
		return time.Time{}, false, fmt.Errorf("renew job %s: %w", jobID, broker.ErrLeaseMismatch)
	}
	if job.Status != protocol.JobStatusLeased && job.Status != protocol.JobStatusRunning {
		return time.Time{}, false, fmt.Errorf("renew job %s in status %s: %w", jobID, job.Status, broker.ErrInvalidTransition)
	}
	nowUTC := now.UTC()
	if job.LeaseExpiresAt != nil && !nowUTC.Before(*job.LeaseExpiresAt) {
		return time.Time{}, false, fmt.Errorf("lease expired for job %s: %w", jobID, broker.ErrLeaseMismatch)
	}

	progressJSON, err := json.Marshal(progress)
	if err != nil {
		return time.Time{}, false, fmt.Errorf("marshal job progress: %w", err)
	}
	newExpiry := nowUTC.Add(leaseTTL)

	const updateQuery = `
		UPDATE jobs
		SET status = ?,
		    lease_expires_at = ?,
		    progress_json = ?,
		    updated_at = ?
		WHERE job_id = ?`
	if _, err := tx.ExecContext(
		ctx, updateQuery,
		string(protocol.JobStatusRunning), newExpiry.UnixNano(), progressJSON, nowUTC.UnixNano(), jobID,
	); err != nil {
		return time.Time{}, false, fmt.Errorf("update renewed lease for job %s: %w", jobID, err)
	}
	if err := tx.Commit(); err != nil {
		return time.Time{}, false, fmt.Errorf("commit renew lease tx: %w", err)
	}
	return newExpiry, job.CancelRequested, nil
}

// CompleteJob verifies the lease token and state transition, writes the
// terminal outcome, and clears the lease.
func (s *Store) CompleteJob(
	ctx context.Context,
	jobID, leaseToken string,
	status protocol.JobStatus,
	deliverable []byte,
	usage *protocol.UsageReport,
	errCode protocol.ErrorCode,
	errMsg string,
	now time.Time,
) (broker.JobRecord, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("begin complete job tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	job, err := getJobTx(ctx, tx, jobID)
	if err != nil {
		return broker.JobRecord{}, err
	}
	if !tokensMatch(job.LeaseToken, leaseToken) {
		return broker.JobRecord{}, fmt.Errorf("complete job %s: %w", jobID, broker.ErrLeaseMismatch)
	}
	if !status.IsTerminal() || !protocol.CanTransition(job.Status, status) {
		return broker.JobRecord{}, fmt.Errorf("transition %s -> %s: %w", job.Status, status, broker.ErrInvalidTransition)
	}

	usageJSON, err := marshalUsageReport(usage)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("marshal usage report: %w", err)
	}

	nowUTC := now.UTC()
	const updateQuery = `
		UPDATE jobs
		SET status = ?,
		    deliverable_json = ?,
		    usage_json = ?,
		    error_code = ?,
		    error_message = ?,
		    lease_token = '',
		    lease_expires_at = NULL,
		    updated_at = ?
		WHERE job_id = ?`
	if _, err := tx.ExecContext(
		ctx, updateQuery,
		string(status), deliverable, usageJSON, string(errCode), errMsg, nowUTC.UnixNano(), jobID,
	); err != nil {
		return broker.JobRecord{}, fmt.Errorf("complete job %s: %w", jobID, err)
	}

	updated, err := getJobTx(ctx, tx, jobID)
	if err != nil {
		return broker.JobRecord{}, err
	}
	if err := tx.Commit(); err != nil {
		return broker.JobRecord{}, fmt.Errorf("commit complete job tx: %w", err)
	}
	return updated, nil
}

// RequestCancel cancels a QUEUED job immediately or flags a LEASED/RUNNING job
// for cooperative cancellation by its worker.
func (s *Store) RequestCancel(ctx context.Context, jobID, requesterID string, now time.Time) (broker.JobRecord, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("begin request cancel tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	job, err := getJobTx(ctx, tx, jobID)
	if err != nil {
		return broker.JobRecord{}, err
	}
	if job.RequesterID != requesterID {
		return broker.JobRecord{}, fmt.Errorf("cancel job %s: %w", jobID, broker.ErrUnauthorized)
	}

	nowUTC := now.UTC()
	switch job.Status {
	case protocol.JobStatusQueued:
		const q = `UPDATE jobs SET status = ?, cancel_requested = 1, updated_at = ? WHERE job_id = ?`
		if _, err := tx.ExecContext(ctx, q, string(protocol.JobStatusCanceled), nowUTC.UnixNano(), jobID); err != nil {
			return broker.JobRecord{}, fmt.Errorf("cancel queued job %s: %w", jobID, err)
		}
	case protocol.JobStatusLeased, protocol.JobStatusRunning:
		const q = `UPDATE jobs SET cancel_requested = 1, updated_at = ? WHERE job_id = ?`
		if _, err := tx.ExecContext(ctx, q, nowUTC.UnixNano(), jobID); err != nil {
			return broker.JobRecord{}, fmt.Errorf("flag cancel for job %s: %w", jobID, err)
		}
	default:
		return broker.JobRecord{}, fmt.Errorf("cancel job %s in status %s: %w", jobID, job.Status, broker.ErrInvalidTransition)
	}

	updated, err := getJobTx(ctx, tx, jobID)
	if err != nil {
		return broker.JobRecord{}, err
	}
	if err := tx.Commit(); err != nil {
		return broker.JobRecord{}, fmt.Errorf("commit request cancel tx: %w", err)
	}
	return updated, nil
}

// SweepExpiredLeases requeues expired leases with remaining attempts and times
// out expired leases that have exhausted max_attempts.
func (s *Store) SweepExpiredLeases(ctx context.Context, now time.Time) (int, int, error) {
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return 0, 0, fmt.Errorf("begin sweep tx: %w", err)
	}
	defer func() { _ = tx.Rollback() }()

	nowNs := now.UTC().UnixNano()
	const requeueQuery = `
		UPDATE jobs
		SET status = ?,
		    worker_id = '',
		    worker_alias = '',
		    lease_token = '',
		    lease_expires_at = NULL,
		    updated_at = ?
		WHERE status IN (?, ?)
		  AND lease_expires_at IS NOT NULL
		  AND lease_expires_at <= ?
		  AND attempts < max_attempts`
	requeueRes, err := tx.ExecContext(
		ctx, requeueQuery,
		string(protocol.JobStatusQueued), nowNs,
		string(protocol.JobStatusLeased), string(protocol.JobStatusRunning), nowNs,
	)
	if err != nil {
		return 0, 0, fmt.Errorf("requeue expired leases: %w", err)
	}
	requeued, err := requeueRes.RowsAffected()
	if err != nil {
		return 0, 0, fmt.Errorf("requeue rows affected: %w", err)
	}

	const timeoutQuery = `
		UPDATE jobs
		SET status = ?,
		    lease_token = '',
		    lease_expires_at = NULL,
		    updated_at = ?
		WHERE status IN (?, ?)
		  AND lease_expires_at IS NOT NULL
		  AND lease_expires_at <= ?
		  AND attempts >= max_attempts`
	timeoutRes, err := tx.ExecContext(
		ctx, timeoutQuery,
		string(protocol.JobStatusTimedOut), nowNs,
		string(protocol.JobStatusLeased), string(protocol.JobStatusRunning), nowNs,
	)
	if err != nil {
		return 0, 0, fmt.Errorf("timeout expired leases: %w", err)
	}
	timedOut, err := timeoutRes.RowsAffected()
	if err != nil {
		return 0, 0, fmt.Errorf("timeout rows affected: %w", err)
	}

	if err := tx.Commit(); err != nil {
		return 0, 0, fmt.Errorf("commit sweep tx: %w", err)
	}
	return int(requeued), int(timedOut), nil
}

func findByIdempotencyKey(ctx context.Context, tx *sql.Tx, requesterID, key string) (broker.JobRecord, bool, error) {
	const query = `SELECT ` + jobSelectColumns + ` FROM jobs WHERE requester_id = ? AND idempotency_key = ?`
	rec, err := scanJob(tx.QueryRowContext(ctx, query, requesterID, key))
	if errors.Is(err, sql.ErrNoRows) {
		return broker.JobRecord{}, false, nil
	}
	if err != nil {
		return broker.JobRecord{}, false, fmt.Errorf("lookup idempotency key: %w", err)
	}
	return rec, true, nil
}

func checkRequesterConcurrency(ctx context.Context, tx *sql.Tx, requesterID string, limit int) error {
	if limit <= 0 {
		return nil
	}
	const query = `SELECT COUNT(*) FROM jobs WHERE requester_id = ? AND status IN (?, ?, ?)`
	var active int
	if err := tx.QueryRowContext(
		ctx, query, requesterID,
		string(protocol.JobStatusQueued), string(protocol.JobStatusLeased), string(protocol.JobStatusRunning),
	).Scan(&active); err != nil {
		return fmt.Errorf("count active jobs for requester %s: %w", requesterID, err)
	}
	if active >= limit {
		return fmt.Errorf("requester %s reached max concurrent jobs (%d): %w", requesterID, limit, broker.ErrRateLimited)
	}
	return nil
}

func prepareNewJob(job broker.JobRecord) (broker.JobRecord, error) {
	if job.JobID == "" {
		id, err := randomHexID()
		if err != nil {
			return broker.JobRecord{}, fmt.Errorf("generate job id: %w", err)
		}
		job.JobID = id
	}
	if job.Status == "" {
		job.Status = protocol.JobStatusQueued
	}
	if job.MaxAttempts <= 0 {
		job.MaxAttempts = 1
	}
	if job.CreatedAt.IsZero() {
		job.CreatedAt = time.Now().UTC()
	} else {
		job.CreatedAt = job.CreatedAt.UTC()
	}
	if job.UpdatedAt.IsZero() {
		job.UpdatedAt = job.CreatedAt
	} else {
		job.UpdatedAt = job.UpdatedAt.UTC()
	}
	return job, nil
}

func insertJob(ctx context.Context, tx *sql.Tx, job broker.JobRecord) error {
	const query = `
		INSERT INTO jobs (
			job_id, requester_id, idempotency_key, repo, default_branch, catalog_sha,
			skill, args_json, status, attempts, max_attempts, created_at, updated_at
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`
	if _, err := tx.ExecContext(
		ctx, query,
		job.JobID, job.RequesterID, job.IdempotencyKey, job.Repo, job.DefaultBranch, job.CatalogSha,
		job.Skill, job.ArgsJSON, string(job.Status), job.Attempts, job.MaxAttempts,
		job.CreatedAt.UnixNano(), job.UpdatedAt.UnixNano(),
	); err != nil {
		return fmt.Errorf("insert job %s: %w", job.JobID, err)
	}
	return nil
}

func selectNextQueuedJob(
	ctx context.Context,
	tx *sql.Tx,
	supportedRepos []string,
	supportedSkills map[string][]string,
) (broker.JobRecord, error) {
	reposJSON, err := marshalStringSlice(supportedRepos)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("marshal supported repos: %w", err)
	}
	const query = `
		SELECT ` + jobSelectColumns + `
		FROM jobs
		WHERE status = ? AND repo IN (SELECT value FROM json_each(?))
		ORDER BY created_at ASC, rowid ASC`
	rows, err := tx.QueryContext(ctx, query, string(protocol.JobStatusQueued), reposJSON)
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("query queued jobs: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		rec, err := scanJob(rows)
		if err != nil {
			return broker.JobRecord{}, fmt.Errorf("scan queued job: %w", err)
		}
		if matchesSkillFilter(supportedSkills, rec.Repo, rec.Skill) {
			return rec, nil
		}
	}
	if err := rows.Err(); err != nil {
		return broker.JobRecord{}, fmt.Errorf("iterate queued jobs: %w", err)
	}
	return broker.JobRecord{}, fmt.Errorf("no matching queued job: %w", broker.ErrNoJobAvailable)
}

func matchesSkillFilter(supportedSkills map[string][]string, repo, skill string) bool {
	allowed := supportedSkills[repo]
	if len(allowed) == 0 {
		return true
	}
	return slices.Contains(allowed, skill)
}

func getJobTx(ctx context.Context, tx *sql.Tx, jobID string) (broker.JobRecord, error) {
	const query = `SELECT ` + jobSelectColumns + ` FROM jobs WHERE job_id = ?`
	rec, err := scanJob(tx.QueryRowContext(ctx, query, jobID))
	if errors.Is(err, sql.ErrNoRows) {
		return broker.JobRecord{}, fmt.Errorf("job %s: %w", jobID, broker.ErrNotFound)
	}
	if err != nil {
		return broker.JobRecord{}, fmt.Errorf("get job %s: %w", jobID, err)
	}
	return rec, nil
}

func scanJob(scanner rowScanner) (broker.JobRecord, error) {
	var (
		rec             broker.JobRecord
		statusStr       string
		errCodeStr      string
		leaseExpiresNs  sql.NullInt64
		cancelRequested int
		usageJSON       []byte
		progressJSON    []byte
		createdNs       int64
		updatedNs       int64
	)
	if err := scanner.Scan(
		&rec.JobID, &rec.RequesterID, &rec.IdempotencyKey, &rec.Repo, &rec.DefaultBranch, &rec.CatalogSha,
		&rec.Skill, &rec.ArgsJSON, &statusStr, &rec.Attempts, &rec.MaxAttempts, &rec.WorkerID, &rec.WorkerAlias,
		&rec.LeaseToken, &leaseExpiresNs, &cancelRequested, &rec.DeliverableJSON,
		&usageJSON, &progressJSON, &errCodeStr, &rec.ErrorMessage, &createdNs, &updatedNs,
	); err != nil {
		return broker.JobRecord{}, fmt.Errorf("scan job row: %w", err)
	}

	rec.Status = protocol.JobStatus(statusStr)
	rec.ErrorCode = protocol.ErrorCode(errCodeStr)
	rec.CancelRequested = cancelRequested != 0
	rec.CreatedAt = time.Unix(0, createdNs).UTC()
	rec.UpdatedAt = time.Unix(0, updatedNs).UTC()
	if leaseExpiresNs.Valid {
		t := time.Unix(0, leaseExpiresNs.Int64).UTC()
		rec.LeaseExpiresAt = &t
	}
	if err := decodeJobExtras(&rec, usageJSON, progressJSON); err != nil {
		return broker.JobRecord{}, err
	}
	return rec, nil
}

func decodeJobExtras(rec *broker.JobRecord, usageJSON, progressJSON []byte) error {
	if len(usageJSON) > 0 {
		var u protocol.UsageReport
		if err := json.Unmarshal(usageJSON, &u); err != nil {
			return fmt.Errorf("unmarshal usage report: %w", err)
		}
		rec.UsageReport = &u
	}
	if len(progressJSON) > 0 {
		var p protocol.JobProgress
		if err := json.Unmarshal(progressJSON, &p); err != nil {
			return fmt.Errorf("unmarshal job progress: %w", err)
		}
		rec.Progress = &p
	}
	return nil
}

func marshalUsageReport(usage *protocol.UsageReport) ([]byte, error) {
	if usage == nil {
		return nil, nil
	}
	b, err := json.Marshal(usage)
	if err != nil {
		return nil, fmt.Errorf("marshal usage report json: %w", err)
	}
	return b, nil
}

func tokensMatch(expected, actual string) bool {
	if expected == "" || actual == "" {
		return false
	}
	return subtle.ConstantTimeCompare([]byte(expected), []byte(actual)) == 1
}
