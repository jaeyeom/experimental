// Package broker defines the storage boundary and domain records for the
// agent-pool central broker.
package broker

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

// TokenRole identifies the authorization role of a broker API token.
type TokenRole string

const (
	// TokenRoleProducer authorizes a caller to dispatch and inspect jobs.
	TokenRoleProducer TokenRole = "producer"
	// TokenRoleWorker authorizes a caller to send heartbeats and lease jobs.
	TokenRoleWorker TokenRole = "worker"
)

// Sentinel errors returned by Store implementations.
var (
	ErrNotFound          = errors.New("not found")
	ErrUnauthorized      = errors.New("unauthorized")
	ErrNoJobAvailable    = errors.New("no job available")
	ErrLeaseMismatch     = errors.New("lease mismatch")
	ErrInvalidTransition = errors.New("invalid transition")
	ErrRateLimited       = errors.New("rate limited")
)

// TokenRecord represents metadata for a hashed API token stored by the broker.
type TokenRecord struct {
	ID           string     `json:"id"`
	Role         TokenRole  `json:"role"`
	Name         string     `json:"name"`
	AllowedRepos []string   `json:"allowedRepos,omitempty"`
	CreatedAt    time.Time  `json:"createdAt"`
	RevokedAt    *time.Time `json:"revokedAt,omitempty"`
}

// CatalogRecord stores a repository's synced default-branch skill catalog.
type CatalogRecord struct {
	Repo          string    `json:"repo"`
	DefaultBranch string    `json:"defaultBranch"`
	CatalogSha    string    `json:"catalogSha"`
	CatalogJSON   []byte    `json:"catalogJson"`
	UpdatedAt     time.Time `json:"updatedAt"`
}

// WorkerRecord captures the latest heartbeat state for a connected worker.
type WorkerRecord struct {
	WorkerID             string               `json:"workerId"`
	Alias                string               `json:"alias"`
	SupportedRepos       []string             `json:"supportedRepos"`
	SupportedSkills      map[string][]string  `json:"supportedSkills,omitempty"`
	AcceptingJobs        bool                 `json:"acceptingJobs"`
	ConcurrencyAvailable int                  `json:"concurrencyAvailable"`
	Quota                protocol.QuotaHealth `json:"quota"`
	LastHeartbeat        time.Time            `json:"lastHeartbeat"`
}

// JobRecord represents a queued, active, or completed broker job.
type JobRecord struct {
	JobID           string                `json:"jobId"`
	RequesterID     string                `json:"requesterId"`
	IdempotencyKey  string                `json:"idempotencyKey,omitempty"`
	Repo            string                `json:"repo"`
	DefaultBranch   string                `json:"defaultBranch"`
	CatalogSha      string                `json:"catalogSha"`
	Skill           string                `json:"skill"`
	ArgsJSON        []byte                `json:"argsJson,omitempty"`
	Status          protocol.JobStatus    `json:"status"`
	Attempts        int                   `json:"attempts"`
	MaxAttempts     int                   `json:"maxAttempts"`
	WorkerID        string                `json:"workerId,omitempty"`
	WorkerAlias     string                `json:"workerAlias,omitempty"`
	LeaseToken      string                `json:"leaseToken,omitempty"`
	LeaseExpiresAt  *time.Time            `json:"leaseExpiresAt,omitempty"`
	CancelRequested bool                  `json:"cancelRequested,omitempty"`
	DeliverableJSON []byte                `json:"deliverableJson,omitempty"`
	UsageReport     *protocol.UsageReport `json:"usageReport,omitempty"`
	Progress        *protocol.JobProgress `json:"progress,omitempty"`
	ErrorCode       protocol.ErrorCode    `json:"errorCode,omitempty"`
	ErrorMessage    string                `json:"errorMessage,omitempty"`
	CreatedAt       time.Time             `json:"createdAt"`
	UpdatedAt       time.Time             `json:"updatedAt"`
}

// Store defines the persistence contract for the agent-pool central broker.
type Store interface {
	CreateToken(ctx context.Context, role TokenRole, name string, allowedRepos []string) (plaintext string, rec TokenRecord, err error)
	AuthenticateToken(ctx context.Context, plaintext string) (TokenRecord, error)
	RevokeToken(ctx context.Context, id string) error
	ListTokens(ctx context.Context) ([]TokenRecord, error)

	UpsertCatalog(ctx context.Context, rec CatalogRecord) error
	GetCatalog(ctx context.Context, repo string) (CatalogRecord, error)
	ListCatalogs(ctx context.Context) ([]CatalogRecord, error)

	UpsertWorkerHeartbeat(ctx context.Context, rec WorkerRecord) error
	ListActiveWorkers(ctx context.Context, staleAfter time.Duration, now time.Time) ([]WorkerRecord, error)

	CountPendingJobs(ctx context.Context, repos []string) (int, error)
	EnqueueJob(ctx context.Context, job JobRecord, maxConcurrentPerRequester int) (JobRecord, bool, error)
	GetJob(ctx context.Context, jobID string) (JobRecord, error)
	ClaimLease(ctx context.Context, workerID, workerAlias string, supportedRepos []string, supportedSkills map[string][]string, leaseTTL time.Duration, now time.Time) (JobRecord, error)
	RenewLease(ctx context.Context, jobID, leaseToken string, progress protocol.JobProgress, leaseTTL time.Duration, now time.Time) (expiresAt time.Time, cancelRequested bool, err error)
	CompleteJob(ctx context.Context, jobID, leaseToken string, status protocol.JobStatus, deliverable []byte, usage *protocol.UsageReport, errCode protocol.ErrorCode, errMsg string, now time.Time) (JobRecord, error)
	RequestCancel(ctx context.Context, jobID, requesterID string, now time.Time) (JobRecord, error)
	SweepExpiredLeases(ctx context.Context, now time.Time) (requeued int, timedOut int, err error)

	Close() error
}

// AuthenticateAndAuthorize authenticates plaintext against store and verifies
// that the token has the required role and (when repo is non-empty and
// AllowedRepos is restricted) permission to access repo.
func AuthenticateAndAuthorize(ctx context.Context, store Store, plaintext string, role TokenRole, repo string) (TokenRecord, error) {
	rec, err := store.AuthenticateToken(ctx, plaintext)
	if err != nil {
		return TokenRecord{}, fmt.Errorf("authenticate token: %w", err)
	}
	if role != "" && rec.Role != role {
		return TokenRecord{}, fmt.Errorf("token role %q does not match required %q: %w", rec.Role, role, ErrUnauthorized)
	}
	if repo != "" && len(rec.AllowedRepos) > 0 && !slices.Contains(rec.AllowedRepos, repo) {
		return TokenRecord{}, fmt.Errorf("token not allowed for repo %q: %w", repo, ErrUnauthorized)
	}
	return rec, nil
}
