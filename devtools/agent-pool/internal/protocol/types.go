package protocol

import (
	"encoding/json"
	"math"
	"time"
)

// TokenUsage captures raw token counts consumed during a job run.
type TokenUsage struct {
	Input         int64 `json:"input"`
	CacheRead     int64 `json:"cacheRead"`
	CacheCreation int64 `json:"cacheCreation"`
	Output        int64 `json:"output"`
}

// EffectiveUnits computes the weighted effective token units rounded to the
// nearest integer.
func (t TokenUsage) EffectiveUnits() int64 {
	weighted := float64(t.Input) +
		1.25*float64(t.CacheCreation) +
		0.1*float64(t.CacheRead) +
		5.0*float64(t.Output)
	return int64(math.Round(weighted))
}

// UsageReport summarizes provider, model, token usage, turns, and duration for
// a completed job.
type UsageReport struct {
	Provider       string     `json:"provider"`
	Model          string     `json:"model"`
	Tokens         TokenUsage `json:"tokens"`
	EffectiveUnits int64      `json:"effectiveUnits"`
	TurnsUsed      int        `json:"turnsUsed"`
	DurationMs     int64      `json:"durationMs"`
}

// JobProgress reports in-flight execution metrics during job heartbeats.
type JobProgress struct {
	TurnsUsed      int   `json:"turnsUsed"`
	EffectiveUnits int64 `json:"effectiveUnits"`
	ElapsedMs      int64 `json:"elapsedMs"`
}

// QuotaHealth describes a worker's remaining upstream quota posture.
type QuotaHealth struct {
	State                string     `json:"state"`
	FiveHourRemainingPct *float64   `json:"fiveHourRemainingPct,omitempty"`
	SevenDayRemainingPct *float64   `json:"sevenDayRemainingPct,omitempty"`
	FiveHourResetsAt     *time.Time `json:"fiveHourResetsAt,omitempty"`
	SevenDayResetsAt     *time.Time `json:"sevenDayResetsAt,omitempty"`
}

// SkillSummary describes an available skill advertised by the broker.
type SkillSummary struct {
	Repo          string          `json:"repo"`
	DefaultBranch string          `json:"defaultBranch"`
	CatalogSha    string          `json:"catalogSha"`
	Skill         string          `json:"skill"`
	Description   string          `json:"description"`
	ArgsSchema    json.RawMessage `json:"argsSchema,omitempty"`
	OutputSchema  json.RawMessage `json:"outputSchema,omitempty"`
}

// PoolStatusResponse reports current pool health, capacity, and skills.
type PoolStatusResponse struct {
	ActiveWorkers   int            `json:"activeWorkers"`
	PoolHealth      string         `json:"poolHealth"`
	QueueDepth      int            `json:"queueDepth"`
	SupportedRepos  []string       `json:"supportedRepos"`
	AvailableSkills []SkillSummary `json:"availableSkills"`
}

// DispatchRequest requests that the broker enqueue a new job.
type DispatchRequest struct {
	Repo           string          `json:"repo"`
	Skill          string          `json:"skill"`
	Args           json.RawMessage `json:"args,omitempty"`
	IdempotencyKey string          `json:"idempotencyKey,omitempty"`
}

// DispatchResponse acknowledges an enqueued job.
type DispatchResponse struct {
	JobID      string    `json:"jobId"`
	Status     JobStatus `json:"status"`
	CatalogSha string    `json:"catalogSha"`
}

// JobResultResponse returns the current state, progress, and outcome of a job.
type JobResultResponse struct {
	JobID        string          `json:"jobId"`
	Repo         string          `json:"repo"`
	Skill        string          `json:"skill"`
	CatalogSha   string          `json:"catalogSha"`
	Status       JobStatus       `json:"status"`
	WorkerAlias  string          `json:"workerAlias,omitempty"`
	Deliverable  json.RawMessage `json:"deliverable,omitempty"`
	UsageReport  *UsageReport    `json:"usageReport,omitempty"`
	Progress     *JobProgress    `json:"progress,omitempty"`
	ErrorCode    ErrorCode       `json:"errorCode,omitempty"`
	ErrorMessage string          `json:"errorMessage,omitempty"`
	CreatedAt    time.Time       `json:"createdAt"`
	UpdatedAt    time.Time       `json:"updatedAt"`
}

// CancelJobResponse reports the result of a job cancellation request.
type CancelJobResponse struct {
	JobID           string    `json:"jobId"`
	Status          JobStatus `json:"status"`
	CancelRequested bool      `json:"cancelRequested"`
}

// WorkerHeartbeatRequest reports a worker's supported repos, capacity, and
// quota health to the broker.
type WorkerHeartbeatRequest struct {
	SupportedRepos       []string            `json:"supportedRepos"`
	SupportedSkills      map[string][]string `json:"supportedSkills,omitempty"`
	AcceptingJobs        bool                `json:"acceptingJobs"`
	ConcurrencyAvailable int                 `json:"concurrencyAvailable"`
	Quota                QuotaHealth         `json:"quota"`
}

// WorkerHeartbeatResponse acknowledges a worker heartbeat.
type WorkerHeartbeatResponse struct {
	Ack         bool `json:"ack"`
	PendingJobs int  `json:"pendingJobs"`
}

// LeaseRequest asks the broker to lease an eligible queued job.
type LeaseRequest struct {
	SupportedRepos  []string            `json:"supportedRepos"`
	SupportedSkills map[string][]string `json:"supportedSkills,omitempty"`
}

// LeaseResponse returns the leased job payload and lease credentials.
type LeaseResponse struct {
	JobID           string          `json:"jobId"`
	Repo            string          `json:"repo"`
	DefaultBranch   string          `json:"defaultBranch"`
	CatalogSha      string          `json:"catalogSha"`
	Skill           string          `json:"skill"`
	Args            json.RawMessage `json:"args,omitempty"`
	LeaseToken      string          `json:"leaseToken"`
	LeaseTTLSeconds int             `json:"leaseTtlSeconds"`
}

// JobHeartbeatRequest renews an active job lease and reports progress.
type JobHeartbeatRequest struct {
	LeaseToken string      `json:"leaseToken"`
	Progress   JobProgress `json:"progress"`
}

// JobHeartbeatResponse acknowledges a job heartbeat and indicates cancellation.
type JobHeartbeatResponse struct {
	Ack            bool      `json:"ack"`
	LeaseExpiresAt time.Time `json:"leaseExpiresAt"`
	Cancel         bool      `json:"cancel"`
}

// CompleteJobRequest submits a terminal outcome for a leased job.
type CompleteJobRequest struct {
	LeaseToken   string          `json:"leaseToken"`
	Status       JobStatus       `json:"status"`
	Deliverable  json.RawMessage `json:"deliverable,omitempty"`
	DlpChecked   bool            `json:"dlpChecked"`
	UsageReport  *UsageReport    `json:"usageReport,omitempty"`
	ErrorCode    ErrorCode       `json:"errorCode,omitempty"`
	ErrorMessage string          `json:"errorMessage,omitempty"`
}

// CompleteJobResponse acknowledges job completion.
type CompleteJobResponse struct {
	JobID  string    `json:"jobId"`
	Status JobStatus `json:"status"`
}

// SSEJobAvailable is the server-sent event payload notifying workers that a
// matching job is queued.
type SSEJobAvailable struct {
	Repo  string `json:"repo"`
	Skill string `json:"skill"`
}

// SSEJobCanceled is the server-sent event payload notifying a worker that a
// leased job has been canceled.
type SSEJobCanceled struct {
	JobID string `json:"jobId"`
}
