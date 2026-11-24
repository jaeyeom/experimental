package broker

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	defaultWorkerStaleAfter          = 90 * time.Second
	defaultLeaseTTL                  = 60 * time.Second
	defaultMaxAttempts               = 2
	defaultMaxConcurrentPerRequester = 5
	maxJobWaitDuration               = 25 * time.Second
)

// Config holds operational parameters for the broker Service.
type Config struct {
	WorkerStaleAfter          time.Duration
	LeaseTTL                  time.Duration
	MaxAttempts               int
	MaxConcurrentPerRequester int
	Now                       func() time.Time
	Logger                    *slog.Logger
}

// Service implements the core broker business logic shared by the Worker HTTP
// API, Producer REST API, and Producer MCP endpoint.
type Service struct {
	store   Store
	scanner *dlp.Scanner
	hub     *EventHub
	cfg     Config
	now     func() time.Time
	logger  *slog.Logger
}

// NewService constructs a Service with defaults applied for zero-value config
// fields.
func NewService(store Store, scanner *dlp.Scanner, hub *EventHub, cfg Config) *Service {
	if scanner == nil {
		scanner = dlp.NewScanner()
	}
	if hub == nil {
		hub = NewEventHub()
	}
	if cfg.WorkerStaleAfter <= 0 {
		cfg.WorkerStaleAfter = defaultWorkerStaleAfter
	}
	if cfg.LeaseTTL <= 0 {
		cfg.LeaseTTL = defaultLeaseTTL
	}
	if cfg.MaxAttempts <= 0 {
		cfg.MaxAttempts = defaultMaxAttempts
	}
	if cfg.MaxConcurrentPerRequester <= 0 {
		cfg.MaxConcurrentPerRequester = defaultMaxConcurrentPerRequester
	}
	nowFn := cfg.Now
	if nowFn == nil {
		nowFn = func() time.Time { return time.Now().UTC() }
	}
	logger := cfg.Logger
	if logger == nil {
		logger = slog.New(slog.NewTextHandler(io.Discard, nil))
	}
	return &Service{
		store:   store,
		scanner: scanner,
		hub:     hub,
		cfg:     cfg,
		now:     nowFn,
		logger:  logger,
	}
}

// Hub returns the EventHub used for worker SSE and job completion wakeups.
func (s *Service) Hub() *EventHub {
	return s.hub
}

// PoolStatus aggregates active worker capacity, queue depth, supported repos,
// and pinned catalog skills, filtered by requester.AllowedRepos when non-empty.
func (s *Service) PoolStatus(ctx context.Context, requester TokenRecord) (protocol.PoolStatusResponse, error) {
	workers, err := s.store.ListActiveWorkers(ctx, s.cfg.WorkerStaleAfter, s.now())
	if err != nil {
		return protocol.PoolStatusResponse{}, fmt.Errorf("list active workers: %w", err)
	}
	catalogs, err := s.store.ListCatalogs(ctx)
	if err != nil {
		return protocol.PoolStatusResponse{}, fmt.Errorf("list catalogs: %w", err)
	}
	queueDepth, err := s.store.CountPendingJobs(ctx, requester.AllowedRepos)
	if err != nil {
		return protocol.PoolStatusResponse{}, fmt.Errorf("count pending jobs: %w", err)
	}

	activeWorkers, poolHealth, supportedRepos := summarizeWorkers(workers, requester.AllowedRepos)
	availableSkills := summarizeCatalogSkills(catalogs, requester.AllowedRepos, s.logger)

	return protocol.PoolStatusResponse{
		ActiveWorkers:   activeWorkers,
		PoolHealth:      poolHealth,
		QueueDepth:      queueDepth,
		SupportedRepos:  supportedRepos,
		AvailableSkills: availableSkills,
	}, nil
}

// DispatchSkill validates authorization, catalog skill, arguments, and pool
// capacity before enqueueing a new job and waking subscribed workers.
func (s *Service) DispatchSkill(ctx context.Context, requester TokenRecord, req protocol.DispatchRequest) (protocol.DispatchResponse, error) {
	if req.Repo == "" || req.Skill == "" {
		return protocol.DispatchResponse{}, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: "repo and skill are required",
		}
	}
	if !repoAllowed(requester.AllowedRepos, req.Repo) {
		return protocol.DispatchResponse{}, &protocol.APIError{
			Code:    protocol.ErrCodeForbidden,
			Message: fmt.Sprintf("token is not allowed to dispatch on repo %q", req.Repo),
		}
	}

	catRec, skill, err := s.lookupSkill(ctx, req.Repo, req.Skill)
	if err != nil {
		return protocol.DispatchResponse{}, err
	}

	rawArgs := req.Args
	if len(bytes.TrimSpace(rawArgs)) == 0 {
		rawArgs = json.RawMessage(`{}`)
	}
	canonicalArgs, err := skill.ValidateArgs(rawArgs)
	if err != nil {
		return protocol.DispatchResponse{}, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: err.Error(),
		}
	}

	if err := s.requireCapacity(ctx, req.Repo, req.Skill); err != nil {
		return protocol.DispatchResponse{}, err
	}

	jobRec := JobRecord{
		RequesterID:    requester.ID,
		IdempotencyKey: req.IdempotencyKey,
		Repo:           req.Repo,
		DefaultBranch:  catRec.DefaultBranch,
		CatalogSha:     catRec.CatalogSha,
		Skill:          req.Skill,
		ArgsJSON:       canonicalArgs,
		Status:         protocol.JobStatusQueued,
		MaxAttempts:    s.cfg.MaxAttempts,
		CreatedAt:      s.now(),
	}
	enqueued, deduped, err := s.store.EnqueueJob(ctx, jobRec, s.cfg.MaxConcurrentPerRequester)
	if err != nil {
		return protocol.DispatchResponse{}, mapStoreError(err)
	}
	if !deduped {
		s.hub.PublishJobAvailable(enqueued.Repo, enqueued.Skill)
	}
	return protocol.DispatchResponse{
		JobID:      enqueued.JobID,
		Status:     enqueued.Status,
		CatalogSha: enqueued.CatalogSha,
	}, nil
}

// GetJobResult returns the current status and outcome of jobID, optionally
// long-polling up to 25s until the job reaches a terminal state.
func (s *Service) GetJobResult(ctx context.Context, requesterID, jobID string, wait time.Duration) (protocol.JobResultResponse, error) {
	job, err := s.getOwnedJob(ctx, requesterID, jobID)
	if err != nil {
		return protocol.JobResultResponse{}, err
	}
	if wait <= 0 || job.Status.IsTerminal() {
		return toJobResultResponse(job), nil
	}

	if wait > maxJobWaitDuration {
		wait = maxJobWaitDuration
	}
	waitCtx, cancel := context.WithTimeout(ctx, wait)
	defer cancel()

	for !job.Status.IsTerminal() {
		waitCh := s.hub.WatchJob(jobID)
		job, err = s.getOwnedJob(ctx, requesterID, jobID)
		if err != nil {
			return protocol.JobResultResponse{}, err
		}
		if job.Status.IsTerminal() {
			break
		}
		select {
		case <-waitCh:
		case <-waitCtx.Done():
			if ctx.Err() != nil {
				return protocol.JobResultResponse{}, fmt.Errorf("wait for job %s: %w", jobID, ctx.Err())
			}
			return toJobResultResponse(job), nil
		}
	}
	return toJobResultResponse(job), nil
}

// CancelJob cancels a queued job immediately or flags an active job for
// cooperative cancellation and notifies the assigned worker over SSE.
func (s *Service) CancelJob(ctx context.Context, requesterID, jobID string) (protocol.CancelJobResponse, error) {
	job, err := s.store.RequestCancel(ctx, jobID, requesterID, s.now())
	if err != nil {
		return protocol.CancelJobResponse{}, mapStoreError(err)
	}
	if job.Status.IsTerminal() {
		s.hub.NotifyJob(job.JobID)
	} else if job.WorkerID != "" {
		s.hub.PublishJobCanceled(job.WorkerID, job.JobID)
	}
	return protocol.CancelJobResponse{
		JobID:           job.JobID,
		Status:          job.Status,
		CancelRequested: job.CancelRequested,
	}, nil
}

// WorkerHeartbeat records a worker's capacity and returns pending job count for
// its advertised repositories.
func (s *Service) WorkerHeartbeat(ctx context.Context, worker TokenRecord, req protocol.WorkerHeartbeatRequest) (protocol.WorkerHeartbeatResponse, error) {
	if err := validateWorkerRepos(worker.AllowedRepos, req.SupportedRepos, req.SupportedSkills); err != nil {
		return protocol.WorkerHeartbeatResponse{}, err
	}
	rec := WorkerRecord{
		WorkerID:             worker.ID,
		Alias:                worker.Name,
		SupportedRepos:       req.SupportedRepos,
		SupportedSkills:      req.SupportedSkills,
		AcceptingJobs:        req.AcceptingJobs,
		ConcurrencyAvailable: req.ConcurrencyAvailable,
		Quota:                req.Quota,
		LastHeartbeat:        s.now(),
	}
	if err := s.store.UpsertWorkerHeartbeat(ctx, rec); err != nil {
		return protocol.WorkerHeartbeatResponse{}, fmt.Errorf("upsert worker heartbeat: %w", err)
	}

	pending := 0
	if len(req.SupportedRepos) > 0 {
		count, err := s.store.CountPendingJobs(ctx, req.SupportedRepos)
		if err != nil {
			return protocol.WorkerHeartbeatResponse{}, fmt.Errorf("count pending jobs: %w", err)
		}
		pending = count
	}
	return protocol.WorkerHeartbeatResponse{
		Ack:         true,
		PendingJobs: pending,
	}, nil
}

// ClaimLease attempts to claim the next eligible queued job for worker. It
// returns (resp, false, nil) when no matching job is available.
func (s *Service) ClaimLease(ctx context.Context, worker TokenRecord, req protocol.LeaseRequest) (protocol.LeaseResponse, bool, error) {
	if err := validateWorkerRepos(worker.AllowedRepos, req.SupportedRepos, req.SupportedSkills); err != nil {
		return protocol.LeaseResponse{}, false, err
	}
	job, err := s.store.ClaimLease(
		ctx,
		worker.ID,
		worker.Name,
		req.SupportedRepos,
		req.SupportedSkills,
		s.cfg.LeaseTTL,
		s.now(),
	)
	if errors.Is(err, ErrNoJobAvailable) {
		return protocol.LeaseResponse{}, false, nil
	}
	if err != nil {
		return protocol.LeaseResponse{}, false, mapStoreError(err)
	}
	return protocol.LeaseResponse{
		JobID:           job.JobID,
		Repo:            job.Repo,
		DefaultBranch:   job.DefaultBranch,
		CatalogSha:      job.CatalogSha,
		Skill:           job.Skill,
		Args:            job.ArgsJSON,
		LeaseToken:      job.LeaseToken,
		LeaseTTLSeconds: int(s.cfg.LeaseTTL.Seconds()),
	}, true, nil
}

// JobHeartbeat renews a worker's active job lease, persists progress, and
// reports whether the producer requested cancellation.
func (s *Service) JobHeartbeat(ctx context.Context, _ TokenRecord, jobID string, req protocol.JobHeartbeatRequest) (protocol.JobHeartbeatResponse, error) {
	expiresAt, cancelRequested, err := s.store.RenewLease(
		ctx,
		jobID,
		req.LeaseToken,
		req.Progress,
		s.cfg.LeaseTTL,
		s.now(),
	)
	if err != nil {
		return protocol.JobHeartbeatResponse{}, mapStoreError(err)
	}
	return protocol.JobHeartbeatResponse{
		Ack:            true,
		LeaseExpiresAt: expiresAt,
		Cancel:         cancelRequested,
	}, nil
}

// CompleteJob runs Stage-2 output schema validation and DLP scanning before
// persisting a terminal job outcome and notifying long-poll waiters.
func (s *Service) CompleteJob(ctx context.Context, _ TokenRecord, jobID string, req protocol.CompleteJobRequest) (protocol.CompleteJobResponse, error) {
	if !req.Status.IsTerminal() {
		return protocol.CompleteJobResponse{}, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidTransition,
			Message: fmt.Sprintf("status %q is not terminal", req.Status),
		}
	}

	finalStatus := req.Status
	finalErrCode := req.ErrorCode
	finalErrMsg := req.ErrorMessage
	var finalDeliverable []byte

	if req.Status == protocol.JobStatusSucceeded {
		status, deliverable, errCode, errMsg, err := s.runStage2Gate(ctx, jobID, req.Deliverable)
		if err != nil {
			return protocol.CompleteJobResponse{}, err
		}
		finalStatus = status
		finalDeliverable = deliverable
		finalErrCode = errCode
		finalErrMsg = errMsg
	}

	updated, err := s.store.CompleteJob(
		ctx,
		jobID,
		req.LeaseToken,
		finalStatus,
		finalDeliverable,
		req.UsageReport,
		finalErrCode,
		finalErrMsg,
		s.now(),
	)
	if err != nil {
		return protocol.CompleteJobResponse{}, mapStoreError(err)
	}
	s.hub.NotifyJob(updated.JobID)
	return protocol.CompleteJobResponse{
		JobID:  updated.JobID,
		Status: updated.Status,
	}, nil
}

// StartSweeper periodically sweeps expired leases until ctx is canceled, waking
// workers for requeued jobs and waiters for timed-out jobs.
func (s *Service) StartSweeper(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = 10 * time.Second
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			s.sweepOnce(ctx)
		}
	}
}

func (s *Service) sweepOnce(ctx context.Context) {
	requeued, timedOut, err := s.store.SweepExpiredLeases(ctx, s.now())
	if err != nil {
		s.logger.Warn("sweep expired leases failed", "error", err)
		return
	}
	if requeued > 0 {
		s.hub.BroadcastJobAvailable()
	}
	if timedOut > 0 {
		s.hub.NotifyAllJobs()
	}
}

func (s *Service) runStage2Gate(ctx context.Context, jobID string, rawDeliverable json.RawMessage) (protocol.JobStatus, []byte, protocol.ErrorCode, string, error) {
	job, err := s.store.GetJob(ctx, jobID)
	if err != nil {
		return "", nil, "", "", mapStoreError(err)
	}
	_, skill, err := s.lookupSkill(ctx, job.Repo, job.Skill)
	if err != nil {
		return "", nil, "", "", err
	}
	if err := skill.ValidateOutput(rawDeliverable); err != nil {
		return protocol.JobStatusFailed, nil, protocol.ErrCodeOutputInvalid, err.Error(), nil
	}
	scanRes := s.scanner.Scan(rawDeliverable, dlp.HostContext{})
	if scanRes.Blocked {
		return protocol.JobStatusDLPBlocked, nil, protocol.ErrCodeDLPBlocked, formatDLPFindings(scanRes.Findings), nil
	}
	return protocol.JobStatusSucceeded, scanRes.Normalized, "", "", nil
}

func (s *Service) lookupSkill(ctx context.Context, repo, skillName string) (CatalogRecord, catalog.Skill, error) {
	catRec, err := s.store.GetCatalog(ctx, repo)
	if errors.Is(err, ErrNotFound) {
		return CatalogRecord{}, catalog.Skill{}, &protocol.APIError{
			Code:    protocol.ErrCodeUnknownRepo,
			Message: fmt.Sprintf("unknown repo %q", repo),
		}
	}
	if err != nil {
		return CatalogRecord{}, catalog.Skill{}, fmt.Errorf("get catalog %q: %w", repo, err)
	}
	cat, err := catalog.Parse(catRec.CatalogJSON)
	if err != nil {
		return CatalogRecord{}, catalog.Skill{}, fmt.Errorf("parse catalog %q: %w", repo, err)
	}
	skill, ok := cat.Skills[skillName]
	if !ok {
		return CatalogRecord{}, catalog.Skill{}, &protocol.APIError{
			Code:    protocol.ErrCodeUnknownSkill,
			Message: fmt.Sprintf("unknown skill %q for repo %q", skillName, repo),
		}
	}
	return catRec, skill, nil
}

func (s *Service) requireCapacity(ctx context.Context, repo, skillName string) error {
	workers, err := s.store.ListActiveWorkers(ctx, s.cfg.WorkerStaleAfter, s.now())
	if err != nil {
		return fmt.Errorf("list active workers: %w", err)
	}
	for _, w := range workers {
		if workerSupports(w, repo, skillName) {
			return nil
		}
	}
	return &protocol.APIError{
		Code:    protocol.ErrCodeNoCapacity,
		Message: fmt.Sprintf("no active accepting worker supports %s/%s", repo, skillName),
	}
}

func (s *Service) getOwnedJob(ctx context.Context, requesterID, jobID string) (JobRecord, error) {
	job, err := s.store.GetJob(ctx, jobID)
	if err != nil {
		return JobRecord{}, mapStoreError(err)
	}
	if job.RequesterID != requesterID {
		return JobRecord{}, &protocol.APIError{
			Code:    protocol.ErrCodeForbidden,
			Message: "job belongs to another requester",
		}
	}
	return job, nil
}

func summarizeWorkers(workers []WorkerRecord, allowedRepos []string) (int, string, []string) {
	acceptingCount := 0
	freeCount := 0
	hasAmber := false
	repoSet := make(map[string]struct{})

	for _, w := range workers {
		if !w.AcceptingJobs {
			continue
		}
		acceptingCount++
		if w.ConcurrencyAvailable > 0 {
			freeCount++
		}
		if w.Quota.State == "amber" {
			hasAmber = true
		}
		for _, r := range w.SupportedRepos {
			if repoAllowed(allowedRepos, r) {
				repoSet[r] = struct{}{}
			}
		}
	}

	health := "healthy"
	switch {
	case acceptingCount == 0:
		health = "empty"
	case hasAmber || freeCount == 0:
		health = "constrained"
	}

	repos := make([]string, 0, len(repoSet))
	for r := range repoSet {
		repos = append(repos, r)
	}
	slices.Sort(repos)
	return freeCount, health, repos
}

func summarizeCatalogSkills(catalogs []CatalogRecord, allowedRepos []string, logger *slog.Logger) []protocol.SkillSummary {
	out := make([]protocol.SkillSummary, 0)
	for _, rec := range catalogs {
		if !repoAllowed(allowedRepos, rec.Repo) {
			continue
		}
		cat, err := catalog.Parse(rec.CatalogJSON)
		if err != nil {
			logger.Warn("skipping invalid stored catalog", "repo", rec.Repo, "error", err)
			continue
		}
		names := make([]string, 0, len(cat.Skills))
		for name := range cat.Skills {
			names = append(names, name)
		}
		slices.Sort(names)
		for _, name := range names {
			sk := cat.Skills[name]
			out = append(out, protocol.SkillSummary{
				Repo:          rec.Repo,
				DefaultBranch: rec.DefaultBranch,
				CatalogSha:    rec.CatalogSha,
				Skill:         name,
				Description:   sk.Description,
				ArgsSchema:    sk.ArgsSchema,
				OutputSchema:  sk.OutputSchema,
			})
		}
	}
	return out
}

func workerSupports(w WorkerRecord, repo, skill string) bool {
	if !w.AcceptingJobs || !slices.Contains(w.SupportedRepos, repo) {
		return false
	}
	allowedSkills := w.SupportedSkills[repo]
	if len(allowedSkills) == 0 {
		return true
	}
	return slices.Contains(allowedSkills, skill)
}

func validateWorkerRepos(allowedRepos, supportedRepos []string, supportedSkills map[string][]string) error {
	if len(allowedRepos) == 0 {
		return nil
	}
	for _, r := range supportedRepos {
		if !slices.Contains(allowedRepos, r) {
			return &protocol.APIError{
				Code:    protocol.ErrCodeForbidden,
				Message: fmt.Sprintf("worker token is not allowed for repo %q", r),
			}
		}
	}
	for r := range supportedSkills {
		if !slices.Contains(allowedRepos, r) {
			return &protocol.APIError{
				Code:    protocol.ErrCodeForbidden,
				Message: fmt.Sprintf("worker token is not allowed for repo %q", r),
			}
		}
	}
	return nil
}

func repoAllowed(allowedRepos []string, repo string) bool {
	if len(allowedRepos) == 0 {
		return true
	}
	return slices.Contains(allowedRepos, repo)
}

func mapStoreError(err error) error {
	switch {
	case errors.Is(err, ErrNotFound):
		return &protocol.APIError{Code: protocol.ErrCodeNotFound, Message: err.Error()}
	case errors.Is(err, ErrUnauthorized):
		return &protocol.APIError{Code: protocol.ErrCodeForbidden, Message: err.Error()}
	case errors.Is(err, ErrLeaseMismatch):
		return &protocol.APIError{Code: protocol.ErrCodeLeaseExpired, Message: err.Error()}
	case errors.Is(err, ErrInvalidTransition):
		return &protocol.APIError{Code: protocol.ErrCodeInvalidTransition, Message: err.Error()}
	case errors.Is(err, ErrRateLimited):
		return &protocol.APIError{Code: protocol.ErrCodeRateLimited, Message: err.Error()}
	default:
		return fmt.Errorf("broker store error: %w", err)
	}
}

func toJobResultResponse(job JobRecord) protocol.JobResultResponse {
	return protocol.JobResultResponse{
		JobID:        job.JobID,
		Repo:         job.Repo,
		Skill:        job.Skill,
		CatalogSha:   job.CatalogSha,
		Status:       job.Status,
		WorkerAlias:  job.WorkerAlias,
		Deliverable:  job.DeliverableJSON,
		UsageReport:  job.UsageReport,
		Progress:     job.Progress,
		ErrorCode:    job.ErrorCode,
		ErrorMessage: job.ErrorMessage,
		CreatedAt:    job.CreatedAt,
		UpdatedAt:    job.UpdatedAt,
	}
}

func formatDLPFindings(findings []dlp.Finding) string {
	parts := make([]string, 0, len(findings))
	for _, f := range findings {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Rule, f.Detail))
	}
	return "deliverable blocked by stage-2 DLP (" + strings.Join(parts, "; ") + ")"
}
