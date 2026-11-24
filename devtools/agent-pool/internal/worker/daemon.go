package worker

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/harness"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/sandbox"
)

const (
	processTerminateGrace = 5 * time.Second
	completeCallTimeout   = 15 * time.Second
	maxStderrCaptureBytes = 64 << 10 // 64 KiB.
)

// Deps holds external and injectable dependencies for the consumer worker
// Daemon.
type Deps struct {
	Client    *BrokerClient
	Quota     quota.Guard
	Ledger    *quota.Ledger
	Worktrees *WorktreeManager
	Sandbox   sandbox.Runner
	Harness   harness.Runner
	DLP       *dlp.Scanner
	HostCtx   dlp.HostContext // Real HomeDir, Username, Hostname, PersonalEmail (WorktreePath filled per job).
	Logger    *slog.Logger
}

// Daemon runs the outbound-only consumer worker lifecycle: worker heartbeats,
// SSE wakeups with polling fallback, lease claiming, sandboxed execution,
// Stage-1 DLP, and local donation ledger recording.
type Daemon struct {
	cfg  *Config
	deps Deps

	wakeCh chan struct{}

	mu            sync.Mutex
	inFlight      int
	draining      bool
	activeCancels map[string]func(reason string)
}

// NewDaemon constructs a Daemon from cfg and deps, applying safe defaults for
// optional fields.
func NewDaemon(cfg *Config, deps Deps) *Daemon {
	if cfg == nil {
		cfg = DefaultConfig()
	}
	if deps.DLP == nil {
		deps.DLP = dlp.NewScanner()
	}
	if deps.Logger == nil {
		deps.Logger = slog.New(slog.NewTextHandler(io.Discard, nil))
	}
	return &Daemon{
		cfg:           cfg,
		deps:          deps,
		wakeCh:        make(chan struct{}, 1),
		activeCancels: make(map[string]func(reason string)),
	}
}

// Run executes the worker daemon loop until ctx is canceled, then drains any
// in-flight jobs up to cfg.Worker.DrainTimeout.
func (d *Daemon) Run(ctx context.Context) error {
	if d.deps.Worktrees != nil {
		if err := d.deps.Worktrees.PruneOrphans(ctx); err != nil {
			d.deps.Logger.Warn("prune orphan worktrees failed", "error", err)
		}
	}

	d.sendHeartbeat(ctx)

	var bgWG sync.WaitGroup
	bgWG.Add(2)
	go func() {
		defer bgWG.Done()
		d.runHeartbeatLoop(ctx)
	}()
	go func() {
		defer bgWG.Done()
		_ = d.deps.Client.StreamEvents(ctx, d.handleSSEEvent)
	}()

	var jobsWG sync.WaitGroup
	jobCtx, cancelJobs := context.WithCancel(context.Background())
	defer cancelJobs()

	d.runDispatchLoop(ctx, jobCtx, &jobsWG)

	d.setDraining()
	d.sendFinalHeartbeat()
	d.drainInFlight(&jobsWG, cancelJobs)
	bgWG.Wait()
	return nil
}

// RunOnce checks admission, claims at most one eligible job, and executes it
// synchronously. It returns (true, nil) if a job was claimed and executed.
func (d *Daemon) RunOnce(ctx context.Context) (bool, error) {
	d.sendHeartbeat(ctx)
	allowed, reason, _ := d.canAccept(ctx)
	if !allowed {
		d.deps.Logger.Info("skipping claim: worker not accepting jobs", "reason", reason)
		return false, nil
	}
	if d.cfg.DryRun {
		d.deps.Logger.Info("dry-run enabled: skipping lease claim")
		return false, nil
	}
	lease, err := d.deps.Client.ClaimLease(ctx, protocol.LeaseRequest{
		SupportedRepos:  d.cfg.Worker.SupportedRepos,
		SupportedSkills: d.cfg.Worker.SupportedSkills,
	})
	if err != nil {
		return false, err
	}
	if lease == nil {
		return false, nil
	}
	d.executeJob(ctx, lease)
	return true, nil
}

func (d *Daemon) runHeartbeatLoop(ctx context.Context) {
	interval := d.cfg.Worker.HeartbeatInterval
	if interval <= 0 {
		interval = defaultHeartbeatInterval
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			d.sendHeartbeat(ctx)
		}
	}
}

func (d *Daemon) sendHeartbeat(ctx context.Context) {
	allowed, _, health := d.canAccept(ctx)
	avail := d.availableSlots()
	if !allowed {
		avail = 0
	}
	resp, err := d.deps.Client.Heartbeat(ctx, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       d.cfg.Worker.SupportedRepos,
		SupportedSkills:      d.cfg.Worker.SupportedSkills,
		AcceptingJobs:        allowed,
		ConcurrencyAvailable: avail,
		Quota:                health,
	})
	if err != nil {
		if ctx.Err() == nil {
			d.deps.Logger.Warn("worker heartbeat failed", "error", err)
		}
		return
	}
	if allowed && resp.PendingJobs > 0 {
		d.triggerWake()
	}
}

func (d *Daemon) sendFinalHeartbeat() {
	hbCtx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	_, _, health := d.canAccept(hbCtx)
	_, _ = d.deps.Client.Heartbeat(hbCtx, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       d.cfg.Worker.SupportedRepos,
		SupportedSkills:      d.cfg.Worker.SupportedSkills,
		AcceptingJobs:        false,
		ConcurrencyAvailable: 0,
		Quota:                health,
	})
}

func (d *Daemon) handleSSEEvent(eventType string, data []byte) {
	switch eventType {
	case "job_available":
		d.triggerWake()
	case "job_canceled":
		var ev protocol.SSEJobCanceled
		if err := json.Unmarshal(data, &ev); err != nil {
			d.deps.Logger.Warn("invalid job_canceled SSE payload", "error", err)
			return
		}
		d.cancelActiveJob(ev.JobID, "canceled by requester")
	}
}

func (d *Daemon) runDispatchLoop(ctx, jobCtx context.Context, jobsWG *sync.WaitGroup) {
	interval := d.cfg.Worker.PollInterval
	if interval <= 0 {
		interval = defaultPollInterval
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	d.tryClaimAvailable(ctx, jobCtx, jobsWG)
	for {
		select {
		case <-ctx.Done():
			return
		case <-d.wakeCh:
			d.tryClaimAvailable(ctx, jobCtx, jobsWG)
		case <-ticker.C:
			d.tryClaimAvailable(ctx, jobCtx, jobsWG)
		}
	}
}

func (d *Daemon) tryClaimAvailable(ctx, jobCtx context.Context, jobsWG *sync.WaitGroup) {
	for d.availableSlots() > 0 && ctx.Err() == nil {
		allowed, _, _ := d.canAccept(ctx)
		if !allowed {
			return
		}
		if d.cfg.DryRun {
			d.deps.Logger.Info("dry-run mode: would claim eligible job")
			return
		}
		lease, err := d.deps.Client.ClaimLease(ctx, protocol.LeaseRequest{
			SupportedRepos:  d.cfg.Worker.SupportedRepos,
			SupportedSkills: d.cfg.Worker.SupportedSkills,
		})
		if err != nil {
			if ctx.Err() == nil {
				d.deps.Logger.Warn("claim lease failed", "error", err)
			}
			return
		}
		if lease == nil {
			return
		}
		d.launchLeasedJob(jobCtx, jobsWG, lease)
	}
}

func (d *Daemon) launchLeasedJob(jobCtx context.Context, jobsWG *sync.WaitGroup, lease *protocol.LeaseResponse) {
	d.mu.Lock()
	d.inFlight++
	d.mu.Unlock()

	jobsWG.Add(1)
	go func() {
		defer jobsWG.Done()
		defer func() {
			d.mu.Lock()
			d.inFlight--
			d.mu.Unlock()
			d.triggerWake()
		}()
		d.executeJob(jobCtx, lease)
	}()
}

func (d *Daemon) drainInFlight(jobsWG *sync.WaitGroup, cancelJobs context.CancelFunc) {
	done := make(chan struct{})
	go func() {
		jobsWG.Wait()
		close(done)
	}()

	timeout := d.cfg.Worker.DrainTimeout
	if timeout <= 0 {
		timeout = defaultDrainTimeout
	}
	timer := time.NewTimer(timeout)
	defer timer.Stop()

	select {
	case <-done:
	case <-timer.C:
		d.deps.Logger.Warn("drain timeout reached; canceling remaining in-flight jobs", "timeout", timeout)
		cancelJobs()
		<-done
	}
}

func (d *Daemon) canAccept(ctx context.Context) (bool, string, protocol.QuotaHealth) {
	d.mu.Lock()
	draining := d.draining
	d.mu.Unlock()

	health := protocol.QuotaHealth{State: quota.StateUnknown}
	quotaAllow := true
	var quotaReason string
	if d.deps.Quota != nil {
		dec := d.deps.Quota.CanAccept(ctx)
		quotaAllow = dec.Allow
		quotaReason = dec.Reason
		health = dec.Health
	}
	if draining {
		return false, "worker draining", health
	}
	if paused, reason := IsPaused(d.cfg.Worker.StateDir); paused {
		return false, "locally paused: " + reason, health
	}
	if !quotaAllow {
		return false, quotaReason, health
	}
	return true, "", health
}

func (d *Daemon) availableSlots() int {
	maxConc := d.cfg.Worker.MaxConcurrentJobs
	if maxConc <= 0 {
		maxConc = 1
	}
	d.mu.Lock()
	defer d.mu.Unlock()
	return max(0, maxConc-d.inFlight)
}

func (d *Daemon) setDraining() {
	d.mu.Lock()
	d.draining = true
	d.mu.Unlock()
}

func (d *Daemon) triggerWake() {
	select {
	case d.wakeCh <- struct{}{}:
	default:
	}
}

func (d *Daemon) registerActiveJob(jobID string, cancelFn func(reason string)) {
	d.mu.Lock()
	d.activeCancels[jobID] = cancelFn
	d.mu.Unlock()
}

func (d *Daemon) unregisterActiveJob(jobID string) {
	d.mu.Lock()
	delete(d.activeCancels, jobID)
	d.mu.Unlock()
}

func (d *Daemon) cancelActiveJob(jobID, reason string) {
	d.mu.Lock()
	fn := d.activeCancels[jobID]
	d.mu.Unlock()
	if fn != nil {
		fn(reason)
	}
}

// --- Per-job execution pipeline ---

type terminationCause int

const (
	causeNone terminationCause = iota
	causeBudget
	causeTimeout
	causeCanceled
)

type jobControl struct {
	mu      sync.Mutex
	cause   terminationCause
	reason  string
	proc    *sandbox.Process
	termSeq sync.Once

	turnsUsed      int
	effectiveUnits int64
}

func (jc *jobControl) setProcess(p *sandbox.Process) {
	jc.mu.Lock()
	jc.proc = p
	shouldTerm := jc.cause != causeNone
	jc.mu.Unlock()
	if shouldTerm {
		jc.terminateProcess(p)
	}
}

func (jc *jobControl) trigger(cause terminationCause, reason string) {
	jc.mu.Lock()
	if jc.cause == causeNone {
		jc.cause = cause
		jc.reason = reason
	}
	p := jc.proc
	jc.mu.Unlock()
	if p != nil {
		jc.terminateProcess(p)
	}
}

func (jc *jobControl) terminateProcess(p *sandbox.Process) {
	jc.termSeq.Do(func() {
		_ = p.Terminate(processTerminateGrace)
	})
}

func (jc *jobControl) updateProgress(turns int, units int64) {
	jc.mu.Lock()
	jc.turnsUsed = turns
	jc.effectiveUnits = units
	jc.mu.Unlock()
}

func (jc *jobControl) progress(elapsed time.Duration) protocol.JobProgress {
	jc.mu.Lock()
	defer jc.mu.Unlock()
	return protocol.JobProgress{
		TurnsUsed:      jc.turnsUsed,
		EffectiveUnits: jc.effectiveUnits,
		ElapsedMs:      elapsed.Milliseconds(),
	}
}

func (jc *jobControl) outcome() (terminationCause, string) {
	jc.mu.Lock()
	defer jc.mu.Unlock()
	return jc.cause, jc.reason
}

type preparedJob struct {
	skill         catalog.Skill
	canonicalArgs []byte
	skillMarkdown string
	limits        catalog.Limits
}

func (d *Daemon) executeJob(ctx context.Context, lease *protocol.LeaseResponse) {
	startedAt := time.Now()
	_, _, preQuota := d.canAccept(ctx)

	wt, err := d.deps.Worktrees.Prepare(ctx, lease.JobID, lease.Repo, lease.DefaultBranch, lease.CatalogSha)
	if err != nil {
		code := protocol.ErrCodeInternal
		if errors.Is(err, ErrNotDefaultBranchAncestor) {
			code = protocol.ErrCodeCatalogMismatch
		}
		d.finishJob(lease, protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusFailed,
			ErrorCode:    code,
			ErrorMessage: fmt.Sprintf("prepare worktree: %v", err),
		}, preQuota, startedAt)
		return
	}
	defer func() {
		if cleanupErr := wt.Cleanup(); cleanupErr != nil {
			d.deps.Logger.Warn("worktree cleanup failed", "job_id", lease.JobID, "error", cleanupErr)
		}
	}()

	prep, errCode, err := d.validateLocalCatalog(wt.Path, lease)
	if err != nil {
		d.finishJob(lease, protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusFailed,
			ErrorCode:    errCode,
			ErrorMessage: err.Error(),
		}, preQuota, startedAt)
		return
	}

	req := d.runSandboxedJob(ctx, lease, wt.Path, prep, startedAt)
	if cleanupErr := wt.Cleanup(); cleanupErr != nil {
		d.deps.Logger.Warn("worktree cleanup failed", "job_id", lease.JobID, "error", cleanupErr)
	}
	d.finishJob(lease, req, preQuota, startedAt)
}

func (d *Daemon) validateLocalCatalog(worktreePath string, lease *protocol.LeaseResponse) (preparedJob, protocol.ErrorCode, error) {
	cat, err := catalog.LoadFromDir(worktreePath)
	if err != nil {
		return preparedJob{}, protocol.ErrCodeCatalogMismatch, fmt.Errorf("load local catalog: %w", err)
	}
	skill, ok := cat.Skills[lease.Skill]
	if !ok {
		return preparedJob{}, protocol.ErrCodeCatalogMismatch, fmt.Errorf("skill %q not found in local catalog", lease.Skill)
	}
	if skill.Mode != catalog.ModeReadOnly {
		return preparedJob{}, protocol.ErrCodeCatalogMismatch, fmt.Errorf("skill %q mode %q is not readonly", lease.Skill, skill.Mode)
	}
	if !d.isSkillLocallyAllowed(lease.Repo, lease.Skill) {
		return preparedJob{}, protocol.ErrCodeCatalogMismatch, fmt.Errorf("skill %q is not in local worker allowlist for %q", lease.Skill, lease.Repo)
	}

	rawArgs := lease.Args
	if len(bytes.TrimSpace(rawArgs)) == 0 {
		rawArgs = json.RawMessage(`{}`)
	}
	canonicalArgs, err := skill.ValidateArgs(rawArgs)
	if err != nil {
		return preparedJob{}, protocol.ErrCodeInvalidArgs, fmt.Errorf("validate local args: %w", err)
	}

	skillMDBytes, err := os.ReadFile(filepath.Join(worktreePath, filepath.FromSlash(skill.SkillPath)))
	if err != nil {
		return preparedJob{}, protocol.ErrCodeCatalogMismatch, fmt.Errorf("read SKILL.md: %w", err)
	}

	return preparedJob{
		skill:         skill,
		canonicalArgs: canonicalArgs,
		skillMarkdown: string(skillMDBytes),
		limits:        d.cfg.EffectiveLimits(lease.Skill, skill.Limits),
	}, "", nil
}

func (d *Daemon) isSkillLocallyAllowed(repo, skill string) bool {
	allowed := d.cfg.Worker.SupportedSkills[repo]
	if len(allowed) == 0 {
		return true
	}
	return slices.Contains(allowed, skill)
}

func (d *Daemon) runSandboxedJob(ctx context.Context, lease *protocol.LeaseResponse, worktreePath string, prep preparedJob, startedAt time.Time) protocol.CompleteJobRequest {
	inv, sandboxCleanup, err := d.prepareSandboxInvocation(lease.Skill, worktreePath, prep)
	if err != nil {
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusFailed,
			ErrorCode:    protocol.ErrCodeInternal,
			ErrorMessage: err.Error(),
		}
	}
	defer func() {
		if cleanupErr := sandboxCleanup(); cleanupErr != nil {
			d.deps.Logger.Warn("sandbox cleanup failed", "job_id", lease.JobID, "error", cleanupErr)
		}
	}()

	result, cause, causeReason, parseErr := d.executeProcess(ctx, lease, inv, prep.limits, startedAt)
	elapsedMs := time.Since(startedAt).Milliseconds()
	usage := buildUsageReport(result, elapsedMs, d.deps.Harness.Name())

	return d.buildCompletionRequest(lease, worktreePath, prep.skill, result, parseErr, cause, causeReason, usage)
}

func (d *Daemon) prepareSandboxInvocation(skillName, worktreePath string, prep preparedJob) (sandbox.Invocation, func() error, error) {
	emptyMCPPath := filepath.Join(d.cfg.Worker.IsolatedHome, "empty-mcp.json")
	cmdSpec, err := d.deps.Harness.BuildCommand(harness.JobSpec{
		SkillName:        skillName,
		SkillMarkdown:    prep.skillMarkdown,
		CanonicalArgs:    prep.canonicalArgs,
		OutputSchema:     prep.skill.OutputSchema,
		EmptyMCPPath:     emptyMCPPath,
		MaxTurns:         prep.limits.MaxTurns,
		MaxEffectiveToks: prep.limits.MaxEffectiveTokens,
	})
	if err != nil {
		return sandbox.Invocation{}, nil, fmt.Errorf("build harness command: %w", err)
	}

	claudeDir := filepath.Join(d.cfg.Worker.IsolatedHome, ".claude")
	oauthToken, err := sandbox.ReadIsolatedOAuthToken(claudeDir)
	if err != nil {
		d.deps.Logger.Warn("read isolated OAuth token failed", "error", err)
	}

	inv, cleanup, err := d.deps.Sandbox.Prepare(sandbox.Spec{
		Agent:            cmdSpec.Agent,
		ProjectDir:       worktreePath,
		IsolatedHome:     d.cfg.Worker.IsolatedHome,
		RealHome:         d.deps.HostCtx.HomeDir,
		ClaudeConfigDir:  claudeDir,
		ClaudeOAuthToken: oauthToken,
		GitHubAuth:       d.cfg.Worker.GitHubAuth,
		AgentArgs:        cmdSpec.AgentArgs,
	})
	if err != nil {
		return sandbox.Invocation{}, nil, fmt.Errorf("prepare sandbox: %w", err)
	}
	return inv, cleanup, nil
}

func (d *Daemon) executeProcess(ctx context.Context, lease *protocol.LeaseResponse, inv sandbox.Invocation, limits catalog.Limits, startedAt time.Time) (harness.RunResult, terminationCause, string, error) {
	jc := &jobControl{}
	runCtx, cancelRun := context.WithCancel(ctx)
	defer cancelRun()

	d.registerActiveJob(lease.JobID, func(reason string) {
		jc.trigger(causeCanceled, reason)
	})
	defer d.unregisterActiveJob(lease.JobID)

	proc, err := d.deps.Sandbox.Start(runCtx, inv)
	if err != nil {
		return harness.RunResult{}, causeNone, "", fmt.Errorf("start sandbox: %w", err)
	}
	jc.setProcess(proc)

	var timeoutTimer *time.Timer
	if limits.TimeoutSeconds > 0 {
		timeoutTimer = time.AfterFunc(time.Duration(limits.TimeoutSeconds)*time.Second, func() {
			jc.trigger(causeTimeout, fmt.Sprintf("job exceeded timeout of %ds", limits.TimeoutSeconds))
		})
		defer timeoutTimer.Stop()
	}

	hbCtx, cancelHB := context.WithCancel(runCtx)
	var hbWG sync.WaitGroup
	hbWG.Add(1)
	go func() {
		defer hbWG.Done()
		d.runJobHeartbeatLoop(hbCtx, lease, jc, startedAt)
	}()

	var stderrWG sync.WaitGroup
	stderrWG.Add(1)
	go func() {
		defer stderrWG.Done()
		if proc.Stderr != nil {
			_, _ = io.Copy(io.Discard, io.LimitReader(proc.Stderr, maxStderrCaptureBytes))
			_ = proc.Stderr.Close()
		}
	}()

	cb := harness.StreamCallbacks{
		OnProgress: func(p harness.StreamProgress) {
			jc.updateProgress(p.TurnsUsed, p.EffectiveUnits)
		},
		OnBudgetExceeded: func(p harness.StreamProgress, reason string) {
			jc.updateProgress(p.TurnsUsed, p.EffectiveUnits)
			jc.trigger(causeBudget, reason)
		},
	}

	result, parseErr := d.deps.Harness.ParseStream(proc.Stdout, limits, cb)
	if proc.Stdout != nil {
		_ = proc.Stdout.Close()
	}
	_ = proc.Wait()
	stderrWG.Wait()
	cancelHB()
	hbWG.Wait()

	cause, reason := jc.outcome()
	if parseErr != nil {
		parseErr = fmt.Errorf("parse harness stream: %w", parseErr)
	}
	return result, cause, reason, parseErr
}

func (d *Daemon) runJobHeartbeatLoop(ctx context.Context, lease *protocol.LeaseResponse, jc *jobControl, startedAt time.Time) {
	interval := d.cfg.Worker.JobHeartbeatInterval
	if interval <= 0 {
		interval = defaultJobHeartbeatInterval
	}

	sendOnce := func() {
		resp, err := d.deps.Client.JobHeartbeat(ctx, lease.JobID, protocol.JobHeartbeatRequest{
			LeaseToken: lease.LeaseToken,
			Progress:   jc.progress(time.Since(startedAt)),
		})
		if err != nil {
			if ctx.Err() == nil {
				d.deps.Logger.Warn("job heartbeat failed", "job_id", lease.JobID, "error", err)
			}
			return
		}
		if resp.Cancel {
			jc.trigger(causeCanceled, "canceled by requester")
		}
	}

	sendOnce()
	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			sendOnce()
		}
	}
}

func (d *Daemon) buildCompletionRequest(
	lease *protocol.LeaseResponse,
	worktreePath string,
	skill catalog.Skill,
	result harness.RunResult,
	parseErr error,
	cause terminationCause,
	causeReason string,
	usage *protocol.UsageReport,
) protocol.CompleteJobRequest {
	if cause == causeBudget || result.BudgetExceeded {
		msg := causeReason
		if msg == "" {
			msg = result.ExceedReason
		}
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusBudgetExceeded,
			UsageReport:  usage,
			ErrorMessage: msg,
		}
	}
	if cause == causeTimeout {
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusTimedOut,
			UsageReport:  usage,
			ErrorMessage: causeReason,
		}
	}
	if cause == causeCanceled {
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusCanceled,
			UsageReport:  usage,
			ErrorMessage: causeReason,
		}
	}
	if result.IsError || parseErr != nil {
		msg := result.ErrorMessage
		if msg == "" && parseErr != nil {
			msg = parseErr.Error()
		}
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusFailed,
			UsageReport:  usage,
			ErrorCode:    protocol.ErrCodeInternal,
			ErrorMessage: msg,
		}
	}
	return d.validateAndScanDeliverable(lease, worktreePath, skill, result.Deliverable, usage)
}

func (d *Daemon) validateAndScanDeliverable(
	lease *protocol.LeaseResponse,
	worktreePath string,
	skill catalog.Skill,
	deliverable json.RawMessage,
	usage *protocol.UsageReport,
) protocol.CompleteJobRequest {
	if err := skill.ValidateOutput(deliverable); err != nil {
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusFailed,
			UsageReport:  usage,
			ErrorCode:    protocol.ErrCodeOutputInvalid,
			ErrorMessage: err.Error(),
		}
	}

	hostCtx := d.deps.HostCtx
	hostCtx.WorktreePath = worktreePath
	dlpRes := d.deps.DLP.Scan(deliverable, hostCtx)
	if dlpRes.Blocked {
		if qErr := d.quarantineDeliverable(lease.JobID, deliverable); qErr != nil {
			d.deps.Logger.Warn("failed to quarantine blocked deliverable", "job_id", lease.JobID, "error", qErr)
		}
		return protocol.CompleteJobRequest{
			LeaseToken:   lease.LeaseToken,
			Status:       protocol.JobStatusDLPBlocked,
			DlpChecked:   true,
			UsageReport:  usage,
			ErrorCode:    protocol.ErrCodeDLPBlocked,
			ErrorMessage: formatStage1DLPFindings(dlpRes.Findings),
		}
	}

	return protocol.CompleteJobRequest{
		LeaseToken:  lease.LeaseToken,
		Status:      protocol.JobStatusSucceeded,
		Deliverable: dlpRes.Normalized,
		DlpChecked:  true,
		UsageReport: usage,
	}
}

func (d *Daemon) quarantineDeliverable(jobID string, raw []byte) error {
	qDir := filepath.Join(d.cfg.Worker.StateDir, "quarantine")
	if err := os.MkdirAll(qDir, 0o700); err != nil {
		return fmt.Errorf("create quarantine dir: %w", err)
	}
	qPath := filepath.Join(qDir, jobID+".json")
	if err := os.WriteFile(qPath, raw, 0o600); err != nil {
		return fmt.Errorf("write quarantine file: %w", err)
	}
	return nil
}

func (d *Daemon) finishJob(lease *protocol.LeaseResponse, req protocol.CompleteJobRequest, preQuota protocol.QuotaHealth, startedAt time.Time) {
	finishCtx, cancel := context.WithTimeout(context.Background(), completeCallTimeout)
	defer cancel()

	finalStatus := req.Status
	if resp, err := d.deps.Client.CompleteJob(finishCtx, lease.JobID, req); err != nil {
		d.deps.Logger.Error("complete job RPC failed", "job_id", lease.JobID, "status", req.Status, "error", err)
	} else {
		finalStatus = resp.Status
	}

	var postQuota protocol.QuotaHealth
	if d.deps.Quota != nil {
		if refreshed, err := d.deps.Quota.Refresh(finishCtx); err != nil {
			d.deps.Logger.Warn("post-job quota refresh failed", "job_id", lease.JobID, "error", err)
		} else {
			postQuota = refreshed
		}
	}

	if d.deps.Ledger != nil {
		rec := buildRunRecord(lease, finalStatus, req.UsageReport, preQuota, postQuota, startedAt)
		if err := d.deps.Ledger.RecordRun(finishCtx, rec); err != nil {
			d.deps.Logger.Warn("record ledger run failed", "job_id", lease.JobID, "error", err)
		}
	}
}

func buildUsageReport(res harness.RunResult, elapsedMs int64, fallbackProvider string) *protocol.UsageReport {
	provider := res.Provider
	if provider == "" {
		provider = fallbackProvider
	}
	effective := res.EffectiveUnits
	if effective == 0 {
		effective = res.Tokens.EffectiveUnits()
	}
	return &protocol.UsageReport{
		Provider:       provider,
		Model:          res.Model,
		Tokens:         res.Tokens,
		EffectiveUnits: effective,
		TurnsUsed:      res.TurnsUsed,
		DurationMs:     elapsedMs,
	}
}

func buildRunRecord(
	lease *protocol.LeaseResponse,
	status protocol.JobStatus,
	usage *protocol.UsageReport,
	preQuota protocol.QuotaHealth,
	postQuota protocol.QuotaHealth,
	startedAt time.Time,
) quota.RunRecord {
	rec := quota.RunRecord{
		JobID:           lease.JobID,
		Repo:            lease.Repo,
		Skill:           lease.Skill,
		Status:          status,
		DurationMs:      time.Since(startedAt).Milliseconds(),
		PreFiveHourPct:  preQuota.FiveHourRemainingPct,
		PostFiveHourPct: postQuota.FiveHourRemainingPct,
		PreSevenDayPct:  preQuota.SevenDayRemainingPct,
		PostSevenDayPct: postQuota.SevenDayRemainingPct,
		FinishedAt:      time.Now().UTC(),
	}
	if usage != nil {
		rec.Provider = usage.Provider
		rec.Model = usage.Model
		rec.InputTokens = usage.Tokens.Input
		rec.CacheReadTokens = usage.Tokens.CacheRead
		rec.CacheCreationTokens = usage.Tokens.CacheCreation
		rec.OutputTokens = usage.Tokens.Output
		rec.EffectiveUnits = usage.EffectiveUnits
		rec.TurnsUsed = usage.TurnsUsed
		rec.DurationMs = usage.DurationMs
	}
	return rec
}

func formatStage1DLPFindings(findings []dlp.Finding) string {
	parts := make([]string, 0, len(findings))
	for _, f := range findings {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Rule, f.Detail))
	}
	return "deliverable blocked by stage-1 DLP (" + strings.Join(parts, "; ") + ")"
}
