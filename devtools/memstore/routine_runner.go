package memstore

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"golang.org/x/sys/unix"
)

// RoutineRunOptions configures RunRoutine and Tick execution.
type RoutineRunOptions struct {
	IdempotencyKey string
	Trigger        string
	DryRun         bool
	Now            func() time.Time
	Decide         DecideSubmitter
	Actor          Actor
}

// RoutineRunResult summarizes the outcome of a single routine evaluation or run.
type RoutineRunResult struct {
	RoutineID      string    `json:"routineId"`
	RunID          string    `json:"runId,omitempty"`
	IdempotencyKey string    `json:"idempotencyKey"`
	Status         string    `json:"status"`
	SkippedReason  string    `json:"skippedReason,omitempty"`
	DryRun         bool      `json:"dryRun,omitempty"`
	ExitCode       *int      `json:"exitCode,omitempty"`
	EventsAdded    int       `json:"eventsAdded,omitempty"`
	FactsSet       int       `json:"factsSet,omitempty"`
	ProposalsAdded int       `json:"proposalsAdded,omitempty"`
	OutboxAdded    int       `json:"outboxAdded,omitempty"`
	StateUpdated   int       `json:"stateUpdated,omitempty"`
	ErrorSummary   string    `json:"errorSummary,omitempty"`
	StartedAt      time.Time `json:"startedAt,omitempty"`
}

// RoutineHandlerInput is the JSON envelope written to a routine handler's stdin.
type RoutineHandlerInput struct {
	RoutineID string         `json:"routineId"`
	RunID     string         `json:"runId"`
	Profile   string         `json:"profile"`
	State     map[string]any `json:"state"`
	NowISO    string         `json:"nowIso"`
}

// RoutineHandlerOutput is the optional JSON payload read from a routine handler's stdout.
type RoutineHandlerOutput struct {
	StateUpdates  map[string]any      `json:"stateUpdates,omitempty"`
	Events        []EventInput        `json:"events,omitempty"`
	Facts         []FactInput         `json:"facts,omitempty"`
	Proposals     []TriageInput       `json:"proposals,omitempty"`
	OutboxActions []OutboxActionInput `json:"outboxActions,omitempty"`
	Metrics       map[string]any      `json:"metrics,omitempty"`
}

// OutboxActionInput describes a side-effecting action staged by a routine handler.
type OutboxActionInput struct {
	ActionKind  string `json:"actionKind"`
	PayloadJSON string `json:"payloadJson,omitempty"`
	Payload     any    `json:"payload,omitempty"`
	Question    string `json:"question,omitempty"`
	Verbatim    string `json:"verbatim,omitempty"`
	Owner       string `json:"owner,omitempty"`
	Parent      string `json:"parent,omitempty"`
}

// RunRoutine executes a single routine manifest under its declared profile policy,
// acquiring a non-blocking per-routine flock, enforcing idempotency, running the
// handler with a JSON stdin envelope, and applying policy-checked outputs.
func RunRoutine(ctx context.Context, cfg Config, manifest RoutineManifest, opts RoutineRunOptions) (RoutineRunResult, error) {
	nowFn := opts.Now
	if nowFn == nil {
		nowFn = time.Now
	}
	now := nowFn().UTC()

	vc, err := validateRoutineTarget(cfg, manifest)
	if err != nil {
		return RoutineRunResult{}, err
	}

	idemKey := opts.IdempotencyKey
	if idemKey == "" {
		idemKey, err = computeIdempotencyKey(manifest, now)
		if err != nil {
			return RoutineRunResult{}, err
		}
	}

	if opts.DryRun {
		return RoutineRunResult{
			RoutineID:      manifest.ID,
			IdempotencyKey: idemKey,
			Status:         "dry-run",
			DryRun:         true,
			StartedAt:      now,
		}, nil
	}

	unlock, locked, err := acquireRoutineLock(filepath.Dir(vc.Path), manifest.ID)
	if err != nil {
		return RoutineRunResult{}, err
	}
	if !locked {
		return RoutineRunResult{
			RoutineID:      manifest.ID,
			IdempotencyKey: idemKey,
			Status:         RoutineRunSkipped,
			SkippedReason:  "already running",
			StartedAt:      now,
		}, nil
	}
	defer unlock()

	svc, err := OpenService(cfg, manifest.Profile)
	if err != nil {
		return RoutineRunResult{}, err
	}
	defer svc.Close()

	v, err := svc.openVault(manifest.Vault)
	if err != nil {
		return RoutineRunResult{}, err
	}
	v.now = func() time.Time { return now }

	trigger := opts.Trigger
	if trigger == "" {
		trigger = "manual"
	}
	run, err := v.BeginRoutineRun(ctx, manifest.ID, idemKey, trigger)
	if err != nil {
		if errors.Is(err, ErrAlreadyRan) {
			return RoutineRunResult{
				RoutineID:      manifest.ID,
				IdempotencyKey: idemKey,
				Status:         RoutineRunSkipped,
				SkippedReason:  "already ran",
				StartedAt:      now,
			}, nil
		}
		return RoutineRunResult{}, err
	}

	return executeAndCommitRoutine(ctx, svc, v, manifest, run, now, opts)
}

// Tick evaluates all enabled local-residency manifests via IsDue and runs due
// routines. With opts.DryRun, due routines are reported without executing or
// mutating state.
func Tick(ctx context.Context, cfg Config, manifests []RoutineManifest, now time.Time, opts RoutineRunOptions) ([]RoutineRunResult, error) {
	nowUTC := now.UTC()
	var results []RoutineRunResult
	var errs []error

	for _, m := range manifests {
		if !m.Enabled || m.Residency != "local" {
			continue
		}
		due, err := evaluateRoutineDue(ctx, cfg, m, nowUTC)
		if err != nil {
			errs = append(errs, err)
			results = append(results, RoutineRunResult{
				RoutineID:    m.ID,
				Status:       RoutineRunFailed,
				ErrorSummary: err.Error(),
			})
			continue
		}
		if !due {
			continue
		}

		runOpts := opts
		if runOpts.Trigger == "" {
			runOpts.Trigger = "tick"
		}
		if runOpts.Now == nil {
			runOpts.Now = func() time.Time { return nowUTC }
		}
		r, err := RunRoutine(ctx, cfg, m, runOpts)
		if r.RoutineID != "" {
			results = append(results, r)
		}
		if err != nil {
			errs = append(errs, err)
		}
	}

	return results, errors.Join(errs...)
}

func evaluateRoutineDue(ctx context.Context, cfg Config, m RoutineManifest, now time.Time) (bool, error) {
	vc, ok := cfg.Vaults[m.Vault]
	if !ok {
		return false, fmt.Errorf("routine %q: vault %q not found in config", m.ID, m.Vault)
	}
	v, err := OpenVault(vc)
	if err != nil {
		return false, fmt.Errorf("routine %q: open vault %q: %w", m.ID, m.Vault, err)
	}
	defer v.Close()

	runs, err := v.ListRoutineRuns(ctx, RoutineRunFilter{
		RoutineID: m.ID,
		Status:    RoutineRunOK,
		Limit:     1,
	})
	if err != nil {
		return false, fmt.Errorf("routine %q: list runs: %w", m.ID, err)
	}
	var lastRun *time.Time
	if len(runs) > 0 {
		t := runs[0].StartedAt
		lastRun = &t
	}
	due, _, err := IsDue(m.Schedule, m.Timezone, lastRun, now)
	if err != nil {
		return false, fmt.Errorf("routine %q: evaluate schedule: %w", m.ID, err)
	}
	return due, nil
}

func validateRoutineTarget(cfg Config, manifest RoutineManifest) (VaultConfig, error) {
	policy, err := ResolveProfile(cfg, manifest.Profile)
	if err != nil {
		return VaultConfig{}, fmt.Errorf("%w: %w", ErrForbidden, err)
	}
	if !slices.Contains(policy.AllowedVaults(), manifest.Vault) {
		return VaultConfig{}, fmt.Errorf("%w: profile %q does not allow vault %q", ErrForbidden, manifest.Profile, manifest.Vault)
	}
	vc, ok := cfg.Vaults[manifest.Vault]
	if !ok {
		return VaultConfig{}, fmt.Errorf("vault %q not found in config", manifest.Vault)
	}
	return vc, nil
}

func computeIdempotencyKey(m RoutineManifest, now time.Time) (string, error) {
	loc, err := loadScheduleLocation(m.Timezone)
	if err != nil {
		return "", err
	}
	local := now.In(loc)
	switch m.Idempotency {
	case "date":
		return local.Format("2006-01-02"), nil
	case "date-hour":
		return local.Format("2006-01-02T15"), nil
	case "run":
		return newID("idem")
	default:
		return "", fmt.Errorf("unsupported idempotency mode %q", m.Idempotency)
	}
}

func acquireRoutineLock(vaultDir, routineID string) (func(), bool, error) {
	if err := EnsureSecureDir(vaultDir); err != nil {
		return nil, false, err
	}
	lockPath := filepath.Join(vaultDir, ".routine-"+routineID+".lock")
	f, err := os.OpenFile(lockPath, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, false, fmt.Errorf("open routine lock %s: %w", lockPath, err)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		closeErr := f.Close()
		if errors.Is(err, unix.EWOULDBLOCK) {
			if closeErr != nil {
				return nil, false, fmt.Errorf("close routine lock %s: %w", lockPath, closeErr)
			}
			return nil, false, nil
		}
		if closeErr != nil {
			return nil, false, errors.Join(fmt.Errorf("flock %s: %w", lockPath, err), fmt.Errorf("close routine lock %s: %w", lockPath, closeErr))
		}
		return nil, false, fmt.Errorf("flock %s: %w", lockPath, err)
	}
	return func() { _ = f.Close() }, true, nil
}

func executeAndCommitRoutine(
	ctx context.Context,
	svc *Service,
	v *Vault,
	manifest RoutineManifest,
	run RoutineRun,
	now time.Time,
	opts RoutineRunOptions,
) (RoutineRunResult, error) {
	res := RoutineRunResult{
		RoutineID:      manifest.ID,
		RunID:          run.ID,
		IdempotencyKey: run.IdempotencyKey,
		StartedAt:      run.StartedAt,
	}

	stdout, exitCode, execErr := invokeRoutineHandler(ctx, v, manifest, run, now)
	if execErr != nil {
		res.Status = RoutineRunFailed
		res.ExitCode = exitCode
		res.ErrorSummary = execErr.Error()
		if finErr := v.FinishRoutineRun(ctx, run.ID, RoutineRunFailed, exitCode, "", res.ErrorSummary); finErr != nil {
			return res, errors.Join(execErr, finErr)
		}
		return res, execErr
	}

	var metricsJSON string
	if manifest.Handler.OutputFormat == "json" && len(bytes.TrimSpace(stdout)) > 0 {
		var err error
		metricsJSON, err = applyHandlerOutput(ctx, svc, v, manifest, stdout, &res, opts)
		if err != nil {
			res.Status = RoutineRunFailed
			res.ExitCode = exitCode
			res.ErrorSummary = err.Error()
			if finErr := v.FinishRoutineRun(ctx, run.ID, RoutineRunFailed, exitCode, metricsJSON, res.ErrorSummary); finErr != nil {
				return res, errors.Join(err, finErr)
			}
			return res, err
		}
	}

	res.Status = RoutineRunOK
	res.ExitCode = exitCode
	if err := v.FinishRoutineRun(ctx, run.ID, RoutineRunOK, exitCode, metricsJSON, ""); err != nil {
		return res, err
	}
	return res, nil
}

func invokeRoutineHandler(ctx context.Context, v *Vault, manifest RoutineManifest, run RoutineRun, now time.Time) ([]byte, *int, error) {
	rawState, err := v.AllRoutineState(ctx, manifest.ID)
	if err != nil {
		return nil, nil, err
	}
	state := make(map[string]any, len(rawState))
	for k, raw := range rawState {
		var val any
		if err := json.Unmarshal([]byte(raw), &val); err != nil {
			return nil, nil, fmt.Errorf("decode routine state %s/%s: %w", manifest.ID, k, err)
		}
		state[k] = val
	}

	envInput := RoutineHandlerInput{
		RoutineID: manifest.ID,
		RunID:     run.ID,
		Profile:   manifest.Profile,
		State:     state,
		NowISO:    now.UTC().Format(time.RFC3339),
	}
	stdinBytes, err := json.Marshal(envInput)
	if err != nil {
		return nil, nil, fmt.Errorf("marshal handler input: %w", err)
	}

	timeout, err := manifest.TimeoutDuration()
	if err != nil {
		return nil, nil, err
	}
	runCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command -- command comes from trusted local routine manifest
	cmd := exec.CommandContext(runCtx, manifest.Handler.Command[0], manifest.Handler.Command[1:]...) //nolint:gosec // command comes from trusted local routine manifest
	cmd.Stdin = bytes.NewReader(stdinBytes)
	cmd.Env = setEnvVar(os.Environ(), "MEMSTORE_PROFILE", manifest.Profile)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	if err != nil {
		code := extractExitCode(err)
		if errors.Is(runCtx.Err(), context.DeadlineExceeded) {
			return stdout.Bytes(), code, fmt.Errorf("routine %s handler timeout after %s: %w", manifest.ID, timeout, runCtx.Err())
		}
		errMsg := strings.TrimSpace(stderr.String())
		if errMsg != "" {
			return stdout.Bytes(), code, fmt.Errorf("routine %s handler failed: %w: %s", manifest.ID, err, errMsg)
		}
		return stdout.Bytes(), code, fmt.Errorf("routine %s handler failed: %w", manifest.ID, err)
	}
	zero := 0
	return stdout.Bytes(), &zero, nil
}

func applyHandlerOutput(
	ctx context.Context,
	svc *Service,
	v *Vault,
	manifest RoutineManifest,
	stdout []byte,
	res *RoutineRunResult,
	opts RoutineRunOptions,
) (string, error) {
	var out RoutineHandlerOutput
	if err := json.Unmarshal(stdout, &out); err != nil {
		return "", fmt.Errorf("parse handler stdout json: %w", err)
	}

	var metricsJSON string
	if out.Metrics != nil {
		mb, err := json.Marshal(out.Metrics)
		if err != nil {
			return "", fmt.Errorf("marshal handler metrics: %w", err)
		}
		metricsJSON = string(mb)
	}

	actor := opts.Actor
	if actor.Caller == "" {
		actor.Caller = "routine:" + manifest.ID
	}
	if actor.Reason == "" {
		actor.Reason = "routine run " + res.RunID
	}

	if err := applyPolicyWrites(ctx, svc, manifest, out, actor, res); err != nil {
		return metricsJSON, err
	}
	if err := applyOutboxActions(ctx, v, manifest, out.OutboxActions, opts.Decide, res); err != nil {
		return metricsJSON, err
	}
	for k, val := range out.StateUpdates {
		encoded, err := json.Marshal(val)
		if err != nil {
			return metricsJSON, fmt.Errorf("encode state update %q: %w", k, err)
		}
		if err := v.SetRoutineState(ctx, manifest.ID, k, string(encoded)); err != nil {
			return metricsJSON, err
		}
		res.StateUpdated++
	}
	return metricsJSON, nil
}

func applyPolicyWrites(ctx context.Context, svc *Service, manifest RoutineManifest, out RoutineHandlerOutput, actor Actor, res *RoutineRunResult) error {
	for _, ev := range out.Events {
		if ev.Reporter == "" {
			ev.Reporter = "routine:" + manifest.ID
		}
		if _, err := svc.AddEvent(ctx, manifest.Vault, ev, actor); err != nil {
			return err
		}
		res.EventsAdded++
	}
	for _, f := range out.Facts {
		if f.SourceType == "" {
			f.SourceType = "routine"
		}
		if _, err := svc.SetFact(ctx, manifest.Vault, f, actor); err != nil {
			return err
		}
		res.FactsSet++
	}
	for _, p := range out.Proposals {
		if p.Source == "" {
			p.Source = "routine:" + manifest.ID
		}
		if _, err := svc.EnqueueTriage(ctx, manifest.Vault, p); err != nil {
			return err
		}
		res.ProposalsAdded++
	}
	return nil
}

func applyOutboxActions(ctx context.Context, v *Vault, manifest RoutineManifest, actions []OutboxActionInput, submitter DecideSubmitter, res *RoutineRunResult) error {
	if len(actions) == 0 {
		return nil
	}
	if submitter == nil {
		submitter = &ExecDecideSubmitter{}
	}
	for _, act := range actions {
		payloadJSON := act.PayloadJSON
		if payloadJSON == "" && act.Payload != nil {
			b, err := json.Marshal(act.Payload)
			if err != nil {
				return fmt.Errorf("marshal outbox payload: %w", err)
			}
			payloadJSON = string(b)
		}
		owner := act.Owner
		if owner == "" {
			owner = "routine:" + manifest.ID
		}
		question := act.Question
		if question == "" {
			question = fmt.Sprintf("Approve %s action from routine %s?", act.ActionKind, manifest.ID)
		}
		verbatim := act.Verbatim
		if verbatim == "" {
			verbatim = payloadJSON
		}
		decideID, err := submitter.Submit(ctx, DecideRequest{
			Owner:    owner,
			Parent:   act.Parent,
			Question: question,
			Verbatim: verbatim,
		})
		if err != nil {
			return fmt.Errorf("submit decide approval: %w", err)
		}
		if _, err := v.AddRoutineOutbox(ctx, manifest.ID, act.ActionKind, payloadJSON, decideID); err != nil {
			return err
		}
		res.OutboxAdded++
	}
	return nil
}

func extractExitCode(err error) *int {
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		c := exitErr.ExitCode()
		return &c
	}
	return nil
}

func setEnvVar(env []string, key, val string) []string {
	prefix := key + "="
	out := make([]string, 0, len(env)+1)
	for _, entry := range env {
		if !strings.HasPrefix(entry, prefix) {
			out = append(out, entry)
		}
	}
	return append(out, prefix+val)
}
