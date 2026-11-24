package worker

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/httpapi"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/harness"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/sandbox"
)

const reviewPRCatalogJSON = `{
  "version": 1,
  "skills": {
    "review-pr": {
      "description": "Review a pull request.",
      "mode": "readonly",
      "skillPath": ".agent-pool/skills/review-pr/SKILL.md",
      "argsSchema": {
        "type": "object",
        "properties": {
          "prNumber": { "type": "integer", "minimum": 1 }
        },
        "required": ["prNumber"],
        "additionalProperties": false
      },
      "outputSchema": {
        "type": "object",
        "properties": {
          "summary": { "type": "string", "minLength": 1 }
        },
        "required": ["summary"],
        "additionalProperties": false
      },
      "limits": {
        "maxTurns": 20,
        "maxEffectiveTokens": 100000,
        "timeoutSeconds": 60,
        "maxDeliverableBytes": 65536
      }
    }
  }
}
`

type daemonTestHarness struct {
	store     broker.Store
	svc       *broker.Service
	server    *httptest.Server
	prodToken string
	workToken string
	bareRepo  string
	catalogSH string
	stateDir  string
	isoHome   string
	ledger    *quota.Ledger

	mu          sync.Mutex
	seenReqBody []string
}

func setupDaemonTestHarness(t *testing.T) *daemonTestHarness {
	t.Helper()

	root := t.TempDir()
	bareDir := filepath.Join(root, "remote.git")
	workDir := filepath.Join(root, "work")

	runGitTest(t, "", "init", "--bare", bareDir)
	runGitTest(t, bareDir, "symbolic-ref", "HEAD", "refs/heads/main")

	runGitTest(t, "", "init", workDir)
	runGitTest(t, workDir, "checkout", "-b", "main")
	runGitTest(t, workDir, "config", "user.name", "Test User")
	runGitTest(t, workDir, "config", "user.email", "test@example.com")
	runGitTest(t, workDir, "remote", "add", "origin", bareDir)

	writeTestFile(t, filepath.Join(workDir, ".agent-pool", "catalog.json"), reviewPRCatalogJSON)
	writeTestFile(t, filepath.Join(workDir, ".agent-pool", "skills", "review-pr", "SKILL.md"), "# Review PR\n")
	runGitTest(t, workDir, "add", ".")
	runGitTest(t, workDir, "commit", "-m", "add review-pr catalog")
	catalogSHA := strings.TrimSpace(runGitTest(t, workDir, "rev-parse", "HEAD"))
	runGitTest(t, workDir, "push", "origin", "main")

	dbPath := filepath.Join(root, "broker.db")
	st, err := sqlite.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlite.Open() error = %v", err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("store.Close() error = %v", err)
		}
	})

	ctx := context.Background()
	prodToken, _, err := st.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, _, err := st.CreateToken(ctx, broker.TokenRoleWorker, "volunteer-01", []string{"org/core-api"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/core-api",
		DefaultBranch: "main",
		CatalogSha:    catalogSHA,
		CatalogJSON:   []byte(reviewPRCatalogJSON),
		UpdatedAt:     time.Now().UTC(),
	}); err != nil {
		t.Fatalf("UpsertCatalog() error = %v", err)
	}

	hub := broker.NewEventHub()
	svc := broker.NewService(st, dlp.NewScanner(), hub, broker.Config{
		WorkerStaleAfter:          90 * time.Second,
		LeaseTTL:                  60 * time.Second,
		MaxAttempts:               2,
		MaxConcurrentPerRequester: 5,
	})
	baseHandler := httpapi.NewHandler(svc, st, httpapi.Options{
		KeepaliveInterval: 5 * time.Second,
	})

	h := &daemonTestHarness{
		store:     st,
		svc:       svc,
		prodToken: prodToken,
		workToken: workToken,
		bareRepo:  bareDir,
		catalogSH: catalogSHA,
		stateDir:  filepath.Join(root, "state"),
		isoHome:   filepath.Join(root, "isolated-home"),
	}

	recordingHandler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Body != nil {
			raw, readErr := io.ReadAll(r.Body)
			if readErr == nil {
				h.mu.Lock()
				h.seenReqBody = append(h.seenReqBody, string(raw))
				h.mu.Unlock()
				r.Body = io.NopCloser(bytes.NewReader(raw))
			}
		}
		baseHandler.ServeHTTP(w, r)
	})
	h.server = httptest.NewServer(recordingHandler)
	t.Cleanup(h.server.Close)

	if _, err := InitAuth(ctx, InitAuthOptions{
		IsolatedHome: h.isoHome,
		SkipLogin:    true,
	}); err != nil {
		t.Fatalf("InitAuth() error = %v", err)
	}

	if err := os.MkdirAll(h.stateDir, 0o700); err != nil {
		t.Fatalf("MkdirAll(stateDir) error = %v", err)
	}

	ledger, err := quota.OpenLedger(filepath.Join(h.stateDir, "usage.db"))
	if err != nil {
		t.Fatalf("OpenLedger() error = %v", err)
	}
	t.Cleanup(func() {
		if err := ledger.Close(); err != nil {
			t.Errorf("ledger.Close() error = %v", err)
		}
	})
	h.ledger = ledger

	return h
}

func (h *daemonTestHarness) newConfig() *Config {
	cfg := DefaultConfig()
	cfg.Worker.BrokerURL = h.server.URL
	cfg.Worker.Alias = "volunteer-01"
	cfg.Worker.SupportedRepos = []string{"org/core-api"}
	cfg.Worker.MaxConcurrentJobs = 1
	cfg.Worker.GitHubAuth = sandbox.GitHubAuthNone
	cfg.Worker.IsolatedHome = h.isoHome
	cfg.Worker.StateDir = h.stateDir
	cfg.Worker.PollInterval = 5 * time.Second
	cfg.Worker.HeartbeatInterval = 50 * time.Millisecond
	cfg.Worker.JobHeartbeatInterval = 50 * time.Millisecond
	cfg.Worker.DrainTimeout = 5 * time.Second
	cfg.Repos = []RepoRemote{
		{
			Name:          "org/core-api",
			GitURL:        h.bareRepo,
			DefaultBranch: "main",
		},
	}
	return cfg
}

func (h *daemonTestHarness) dispatchReviewPR(t *testing.T, prNumber int) protocol.DispatchResponse {
	t.Helper()
	body, err := json.Marshal(protocol.DispatchRequest{
		Repo:  "org/core-api",
		Skill: "review-pr",
		Args:  json.RawMessage(`{"prNumber":` + jsonNumber(prNumber) + `}`),
	})
	if err != nil {
		t.Fatalf("marshal dispatch request: %v", err)
	}
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost, h.server.URL+"/v1/jobs", bytes.NewReader(body))
	if err != nil {
		t.Fatalf("new dispatch request: %v", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+h.prodToken)

	resp, err := h.server.Client().Do(req)
	if err != nil {
		t.Fatalf("dispatch POST /v1/jobs: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(resp.Body)
		t.Fatalf("POST /v1/jobs status = %d, body = %s", resp.StatusCode, string(raw))
	}
	var out protocol.DispatchResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode dispatch response: %v", err)
	}
	return out
}

func (h *daemonTestHarness) getJob(t *testing.T, jobID string) protocol.JobResultResponse {
	t.Helper()
	req, err := http.NewRequestWithContext(context.Background(), http.MethodGet, h.server.URL+"/v1/jobs/"+jobID, nil)
	if err != nil {
		t.Fatalf("new getJob request: %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+h.prodToken)
	resp, err := h.server.Client().Do(req)
	if err != nil {
		t.Fatalf("GET /v1/jobs/%s: %v", jobID, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(resp.Body)
		t.Fatalf("GET /v1/jobs/%s status = %d, body = %s", jobID, resp.StatusCode, string(raw))
	}
	var out protocol.JobResultResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode job result: %v", err)
	}
	return out
}

func (h *daemonTestHarness) cancelJob(t *testing.T, jobID string) protocol.CancelJobResponse {
	t.Helper()
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost, h.server.URL+"/v1/jobs/"+jobID+"/cancel", nil)
	if err != nil {
		t.Fatalf("new cancel request: %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+h.prodToken)
	resp, err := h.server.Client().Do(req)
	if err != nil {
		t.Fatalf("POST /v1/jobs/%s/cancel: %v", jobID, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(resp.Body)
		t.Fatalf("cancel status = %d, body = %s", resp.StatusCode, string(raw))
	}
	var out protocol.CancelJobResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode cancel response: %v", err)
	}
	return out
}

func (h *daemonTestHarness) waitForActiveWorkers(t *testing.T, want int) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		st := h.getPoolStatus(t)
		if st.ActiveWorkers == want {
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
	st := h.getPoolStatus(t)
	t.Fatalf("timed out waiting for ActiveWorkers=%d, got %d (health=%s)", want, st.ActiveWorkers, st.PoolHealth)
}

func (h *daemonTestHarness) getPoolStatus(t *testing.T) protocol.PoolStatusResponse {
	t.Helper()
	req, err := http.NewRequestWithContext(context.Background(), http.MethodGet, h.server.URL+"/v1/status", nil)
	if err != nil {
		t.Fatalf("new status request: %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+h.prodToken)
	resp, err := h.server.Client().Do(req)
	if err != nil {
		t.Fatalf("GET /v1/status: %v", err)
	}
	defer resp.Body.Close()
	var out protocol.PoolStatusResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode pool status: %v", err)
	}
	return out
}

func (h *daemonTestHarness) waitForTerminalJob(t *testing.T, jobID string) protocol.JobResultResponse {
	t.Helper()
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		res := h.getJob(t, jobID)
		if res.Status.IsTerminal() {
			return res
		}
		time.Sleep(25 * time.Millisecond)
	}
	res := h.getJob(t, jobID)
	t.Fatalf("timed out waiting for job %s to reach terminal state; last status = %s", jobID, res.Status)
	return res
}

func jsonNumber(n int) string {
	raw, _ := json.Marshal(n)
	return string(raw)
}

type fakeQuotaGuard struct {
	mu      sync.Mutex
	allow   bool
	reason  string
	fiveHr  float64
	sevenD  float64
	refresh atomic.Int32
}

func newFakeQuotaGuard(allow bool) *fakeQuotaGuard {
	return &fakeQuotaGuard{
		allow:  allow,
		fiveHr: 80.0,
		sevenD: 75.0,
	}
}

func (g *fakeQuotaGuard) SetAllow(allow bool, reason string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.allow = allow
	g.reason = reason
}

func (g *fakeQuotaGuard) CanAccept(_ context.Context) quota.Decision {
	g.mu.Lock()
	defer g.mu.Unlock()
	five := g.fiveHr
	seven := g.sevenD
	state := "green"
	if !g.allow {
		state = "red"
	}
	return quota.Decision{
		Allow:  g.allow,
		Reason: g.reason,
		Health: protocol.QuotaHealth{
			State:                state,
			FiveHourRemainingPct: &five,
			SevenDayRemainingPct: &seven,
		},
	}
}

func (g *fakeQuotaGuard) Refresh(_ context.Context) (protocol.QuotaHealth, error) {
	g.refresh.Add(1)
	g.mu.Lock()
	defer g.mu.Unlock()
	five := g.fiveHr - 2.0
	seven := g.sevenD - 1.0
	return protocol.QuotaHealth{
		State:                "green",
		FiveHourRemainingPct: &five,
		SevenDayRemainingPct: &seven,
	}, nil
}

type fakeSandboxRunner struct {
	script    string
	realStart sandbox.CpltRunner

	mu           sync.Mutex
	lastProject  string
	startedCh    chan struct{}
	startOnce    sync.Once
	prepareCount atomic.Int32
}

func newFakeSandboxRunner(script string) *fakeSandboxRunner {
	return &fakeSandboxRunner{
		script:    script,
		startedCh: make(chan struct{}),
	}
}

func (f *fakeSandboxRunner) Prepare(spec sandbox.Spec) (sandbox.Invocation, func() error, error) {
	f.prepareCount.Add(1)
	f.mu.Lock()
	f.lastProject = spec.ProjectDir
	f.mu.Unlock()
	inv := sandbox.Invocation{
		Binary: "/bin/sh",
		Args:   []string{"-c", f.script},
		Dir:    spec.ProjectDir,
	}
	return inv, func() error { return nil }, nil
}

func (f *fakeSandboxRunner) Start(ctx context.Context, inv sandbox.Invocation) (*sandbox.Process, error) {
	proc, err := f.realStart.Start(ctx, inv)
	if err != nil {
		return nil, fmt.Errorf("fake sandbox start: %w", err)
	}
	f.startOnce.Do(func() { close(f.startedCh) })
	return proc, nil
}

func (f *fakeSandboxRunner) ProjectDir() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.lastProject
}

type fakeHarnessRunner struct {
	parseFn func(r io.Reader, limits catalog.Limits, cb harness.StreamCallbacks) (harness.RunResult, error)
}

func (f *fakeHarnessRunner) Name() string {
	return "claude"
}

func (f *fakeHarnessRunner) BuildCommand(_ harness.JobSpec) (harness.CommandSpec, error) {
	return harness.CommandSpec{
		Agent:     "claude",
		AgentArgs: []string{"-p", "test"},
	}, nil
}

func (f *fakeHarnessRunner) ParseStream(r io.Reader, limits catalog.Limits, cb harness.StreamCallbacks) (harness.RunResult, error) {
	return f.parseFn(r, limits, cb)
}

func startTestDaemon(t *testing.T, d *Daemon) func() {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	errCh := make(chan error, 1)
	go func() {
		errCh <- d.Run(ctx)
	}()
	var stopOnce sync.Once
	stop := func() {
		stopOnce.Do(func() {
			cancel()
			select {
			case err := <-errCh:
				if err != nil {
					t.Errorf("daemon Run() error = %v", err)
				}
			case <-time.After(5 * time.Second):
				t.Errorf("daemon did not stop within 5s")
			}
		})
	}
	t.Cleanup(stop)
	return stop
}

func TestDaemonHappyPathEndToEnd(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(true)
	sb := newFakeSandboxRunner(`printf "done\n"`)
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, cb harness.StreamCallbacks) (harness.RunResult, error) {
			_, _ = io.ReadAll(r)
			if cb.OnProgress != nil {
				cb.OnProgress(harness.StreamProgress{
					TurnsUsed:      2,
					EffectiveUnits: 1200,
					Model:          "claude-sonnet-4-6",
				})
			}
			wtPath := sb.ProjectDir()
			payload, err := json.Marshal(map[string]string{
				"summary": "Reviewed file at " + filepath.Join(wtPath, "main.go"),
			})
			if err != nil {
				return harness.RunResult{}, fmt.Errorf("marshal deliverable: %w", err)
			}
			return harness.RunResult{
				Provider:       "claude",
				Model:          "claude-sonnet-4-6",
				Deliverable:    payload,
				Tokens:         protocol.TokenUsage{Input: 500, Output: 140},
				EffectiveUnits: 1200,
				TurnsUsed:      2,
			}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		HostCtx:   dlp.HostContext{HomeDir: "/Users/testvolunteer", Username: "testvolunteer", Hostname: "test-workstation"},
		Logger:    testLogger(),
	})

	stop := startTestDaemon(t, d)
	h.waitForActiveWorkers(t, 1)

	disp := h.dispatchReviewPR(t, 42)
	res := h.waitForTerminalJob(t, disp.JobID)
	stop()

	if res.Status != protocol.JobStatusSucceeded {
		t.Fatalf("job status = %s, want SUCCEEDED (errCode=%s, errMsg=%s)", res.Status, res.ErrorCode, res.ErrorMessage)
	}
	var deliverable struct {
		Summary string `json:"summary"`
	}
	if err := json.Unmarshal(res.Deliverable, &deliverable); err != nil {
		t.Fatalf("unmarshal deliverable %s: %v", string(res.Deliverable), err)
	}
	if !strings.Contains(deliverable.Summary, "<repo>/main.go") {
		t.Errorf("deliverable.Summary = %q, want normalized <repo>/main.go path", deliverable.Summary)
	}
	if wtPath := sb.ProjectDir(); wtPath == "" {
		t.Fatal("expected sandbox to receive worktree project dir")
	} else if _, err := os.Stat(wtPath); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("expected worktree %q to be removed after job, stat err = %v", wtPath, err)
	}

	stats, err := h.ledger.Stats(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("ledger.Stats() error = %v", err)
	}
	if stats.TotalJobs != 1 {
		t.Fatalf("ledger TotalJobs = %d, want 1", stats.TotalJobs)
	}
	if stats.EffectiveUnits != 1200 {
		t.Errorf("ledger EffectiveUnits = %d, want 1200", stats.EffectiveUnits)
	}
	if guard.refresh.Load() < 1 {
		t.Errorf("expected post-job Quota.Refresh to be called")
	}
}

func TestDaemonMidFlightBudgetKill(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(true)
	sb := newFakeSandboxRunner(`printf "tick\n"; sleep 30`)

	var streamEOF atomic.Bool
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, cb harness.StreamCallbacks) (harness.RunResult, error) {
			sc := bufio.NewScanner(r)
			if sc.Scan() && cb.OnBudgetExceeded != nil {
				cb.OnBudgetExceeded(harness.StreamProgress{
					TurnsUsed:      3,
					EffectiveUnits: 200000,
					Model:          "claude-sonnet-4-6",
				}, "effective tokens 200000 exceeded cap 100000")
			}
			_, _ = io.Copy(io.Discard, r)
			streamEOF.Store(true)
			return harness.RunResult{
				Provider:       "claude",
				Model:          "claude-sonnet-4-6",
				EffectiveUnits: 200000,
				TurnsUsed:      3,
				BudgetExceeded: true,
				ExceedReason:   "effective tokens 200000 exceeded cap 100000",
			}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		Logger:    testLogger(),
	})

	stop := startTestDaemon(t, d)
	h.waitForActiveWorkers(t, 1)

	start := time.Now()
	disp := h.dispatchReviewPR(t, 7)
	res := h.waitForTerminalJob(t, disp.JobID)
	elapsed := time.Since(start)
	stop()

	if res.Status != protocol.JobStatusBudgetExceeded {
		t.Fatalf("job status = %s, want BUDGET_EXCEEDED (errMsg=%s)", res.Status, res.ErrorMessage)
	}
	if !streamEOF.Load() {
		t.Errorf("expected stream to reach EOF after Terminate killed process group")
	}
	if elapsed > 8*time.Second {
		t.Errorf("mid-flight budget kill took %v, expected fast process-group termination", elapsed)
	}
	if wtPath := sb.ProjectDir(); wtPath != "" {
		if _, err := os.Stat(wtPath); !errors.Is(err, os.ErrNotExist) {
			t.Errorf("expected worktree %q to be removed, stat err = %v", wtPath, err)
		}
	}
}

func TestDaemonTimeoutKill(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	cfg.JobDefaults.TimeoutSeconds = 1
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(true)
	sb := newFakeSandboxRunner(`sleep 30`)
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, _ harness.StreamCallbacks) (harness.RunResult, error) {
			_, _ = io.Copy(io.Discard, r)
			return harness.RunResult{Provider: "claude"}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		Logger:    testLogger(),
	})

	startTestDaemon(t, d)
	h.waitForActiveWorkers(t, 1)

	start := time.Now()
	disp := h.dispatchReviewPR(t, 8)
	res := h.waitForTerminalJob(t, disp.JobID)
	elapsed := time.Since(start)

	if res.Status != protocol.JobStatusTimedOut {
		t.Fatalf("job status = %s, want TIMED_OUT (errMsg=%s)", res.Status, res.ErrorMessage)
	}
	if elapsed > 8*time.Second {
		t.Errorf("timeout kill took %v, expected fast termination after 1s timeout", elapsed)
	}
}

func TestDaemonRemoteCancel(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(true)
	sb := newFakeSandboxRunner(`printf "started\n"; sleep 30`)
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, _ harness.StreamCallbacks) (harness.RunResult, error) {
			_, _ = io.Copy(io.Discard, r)
			return harness.RunResult{Provider: "claude"}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		Logger:    testLogger(),
	})

	startTestDaemon(t, d)
	h.waitForActiveWorkers(t, 1)

	disp := h.dispatchReviewPR(t, 9)

	// Wait until the sandbox process has actually started.
	select {
	case <-sb.startedCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timed out waiting for sandbox process to start")
	}

	cancelResp := h.cancelJob(t, disp.JobID)
	if !cancelResp.CancelRequested {
		t.Fatalf("expected CancelRequested=true, got %+v", cancelResp)
	}

	res := h.waitForTerminalJob(t, disp.JobID)
	if res.Status != protocol.JobStatusCanceled {
		t.Fatalf("job status = %s, want CANCELED (errMsg=%s)", res.Status, res.ErrorMessage)
	}
}

func TestDaemonStage1DLPQuarantine(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(true)
	sb := newFakeSandboxRunner(`printf "done\n"`)

	// Dynamically constructed synthetic secret so static secret scanners do not flag test source.
	syntheticSecret := "ghp_" + strings.Repeat("A1b2C3", 6)
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, _ harness.StreamCallbacks) (harness.RunResult, error) {
			_, _ = io.ReadAll(r)
			payload, err := json.Marshal(map[string]string{
				"summary": "Found token " + syntheticSecret,
			})
			if err != nil {
				return harness.RunResult{}, fmt.Errorf("marshal deliverable: %w", err)
			}
			return harness.RunResult{
				Provider:    "claude",
				Model:       "claude-sonnet-4-6",
				Deliverable: payload,
				TurnsUsed:   1,
			}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		Logger:    testLogger(),
	})

	startTestDaemon(t, d)
	h.waitForActiveWorkers(t, 1)

	disp := h.dispatchReviewPR(t, 10)
	res := h.waitForTerminalJob(t, disp.JobID)

	if res.Status != protocol.JobStatusDLPBlocked {
		t.Fatalf("job status = %s, want DLP_BLOCKED (errCode=%s, errMsg=%s)", res.Status, res.ErrorCode, res.ErrorMessage)
	}
	if res.ErrorCode != protocol.ErrCodeDLPBlocked {
		t.Errorf("errorCode = %s, want DLP_BLOCKED", res.ErrorCode)
	}
	if len(res.Deliverable) != 0 {
		t.Errorf("expected empty deliverable on DLP_BLOCKED, got %s", string(res.Deliverable))
	}

	// Verify the synthetic secret was NEVER transmitted over HTTP.
	h.mu.Lock()
	for _, body := range h.seenReqBody {
		if strings.Contains(body, syntheticSecret) {
			t.Errorf("synthetic secret was transmitted over HTTP in request body!")
		}
	}
	h.mu.Unlock()

	// Verify local quarantine file exists with mode 0600 and contains raw output.
	qPath := filepath.Join(cfg.Worker.StateDir, "quarantine", disp.JobID+".json")
	info, err := os.Stat(qPath)
	if err != nil {
		t.Fatalf("stat quarantine file %q: %v", qPath, err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("quarantine file mode = %04o, want 0600", perm)
	}
	raw, err := os.ReadFile(qPath)
	if err != nil {
		t.Fatalf("read quarantine file: %v", err)
	}
	if !strings.Contains(string(raw), syntheticSecret) {
		t.Errorf("expected quarantine file to preserve raw deliverable for local audit")
	}
}

func TestDaemonQuotaPausedAndLocalPause(t *testing.T) {
	h := setupDaemonTestHarness(t)
	cfg := h.newConfig()
	cfg.Worker.PollInterval = 40 * time.Millisecond
	client := NewBrokerClient(h.server.URL, h.workToken, h.server.Client())
	client.SetRepos(cfg.Worker.SupportedRepos)
	guard := newFakeQuotaGuard(false)
	guard.SetAllow(false, "quota exceeded")
	sb := newFakeSandboxRunner(`printf "done\n"`)
	hr := &fakeHarnessRunner{
		parseFn: func(r io.Reader, _ catalog.Limits, _ harness.StreamCallbacks) (harness.RunResult, error) {
			_, _ = io.ReadAll(r)
			return harness.RunResult{
				Provider:    "claude",
				Deliverable: json.RawMessage(`{"summary":"ok"}`),
				TurnsUsed:   1,
			}, nil
		},
	}

	d := NewDaemon(cfg, Deps{
		Client:    client,
		Quota:     guard,
		Ledger:    h.ledger,
		Worktrees: NewWorktreeManager(cfg.Worker.StateDir, cfg.RepoRemotes(), testLogger()),
		Sandbox:   sb,
		Harness:   hr,
		DLP:       dlp.NewScanner(),
		Logger:    testLogger(),
	})

	startTestDaemon(t, d)

	// While quota guard denies, broker sees 0 active accepting workers.
	time.Sleep(150 * time.Millisecond)
	if st := h.getPoolStatus(t); st.ActiveWorkers != 0 {
		t.Fatalf("ActiveWorkers while quota denied = %d, want 0", st.ActiveWorkers)
	}

	// Enable quota, but create local pause file first -> still AcceptingJobs: false.
	if err := Pause(cfg.Worker.StateDir, "maintenance"); err != nil {
		t.Fatalf("Pause() error = %v", err)
	}
	paused, reason := IsPaused(cfg.Worker.StateDir)
	if !paused || reason != "maintenance" {
		t.Fatalf("IsPaused() = (%v, %q), want (true, %q)", paused, reason, "maintenance")
	}
	guard.SetAllow(true, "")

	time.Sleep(150 * time.Millisecond)
	if st := h.getPoolStatus(t); st.ActiveWorkers != 0 {
		t.Fatalf("ActiveWorkers while locally paused = %d, want 0", st.ActiveWorkers)
	}

	// Enqueue a job directly in the store while paused so we can verify daemon does not claim it.
	enqueued, _, err := h.store.EnqueueJob(context.Background(), broker.JobRecord{
		RequesterID:   "req-1",
		Repo:          "org/core-api",
		DefaultBranch: "main",
		CatalogSha:    h.catalogSH,
		Skill:         "review-pr",
		ArgsJSON:      []byte(`{"prNumber":11}`),
		Status:        protocol.JobStatusQueued,
		MaxAttempts:   2,
		CreatedAt:     time.Now().UTC(),
	}, 5)
	if err != nil {
		t.Fatalf("EnqueueJob() error = %v", err)
	}

	time.Sleep(150 * time.Millisecond)
	job, err := h.store.GetJob(context.Background(), enqueued.JobID)
	if err != nil {
		t.Fatalf("GetJob() error = %v", err)
	}
	if job.Status != protocol.JobStatusQueued {
		t.Fatalf("job status while paused = %s, want QUEUED", job.Status)
	}

	// Resume local pause -> daemon heartbeats AcceptingJobs: true and claims + completes the job.
	if err := Resume(cfg.Worker.StateDir); err != nil {
		t.Fatalf("Resume() error = %v", err)
	}
	h.waitForActiveWorkers(t, 1)

	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		job, err = h.store.GetJob(context.Background(), enqueued.JobID)
		if err != nil {
			t.Fatalf("GetJob() error = %v", err)
		}
		if job.Status == protocol.JobStatusSucceeded {
			return
		}
		time.Sleep(25 * time.Millisecond)
	}
	t.Fatalf("timed out waiting for queued job to complete after Resume; status = %s", job.Status)
}

func TestInitAuthCreatesIsolatedLayout(t *testing.T) {
	isoHome := filepath.Join(t.TempDir(), "isolated-home")
	res, err := InitAuth(context.Background(), InitAuthOptions{
		IsolatedHome: isoHome,
		SkipLogin:    true,
	})
	if err != nil {
		t.Fatalf("InitAuth() error = %v", err)
	}

	for _, dir := range []string{
		isoHome,
		filepath.Join(isoHome, ".claude"),
		filepath.Join(isoHome, ".config", "cplt"),
		filepath.Join(isoHome, ".cache"),
	} {
		info, err := os.Stat(dir)
		if err != nil {
			t.Fatalf("stat %q: %v", dir, err)
		}
		if !info.IsDir() {
			t.Errorf("%q is not a directory", dir)
		}
		if perm := info.Mode().Perm(); perm != 0o700 {
			t.Errorf("dir %q mode = %04o, want 0700", dir, perm)
		}
	}

	mcpPath := filepath.Join(isoHome, "empty-mcp.json")
	mcpInfo, err := os.Stat(mcpPath)
	if err != nil {
		t.Fatalf("stat empty-mcp.json: %v", err)
	}
	if perm := mcpInfo.Mode().Perm(); perm != 0o600 {
		t.Errorf("empty-mcp.json mode = %04o, want 0600", perm)
	}
	mcpRaw, err := os.ReadFile(mcpPath)
	if err != nil {
		t.Fatalf("read empty-mcp.json: %v", err)
	}
	if string(mcpRaw) != "{\"mcpServers\":{}}\n" {
		t.Errorf("empty-mcp.json = %q, want %q", string(mcpRaw), "{\"mcpServers\":{}}\n")
	}

	gitCfgPath := filepath.Join(isoHome, ".gitconfig")
	gitInfo, err := os.Stat(gitCfgPath)
	if err != nil {
		t.Fatalf("stat .gitconfig: %v", err)
	}
	if perm := gitInfo.Mode().Perm(); perm != 0o600 {
		t.Errorf(".gitconfig mode = %04o, want 0600", perm)
	}
	gitRaw, err := os.ReadFile(gitCfgPath)
	if err != nil {
		t.Fatalf("read .gitconfig: %v", err)
	}
	gitText := string(gitRaw)
	if strings.Contains(gitText, "signingkey") {
		t.Errorf(".gitconfig should not contain signingkey: %s", gitText)
	}
	if !strings.Contains(gitText, "gpgsign = false") {
		t.Errorf(".gitconfig should disable gpgsign: %s", gitText)
	}
	if !strings.Contains(strings.ToLower(res.Instructions), "connector") {
		t.Errorf("Instructions should remind volunteer about cloud connectors, got: %q", res.Instructions)
	}
}

func TestEffectiveLimitsTakesMinimum(t *testing.T) {
	cfg := DefaultConfig()
	cfg.JobDefaults = JobLimits{
		MaxTurns:            25,
		MaxEffectiveTokens:  150000,
		TimeoutSeconds:      300,
		MaxDeliverableBytes: 131072,
	}
	cfg.SkillOverrides = map[string]JobLimits{
		"review-pr": {
			MaxTurns:           20,
			MaxEffectiveTokens: 120000,
			TimeoutSeconds:     240,
		},
	}

	// Looser repo catalog limits must be capped by stricter local worker limits.
	looseCatalog := catalog.Limits{
		MaxTurns:            80,
		MaxEffectiveTokens:  500000,
		TimeoutSeconds:      900,
		MaxDeliverableBytes: 524288,
	}
	got := cfg.EffectiveLimits("review-pr", looseCatalog)
	if got.MaxTurns != 20 {
		t.Errorf("MaxTurns = %d, want 20", got.MaxTurns)
	}
	if got.MaxEffectiveTokens != 120000 {
		t.Errorf("MaxEffectiveTokens = %d, want 120000", got.MaxEffectiveTokens)
	}
	if got.TimeoutSeconds != 240 {
		t.Errorf("TimeoutSeconds = %d, want 240", got.TimeoutSeconds)
	}
	if got.MaxDeliverableBytes != 131072 {
		t.Errorf("MaxDeliverableBytes = %d, want 131072", got.MaxDeliverableBytes)
	}

	// Stricter repo catalog limits must be respected even when local caps are looser.
	strictCatalog := catalog.Limits{
		MaxTurns:            10,
		MaxEffectiveTokens:  50000,
		TimeoutSeconds:      120,
		MaxDeliverableBytes: 32768,
	}
	gotStrict := cfg.EffectiveLimits("review-pr", strictCatalog)
	if gotStrict != strictCatalog {
		t.Errorf("EffectiveLimits(strict) = %+v, want %+v", gotStrict, strictCatalog)
	}
}

func TestLoadConfigTOMLAndXDGDefaults(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("XDG_DATA_HOME", filepath.Join(tmp, "data"))
	t.Setenv("XDG_STATE_HOME", filepath.Join(tmp, "state"))

	cfgPath := filepath.Join(tmp, "worker.toml")
	content := `
[worker]
broker_url = "https://agent-pool.internal.example.com"
token_env = "AGENT_POOL_WORKER_TOKEN"
alias = "volunteer-01"
supported_repos = ["org/core-api"]
max_concurrent_jobs = 2
github_auth = "cplt-gh"
poll_interval = "15s"
heartbeat_interval = "25s"
job_heartbeat_interval = "10s"
drain_timeout = "45s"

[worker.supported_skills]
"org/core-api" = ["review-pr"]

[[repos]]
name = "org/core-api"
git_url = "https://github.com/org/core-api.git"

[quota]
provider = "claude-usage"
poll_interval = "3m"
max_five_hour_pct = 70.0
max_seven_day_pct = 85.0
quotapace_profile = "agent-pool"
require_green_pace = true
fail_closed_on_error = true

[job_defaults]
max_turns = 25
max_effective_tokens = 150000
timeout_seconds = 300

[skill_overrides."review-pr"]
max_turns = 20
max_effective_tokens = 120000
timeout_seconds = 300
`
	writeTestFile(t, cfgPath, content)

	cfg, err := LoadConfig(cfgPath)
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if cfg.Worker.BrokerURL != "https://agent-pool.internal.example.com" {
		t.Errorf("BrokerURL = %q", cfg.Worker.BrokerURL)
	}
	if cfg.Worker.IsolatedHome != filepath.Join(tmp, "data", "agent-pool", "isolated-home") {
		t.Errorf("IsolatedHome = %q", cfg.Worker.IsolatedHome)
	}
	if cfg.Worker.StateDir != filepath.Join(tmp, "state", "agent-pool") {
		t.Errorf("StateDir = %q", cfg.Worker.StateDir)
	}
	if cfg.Worker.PollInterval != 15*time.Second {
		t.Errorf("PollInterval = %v, want 15s", cfg.Worker.PollInterval)
	}
	if cfg.Quota.PollInterval != 3*time.Minute {
		t.Errorf("Quota.PollInterval = %v, want 3m", cfg.Quota.PollInterval)
	}
	if cfg.SkillOverrides["review-pr"].MaxTurns != 20 {
		t.Errorf("SkillOverrides[review-pr].MaxTurns = %d, want 20", cfg.SkillOverrides["review-pr"].MaxTurns)
	}
}
