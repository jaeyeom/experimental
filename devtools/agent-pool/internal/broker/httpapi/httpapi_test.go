package httpapi_test

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/ghacl"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/httpapi"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const testCatalogJSON = `{
	"version": 1,
	"skills": {
		"summarize": {
			"description": "Summarize a file",
			"mode": "readonly",
			"skillPath": ".agent-pool/skills/summarize/SKILL.md",
			"argsSchema": {
				"type": "object",
				"properties": {
					"path": { "type": "string", "minLength": 1, "maxLength": 128, "pattern": "^[A-Za-z0-9._/-]+$" }
				},
				"required": ["path"],
				"additionalProperties": false
			},
			"outputSchema": {
				"type": "object",
				"properties": {
					"summary": { "type": "string", "minLength": 1 }
				},
				"required": ["summary"],
				"additionalProperties": false
			}
		}
	}
}`

type testEnv struct {
	store  broker.Store
	svc    *broker.Service
	server *httptest.Server
}

func newTestEnv(t *testing.T) *testEnv {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "broker.db")
	st, err := sqlite.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlite.Open() error = %v", err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("store.Close() error = %v", err)
		}
	})

	hub := broker.NewEventHub()
	svc := broker.NewService(st, dlp.NewScanner(), hub, broker.Config{
		WorkerStaleAfter:          90 * time.Second,
		LeaseTTL:                  60 * time.Second,
		MaxAttempts:               2,
		MaxConcurrentPerRequester: 5,
	})
	handler := httpapi.NewHandler(svc, st, httpapi.Options{
		KeepaliveInterval: 5 * time.Second,
	})
	srv := httptest.NewServer(handler)
	t.Cleanup(srv.Close)

	return &testEnv{store: st, svc: svc, server: srv}
}

func seedCatalog(t *testing.T, st broker.Store, repo string) {
	t.Helper()
	err := st.UpsertCatalog(context.Background(), broker.CatalogRecord{
		Repo:          repo,
		DefaultBranch: "main",
		CatalogSha:    "abc123def456",
		CatalogJSON:   []byte(testCatalogJSON),
		UpdatedAt:     time.Now().UTC(),
	})
	if err != nil {
		t.Fatalf("UpsertCatalog(%q) error = %v", repo, err)
	}
}

func doJSON(t *testing.T, srv *httptest.Server, method, path, token string, body any) *http.Response {
	t.Helper()
	var reader io.Reader
	if body != nil {
		raw, err := json.Marshal(body)
		if err != nil {
			t.Fatalf("json.Marshal() error = %v", err)
		}
		reader = bytes.NewReader(raw)
	}
	req, err := http.NewRequestWithContext(context.Background(), method, srv.URL+path, reader)
	if err != nil {
		t.Fatalf("NewRequest() error = %v", err)
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	resp, err := srv.Client().Do(req)
	if err != nil {
		t.Fatalf("Do(%s %s) error = %v", method, path, err)
	}
	return resp
}

func decodeBody[T any](t *testing.T, resp *http.Response) T {
	t.Helper()
	defer resp.Body.Close()
	var out T
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("Decode() error = %v", err)
	}
	return out
}

type sseEvent struct {
	Event string
	Data  string
}

func openSSE(t *testing.T, ctx context.Context, srv *httptest.Server, path, token string) <-chan sseEvent {
	t.Helper()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, srv.URL+path, nil)
	if err != nil {
		t.Fatalf("NewRequest SSE error = %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+token)
	resp, err := srv.Client().Do(req)
	if err != nil {
		t.Fatalf("Do SSE error = %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		t.Fatalf("SSE status = %d, want 200", resp.StatusCode)
	}

	ch := make(chan sseEvent, 16)
	go func() {
		defer resp.Body.Close()
		defer close(ch)
		scanner := bufio.NewScanner(resp.Body)
		var cur sseEvent
		for scanner.Scan() {
			line := scanner.Text()
			if line == "" {
				if cur.Event != "" || cur.Data != "" {
					ch <- cur
					cur = sseEvent{}
				}
				continue
			}
			if strings.HasPrefix(line, ":") {
				continue
			}
			if v, ok := strings.CutPrefix(line, "event:"); ok {
				cur.Event = strings.TrimSpace(v)
			} else if v, ok := strings.CutPrefix(line, "data:"); ok {
				cur.Data = strings.TrimSpace(v)
			}
		}
	}()
	return ch
}

func waitSSEEvent(t *testing.T, ch <-chan sseEvent, wantEvent string) sseEvent {
	t.Helper()
	timer := time.NewTimer(3 * time.Second)
	defer timer.Stop()
	for {
		select {
		case ev, ok := <-ch:
			if !ok {
				t.Fatalf("SSE channel closed while waiting for %q", wantEvent)
			}
			if ev.Event == wantEvent {
				return ev
			}
		case <-timer.C:
			t.Fatalf("timed out waiting for SSE event %q", wantEvent)
		}
	}
}

func TestEndToEndRESTAndWorkerFlow(t *testing.T) {
	env := newTestEnv(t)
	ctx := context.Background()

	prodToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	seedCatalog(t, env.store, "org/repo-a")

	// Healthz is unauthenticated.
	hz := doJSON(t, env.server, http.MethodGet, "/healthz", "", nil)
	if hz.StatusCode != http.StatusOK {
		t.Fatalf("/healthz status = %d, want 200", hz.StatusCode)
	}
	hz.Body.Close()

	// Worker heartbeat.
	hbResp := doJSON(t, env.server, http.MethodPost, "/workers/heartbeat", workToken, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/repo-a"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	})
	if hbResp.StatusCode != http.StatusOK {
		t.Fatalf("worker heartbeat status = %d, want 200", hbResp.StatusCode)
	}
	hb := decodeBody[protocol.WorkerHeartbeatResponse](t, hbResp)
	if !hb.Ack {
		t.Fatalf("expected heartbeat ack=true")
	}

	// Open SSE stream for worker.
	sseCtx, cancelSSE := context.WithCancel(ctx)
	defer cancelSSE()
	sseCh := openSSE(t, sseCtx, env.server, "/workers/events?repos=org/repo-a", workToken)

	// Producer checks status.
	statusResp := doJSON(t, env.server, http.MethodGet, "/v1/status", prodToken, nil)
	if statusResp.StatusCode != http.StatusOK {
		t.Fatalf("GET /v1/status = %d, want 200", statusResp.StatusCode)
	}
	stBody := decodeBody[protocol.PoolStatusResponse](t, statusResp)
	if stBody.ActiveWorkers != 1 || stBody.PoolHealth != "healthy" {
		t.Fatalf("unexpected pool status: %+v", stBody)
	}

	// Producer dispatches job.
	dispResp := doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodToken, protocol.DispatchRequest{
		Repo:  "org/repo-a",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"README.md"}`),
	})
	if dispResp.StatusCode != http.StatusCreated && dispResp.StatusCode != http.StatusOK {
		t.Fatalf("POST /v1/jobs status = %d, want 200/201", dispResp.StatusCode)
	}
	disp := decodeBody[protocol.DispatchResponse](t, dispResp)
	if disp.JobID == "" || disp.Status != protocol.JobStatusQueued {
		t.Fatalf("unexpected dispatch response: %+v", disp)
	}

	// Worker receives job_available via SSE.
	ev := waitSSEEvent(t, sseCh, "job_available")
	var avail protocol.SSEJobAvailable
	if err := json.Unmarshal([]byte(ev.Data), &avail); err != nil {
		t.Fatalf("unmarshal SSEJobAvailable: %v", err)
	}
	if avail.Repo != "org/repo-a" || avail.Skill != "summarize" {
		t.Fatalf("unexpected SSEJobAvailable: %+v", avail)
	}

	// Worker claims lease.
	leaseResp := doJSON(t, env.server, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/repo-a"},
	})
	if leaseResp.StatusCode != http.StatusOK {
		t.Fatalf("POST /workers/lease status = %d, want 200", leaseResp.StatusCode)
	}
	lease := decodeBody[protocol.LeaseResponse](t, leaseResp)
	if lease.JobID != disp.JobID || lease.LeaseToken == "" {
		t.Fatalf("unexpected lease response: %+v", lease)
	}

	// Second lease returns 204 No Content.
	emptyLease := doJSON(t, env.server, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/repo-a"},
	})
	emptyLease.Body.Close()
	if emptyLease.StatusCode != http.StatusNoContent {
		t.Fatalf("second lease status = %d, want 204", emptyLease.StatusCode)
	}

	// Worker sends job heartbeat.
	jhbResp := doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease.JobID+"/heartbeat", workToken, protocol.JobHeartbeatRequest{
		LeaseToken: lease.LeaseToken,
		Progress:   protocol.JobProgress{TurnsUsed: 1, EffectiveUnits: 100, ElapsedMs: 500},
	})
	if jhbResp.StatusCode != http.StatusOK {
		t.Fatalf("job heartbeat status = %d, want 200", jhbResp.StatusCode)
	}
	jhb := decodeBody[protocol.JobHeartbeatResponse](t, jhbResp)
	if !jhb.Ack || jhb.Cancel {
		t.Fatalf("unexpected job heartbeat response: %+v", jhb)
	}

	// Producer starts long-poll in background.
	pollDone := make(chan protocol.JobResultResponse, 1)
	go func() {
		resp := doJSON(t, env.server, http.MethodGet, "/v1/jobs/"+disp.JobID+"?wait=5s", prodToken, nil)
		if resp.StatusCode != http.StatusOK {
			t.Errorf("long-poll status = %d, want 200", resp.StatusCode)
			resp.Body.Close()
			return
		}
		pollDone <- decodeBody[protocol.JobResultResponse](t, resp)
	}()

	time.Sleep(50 * time.Millisecond)

	// Worker completes job with valid deliverable.
	compResp := doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease.JobID+"/complete", workToken, protocol.CompleteJobRequest{
		LeaseToken:  lease.LeaseToken,
		Status:      protocol.JobStatusSucceeded,
		Deliverable: json.RawMessage(`{"summary":"All good"}`),
		DlpChecked:  true,
	})
	if compResp.StatusCode != http.StatusOK {
		t.Fatalf("complete status = %d, want 200", compResp.StatusCode)
	}
	comp := decodeBody[protocol.CompleteJobResponse](t, compResp)
	if comp.Status != protocol.JobStatusSucceeded {
		t.Fatalf("complete status = %s, want SUCCEEDED", comp.Status)
	}

	// Producer long-poll returns immediately with SUCCEEDED.
	select {
	case res := <-pollDone:
		if res.Status != protocol.JobStatusSucceeded {
			t.Fatalf("polled status = %s, want SUCCEEDED", res.Status)
		}
		if !strings.Contains(string(res.Deliverable), "All good") {
			t.Fatalf("unexpected deliverable: %s", string(res.Deliverable))
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for producer long-poll to return")
	}
}

func TestAuthAndOwnershipIsolation(t *testing.T) {
	env := newTestEnv(t)
	ctx := context.Background()

	prodAToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleProducer, "prod-a", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(prod-a) error = %v", err)
	}
	prodBToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleProducer, "prod-b", nil)
	if err != nil {
		t.Fatalf("CreateToken(prod-b) error = %v", err)
	}
	workToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a", "org/repo-b"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}

	seedCatalog(t, env.store, "org/repo-a")
	seedCatalog(t, env.store, "org/repo-b")

	hbResp := doJSON(t, env.server, http.MethodPost, "/workers/heartbeat", workToken, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/repo-a", "org/repo-b"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	})
	hbResp.Body.Close()

	// Missing token -> 401.
	resp := doJSON(t, env.server, http.MethodGet, "/v1/status", "", nil)
	resp.Body.Close()
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("missing token status = %d, want 401", resp.StatusCode)
	}

	// Wrong token -> 401.
	resp = doJSON(t, env.server, http.MethodGet, "/v1/status", "ap_prod_invalid", nil)
	resp.Body.Close()
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("wrong token status = %d, want 401", resp.StatusCode)
	}

	// Worker token on producer endpoint -> 403.
	resp = doJSON(t, env.server, http.MethodGet, "/v1/status", workToken, nil)
	resp.Body.Close()
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("worker on producer endpoint status = %d, want 403", resp.StatusCode)
	}

	// Producer A scoped to org/repo-a: pool_status hides org/repo-b.
	stResp := doJSON(t, env.server, http.MethodGet, "/v1/status", prodAToken, nil)
	if stResp.StatusCode != http.StatusOK {
		t.Fatalf("prodA status = %d, want 200", stResp.StatusCode)
	}
	stBody := decodeBody[protocol.PoolStatusResponse](t, stResp)
	for _, r := range stBody.SupportedRepos {
		if r == "org/repo-b" {
			t.Fatalf("prodA status leaked org/repo-b in supportedRepos: %v", stBody.SupportedRepos)
		}
	}
	for _, sk := range stBody.AvailableSkills {
		if sk.Repo == "org/repo-b" {
			t.Fatalf("prodA status leaked org/repo-b in availableSkills")
		}
	}

	// Producer A dispatching on org/repo-b -> 403.
	dispB := doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodAToken, protocol.DispatchRequest{
		Repo:  "org/repo-b",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"README.md"}`),
	})
	dispB.Body.Close()
	if dispB.StatusCode != http.StatusForbidden {
		t.Fatalf("prodA dispatch org/repo-b status = %d, want 403", dispB.StatusCode)
	}

	// Producer A dispatches on org/repo-a -> succeeds.
	dispA := doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodAToken, protocol.DispatchRequest{
		Repo:  "org/repo-a",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"README.md"}`),
	})
	jobA := decodeBody[protocol.DispatchResponse](t, dispA)

	// Producer B calling GET /v1/jobs/{id} on Producer A's job -> 403.
	getB := doJSON(t, env.server, http.MethodGet, "/v1/jobs/"+jobA.JobID, prodBToken, nil)
	getB.Body.Close()
	if getB.StatusCode != http.StatusForbidden {
		t.Fatalf("prodB GET prodA job status = %d, want 403", getB.StatusCode)
	}

	// Producer B calling cancel on Producer A's job -> 403.
	cancelB := doJSON(t, env.server, http.MethodPost, "/v1/jobs/"+jobA.JobID+"/cancel", prodBToken, nil)
	cancelB.Body.Close()
	if cancelB.StatusCode != http.StatusForbidden {
		t.Fatalf("prodB cancel prodA job status = %d, want 403", cancelB.StatusCode)
	}
}

func TestStage2DLPAndOutputValidation(t *testing.T) {
	env := newTestEnv(t)
	ctx := context.Background()

	prodToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	seedCatalog(t, env.store, "org/repo-a")

	hbResp := doJSON(t, env.server, http.MethodPost, "/workers/heartbeat", workToken, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/repo-a"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	})
	hbResp.Body.Close()

	// Case 1: Synthetic secret triggers Stage-2 DLP_BLOCKED and strips deliverable.
	disp1 := decodeBody[protocol.DispatchResponse](t, doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodToken, protocol.DispatchRequest{
		Repo:  "org/repo-a",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"a.go"}`),
	}))
	lease1 := decodeBody[protocol.LeaseResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/repo-a"},
	}))

	// Build synthetic GitHub PAT dynamically so gitleaks does not flag test source.
	syntheticSecret := "ghp_" + strings.Repeat("A1b2C3", 6)
	secretPayload, err := json.Marshal(map[string]string{"summary": "leaked " + syntheticSecret})
	if err != nil {
		t.Fatalf("marshal secret payload: %v", err)
	}

	comp1 := decodeBody[protocol.CompleteJobResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease1.JobID+"/complete", workToken, protocol.CompleteJobRequest{
		LeaseToken:  lease1.LeaseToken,
		Status:      protocol.JobStatusSucceeded,
		Deliverable: secretPayload,
		DlpChecked:  true,
	}))
	if comp1.Status != protocol.JobStatusDLPBlocked {
		t.Fatalf("DLP complete status = %s, want DLP_BLOCKED", comp1.Status)
	}

	res1 := decodeBody[protocol.JobResultResponse](t, doJSON(t, env.server, http.MethodGet, "/v1/jobs/"+disp1.JobID, prodToken, nil))
	if res1.Status != protocol.JobStatusDLPBlocked || res1.ErrorCode != protocol.ErrCodeDLPBlocked {
		t.Fatalf("unexpected DLP result: %+v", res1)
	}
	if len(res1.Deliverable) != 0 {
		t.Fatalf("expected deliverable stripped on DLP_BLOCKED, got %s", string(res1.Deliverable))
	}

	// Case 2: Invalid output schema -> FAILED (OUTPUT_INVALID).
	disp2 := decodeBody[protocol.DispatchResponse](t, doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodToken, protocol.DispatchRequest{
		Repo:  "org/repo-a",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"b.go"}`),
	}))
	lease2 := decodeBody[protocol.LeaseResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/repo-a"},
	}))

	comp2 := decodeBody[protocol.CompleteJobResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease2.JobID+"/complete", workToken, protocol.CompleteJobRequest{
		LeaseToken:  lease2.LeaseToken,
		Status:      protocol.JobStatusSucceeded,
		Deliverable: json.RawMessage(`{"wrongField": 123}`),
		DlpChecked:  true,
	}))
	if comp2.Status != protocol.JobStatusFailed {
		t.Fatalf("invalid output complete status = %s, want FAILED", comp2.Status)
	}

	res2 := decodeBody[protocol.JobResultResponse](t, doJSON(t, env.server, http.MethodGet, "/v1/jobs/"+disp2.JobID, prodToken, nil))
	if res2.Status != protocol.JobStatusFailed || res2.ErrorCode != protocol.ErrCodeOutputInvalid {
		t.Fatalf("unexpected OUTPUT_INVALID result: %+v", res2)
	}
	if len(res2.Deliverable) != 0 {
		t.Fatalf("expected deliverable stripped on OUTPUT_INVALID, got %s", string(res2.Deliverable))
	}
}

func TestCancelPropagation(t *testing.T) {
	env := newTestEnv(t)
	ctx := context.Background()

	prodToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, _, err := env.store.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	seedCatalog(t, env.store, "org/repo-a")

	hbResp := doJSON(t, env.server, http.MethodPost, "/workers/heartbeat", workToken, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/repo-a"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 1,
		Quota:                protocol.QuotaHealth{State: "green"},
	})
	hbResp.Body.Close()

	sseCtx, cancelSSE := context.WithCancel(ctx)
	defer cancelSSE()
	sseCh := openSSE(t, sseCtx, env.server, "/workers/events?repos=org/repo-a", workToken)

	disp := decodeBody[protocol.DispatchResponse](t, doJSON(t, env.server, http.MethodPost, "/v1/jobs", prodToken, protocol.DispatchRequest{
		Repo:  "org/repo-a",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"c.go"}`),
	}))
	lease := decodeBody[protocol.LeaseResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/repo-a"},
	}))

	// Move job to RUNNING via heartbeat.
	jhb1 := decodeBody[protocol.JobHeartbeatResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease.JobID+"/heartbeat", workToken, protocol.JobHeartbeatRequest{
		LeaseToken: lease.LeaseToken,
	}))
	if jhb1.Cancel {
		t.Fatalf("expected cancel=false before cancel request")
	}

	// Producer cancels RUNNING job.
	cancelResp := doJSON(t, env.server, http.MethodPost, "/v1/jobs/"+disp.JobID+"/cancel", prodToken, nil)
	if cancelResp.StatusCode != http.StatusOK {
		t.Fatalf("cancel status = %d, want 200", cancelResp.StatusCode)
	}
	cancelBody := decodeBody[protocol.CancelJobResponse](t, cancelResp)
	if !cancelBody.CancelRequested {
		t.Fatalf("expected cancelRequested=true, got %+v", cancelBody)
	}

	// SSE stream receives job_canceled.
	ev := waitSSEEvent(t, sseCh, "job_canceled")
	var canceled protocol.SSEJobCanceled
	if err := json.Unmarshal([]byte(ev.Data), &canceled); err != nil {
		t.Fatalf("unmarshal SSEJobCanceled: %v", err)
	}
	if canceled.JobID != disp.JobID {
		t.Fatalf("SSE job_canceled jobId = %q, want %q", canceled.JobID, disp.JobID)
	}

	// Worker's next job heartbeat receives cancel=true.
	jhb2 := decodeBody[protocol.JobHeartbeatResponse](t, doJSON(t, env.server, http.MethodPost, "/workers/jobs/"+lease.JobID+"/heartbeat", workToken, protocol.JobHeartbeatRequest{
		LeaseToken: lease.LeaseToken,
	}))
	if !jhb2.Cancel {
		t.Fatalf("expected cancel=true on next job heartbeat")
	}
}

func doJSONWithHeaders(t *testing.T, srv *httptest.Server, method, path, token string, headers map[string]string, body any) *http.Response {
	t.Helper()
	var reader io.Reader
	if body != nil {
		raw, err := json.Marshal(body)
		if err != nil {
			t.Fatalf("json.Marshal() error = %v", err)
		}
		reader = bytes.NewReader(raw)
	}
	req, err := http.NewRequestWithContext(context.Background(), method, srv.URL+path, reader)
	if err != nil {
		t.Fatalf("NewRequest() error = %v", err)
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	for k, v := range headers {
		req.Header.Set(k, v)
	}
	resp, err := srv.Client().Do(req)
	if err != nil {
		t.Fatalf("Do(%s %s) error = %v", method, path, err)
	}
	return resp
}

func TestDispatchRequiresGitHubACLWhenEnabled(t *testing.T) {
	allowHeader := "rest-allow-fixture-987654321"
	denyHeader := "rest-deny-fixture-123456789"
	invalidHeader := "rest-bad-fixture-000000000"

	ghServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/repos/org/secret-repo" {
			w.WriteHeader(http.StatusNotFound)
			return
		}
		switch r.Header.Get("Authorization") {
		case "Bearer " + allowHeader:
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			_, _ = w.Write([]byte(`{"permissions":{"pull":true}}`))
		case "Bearer " + invalidHeader:
			w.WriteHeader(http.StatusUnauthorized)
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	t.Cleanup(ghServer.Close)

	dbPath := filepath.Join(t.TempDir(), "broker.db")
	st, err := sqlite.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlite.Open() error = %v", err)
	}
	t.Cleanup(func() { _ = st.Close() })

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: ghServer.URL,
		CacheTTL:   5 * time.Minute,
	}, ghServer.Client())

	svc := broker.NewService(st, dlp.NewScanner(), broker.NewEventHub(), broker.Config{
		WorkerStaleAfter:          90 * time.Second,
		LeaseTTL:                  60 * time.Second,
		MaxAttempts:               2,
		MaxConcurrentPerRequester: 5,
		GitHubACLVerifier:         verifier,
		RequireRepoAccessCheck:    true,
	})
	srv := httptest.NewServer(httpapi.NewHandler(svc, st, httpapi.Options{}))
	t.Cleanup(srv.Close)

	ctx := context.Background()
	prodToken, _, err := st.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, _, err := st.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/secret-repo"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	seedCatalog(t, st, "org/secret-repo")

	hbResp := doJSON(t, srv, http.MethodPost, "/workers/heartbeat", workToken, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/secret-repo"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	})
	hbResp.Body.Close()

	dispatchReq := protocol.DispatchRequest{
		Repo:  "org/secret-repo",
		Skill: "summarize",
		Args:  json.RawMessage(`{"path":"README.md"}`),
	}

	// 1. Dispatch without X-GitHub-Token -> 401 UNAUTHORIZED.
	noTokResp := doJSON(t, srv, http.MethodPost, "/v1/jobs", prodToken, dispatchReq)
	if noTokResp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("dispatch without X-GitHub-Token status = %d, want 401", noTokResp.StatusCode)
	}
	noTokErr := decodeBody[protocol.APIError](t, noTokResp)
	if noTokErr.Code != protocol.ErrCodeUnauthorized {
		t.Fatalf("missing token error code = %s, want UNAUTHORIZED", noTokErr.Code)
	}

	// 2. Dispatch with invalid token (401 from GitHub) -> 401 UNAUTHORIZED.
	badTokResp := doJSONWithHeaders(t, srv, http.MethodPost, "/v1/jobs", prodToken, map[string]string{
		"X-GitHub-Token": invalidHeader,
	}, dispatchReq)
	if badTokResp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("dispatch with invalid X-GitHub-Token status = %d, want 401", badTokResp.StatusCode)
	}
	badTokErr := decodeBody[protocol.APIError](t, badTokResp)
	if badTokErr.Code != protocol.ErrCodeUnauthorized {
		t.Fatalf("invalid token error code = %s, want UNAUTHORIZED", badTokErr.Code)
	}

	// 3. Dispatch with token lacking access (404 from GitHub) -> 403 FORBIDDEN.
	denyResp := doJSONWithHeaders(t, srv, http.MethodPost, "/v1/jobs", prodToken, map[string]string{
		"X-GitHub-Token": denyHeader,
	}, dispatchReq)
	if denyResp.StatusCode != http.StatusForbidden {
		t.Fatalf("dispatch with unauthorized X-GitHub-Token status = %d, want 403", denyResp.StatusCode)
	}
	denyErr := decodeBody[protocol.APIError](t, denyResp)
	if denyErr.Code != protocol.ErrCodeForbidden {
		t.Fatalf("denied token error code = %s, want FORBIDDEN", denyErr.Code)
	}

	// 4. Dispatch with valid token (200 pull:true) -> 201 QUEUED.
	okResp := doJSONWithHeaders(t, srv, http.MethodPost, "/v1/jobs", prodToken, map[string]string{
		"X-GitHub-Token": allowHeader,
	}, dispatchReq)
	if okResp.StatusCode != http.StatusCreated && okResp.StatusCode != http.StatusOK && okResp.StatusCode != http.StatusAccepted {
		t.Fatalf("dispatch with valid X-GitHub-Token status = %d, want 200/201/202", okResp.StatusCode)
	}
	disp := decodeBody[protocol.DispatchResponse](t, okResp)
	if disp.JobID == "" || disp.Status != protocol.JobStatusQueued {
		t.Fatalf("unexpected dispatch response: %+v", disp)
	}

	// Verify leased LeaseResponse contains NO trace of requester's GitHub token.
	leaseResp := doJSON(t, srv, http.MethodPost, "/workers/lease", workToken, protocol.LeaseRequest{
		SupportedRepos: []string{"org/secret-repo"},
	})
	if leaseResp.StatusCode != http.StatusOK {
		t.Fatalf("lease status = %d, want 200", leaseResp.StatusCode)
	}
	leaseBody, err := io.ReadAll(leaseResp.Body)
	leaseResp.Body.Close()
	if err != nil {
		t.Fatalf("read lease body: %v", err)
	}
	if strings.Contains(string(leaseBody), allowHeader) {
		t.Fatalf("LeaseResponse leaked requester GitHub token!")
	}

	// Verify SQLite jobs row contains NO trace of requester's GitHub token.
	jobRec, err := st.GetJob(ctx, disp.JobID)
	if err != nil {
		t.Fatalf("GetJob() error = %v", err)
	}
	jobJSON, err := json.Marshal(jobRec)
	if err != nil {
		t.Fatalf("json.Marshal(jobRec) error = %v", err)
	}
	if strings.Contains(string(jobJSON), allowHeader) {
		t.Fatalf("SQLite JobRecord leaked requester GitHub token!")
	}
}
