package mcp_test

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/ghacl"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/mcp"
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

type rpcResponse struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Result  json.RawMessage `json:"result"`
	Error   *rpcError       `json:"error"`
}

type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

type toolCallResult struct {
	Content []struct {
		Type string `json:"type"`
		Text string `json:"text"`
	} `json:"content"`
	IsError bool `json:"isError"`
}

func postMCP(t *testing.T, srv *httptest.Server, token string, payload any) *http.Response {
	t.Helper()
	raw, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("json.Marshal() error = %v", err)
	}
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost, srv.URL+"/mcp", bytes.NewReader(raw))
	if err != nil {
		t.Fatalf("NewRequest() error = %v", err)
	}
	req.Header.Set("Content-Type", "application/json")
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	resp, err := srv.Client().Do(req)
	if err != nil {
		t.Fatalf("Do(/mcp) error = %v", err)
	}
	return resp
}

func decodeRPC(t *testing.T, resp *http.Response) rpcResponse {
	t.Helper()
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("POST /mcp status = %d, want 200", resp.StatusCode)
	}
	var out rpcResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode rpcResponse: %v", err)
	}
	return out
}

func TestMCPStreamableHTTP(t *testing.T) {
	ctx := context.Background()
	st, err := sqlite.Open(filepath.Join(t.TempDir(), "broker.db"))
	if err != nil {
		t.Fatalf("sqlite.Open() error = %v", err)
	}
	t.Cleanup(func() { _ = st.Close() })

	svc := broker.NewService(st, dlp.NewScanner(), broker.NewEventHub(), broker.Config{})
	mux := http.NewServeMux()
	mux.Handle("POST /mcp", mcp.NewHandler(svc, st))
	srv := httptest.NewServer(mux)
	t.Cleanup(srv.Close)

	prodToken, _, err := st.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	workToken, workRec, err := st.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}

	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/repo-a",
		DefaultBranch: "main",
		CatalogSha:    "sha123",
		CatalogJSON:   []byte(testCatalogJSON),
		UpdatedAt:     time.Now().UTC(),
	}); err != nil {
		t.Fatalf("UpsertCatalog() error = %v", err)
	}
	if _, err := svc.WorkerHeartbeat(ctx, workRec, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/repo-a"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	}); err != nil {
		t.Fatalf("WorkerHeartbeat() error = %v", err)
	}

	// Worker token rejected with 403.
	unauthResp := postMCP(t, srv, workToken, map[string]any{"jsonrpc": "2.0", "id": 1, "method": "initialize"})
	unauthResp.Body.Close()
	if unauthResp.StatusCode != http.StatusForbidden {
		t.Fatalf("worker token on /mcp status = %d, want 403", unauthResp.StatusCode)
	}

	// 1. initialize.
	initRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      1,
		"method":  "initialize",
		"params":  map[string]any{"protocolVersion": "2025-03-26"},
	}))
	if initRPC.Error != nil {
		t.Fatalf("initialize error = %+v", initRPC.Error)
	}
	var initRes struct {
		ProtocolVersion string `json:"protocolVersion"`
		ServerInfo      struct {
			Name    string `json:"name"`
			Version string `json:"version"`
		} `json:"serverInfo"`
	}
	if err := json.Unmarshal(initRPC.Result, &initRes); err != nil {
		t.Fatalf("unmarshal initialize result: %v", err)
	}
	if initRes.ProtocolVersion != "2025-03-26" || initRes.ServerInfo.Name != "agent-poold" {
		t.Fatalf("unexpected initialize result: %+v", initRes)
	}

	// 2. notifications/initialized -> 202 Accepted.
	notifResp := postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"method":  "notifications/initialized",
	})
	notifResp.Body.Close()
	if notifResp.StatusCode != http.StatusAccepted {
		t.Fatalf("notifications/initialized status = %d, want 202", notifResp.StatusCode)
	}

	// 3. tools/list.
	listRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      2,
		"method":  "tools/list",
	}))
	if listRPC.Error != nil {
		t.Fatalf("tools/list error = %+v", listRPC.Error)
	}
	var listRes struct {
		Tools []struct {
			Name string `json:"name"`
		} `json:"tools"`
	}
	if err := json.Unmarshal(listRPC.Result, &listRes); err != nil {
		t.Fatalf("unmarshal tools/list: %v", err)
	}
	if len(listRes.Tools) != 4 {
		t.Fatalf("expected 4 tools, got %d: %+v", len(listRes.Tools), listRes.Tools)
	}

	// 4. tools/call pool_status.
	statusRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      3,
		"method":  "tools/call",
		"params": map[string]any{
			"name":      "pool_status",
			"arguments": map[string]any{},
		},
	}))
	if statusRPC.Error != nil {
		t.Fatalf("pool_status rpc error = %+v", statusRPC.Error)
	}
	var statusCall toolCallResult
	if err := json.Unmarshal(statusRPC.Result, &statusCall); err != nil {
		t.Fatalf("unmarshal pool_status result: %v", err)
	}
	if statusCall.IsError || len(statusCall.Content) != 1 {
		t.Fatalf("unexpected pool_status call result: %+v", statusCall)
	}
	var poolSt protocol.PoolStatusResponse
	if err := json.Unmarshal([]byte(statusCall.Content[0].Text), &poolSt); err != nil {
		t.Fatalf("unmarshal pool status text: %v", err)
	}
	if poolSt.ActiveWorkers != 1 || poolSt.PoolHealth != "healthy" {
		t.Fatalf("unexpected pool status: %+v", poolSt)
	}

	// 5. tools/call dispatch_skill with invalid args -> isError=true (not JSON-RPC error).
	badDispRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      4,
		"method":  "tools/call",
		"params": map[string]any{
			"name": "dispatch_skill",
			"arguments": map[string]any{
				"repo":  "org/repo-a",
				"skill": "summarize",
				"args":  map[string]any{"wrong": 1},
			},
		},
	}))
	if badDispRPC.Error != nil {
		t.Fatalf("expected domain error in tool result, got JSON-RPC error: %+v", badDispRPC.Error)
	}
	var badCall toolCallResult
	if err := json.Unmarshal(badDispRPC.Result, &badCall); err != nil {
		t.Fatalf("unmarshal bad dispatch result: %v", err)
	}
	if !badCall.IsError {
		t.Fatalf("expected isError=true for invalid args, got %+v", badCall)
	}

	// 6. tools/call dispatch_skill with valid args.
	dispRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      5,
		"method":  "tools/call",
		"params": map[string]any{
			"name": "dispatch_skill",
			"arguments": map[string]any{
				"repo":  "org/repo-a",
				"skill": "summarize",
				"args":  map[string]any{"path": "README.md"},
			},
		},
	}))
	if dispRPC.Error != nil {
		t.Fatalf("dispatch_skill rpc error = %+v", dispRPC.Error)
	}
	var dispCall toolCallResult
	if err := json.Unmarshal(dispRPC.Result, &dispCall); err != nil {
		t.Fatalf("unmarshal dispatch_skill result: %v", err)
	}
	if dispCall.IsError || len(dispCall.Content) != 1 {
		t.Fatalf("unexpected dispatch_skill call result: %+v", dispCall)
	}
	var dispRes protocol.DispatchResponse
	if err := json.Unmarshal([]byte(dispCall.Content[0].Text), &dispRes); err != nil {
		t.Fatalf("unmarshal DispatchResponse: %v", err)
	}
	if dispRes.JobID == "" {
		t.Fatalf("expected non-empty jobId")
	}

	// 7. tools/call get_job_result.
	getRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      6,
		"method":  "tools/call",
		"params": map[string]any{
			"name": "get_job_result",
			"arguments": map[string]any{
				"jobId": dispRes.JobID,
			},
		},
	}))
	if getRPC.Error != nil {
		t.Fatalf("get_job_result rpc error = %+v", getRPC.Error)
	}
	var getCall toolCallResult
	if err := json.Unmarshal(getRPC.Result, &getCall); err != nil {
		t.Fatalf("unmarshal get_job_result: %v", err)
	}
	var jobRes protocol.JobResultResponse
	if err := json.Unmarshal([]byte(getCall.Content[0].Text), &jobRes); err != nil {
		t.Fatalf("unmarshal JobResultResponse: %v", err)
	}
	if jobRes.JobID != dispRes.JobID || jobRes.Status != protocol.JobStatusQueued {
		t.Fatalf("unexpected JobResultResponse: %+v", jobRes)
	}

	// 8. tools/call cancel_job.
	cancelRPC := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      7,
		"method":  "tools/call",
		"params": map[string]any{
			"name": "cancel_job",
			"arguments": map[string]any{
				"jobId": dispRes.JobID,
			},
		},
	}))
	if cancelRPC.Error != nil {
		t.Fatalf("cancel_job rpc error = %+v", cancelRPC.Error)
	}
	var cancelCall toolCallResult
	if err := json.Unmarshal(cancelRPC.Result, &cancelCall); err != nil {
		t.Fatalf("unmarshal cancel_job: %v", err)
	}
	var cancelRes protocol.CancelJobResponse
	if err := json.Unmarshal([]byte(cancelCall.Content[0].Text), &cancelRes); err != nil {
		t.Fatalf("unmarshal CancelJobResponse: %v", err)
	}
	if cancelRes.Status != protocol.JobStatusCanceled {
		t.Fatalf("expected CANCELED status, got %s", cancelRes.Status)
	}

	// 9. Unknown method -> JSON-RPC error.
	unknownMethod := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      8,
		"method":  "no/such_method",
	}))
	if unknownMethod.Error == nil {
		t.Fatalf("expected JSON-RPC error for unknown method")
	}

	// 10. Unknown tool -> JSON-RPC error.
	unknownTool := decodeRPC(t, postMCP(t, srv, prodToken, map[string]any{
		"jsonrpc": "2.0",
		"id":      9,
		"method":  "tools/call",
		"params": map[string]any{
			"name":      "unknown_tool",
			"arguments": map[string]any{},
		},
	}))
	if unknownTool.Error == nil {
		t.Fatalf("expected JSON-RPC error for unknown tool")
	}
}

func postMCPWithHeaders(t *testing.T, srv *httptest.Server, token string, headers map[string]string, payload any) *http.Response {
	t.Helper()
	raw, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("json.Marshal() error = %v", err)
	}
	req, err := http.NewRequestWithContext(context.Background(), http.MethodPost, srv.URL+"/mcp", bytes.NewReader(raw))
	if err != nil {
		t.Fatalf("NewRequest() error = %v", err)
	}
	req.Header.Set("Content-Type", "application/json")
	if token != "" {
		req.Header.Set("Authorization", "Bearer "+token)
	}
	for k, v := range headers {
		req.Header.Set(k, v)
	}
	resp, err := srv.Client().Do(req)
	if err != nil {
		t.Fatalf("Do(/mcp) error = %v", err)
	}
	return resp
}

func TestMCPDispatchWithGitHubTokenHeader(t *testing.T) {
	allowHeader := "mcp-allow-fixture-987654"
	denyHeader := "mcp-deny-fixture-123456"

	ghServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/repos/org/secret-repo" && r.Header.Get("Authorization") == "Bearer "+allowHeader {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			_, _ = w.Write([]byte(`{"permissions":{"pull":true}}`))
			return
		}
		w.WriteHeader(http.StatusNotFound)
	}))
	t.Cleanup(ghServer.Close)

	ctx := context.Background()
	st, err := sqlite.Open(filepath.Join(t.TempDir(), "broker.db"))
	if err != nil {
		t.Fatalf("sqlite.Open() error = %v", err)
	}
	t.Cleanup(func() { _ = st.Close() })

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: ghServer.URL,
		CacheTTL:   5 * time.Minute,
	}, ghServer.Client())

	svc := broker.NewService(st, dlp.NewScanner(), broker.NewEventHub(), broker.Config{
		GitHubACLVerifier:      verifier,
		RequireRepoAccessCheck: true,
	})
	mux := http.NewServeMux()
	mux.Handle("POST /mcp", mcp.NewHandler(svc, st))
	srv := httptest.NewServer(mux)
	t.Cleanup(srv.Close)

	prodToken, _, err := st.CreateToken(ctx, broker.TokenRoleProducer, "prod-1", nil)
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	_, workRec, err := st.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/secret-repo"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/secret-repo",
		DefaultBranch: "main",
		CatalogSha:    "sha123",
		CatalogJSON:   []byte(testCatalogJSON),
		UpdatedAt:     time.Now().UTC(),
	}); err != nil {
		t.Fatalf("UpsertCatalog() error = %v", err)
	}
	if _, err := svc.WorkerHeartbeat(ctx, workRec, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{"org/secret-repo"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	}); err != nil {
		t.Fatalf("WorkerHeartbeat() error = %v", err)
	}

	dispatchPayload := map[string]any{
		"jsonrpc": "2.0",
		"id":      1,
		"method":  "tools/call",
		"params": map[string]any{
			"name": "dispatch_skill",
			"arguments": map[string]any{
				"repo":  "org/secret-repo",
				"skill": "summarize",
				"args":  map[string]any{"path": "README.md"},
			},
		},
	}

	// 1. Missing X-GitHub-Token -> tool error UNAUTHORIZED.
	noTokRPC := decodeRPC(t, postMCP(t, srv, prodToken, dispatchPayload))
	if noTokRPC.Error != nil {
		t.Fatalf("unexpected JSON-RPC error: %+v", noTokRPC.Error)
	}
	var noTokCall toolCallResult
	if err := json.Unmarshal(noTokRPC.Result, &noTokCall); err != nil {
		t.Fatalf("unmarshal noTokCall: %v", err)
	}
	if !noTokCall.IsError || len(noTokCall.Content) != 1 {
		t.Fatalf("expected isError=true for missing token, got %+v", noTokCall)
	}
	var noTokErr protocol.APIError
	if err := json.Unmarshal([]byte(noTokCall.Content[0].Text), &noTokErr); err != nil {
		t.Fatalf("unmarshal APIError: %v", err)
	}
	if noTokErr.Code != protocol.ErrCodeUnauthorized {
		t.Fatalf("missing token code = %s, want UNAUTHORIZED", noTokErr.Code)
	}

	// 2. Unauthorized X-GitHub-Token (404 from GitHub) -> tool error FORBIDDEN.
	denyRPC := decodeRPC(t, postMCPWithHeaders(t, srv, prodToken, map[string]string{
		"X-GitHub-Token": denyHeader,
	}, dispatchPayload))
	if denyRPC.Error != nil {
		t.Fatalf("unexpected JSON-RPC error: %+v", denyRPC.Error)
	}
	var denyCall toolCallResult
	if err := json.Unmarshal(denyRPC.Result, &denyCall); err != nil {
		t.Fatalf("unmarshal denyCall: %v", err)
	}
	if !denyCall.IsError || len(denyCall.Content) != 1 {
		t.Fatalf("expected isError=true for denied token, got %+v", denyCall)
	}
	var denyErr protocol.APIError
	if err := json.Unmarshal([]byte(denyCall.Content[0].Text), &denyErr); err != nil {
		t.Fatalf("unmarshal APIError: %v", err)
	}
	if denyErr.Code != protocol.ErrCodeForbidden {
		t.Fatalf("denied token code = %s, want FORBIDDEN", denyErr.Code)
	}

	// 3. Valid X-GitHub-Token (200 pull:true) -> QUEUED, and zero leakage.
	okRPC := decodeRPC(t, postMCPWithHeaders(t, srv, prodToken, map[string]string{
		"X-GitHub-Token": allowHeader,
	}, dispatchPayload))
	if okRPC.Error != nil {
		t.Fatalf("unexpected JSON-RPC error: %+v", okRPC.Error)
	}
	var okCall toolCallResult
	if err := json.Unmarshal(okRPC.Result, &okCall); err != nil {
		t.Fatalf("unmarshal okCall: %v", err)
	}
	if okCall.IsError || len(okCall.Content) != 1 {
		t.Fatalf("expected success for valid token, got %+v", okCall)
	}
	var dispRes protocol.DispatchResponse
	if err := json.Unmarshal([]byte(okCall.Content[0].Text), &dispRes); err != nil {
		t.Fatalf("unmarshal DispatchResponse: %v", err)
	}
	if dispRes.JobID == "" || dispRes.Status != protocol.JobStatusQueued {
		t.Fatalf("unexpected DispatchResponse: %+v", dispRes)
	}

	lease, ok, err := svc.ClaimLease(ctx, workRec, protocol.LeaseRequest{
		SupportedRepos: []string{"org/secret-repo"},
	})
	if err != nil || !ok {
		t.Fatalf("ClaimLease() = (%+v, %v, %v), want leased job", lease, ok, err)
	}
	leaseJSON, err := json.Marshal(lease)
	if err != nil {
		t.Fatalf("marshal lease: %v", err)
	}
	if strings.Contains(string(leaseJSON), allowHeader) {
		t.Fatalf("LeaseResponse leaked requester GitHub token!")
	}
	jobRec, err := st.GetJob(ctx, dispRes.JobID)
	if err != nil {
		t.Fatalf("GetJob() error = %v", err)
	}
	jobJSON, err := json.Marshal(jobRec)
	if err != nil {
		t.Fatalf("marshal jobRec: %v", err)
	}
	if strings.Contains(string(jobJSON), allowHeader) {
		t.Fatalf("SQLite JobRecord leaked requester GitHub token!")
	}
}
