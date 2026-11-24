package mcp_test

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
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
