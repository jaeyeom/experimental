// Package mcp implements the Producer MCP Streamable HTTP endpoint (POST /mcp)
// using JSON-RPC 2.0 over HTTP.
package mcp

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/chorong-sh/chorong/pkg/version"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	maxMCPBodyBytes    = 2 << 20 // 2 MiB.
	mcpProtocolVersion = "2025-03-26"

	rpcCodeParseError     = -32700
	rpcCodeInvalidRequest = -32600
	rpcCodeMethodNotFound = -32601
	rpcCodeInvalidParams  = -32602
)

type rpcRequest struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type rpcResponse struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Result  any             `json:"result,omitempty"`
	Error   *rpcError       `json:"error,omitempty"`
}

type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

type initializeResult struct {
	ProtocolVersion string                 `json:"protocolVersion"`
	Capabilities    initializeCapabilities `json:"capabilities"`
	ServerInfo      serverInfo             `json:"serverInfo"`
}

type initializeCapabilities struct {
	Tools map[string]any `json:"tools"`
}

type serverInfo struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type toolDefinition struct {
	Name        string          `json:"name"`
	Description string          `json:"description"`
	InputSchema json.RawMessage `json:"inputSchema"`
}

type toolsListResult struct {
	Tools []toolDefinition `json:"tools"`
}

type toolsCallParams struct {
	Name      string          `json:"name"`
	Arguments json.RawMessage `json:"arguments,omitempty"`
}

type contentItem struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

type toolCallResult struct {
	Content []contentItem `json:"content"`
	IsError bool          `json:"isError,omitempty"`
}

type getJobResultArgs struct {
	JobID       string  `json:"jobId"`
	Wait        string  `json:"wait,omitempty"`
	WaitSeconds float64 `json:"waitSeconds,omitempty"`
}

type cancelJobArgs struct {
	JobID string `json:"jobId"`
}

type handler struct {
	svc   *broker.Service
	store broker.Store
}

// NewHandler creates an HTTP handler for POST /mcp authenticated as a producer.
func NewHandler(svc *broker.Service, store broker.Store) http.Handler {
	return &handler{svc: svc, store: store}
}

func (h *handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	requester, ok := h.authenticateProducer(w, r)
	if !ok {
		return
	}

	r.Body = http.MaxBytesReader(w, r.Body, maxMCPBodyBytes)
	var req rpcRequest
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			Error:   &rpcError{Code: rpcCodeParseError, Message: fmt.Sprintf("parse JSON-RPC request: %v", err)},
		})
		return
	}
	if req.JSONRPC != "2.0" || req.Method == "" {
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Error:   &rpcError{Code: rpcCodeInvalidRequest, Message: "invalid JSON-RPC 2.0 request"},
		})
		return
	}

	githubToken := strings.TrimSpace(r.Header.Get("X-GitHub-Token"))
	h.dispatchMethod(w, r.Context(), requester, githubToken, req)
}

func (h *handler) authenticateProducer(w http.ResponseWriter, r *http.Request) (broker.TokenRecord, bool) {
	token, ok := extractBearerToken(r.Header.Get("Authorization"))
	if !ok {
		writeJSON(w, http.StatusUnauthorized, protocol.APIError{
			Code:    protocol.ErrCodeUnauthorized,
			Message: "missing or invalid Authorization bearer token",
		})
		return broker.TokenRecord{}, false
	}
	rec, err := h.store.AuthenticateToken(r.Context(), token)
	if err != nil {
		writeJSON(w, http.StatusUnauthorized, protocol.APIError{
			Code:    protocol.ErrCodeUnauthorized,
			Message: "invalid or revoked bearer token",
		})
		return broker.TokenRecord{}, false
	}
	if rec.Role != broker.TokenRoleProducer {
		writeJSON(w, http.StatusForbidden, protocol.APIError{
			Code:    protocol.ErrCodeForbidden,
			Message: "endpoint requires producer role",
		})
		return broker.TokenRecord{}, false
	}
	return rec, true
}

func (h *handler) dispatchMethod(w http.ResponseWriter, ctx context.Context, requester broker.TokenRecord, githubToken string, req rpcRequest) {
	switch req.Method {
	case "initialize":
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Result: initializeResult{
				ProtocolVersion: mcpProtocolVersion,
				Capabilities:    initializeCapabilities{Tools: map[string]any{}},
				ServerInfo: serverInfo{
					Name:    "agent-poold",
					Version: version.Resolve(),
				},
			},
		})
	case "notifications/initialized":
		w.WriteHeader(http.StatusAccepted)
	case "tools/list":
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Result:  toolsListResult{Tools: mcpTools()},
		})
	case "tools/call":
		h.handleToolsCall(w, ctx, requester, githubToken, req)
	default:
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Error:   &rpcError{Code: rpcCodeMethodNotFound, Message: fmt.Sprintf("unknown method %q", req.Method)},
		})
	}
}

func (h *handler) handleToolsCall(w http.ResponseWriter, ctx context.Context, requester broker.TokenRecord, githubToken string, req rpcRequest) {
	var params toolsCallParams
	if err := json.Unmarshal(req.Params, &params); err != nil || params.Name == "" {
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Error:   &rpcError{Code: rpcCodeInvalidParams, Message: "invalid tools/call params"},
		})
		return
	}

	res, rpcErr := h.invokeTool(ctx, requester, githubToken, params.Name, params.Arguments)
	if rpcErr != nil {
		writeRPC(w, rpcResponse{
			JSONRPC: "2.0",
			ID:      req.ID,
			Error:   rpcErr,
		})
		return
	}
	writeRPC(w, rpcResponse{
		JSONRPC: "2.0",
		ID:      req.ID,
		Result:  res,
	})
}

func (h *handler) invokeTool(ctx context.Context, requester broker.TokenRecord, githubToken, name string, rawArgs json.RawMessage) (toolCallResult, *rpcError) {
	switch name {
	case "pool_status":
		resp, err := h.svc.PoolStatus(ctx, requester)
		return wrapToolOutcome(resp, err), nil
	case "dispatch_skill":
		return h.callDispatchSkill(ctx, requester, githubToken, rawArgs)
	case "get_job_result":
		return h.callGetJobResult(ctx, requester, rawArgs)
	case "cancel_job":
		return h.callCancelJob(ctx, requester, rawArgs)
	default:
		return toolCallResult{}, &rpcError{
			Code:    rpcCodeInvalidParams,
			Message: fmt.Sprintf("unknown tool %q", name),
		}
	}
}

func (h *handler) callDispatchSkill(ctx context.Context, requester broker.TokenRecord, githubToken string, rawArgs json.RawMessage) (toolCallResult, *rpcError) {
	var req protocol.DispatchRequest
	if err := unmarshalArgs(rawArgs, &req); err != nil {
		return toolCallResult{}, &rpcError{Code: rpcCodeInvalidParams, Message: err.Error()}
	}
	resp, err := h.svc.DispatchSkill(ctx, requester, req, githubToken)
	return wrapToolOutcome(resp, err), nil
}

func (h *handler) callGetJobResult(ctx context.Context, requester broker.TokenRecord, rawArgs json.RawMessage) (toolCallResult, *rpcError) {
	var args getJobResultArgs
	if err := unmarshalArgs(rawArgs, &args); err != nil {
		return toolCallResult{}, &rpcError{Code: rpcCodeInvalidParams, Message: err.Error()}
	}
	if args.JobID == "" {
		return wrapToolOutcome(nil, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: "jobId is required",
		}), nil
	}
	waitDur, err := resolveWaitDuration(args)
	if err != nil {
		return wrapToolOutcome(nil, err), nil
	}
	resp, err := h.svc.GetJobResult(ctx, requester.ID, args.JobID, waitDur)
	return wrapToolOutcome(resp, err), nil
}

func (h *handler) callCancelJob(ctx context.Context, requester broker.TokenRecord, rawArgs json.RawMessage) (toolCallResult, *rpcError) {
	var args cancelJobArgs
	if err := unmarshalArgs(rawArgs, &args); err != nil {
		return toolCallResult{}, &rpcError{Code: rpcCodeInvalidParams, Message: err.Error()}
	}
	if args.JobID == "" {
		return wrapToolOutcome(nil, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: "jobId is required",
		}), nil
	}
	resp, err := h.svc.CancelJob(ctx, requester.ID, args.JobID)
	return wrapToolOutcome(resp, err), nil
}

func resolveWaitDuration(args getJobResultArgs) (time.Duration, error) {
	if args.Wait != "" {
		d, err := time.ParseDuration(args.Wait)
		if err != nil || d < 0 {
			return 0, &protocol.APIError{
				Code:    protocol.ErrCodeInvalidArgs,
				Message: fmt.Sprintf("invalid wait duration %q", args.Wait),
			}
		}
		return d, nil
	}
	if args.WaitSeconds < 0 {
		return 0, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: "waitSeconds must be non-negative",
		}
	}
	return time.Duration(args.WaitSeconds * float64(time.Second)), nil
}

func wrapToolOutcome(payload any, err error) toolCallResult {
	if err != nil {
		var apiErr *protocol.APIError
		if !errors.As(err, &apiErr) {
			apiErr = &protocol.APIError{
				Code:    protocol.ErrCodeInternal,
				Message: "internal server error",
			}
		}
		raw, _ := json.Marshal(apiErr)
		return toolCallResult{
			Content: []contentItem{{Type: "text", Text: string(raw)}},
			IsError: true,
		}
	}
	raw, _ := json.Marshal(payload)
	return toolCallResult{
		Content: []contentItem{{Type: "text", Text: string(raw)}},
		IsError: false,
	}
}

func unmarshalArgs(raw json.RawMessage, dst any) error {
	trimmed := bytes.TrimSpace(raw)
	if len(trimmed) == 0 {
		return nil
	}
	if err := json.Unmarshal(trimmed, dst); err != nil {
		return fmt.Errorf("invalid tool arguments: %w", err)
	}
	return nil
}

func mcpTools() []toolDefinition {
	return []toolDefinition{
		{
			Name:        "pool_status",
			Description: "Inspect active worker capacity, pool health, queue depth, and available repository skills.",
			InputSchema: json.RawMessage(`{"type":"object","properties":{},"additionalProperties":false}`),
		},
		{
			Name:        "dispatch_skill",
			Description: "Enqueue a read-only catalog skill job on a configured repository.",
			InputSchema: json.RawMessage(`{"type":"object","properties":{"repo":{"type":"string"},"skill":{"type":"string"},"args":{"type":"object"},"idempotencyKey":{"type":"string"}},"required":["repo","skill"],"additionalProperties":false}`),
		},
		{
			Name:        "get_job_result",
			Description: "Fetch the current state and deliverable of a job, optionally long-polling up to 25 seconds.",
			InputSchema: json.RawMessage(`{"type":"object","properties":{"jobId":{"type":"string"},"wait":{"type":"string"},"waitSeconds":{"type":"number"}},"required":["jobId"],"additionalProperties":false}`),
		},
		{
			Name:        "cancel_job",
			Description: "Cancel a queued job or request cooperative cancellation for a running job.",
			InputSchema: json.RawMessage(`{"type":"object","properties":{"jobId":{"type":"string"}},"required":["jobId"],"additionalProperties":false}`),
		},
	}
}

func extractBearerToken(header string) (string, bool) {
	parts := strings.SplitN(strings.TrimSpace(header), " ", 2)
	if len(parts) != 2 || !strings.EqualFold(parts[0], "Bearer") {
		return "", false
	}
	tok := strings.TrimSpace(parts[1])
	return tok, tok != ""
}

func writeRPC(w http.ResponseWriter, resp rpcResponse) {
	writeJSON(w, http.StatusOK, resp)
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(v)
}
