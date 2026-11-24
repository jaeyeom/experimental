package worker

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	defaultRPCTimeout = 30 * time.Second
	sseInitialBackoff = 1 * time.Second
	sseMaxBackoff     = 30 * time.Second
	maxSSELineBytes   = 1 << 20 // 1 MiB.
)

// BrokerClient communicates outbound over HTTPS with the agent-poold broker
// using Bearer token authentication.
type BrokerClient struct {
	baseURL    string
	token      string
	repos      []string
	httpClient *http.Client
	sseClient  *http.Client
}

// NewBrokerClient constructs a BrokerClient targeting baseURL with the given
// worker bearer token. If httpClient is nil, a default client is used.
func NewBrokerClient(baseURL, token string, httpClient *http.Client) *BrokerClient {
	if httpClient == nil {
		httpClient = &http.Client{Timeout: defaultRPCTimeout}
	}
	sseClient := &http.Client{
		Transport: httpClient.Transport,
	}
	return &BrokerClient{
		baseURL:    strings.TrimRight(baseURL, "/"),
		token:      token,
		httpClient: httpClient,
		sseClient:  sseClient,
	}
}

// SetRepos configures the repository filter query parameter sent when opening
// the worker SSE event stream.
func (c *BrokerClient) SetRepos(repos []string) {
	copied := make([]string, len(repos))
	copy(copied, repos)
	c.repos = copied
}

// Heartbeat reports worker capacity and quota posture to POST /workers/heartbeat.
func (c *BrokerClient) Heartbeat(ctx context.Context, req protocol.WorkerHeartbeatRequest) (protocol.WorkerHeartbeatResponse, error) {
	var resp protocol.WorkerHeartbeatResponse
	if err := c.postJSON(ctx, "/workers/heartbeat", req, &resp); err != nil {
		return protocol.WorkerHeartbeatResponse{}, err
	}
	return resp, nil
}

// ClaimLease requests the next eligible queued job from POST /workers/lease.
// It returns (nil, nil) when the broker responds with 204 No Content.
func (c *BrokerClient) ClaimLease(ctx context.Context, req protocol.LeaseRequest) (*protocol.LeaseResponse, error) {
	body, err := json.Marshal(req)
	if err != nil {
		return nil, fmt.Errorf("marshal lease request: %w", err)
	}
	httpReq, err := c.newRequest(ctx, http.MethodPost, "/workers/lease", bytes.NewReader(body))
	if err != nil {
		return nil, err
	}
	httpReq.Header.Set("Content-Type", "application/json")

	resp, err := c.httpClient.Do(httpReq)
	if err != nil {
		return nil, fmt.Errorf("POST /workers/lease: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNoContent {
		return nil, nil
	}
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return nil, parseHTTPError(http.MethodPost, "/workers/lease", resp)
	}
	var out protocol.LeaseResponse
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		return nil, fmt.Errorf("decode lease response: %w", err)
	}
	return &out, nil
}

// JobHeartbeat renews an active job lease and reports in-flight progress via
// POST /workers/jobs/{jobID}/heartbeat.
func (c *BrokerClient) JobHeartbeat(ctx context.Context, jobID string, req protocol.JobHeartbeatRequest) (protocol.JobHeartbeatResponse, error) {
	var resp protocol.JobHeartbeatResponse
	path := "/workers/jobs/" + url.PathEscape(jobID) + "/heartbeat"
	if err := c.postJSON(ctx, path, req, &resp); err != nil {
		return protocol.JobHeartbeatResponse{}, err
	}
	return resp, nil
}

// CompleteJob reports a terminal job outcome via
// POST /workers/jobs/{jobID}/complete.
func (c *BrokerClient) CompleteJob(ctx context.Context, jobID string, req protocol.CompleteJobRequest) (protocol.CompleteJobResponse, error) {
	var resp protocol.CompleteJobResponse
	path := "/workers/jobs/" + url.PathEscape(jobID) + "/complete"
	if err := c.postJSON(ctx, path, req, &resp); err != nil {
		return protocol.CompleteJobResponse{}, err
	}
	return resp, nil
}

// StreamEvents subscribes to GET /workers/events and dispatches parsed SSE
// events to onEvent until ctx is canceled. Transient disconnects are retried
// with capped exponential backoff from 1s to 30s.
func (c *BrokerClient) StreamEvents(ctx context.Context, onEvent func(eventType string, data []byte)) error {
	backoff := sseInitialBackoff
	for {
		if ctx.Err() != nil {
			return fmt.Errorf("stream events: %w", ctx.Err())
		}
		connected, err := c.streamOnce(ctx, onEvent)
		if ctx.Err() != nil {
			return fmt.Errorf("stream events: %w", ctx.Err())
		}
		if connected {
			backoff = sseInitialBackoff
		}
		_ = err
		timer := time.NewTimer(backoff)
		select {
		case <-ctx.Done():
			timer.Stop()
			return fmt.Errorf("stream events: %w", ctx.Err())
		case <-timer.C:
		}
		backoff = min(backoff*2, sseMaxBackoff)
	}
}

func (c *BrokerClient) streamOnce(ctx context.Context, onEvent func(eventType string, data []byte)) (bool, error) {
	path := "/workers/events"
	if len(c.repos) > 0 {
		q := url.Values{}
		q.Set("repos", strings.Join(c.repos, ","))
		path += "?" + q.Encode()
	}

	req, err := c.newRequest(ctx, http.MethodGet, path, nil)
	if err != nil {
		return false, err
	}
	req.Header.Set("Accept", "text/event-stream")

	resp, err := c.sseClient.Do(req)
	if err != nil {
		return false, fmt.Errorf("GET %s: %w", path, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return false, parseHTTPError(http.MethodGet, path, resp)
	}

	return true, readSSEStream(resp.Body, onEvent)
}

func readSSEStream(r io.Reader, onEvent func(eventType string, data []byte)) error {
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, 4096), maxSSELineBytes)

	var eventType string
	var dataLines []string

	dispatch := func() {
		if eventType == "" && len(dataLines) == 0 {
			return
		}
		if onEvent != nil {
			onEvent(eventType, []byte(strings.Join(dataLines, "\n")))
		}
		eventType = ""
		dataLines = nil
	}

	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), "\r")
		if line == "" {
			dispatch()
			continue
		}
		if strings.HasPrefix(line, ":") {
			continue
		}
		if v, ok := strings.CutPrefix(line, "event:"); ok {
			eventType = strings.TrimSpace(v)
		} else if v, ok := strings.CutPrefix(line, "data:"); ok {
			dataLines = append(dataLines, strings.TrimPrefix(v, " "))
		}
	}
	dispatch()
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("read SSE stream: %w", err)
	}
	return nil
}

func (c *BrokerClient) postJSON(ctx context.Context, path string, reqBody, respDst any) error {
	raw, err := json.Marshal(reqBody)
	if err != nil {
		return fmt.Errorf("marshal request for %s: %w", path, err)
	}
	httpReq, err := c.newRequest(ctx, http.MethodPost, path, bytes.NewReader(raw))
	if err != nil {
		return err
	}
	httpReq.Header.Set("Content-Type", "application/json")

	resp, err := c.httpClient.Do(httpReq)
	if err != nil {
		return fmt.Errorf("POST %s: %w", path, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return parseHTTPError(http.MethodPost, path, resp)
	}
	if respDst != nil {
		if err := json.NewDecoder(resp.Body).Decode(respDst); err != nil {
			return fmt.Errorf("decode response from %s: %w", path, err)
		}
	}
	return nil
}

func (c *BrokerClient) newRequest(ctx context.Context, method, path string, body io.Reader) (*http.Request, error) {
	req, err := http.NewRequestWithContext(ctx, method, c.baseURL+path, body)
	if err != nil {
		return nil, fmt.Errorf("create request %s %s: %w", method, path, err)
	}
	if c.token != "" {
		req.Header.Set("Authorization", "Bearer "+c.token)
	}
	return req, nil
}

func parseHTTPError(method, path string, resp *http.Response) error {
	raw, _ := io.ReadAll(io.LimitReader(resp.Body, 8192))
	var apiErr protocol.APIError
	if err := json.Unmarshal(raw, &apiErr); err == nil && (apiErr.Code != "" || apiErr.Message != "") {
		return fmt.Errorf("broker %s %s (HTTP %d): %w", method, path, resp.StatusCode, &apiErr)
	}
	return fmt.Errorf("broker %s %s failed with HTTP %d: %s", method, path, resp.StatusCode, strings.TrimSpace(string(raw)))
}
