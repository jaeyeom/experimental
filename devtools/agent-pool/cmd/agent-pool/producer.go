package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/worker"
)

const (
	defaultProducerTimeout = 5 * time.Minute
	maxPollWaitStep        = 25 * time.Second
	producerHTTPTimeout    = 35 * time.Second
)

type clientConfig struct {
	BrokerURL string `toml:"broker_url"`
	TokenEnv  string `toml:"token_env"`
	TokenFile string `toml:"token_file"`
}

type producerConnFlags struct {
	brokerURL  string
	tokenFile  string
	configPath string
}

type producerClient struct {
	baseURL     string
	token       string
	githubToken string
	httpClient  *http.Client
}

func defaultClientConfigPath() string {
	return filepath.Join(worker.XDGConfigHome(), "agent-pool", "client.toml")
}

func loadClientConfig(path string) (clientConfig, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return clientConfig{}, fmt.Errorf("read client config %q: %w", path, err)
	}
	var cfg clientConfig
	if err := toml.Unmarshal(raw, &cfg); err != nil {
		return clientConfig{}, fmt.Errorf("parse client config %q: %w", path, err)
	}
	return cfg, nil
}

func registerProducerConnFlags(fs *flag.FlagSet, cf *producerConnFlags) {
	fs.StringVar(&cf.brokerURL, "broker", "", "broker base URL (overrides AGENT_POOL_BROKER_URL and client.toml)")
	fs.StringVar(&cf.tokenFile, "token-file", "", "path to producer bearer token file")
	fs.StringVar(&cf.configPath, "config", "", "path to client.toml (defaults to ~/.config/agent-pool/client.toml)")
}

func newProducerClient(cf producerConnFlags, deps cliDeps) (*producerClient, error) {
	cfgPath := cf.configPath
	if cfgPath == "" {
		cfgPath = defaultClientConfigPath()
	}
	fileCfg, err := loadClientConfig(cfgPath)
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, err
	}

	brokerURL := firstNonEmpty(cf.brokerURL, os.Getenv("AGENT_POOL_BROKER_URL"), fileCfg.BrokerURL)
	if brokerURL == "" {
		return nil, errors.New("broker URL is required (set --broker, AGENT_POOL_BROKER_URL, or client.toml)")
	}

	token, err := resolveProducerToken(cf.tokenFile, fileCfg)
	if err != nil {
		return nil, err
	}

	httpClient := deps.httpClient
	if httpClient == nil {
		httpClient = &http.Client{Timeout: producerHTTPTimeout}
	}

	return &producerClient{
		baseURL:     strings.TrimRight(brokerURL, "/"),
		token:       token,
		githubToken: strings.TrimSpace(os.Getenv("AGENT_POOL_GITHUB_TOKEN")),
		httpClient:  httpClient,
	}, nil
}

func resolveProducerToken(tokenFileFlag string, cfg clientConfig) (string, error) {
	if tokenFileFlag != "" {
		return readTokenFile(tokenFileFlag)
	}
	if tok := strings.TrimSpace(os.Getenv("AGENT_POOL_TOKEN")); tok != "" {
		return tok, nil
	}
	if cfg.TokenEnv != "" {
		if tok := strings.TrimSpace(os.Getenv(cfg.TokenEnv)); tok != "" {
			return tok, nil
		}
	}
	if cfg.TokenFile != "" {
		return readTokenFile(cfg.TokenFile)
	}
	return "", errors.New("producer bearer token not found (set AGENT_POOL_TOKEN, --token-file, or client.toml)")
}

func readTokenFile(path string) (string, error) {
	p := expandUserHome(path)
	raw, err := os.ReadFile(p)
	if err != nil {
		return "", fmt.Errorf("read token file %q: %w", path, err)
	}
	tok := strings.TrimSpace(string(raw))
	if tok == "" {
		return "", fmt.Errorf("token file %q is empty", path)
	}
	return tok, nil
}

func expandUserHome(p string) string {
	if p == "~" {
		if home, err := os.UserHomeDir(); err == nil && home != "" {
			return home
		}
		return p
	}
	if strings.HasPrefix(p, "~/") {
		if home, err := os.UserHomeDir(); err == nil && home != "" {
			return filepath.Join(home, p[2:])
		}
	}
	return p
}

func firstNonEmpty(vals ...string) string {
	for _, v := range vals {
		if trimmed := strings.TrimSpace(v); trimmed != "" {
			return trimmed
		}
	}
	return ""
}

func (c *producerClient) doJSON(ctx context.Context, method, path string, reqBody, respDst any) error {
	var bodyReader io.Reader
	if reqBody != nil {
		raw, err := json.Marshal(reqBody)
		if err != nil {
			return fmt.Errorf("marshal request: %w", err)
		}
		bodyReader = bytes.NewReader(raw)
	}

	req, err := http.NewRequestWithContext(ctx, method, c.baseURL+path, bodyReader)
	if err != nil {
		return fmt.Errorf("build request %s %s: %w", method, path, err)
	}
	req.Header.Set("Authorization", "Bearer "+c.token)
	if reqBody != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if c.githubToken != "" {
		req.Header.Set("X-GitHub-Token", c.githubToken)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return fmt.Errorf("%s %s: %w", method, path, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return decodeBrokerError(method, path, resp)
	}
	if respDst != nil {
		if err := json.NewDecoder(resp.Body).Decode(respDst); err != nil {
			return fmt.Errorf("decode %s %s response: %w", method, path, err)
		}
	}
	return nil
}

func decodeBrokerError(method, path string, resp *http.Response) error {
	raw, _ := io.ReadAll(io.LimitReader(resp.Body, 8192))
	var apiErr protocol.APIError
	if err := json.Unmarshal(raw, &apiErr); err == nil && (apiErr.Code != "" || apiErr.Message != "") {
		return fmt.Errorf("%s %s (HTTP %d): %w", method, path, resp.StatusCode, &apiErr)
	}
	return fmt.Errorf("%s %s failed with HTTP %d: %s", method, path, resp.StatusCode, strings.TrimSpace(string(raw)))
}

func (c *producerClient) getResult(ctx context.Context, jobID string, wait time.Duration) (protocol.JobResultResponse, error) {
	path := "/v1/jobs/" + url.PathEscape(jobID)
	if wait > 0 {
		q := url.Values{}
		q.Set("wait", wait.String())
		path += "?" + q.Encode()
	}
	var out protocol.JobResultResponse
	if err := c.doJSON(ctx, http.MethodGet, path, nil, &out); err != nil {
		return protocol.JobResultResponse{}, err
	}
	return out, nil
}

func (c *producerClient) waitForResult(ctx context.Context, jobID string, timeout time.Duration) (protocol.JobResultResponse, error) {
	deadline := time.Now().Add(timeout)
	for {
		rem := time.Until(deadline)
		if rem <= 0 {
			return c.getResult(ctx, jobID, 0)
		}
		step := min(rem, maxPollWaitStep)
		res, err := c.getResult(ctx, jobID, step)
		if err != nil {
			return protocol.JobResultResponse{}, err
		}
		if res.Status.IsTerminal() {
			return res, nil
		}
	}
}

func cmdStatus(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	fs := flag.NewFlagSet("status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var cf producerConnFlags
	registerProducerConnFlags(fs, &cf)
	asJSON := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "Usage: agent-pool status [--json]")
		return 2
	}

	client, err := newProducerClient(cf, deps)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool status: %v\n", err)
		return 1
	}

	var status protocol.PoolStatusResponse
	if err := client.doJSON(ctx, http.MethodGet, "/v1/status", nil, &status); err != nil {
		fmt.Fprintf(stderr, "agent-pool status: %v\n", err)
		return 1
	}
	if *asJSON {
		if err := writeJSONOut(stdout, status); err != nil {
			fmt.Fprintf(stderr, "agent-pool status: %v\n", err)
			return 1
		}
		return 0
	}

	fmt.Fprintf(stdout, "poolHealth: %s\n", status.PoolHealth)
	fmt.Fprintf(stdout, "activeWorkers: %d\n", status.ActiveWorkers)
	fmt.Fprintf(stdout, "queueDepth: %d\n", status.QueueDepth)
	fmt.Fprintf(stdout, "supportedRepos: %s\n", strings.Join(status.SupportedRepos, ", "))
	for _, sk := range status.AvailableSkills {
		fmt.Fprintf(stdout, "  %s/%s\t%s\n", sk.Repo, sk.Skill, sk.Description)
	}
	return 0
}

func cmdDispatch(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	valueFlags := map[string]bool{
		"broker":          true,
		"token-file":      true,
		"config":          true,
		"args-json":       true,
		"idempotency-key": true,
		"timeout":         true,
	}
	fs := flag.NewFlagSet("dispatch", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var cf producerConnFlags
	registerProducerConnFlags(fs, &cf)
	argsJSON := fs.String("args-json", "{}", "JSON object of skill arguments")
	idemKey := fs.String("idempotency-key", "", "optional idempotency key")
	wait := fs.Bool("wait", false, "wait until the job reaches a terminal state")
	timeout := fs.Duration("timeout", defaultProducerTimeout, "wait timeout when --wait is set")
	asJSON := fs.Bool("json", false, "output JSON")

	if err := fs.Parse(reorderFlags(args, valueFlags)); err != nil {
		return 2
	}
	if fs.NArg() != 2 {
		fmt.Fprintln(stderr, "Usage: agent-pool dispatch <repo> <skill> --args-json '{...}' [--idempotency-key <k>] [--wait] [--timeout 5m] [--json]")
		return 2
	}
	rawArgs := json.RawMessage(strings.TrimSpace(*argsJSON))
	if !json.Valid(rawArgs) {
		fmt.Fprintln(stderr, "agent-pool dispatch: --args-json must be valid JSON")
		return 2
	}

	client, err := newProducerClient(cf, deps)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool dispatch: %v\n", err)
		return 1
	}

	req := protocol.DispatchRequest{
		Repo:           fs.Arg(0),
		Skill:          fs.Arg(1),
		Args:           rawArgs,
		IdempotencyKey: *idemKey,
	}
	var disp protocol.DispatchResponse
	if err := client.doJSON(ctx, http.MethodPost, "/v1/jobs", req, &disp); err != nil {
		fmt.Fprintf(stderr, "agent-pool dispatch: %v\n", err)
		return 1
	}

	if *wait {
		res, err := client.waitForResult(ctx, disp.JobID, *timeout)
		if err != nil {
			fmt.Fprintf(stderr, "agent-pool dispatch: %v\n", err)
			return 1
		}
		return printJobResult(stdout, stderr, res, *asJSON)
	}

	if *asJSON {
		if err := writeJSONOut(stdout, disp); err != nil {
			fmt.Fprintf(stderr, "agent-pool dispatch: %v\n", err)
			return 1
		}
		return 0
	}
	fmt.Fprintf(stdout, "jobId: %s\nstatus: %s\ncatalogSha: %s\n", disp.JobID, disp.Status, disp.CatalogSha)
	return 0
}

func cmdResult(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	valueFlags := map[string]bool{
		"broker":     true,
		"token-file": true,
		"config":     true,
		"timeout":    true,
	}
	fs := flag.NewFlagSet("result", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var cf producerConnFlags
	registerProducerConnFlags(fs, &cf)
	wait := fs.Bool("wait", false, "wait until the job reaches a terminal state")
	timeout := fs.Duration("timeout", defaultProducerTimeout, "wait timeout when --wait is set")
	asJSON := fs.Bool("json", false, "output JSON")

	if err := fs.Parse(reorderFlags(args, valueFlags)); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "Usage: agent-pool result <job_id> [--wait] [--timeout 5m] [--json]")
		return 2
	}

	client, err := newProducerClient(cf, deps)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool result: %v\n", err)
		return 1
	}

	var res protocol.JobResultResponse
	if *wait {
		res, err = client.waitForResult(ctx, fs.Arg(0), *timeout)
	} else {
		res, err = client.getResult(ctx, fs.Arg(0), 0)
	}
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool result: %v\n", err)
		return 1
	}
	return printJobResult(stdout, stderr, res, *asJSON)
}

func printJobResult(stdout, stderr io.Writer, res protocol.JobResultResponse, asJSON bool) int {
	if asJSON {
		if err := writeJSONOut(stdout, res); err != nil {
			fmt.Fprintf(stderr, "agent-pool result: %v\n", err)
			return 1
		}
		return 0
	}
	fmt.Fprintf(stdout, "jobId: %s\nrepo: %s\nskill: %s\nstatus: %s\n", res.JobID, res.Repo, res.Skill, res.Status)
	if res.WorkerAlias != "" {
		fmt.Fprintf(stdout, "workerAlias: %s\n", res.WorkerAlias)
	}
	if res.ErrorCode != "" || res.ErrorMessage != "" {
		fmt.Fprintf(stdout, "error: %s %s\n", res.ErrorCode, res.ErrorMessage)
	}
	if len(res.Deliverable) > 0 {
		fmt.Fprintf(stdout, "deliverable: %s\n", string(res.Deliverable))
	}
	return 0
}

func cmdCancel(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	valueFlags := map[string]bool{
		"broker":     true,
		"token-file": true,
		"config":     true,
	}
	fs := flag.NewFlagSet("cancel", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var cf producerConnFlags
	registerProducerConnFlags(fs, &cf)
	asJSON := fs.Bool("json", false, "output JSON")

	if err := fs.Parse(reorderFlags(args, valueFlags)); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "Usage: agent-pool cancel <job_id> [--json]")
		return 2
	}

	client, err := newProducerClient(cf, deps)
	if err != nil {
		fmt.Fprintf(stderr, "agent-pool cancel: %v\n", err)
		return 1
	}

	path := "/v1/jobs/" + url.PathEscape(fs.Arg(0)) + "/cancel"
	var resp protocol.CancelJobResponse
	if err := client.doJSON(ctx, http.MethodPost, path, nil, &resp); err != nil {
		fmt.Fprintf(stderr, "agent-pool cancel: %v\n", err)
		return 1
	}
	if *asJSON {
		if err := writeJSONOut(stdout, resp); err != nil {
			fmt.Fprintf(stderr, "agent-pool cancel: %v\n", err)
			return 1
		}
		return 0
	}
	fmt.Fprintf(stdout, "jobId: %s\nstatus: %s\ncancelRequested: %t\n", resp.JobID, resp.Status, resp.CancelRequested)
	return 0
}
