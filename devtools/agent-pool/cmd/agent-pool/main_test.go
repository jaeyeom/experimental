package main

import (
	"bytes"
	"context"
	"encoding/json"
	"encoding/xml"
	"errors"
	"io"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/httpapi"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/dlp"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
)

const testValidCatalogJSON = `{
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

func TestToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("XDG_DATA_HOME", filepath.Join(homeDir, ".local", "share"))
			t.Setenv("XDG_STATE_HOME", filepath.Join(homeDir, ".local", "state"))

			exe := filepath.Join(t.TempDir(), "agent-pool")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}

			deps := cliDeps{
				goos: "linux",
				homeDir: func() (string, error) {
					return homeDir, nil
				},
				executable: func() (string, error) {
					return exe, nil
				},
				lookPath: func(file string) (string, error) {
					if file == "agent-pool" {
						return exe, nil
					}
					return "/usr/bin/" + file, nil
				},
				runCmd: func(_ context.Context, _ string, _ []string, _ []string) ([]byte, error) {
					return []byte("ok\n"), nil
				},
			}

			tool := newAgentPoolTool(deps)
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = deps.lookPath
			if tool.ModulePath != "github.com/jaeyeom/experimental" {
				t.Fatalf("ModulePath = %q, want %q", tool.ModulePath, "github.com/jaeyeom/experimental")
			}
			return tool
		},
	})
}

func TestCatalogValidateCLI(t *testing.T) {
	t.Run("valid repo", func(t *testing.T) {
		repoDir := t.TempDir()
		writeTestFile(t, filepath.Join(repoDir, ".agent-pool", "catalog.json"), testValidCatalogJSON)
		writeTestFile(t, filepath.Join(repoDir, ".agent-pool", "skills", "summarize", "SKILL.md"), "# Summarize\n")

		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"catalog", "validate", repoDir}, &stdout, &stderr, defaultDeps())
		if code != 0 {
			t.Fatalf("catalog validate valid repo: exit=%d stderr=%s stdout=%s", code, stderr.String(), stdout.String())
		}
		if !strings.Contains(stdout.String(), "valid") {
			t.Errorf("expected valid message on stdout, got %q", stdout.String())
		}
	})

	t.Run("mutating mode rejected", func(t *testing.T) {
		repoDir := t.TempDir()
		mutatingJSON := strings.Replace(testValidCatalogJSON, `"readonly"`, `"mutating"`, 1)
		writeTestFile(t, filepath.Join(repoDir, ".agent-pool", "catalog.json"), mutatingJSON)
		writeTestFile(t, filepath.Join(repoDir, ".agent-pool", "skills", "summarize", "SKILL.md"), "# Summarize\n")

		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"catalog", "validate", repoDir}, &stdout, &stderr, defaultDeps())
		if code != 1 {
			t.Fatalf("catalog validate mutating mode: exit=%d, want 1 (stderr=%s)", code, stderr.String())
		}
		if !strings.Contains(stderr.String(), "mutating") {
			t.Errorf("expected mutating error on stderr, got %q", stderr.String())
		}
	})

	t.Run("missing file", func(t *testing.T) {
		missingPath := filepath.Join(t.TempDir(), "does-not-exist.json")
		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"catalog", "validate", missingPath}, &stdout, &stderr, defaultDeps())
		if code != 1 {
			t.Fatalf("catalog validate missing file: exit=%d, want 1 (stderr=%s)", code, stderr.String())
		}
	})

	t.Run("usage error", func(t *testing.T) {
		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"catalog", "validate", "a", "b"}, &stdout, &stderr, defaultDeps())
		if code != 2 {
			t.Fatalf("catalog validate extra args: exit=%d, want 2", code)
		}
	})
}

func TestProducerCLIEndToEnd(t *testing.T) {
	ctx := context.Background()
	dbPath := filepath.Join(t.TempDir(), "broker.db")
	store, err := sqlite.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlite.Open: %v", err)
	}
	t.Cleanup(func() { _ = store.Close() })

	const repo = "org/core-api"
	const catalogSha = "0123456789abcdef0123456789abcdef01234567"
	if err := store.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          repo,
		DefaultBranch: "main",
		CatalogSha:    catalogSha,
		CatalogJSON:   []byte(testValidCatalogJSON),
		UpdatedAt:     time.Now().UTC(),
	}); err != nil {
		t.Fatalf("UpsertCatalog: %v", err)
	}

	producerToken, _, err := store.CreateToken(ctx, broker.TokenRoleProducer, "cli-prod", []string{repo})
	if err != nil {
		t.Fatalf("CreateToken producer: %v", err)
	}
	_, workerRec, err := store.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{repo})
	if err != nil {
		t.Fatalf("CreateToken worker: %v", err)
	}

	hub := broker.NewEventHub()
	svc := broker.NewService(store, dlp.NewScanner(), hub, broker.Config{
		WorkerStaleAfter:          90 * time.Second,
		LeaseTTL:                  60 * time.Second,
		MaxAttempts:               2,
		MaxConcurrentPerRequester: 5,
	})
	if _, err := svc.WorkerHeartbeat(ctx, workerRec, protocol.WorkerHeartbeatRequest{
		SupportedRepos:       []string{repo},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota:                protocol.QuotaHealth{State: "green"},
	}); err != nil {
		t.Fatalf("WorkerHeartbeat: %v", err)
	}

	srv := httptest.NewServer(httpapi.NewHandler(svc, store, httpapi.Options{}))
	t.Cleanup(srv.Close)

	t.Setenv("AGENT_POOL_BROKER_URL", srv.URL)
	t.Setenv("AGENT_POOL_TOKEN", producerToken)

	// 1. status --json
	var statusOut, statusErr bytes.Buffer
	if code := runCLI(ctx, []string{"status", "--json"}, &statusOut, &statusErr, defaultDeps()); code != 0 {
		t.Fatalf("status --json: exit=%d stderr=%s", code, statusErr.String())
	}
	assertCamelCaseJSONKeys(t, statusOut.Bytes(), []string{"activeWorkers", "poolHealth", "queueDepth", "supportedRepos", "availableSkills"})
	var statusResp protocol.PoolStatusResponse
	if err := json.Unmarshal(statusOut.Bytes(), &statusResp); err != nil {
		t.Fatalf("decode status json: %v", err)
	}
	if statusResp.ActiveWorkers != 1 || statusResp.PoolHealth != "healthy" {
		t.Fatalf("unexpected status: %+v", statusResp)
	}

	// 2. dispatch <repo> <skill> --args-json ... --json
	var dispOut, dispErr bytes.Buffer
	dispArgs := []string{
		"dispatch", repo, "summarize",
		"--args-json", `{"path":"README.md"}`,
		"--idempotency-key", "idem-cli-1",
		"--json",
	}
	if code := runCLI(ctx, dispArgs, &dispOut, &dispErr, defaultDeps()); code != 0 {
		t.Fatalf("dispatch --json: exit=%d stderr=%s", code, dispErr.String())
	}
	assertCamelCaseJSONKeys(t, dispOut.Bytes(), []string{"jobId", "status", "catalogSha"})
	var dispResp protocol.DispatchResponse
	if err := json.Unmarshal(dispOut.Bytes(), &dispResp); err != nil {
		t.Fatalf("decode dispatch json: %v", err)
	}
	if dispResp.JobID == "" || dispResp.Status != protocol.JobStatusQueued || dispResp.CatalogSha != catalogSha {
		t.Fatalf("unexpected dispatch response: %+v", dispResp)
	}

	// 3. result <job_id> --json
	var resOut, resErr bytes.Buffer
	if code := runCLI(ctx, []string{"result", dispResp.JobID, "--json"}, &resOut, &resErr, defaultDeps()); code != 0 {
		t.Fatalf("result --json: exit=%d stderr=%s", code, resErr.String())
	}
	assertCamelCaseJSONKeys(t, resOut.Bytes(), []string{"jobId", "repo", "skill", "catalogSha", "status", "createdAt", "updatedAt"})
	var resResp protocol.JobResultResponse
	if err := json.Unmarshal(resOut.Bytes(), &resResp); err != nil {
		t.Fatalf("decode result json: %v", err)
	}
	if resResp.JobID != dispResp.JobID || resResp.Status != protocol.JobStatusQueued {
		t.Fatalf("unexpected result response: %+v", resResp)
	}

	// 4. cancel <job_id> --json
	var canOut, canErr bytes.Buffer
	if code := runCLI(ctx, []string{"cancel", dispResp.JobID, "--json"}, &canOut, &canErr, defaultDeps()); code != 0 {
		t.Fatalf("cancel --json: exit=%d stderr=%s", code, canErr.String())
	}
	assertCamelCaseJSONKeys(t, canOut.Bytes(), []string{"jobId", "status", "cancelRequested"})
	var canResp protocol.CancelJobResponse
	if err := json.Unmarshal(canOut.Bytes(), &canResp); err != nil {
		t.Fatalf("decode cancel json: %v", err)
	}
	if canResp.JobID != dispResp.JobID || canResp.Status != protocol.JobStatusCanceled {
		t.Fatalf("unexpected cancel response: %+v", canResp)
	}
}

func TestWorkerInstallTemplatesLinuxAndDarwin(t *testing.T) {
	t.Run("linux systemd unit", func(t *testing.T) {
		homeDir := t.TempDir()
		cfgPath := filepath.Join(homeDir, "my config", "worker.toml")
		deps := cliDeps{
			goos:    "linux",
			homeDir: func() (string, error) { return homeDir, nil },
			executable: func() (string, error) {
				return "/usr/local/bin/agent-pool", nil
			},
		}

		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"worker", "install", "--config", cfgPath, "--dry-run"}, &stdout, &stderr, deps)
		if code != 0 {
			t.Fatalf("linux worker install --dry-run: exit=%d stderr=%s", code, stderr.String())
		}
		out := stdout.String()
		if !strings.Contains(out, "[Unit]") || !strings.Contains(out, "[Service]") || !strings.Contains(out, "[Install]") {
			t.Fatalf("missing systemd sections:\n%s", out)
		}
		if !strings.Contains(out, "Restart=on-failure") {
			t.Errorf("expected Restart=on-failure in systemd unit:\n%s", out)
		}
		// Config path with space must be quoted for systemd ExecStart.
		if !strings.Contains(out, `"`+cfgPath+`"`) {
			t.Errorf("expected quoted config path %q in systemd unit:\n%s", cfgPath, out)
		}
		if !strings.Contains(out, "agent-pool-worker.service") {
			t.Errorf("expected target path mentioning agent-pool-worker.service:\n%s", out)
		}
	})

	t.Run("darwin launchd plist", func(t *testing.T) {
		homeDir := t.TempDir()
		cfgPath := filepath.Join(homeDir, "a&b", "worker.toml")
		deps := cliDeps{
			goos:    "darwin",
			homeDir: func() (string, error) { return homeDir, nil },
			executable: func() (string, error) {
				return "/opt/homebrew/bin/agent-pool", nil
			},
		}

		var stdout, stderr bytes.Buffer
		code := runCLI(context.Background(), []string{"worker", "install", "--config", cfgPath, "--dry-run"}, &stdout, &stderr, deps)
		if code != 0 {
			t.Fatalf("darwin worker install --dry-run: exit=%d stderr=%s", code, stderr.String())
		}
		out := stdout.String()
		if !strings.Contains(out, "com.agent-pool.worker.plist") {
			t.Errorf("expected target path mentioning com.agent-pool.worker.plist:\n%s", out)
		}
		if !strings.Contains(out, "<key>KeepAlive</key>") {
			t.Errorf("expected KeepAlive key in plist:\n%s", out)
		}
		if !strings.Contains(out, "a&amp;b/worker.toml") {
			t.Errorf("expected XML-escaped config path in plist:\n%s", out)
		}

		// Extract XML portion and validate well-formed XML.
		idx := strings.Index(out, "<?xml")
		if idx < 0 {
			t.Fatalf("missing XML declaration in plist output:\n%s", out)
		}
		dec := xml.NewDecoder(strings.NewReader(out[idx:]))
		for {
			if _, err := dec.Token(); err != nil {
				if errors.Is(err, io.EOF) {
					break
				}
				t.Fatalf("invalid XML in launchd plist: %v\n%s", err, out[idx:])
			}
		}
	})
}

func TestWorkerPauseResumeAndStatsCLI(t *testing.T) {
	ctx := context.Background()
	tmpDir := t.TempDir()
	stateDir := filepath.Join(tmpDir, "state")
	cfgPath := filepath.Join(tmpDir, "worker.toml")
	cfgContent := "[worker]\nstate_dir = " + jsonString(t, stateDir) + "\n"
	writeTestFile(t, cfgPath, cfgContent)

	// 1. worker pause
	var pauseOut, pauseErr bytes.Buffer
	if code := runCLI(ctx, []string{"worker", "pause", "--config", cfgPath, "--reason", "deep work"}, &pauseOut, &pauseErr, defaultDeps()); code != 0 {
		t.Fatalf("worker pause: exit=%d stderr=%s", code, pauseErr.String())
	}
	pauseData, err := os.ReadFile(filepath.Join(stateDir, "paused"))
	if err != nil {
		t.Fatalf("expected pause marker file: %v", err)
	}
	if strings.TrimSpace(string(pauseData)) != "deep work" {
		t.Fatalf("pause marker = %q, want %q", strings.TrimSpace(string(pauseData)), "deep work")
	}

	// 2. worker resume
	var resumeOut, resumeErr bytes.Buffer
	if code := runCLI(ctx, []string{"worker", "resume", "--config", cfgPath}, &resumeOut, &resumeErr, defaultDeps()); code != 0 {
		t.Fatalf("worker resume: exit=%d stderr=%s", code, resumeErr.String())
	}
	if _, err := os.Stat(filepath.Join(stateDir, "paused")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("expected pause marker to be removed, stat err = %v", err)
	}

	// 3. Seed usage.db and run worker stats --since 7d --json
	ledger, err := quota.OpenLedger(filepath.Join(stateDir, "usage.db"))
	if err != nil {
		t.Fatalf("OpenLedger: %v", err)
	}
	now := time.Now().UTC()
	if err := ledger.RecordRun(ctx, quota.RunRecord{
		JobID:          "job-recent",
		Repo:           "org/core-api",
		Skill:          "summarize",
		Status:         protocol.JobStatusSucceeded,
		Provider:       "claude",
		Model:          "claude-sonnet-4-5",
		InputTokens:    1000,
		OutputTokens:   200,
		EffectiveUnits: 2000,
		TurnsUsed:      3,
		DurationMs:     4500,
		FinishedAt:     now.Add(-2 * time.Hour),
	}); err != nil {
		t.Fatalf("RecordRun recent: %v", err)
	}
	if err := ledger.RecordRun(ctx, quota.RunRecord{
		JobID:          "job-old",
		Repo:           "org/core-api",
		Skill:          "summarize",
		Status:         protocol.JobStatusSucceeded,
		Provider:       "claude",
		Model:          "claude-sonnet-4-5",
		InputTokens:    5000,
		OutputTokens:   1000,
		EffectiveUnits: 10000,
		TurnsUsed:      5,
		DurationMs:     9000,
		FinishedAt:     now.Add(-14 * 24 * time.Hour),
	}); err != nil {
		t.Fatalf("RecordRun old: %v", err)
	}
	if err := ledger.Close(); err != nil {
		t.Fatalf("ledger.Close: %v", err)
	}

	var statsOut, statsErr bytes.Buffer
	if code := runCLI(ctx, []string{"worker", "stats", "--config", cfgPath, "--since", "7d", "--json"}, &statsOut, &statsErr, defaultDeps()); code != 0 {
		t.Fatalf("worker stats --json: exit=%d stderr=%s", code, statsErr.String())
	}
	assertCamelCaseJSONKeys(t, statsOut.Bytes(), []string{"totalJobs", "jobsByStatus", "totalTokens", "effectiveUnits", "totalDurationMs", "recentRuns"})
	var stats quota.LedgerStats
	if err := json.Unmarshal(statsOut.Bytes(), &stats); err != nil {
		t.Fatalf("decode stats json: %v", err)
	}
	if stats.TotalJobs != 1 || stats.EffectiveUnits != 2000 {
		t.Fatalf("expected 1 recent job with 2000 effectiveUnits, got %+v", stats)
	}
	if len(stats.RecentRuns) != 1 || stats.RecentRuns[0].JobID != "job-recent" {
		t.Fatalf("unexpected recentRuns: %+v", stats.RecentRuns)
	}
}

func writeTestFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o750); err != nil {
		t.Fatalf("MkdirAll %q: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("WriteFile %q: %v", path, err)
	}
}

func jsonString(t *testing.T, s string) string {
	t.Helper()
	b, err := json.Marshal(s)
	if err != nil {
		t.Fatalf("json.Marshal(%q): %v", s, err)
	}
	return string(b)
}

func assertCamelCaseJSONKeys(t *testing.T, raw []byte, requiredKeys []string) {
	t.Helper()
	var m map[string]any
	if err := json.Unmarshal(raw, &m); err != nil {
		t.Fatalf("invalid JSON %q: %v", string(raw), err)
	}
	for k := range m {
		if strings.Contains(k, "_") {
			t.Errorf("JSON key %q contains underscore (expected camelCase)", k)
		}
	}
	for _, rk := range requiredKeys {
		if _, ok := m[rk]; !ok {
			t.Errorf("missing expected camelCase JSON key %q in %s", rk, string(raw))
		}
	}
}
