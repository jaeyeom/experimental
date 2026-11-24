package harness_test

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/harness"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

func openFixture(t *testing.T, name string) *os.File {
	t.Helper()
	f, err := os.Open(filepath.Join("testdata", name))
	if err != nil {
		t.Fatalf("open fixture %s: %v", name, err)
	}
	t.Cleanup(func() { _ = f.Close() })
	return f
}

func TestClaudeBuildCommand(t *testing.T) {
	var r harness.Runner = harness.ClaudeRunner{}
	if got := r.Name(); got != "claude" {
		t.Fatalf("Name() = %q, want %q", got, "claude")
	}

	prettySchema := json.RawMessage(`{
		"type": "object",
		"properties": {
			"summary": { "type": "string" }
		},
		"required": ["summary"]
	}`)

	spec := harness.JobSpec{
		SkillName:        "review-pr",
		SkillMarkdown:    "# Review PR\nInspect the diff carefully.\n",
		CanonicalArgs:    []byte(`{"pr":42}`),
		OutputSchema:     prettySchema,
		EmptyMCPPath:     "/tmp/empty-mcp.json",
		MaxTurns:         12,
		MaxEffectiveToks: 150000,
	}

	cmd, err := r.BuildCommand(spec)
	if err != nil {
		t.Fatalf("BuildCommand() error = %v", err)
	}

	if cmd.Agent != "claude" {
		t.Fatalf("Agent = %q, want %q", cmd.Agent, "claude")
	}

	wantPrompt := catalog.BuildPrompt(spec.SkillName, spec.SkillMarkdown, spec.CanonicalArgs)
	wantCompactSchema := `{"type":"object","properties":{"summary":{"type":"string"}},"required":["summary"]}`

	wantArgs := []string{
		"-p", wantPrompt,
		"--strict-mcp-config",
		"--mcp-config", "/tmp/empty-mcp.json",
		"--setting-sources", "project",
		"--output-format", "stream-json",
		"--verbose",
		"--json-schema", wantCompactSchema,
		"--max-turns", "12",
	}

	if !reflect.DeepEqual(cmd.AgentArgs, wantArgs) {
		t.Fatalf("AgentArgs mismatch:\ngot:  %#v\nwant: %#v", cmd.AgentArgs, wantArgs)
	}
	if !strings.Contains(wantPrompt, "SECURITY NOTICE:") {
		t.Fatalf("expected security-framed prompt, got %q", wantPrompt)
	}
	if strings.Contains(wantCompactSchema, "\n") {
		t.Fatalf("expected compact single-line schema, got %q", wantCompactSchema)
	}
}

func TestClaudeParseStreamSuccessStructuredOutput(t *testing.T) {
	f := openFixture(t, "success_structured.jsonl")

	var progressEvents []harness.StreamProgress
	budgetCalled := false

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(f, catalog.Limits{
		MaxTurns:           10,
		MaxEffectiveTokens: 100000,
	}, harness.StreamCallbacks{
		OnProgress: func(p harness.StreamProgress) {
			progressEvents = append(progressEvents, p)
		},
		OnBudgetExceeded: func(_ harness.StreamProgress, _ string) {
			budgetCalled = true
		},
	})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if budgetCalled || res.BudgetExceeded {
		t.Fatalf("unexpected budget exceeded: called=%v res=%+v", budgetCalled, res)
	}
	if res.IsError {
		t.Fatalf("unexpected IsError: %+v", res)
	}
	if res.Provider != "claude" {
		t.Errorf("Provider = %q, want %q", res.Provider, "claude")
	}
	if res.Model != "claude-sonnet-4-6" {
		t.Errorf("Model = %q, want %q", res.Model, "claude-sonnet-4-6")
	}
	if res.TurnsUsed != 2 {
		t.Errorf("TurnsUsed = %d, want 2", res.TurnsUsed)
	}

	wantTokens := protocol.TokenUsage{
		Input:         2500,
		CacheRead:     1500,
		CacheCreation: 200,
		Output:        350,
	}
	if res.Tokens != wantTokens {
		t.Errorf("Tokens = %+v, want %+v", res.Tokens, wantTokens)
	}
	if res.EffectiveUnits != wantTokens.EffectiveUnits() {
		t.Errorf("EffectiveUnits = %d, want %d", res.EffectiveUnits, wantTokens.EffectiveUnits())
	}

	var gotDeliverable map[string]any
	if err := json.Unmarshal(res.Deliverable, &gotDeliverable); err != nil {
		t.Fatalf("unmarshal deliverable %q: %v", string(res.Deliverable), err)
	}
	if gotDeliverable["status"] != "ok" {
		t.Errorf("deliverable status = %v, want ok", gotDeliverable["status"])
	}

	if len(progressEvents) == 0 {
		t.Fatal("expected OnProgress callbacks, got none")
	}
	last := progressEvents[len(progressEvents)-1]
	if last.TurnsUsed != 2 || last.Tokens != wantTokens || last.Model != "claude-sonnet-4-6" {
		t.Errorf("last OnProgress = %+v, want turns=2 tokens=%+v model=claude-sonnet-4-6", last, wantTokens)
	}
}

func TestClaudeParseStreamSuccessJSONResultFallback(t *testing.T) {
	f := openFixture(t, "success_result_fallback.jsonl")

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(f, catalog.Limits{
		MaxTurns:           10,
		MaxEffectiveTokens: 100000,
	}, harness.StreamCallbacks{})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if res.IsError || res.BudgetExceeded {
		t.Fatalf("unexpected error/budget state: %+v", res)
	}
	if res.Model != "claude-opus-4-6" {
		t.Errorf("Model = %q, want %q", res.Model, "claude-opus-4-6")
	}
	if res.TurnsUsed != 1 {
		t.Errorf("TurnsUsed = %d, want 1", res.TurnsUsed)
	}

	wantTokens := protocol.TokenUsage{
		Input:     400,
		CacheRead: 100,
		Output:    50,
	}
	if res.Tokens != wantTokens {
		t.Errorf("Tokens = %+v, want %+v", res.Tokens, wantTokens)
	}
	if res.EffectiveUnits != wantTokens.EffectiveUnits() {
		t.Errorf("EffectiveUnits = %d, want %d", res.EffectiveUnits, wantTokens.EffectiveUnits())
	}

	var got map[string]any
	if err := json.Unmarshal(res.Deliverable, &got); err != nil {
		t.Fatalf("unmarshal fallback deliverable %q: %v", string(res.Deliverable), err)
	}
	if got["summary"] != "all checks passed" {
		t.Errorf("summary = %v, want %q", got["summary"], "all checks passed")
	}
}

func TestClaudeParseStreamMidFlightTokenKill(t *testing.T) {
	// Read fixture into lines and feed through a pipe/tracking reader so we can
	// verify turn 3 is not processed once turn 2 breaches MaxEffectiveTokens.
	// Turn 1: input=100, output=50 -> effective = 100 + 250 = 350
	// Turn 2: + input=200, output=200 -> total input=300, output=250 -> effective = 300 + 1250 = 1550
	// Limit: 1000 -> turn 2 breaches immediately.
	f := openFixture(t, "budget_token_exceeded.jsonl")

	var exceededProgress harness.StreamProgress
	var exceededReason string
	exceededCount := 0
	progressTurns := 0

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(f, catalog.Limits{
		MaxTurns:           10,
		MaxEffectiveTokens: 1000,
	}, harness.StreamCallbacks{
		OnProgress: func(p harness.StreamProgress) {
			progressTurns = p.TurnsUsed
		},
		OnBudgetExceeded: func(p harness.StreamProgress, reason string) {
			exceededCount++
			exceededProgress = p
			exceededReason = reason
		},
	})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if !res.BudgetExceeded {
		t.Fatalf("BudgetExceeded = false, want true")
	}
	if exceededCount != 1 {
		t.Fatalf("OnBudgetExceeded called %d times, want 1", exceededCount)
	}
	if res.TurnsUsed != 2 || progressTurns != 2 || exceededProgress.TurnsUsed != 2 {
		t.Fatalf("expected immediate stop at turn 2, got res.TurnsUsed=%d progressTurns=%d exceeded=%+v",
			res.TurnsUsed, progressTurns, exceededProgress)
	}
	if res.EffectiveUnits != 1550 || exceededProgress.EffectiveUnits != 1550 {
		t.Fatalf("EffectiveUnits = %d (cb %d), want 1550", res.EffectiveUnits, exceededProgress.EffectiveUnits)
	}
	if res.ExceedReason == "" || exceededReason == "" {
		t.Fatalf("expected non-empty ExceedReason, got res=%q cb=%q", res.ExceedReason, exceededReason)
	}
	if len(res.Deliverable) != 0 {
		t.Fatalf("expected empty Deliverable after mid-flight kill, got %s", string(res.Deliverable))
	}
}

func TestClaudeParseStreamMidFlightTurnKill(t *testing.T) {
	f := openFixture(t, "budget_turn_exceeded.jsonl")

	var exceededProgress harness.StreamProgress
	var exceededReason string
	exceededCount := 0

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(f, catalog.Limits{
		MaxTurns:           2,
		MaxEffectiveTokens: 100000,
	}, harness.StreamCallbacks{
		OnBudgetExceeded: func(p harness.StreamProgress, reason string) {
			exceededCount++
			exceededProgress = p
			exceededReason = reason
		},
	})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if !res.BudgetExceeded {
		t.Fatalf("BudgetExceeded = false, want true")
	}
	if exceededCount != 1 {
		t.Fatalf("OnBudgetExceeded called %d times, want 1", exceededCount)
	}
	if res.TurnsUsed != 3 || exceededProgress.TurnsUsed != 3 {
		t.Fatalf("TurnsUsed = %d (cb %d), want 3", res.TurnsUsed, exceededProgress.TurnsUsed)
	}
	if res.ExceedReason == "" || exceededReason == "" {
		t.Fatalf("expected non-empty ExceedReason, got res=%q cb=%q", res.ExceedReason, exceededReason)
	}
	if len(res.Deliverable) != 0 {
		t.Fatalf("expected empty Deliverable after mid-flight kill, got %s", string(res.Deliverable))
	}
}

func TestClaudeParseStreamErrorResult(t *testing.T) {
	f := openFixture(t, "error_result.jsonl")

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(f, catalog.Limits{
		MaxTurns:           10,
		MaxEffectiveTokens: 100000,
	}, harness.StreamCallbacks{})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if !res.IsError {
		t.Fatalf("IsError = false, want true")
	}
	if !strings.Contains(res.ErrorMessage, "rate limit exceeded") {
		t.Errorf("ErrorMessage = %q, want substring %q", res.ErrorMessage, "rate limit exceeded")
	}
	if res.TurnsUsed != 1 {
		t.Errorf("TurnsUsed = %d, want 1", res.TurnsUsed)
	}
}

func TestClaudeParseStreamToleratesNoiseAndLargeLine(t *testing.T) {
	largePayload := strings.Repeat("x", 256*1024)
	resultEvent := map[string]any{
		"type":     "result",
		"is_error": false,
		"structured_output": map[string]any{
			"blob": largePayload,
		},
	}
	largeLine, err := json.Marshal(resultEvent)
	if err != nil {
		t.Fatalf("marshal large line: %v", err)
	}
	if len(largeLine) <= 256*1024 {
		t.Fatalf("expected >256 KiB JSON line, got %d bytes", len(largeLine))
	}

	var buf bytes.Buffer
	buf.WriteString("\n")
	buf.WriteString("WARNING: stray diagnostic line on stdout\n")
	buf.WriteString("   not valid json {broken\n")
	buf.WriteString(`{"type":"system","model":"claude-sonnet-4-6"}` + "\n")
	buf.WriteString(`{"type":"assistant","message":{"id":"msg_big","usage":{"input_tokens":10,"output_tokens":5}}}` + "\n")
	buf.Write(largeLine)
	buf.WriteString("\n")

	r := harness.ClaudeRunner{}
	res, err := r.ParseStream(io.Reader(&buf), catalog.Limits{
		MaxTurns:           10,
		MaxEffectiveTokens: 100000,
	}, harness.StreamCallbacks{})
	if err != nil {
		t.Fatalf("ParseStream() error = %v", err)
	}
	if res.IsError || res.BudgetExceeded {
		t.Fatalf("unexpected error/budget state: %+v", res)
	}
	if res.Model != "claude-sonnet-4-6" {
		t.Errorf("Model = %q, want %q", res.Model, "claude-sonnet-4-6")
	}

	var got struct {
		Blob string `json:"blob"`
	}
	if err := json.Unmarshal(res.Deliverable, &got); err != nil {
		t.Fatalf("unmarshal large deliverable: %v", err)
	}
	if len(got.Blob) != len(largePayload) {
		t.Errorf("blob len = %d, want %d", len(got.Blob), len(largePayload))
	}
}
