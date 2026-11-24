package harness

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	claudeProviderName   = "claude"
	scannerInitialBufLen = 64 * 1024
	scannerMaxLineBytes  = 4 * 1024 * 1024
)

// ClaudeRunner implements Runner for the headless Claude Code CLI using its
// stream-json output format.
type ClaudeRunner struct{}

var _ Runner = ClaudeRunner{}

// Name returns the provider identifier for Claude Code.
func (ClaudeRunner) Name() string {
	return claudeProviderName
}

// BuildCommand constructs the exact argv slice for running Claude Code
// headlessly with strict MCP isolation, project settings, compact JSON Schema,
// and turn limits.
func (ClaudeRunner) BuildCommand(spec JobSpec) (CommandSpec, error) {
	if spec.EmptyMCPPath == "" {
		return CommandSpec{}, errors.New("empty MCP config path is required")
	}
	if spec.MaxTurns <= 0 {
		return CommandSpec{}, fmt.Errorf("max turns must be positive, got %d", spec.MaxTurns)
	}
	trimmedSchema := bytes.TrimSpace(spec.OutputSchema)
	if len(trimmedSchema) == 0 {
		return CommandSpec{}, errors.New("output schema is required")
	}

	var compactSchema bytes.Buffer
	if err := json.Compact(&compactSchema, trimmedSchema); err != nil {
		return CommandSpec{}, fmt.Errorf("compact output schema: %w", err)
	}

	prompt := catalog.BuildPrompt(spec.SkillName, spec.SkillMarkdown, spec.CanonicalArgs)
	args := []string{
		"-p", prompt,
		"--strict-mcp-config",
		"--mcp-config", spec.EmptyMCPPath,
		"--setting-sources", "project",
		"--output-format", "stream-json",
		"--verbose",
		"--json-schema", compactSchema.String(),
		"--max-turns", strconv.Itoa(spec.MaxTurns),
	}

	return CommandSpec{
		Agent:     claudeProviderName,
		AgentArgs: args,
	}, nil
}

// ParseStream consumes a Claude Code stream-json JSONL stream, tracking turns
// and token usage in real time, enforcing mid-flight turn and effective-token
// budgets, and extracting the final structured deliverable or error details.
func (ClaudeRunner) ParseStream(r io.Reader, limits catalog.Limits, cb StreamCallbacks) (RunResult, error) {
	state := newClaudeStreamState()

	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 0, scannerInitialBufLen), scannerMaxLineBytes)

	for scanner.Scan() {
		line := bytes.TrimSpace(scanner.Bytes())
		if len(line) == 0 {
			continue
		}

		var event map[string]json.RawMessage
		if err := json.Unmarshal(line, &event); err != nil {
			continue
		}

		if stopped := state.handleEvent(event, limits, cb); stopped {
			return state.result, nil
		}
	}

	if err := scanner.Err(); err != nil {
		return state.result, fmt.Errorf("scan claude stream: %w", err)
	}
	return state.result, nil
}

type claudeStreamState struct {
	result       RunResult
	seenMsgIDs   map[string]protocol.TokenUsage
	unnamedUsage protocol.TokenUsage
}

func newClaudeStreamState() *claudeStreamState {
	return &claudeStreamState{
		result:     RunResult{Provider: claudeProviderName},
		seenMsgIDs: make(map[string]protocol.TokenUsage),
	}
}

func (s *claudeStreamState) handleEvent(
	event map[string]json.RawMessage,
	limits catalog.Limits,
	cb StreamCallbacks,
) bool {
	eventType := extractString(event, "type")
	switch eventType {
	case "system":
		s.handleSystem(event)
		return false
	case "assistant":
		return s.handleAssistant(event, limits, cb)
	case "result":
		s.handleResult(event)
		return false
	default:
		return false
	}
}

func (s *claudeStreamState) handleSystem(event map[string]json.RawMessage) {
	if model := extractString(event, "model"); model != "" {
		s.result.Model = model
	}
}

func (s *claudeStreamState) handleAssistant(
	event map[string]json.RawMessage,
	limits catalog.Limits,
	cb StreamCallbacks,
) bool {
	msg := extractObject(event, "message")
	if model := extractString(msg, "model"); model != "" {
		s.result.Model = model
	} else if model := extractString(event, "model"); model != "" {
		s.result.Model = model
	}

	msgID := extractString(msg, "id")
	usage := extractUsage(extractObject(msg, "usage"))

	if msgID == "" {
		s.result.TurnsUsed++
		s.unnamedUsage = addTokenUsage(s.unnamedUsage, usage)
	} else {
		if _, seen := s.seenMsgIDs[msgID]; !seen {
			s.result.TurnsUsed++
		}
		if hasNonZeroUsage(usage) {
			s.seenMsgIDs[msgID] = usage
		} else if _, seen := s.seenMsgIDs[msgID]; !seen {
			s.seenMsgIDs[msgID] = protocol.TokenUsage{}
		}
	}

	s.result.Tokens = s.sumAssistantUsage()
	s.result.EffectiveUnits = s.result.Tokens.EffectiveUnits()

	progress := StreamProgress{
		TurnsUsed:      s.result.TurnsUsed,
		Tokens:         s.result.Tokens,
		EffectiveUnits: s.result.EffectiveUnits,
		Model:          s.result.Model,
	}
	if cb.OnProgress != nil {
		cb.OnProgress(progress)
	}

	reason := checkBudget(limits, s.result.TurnsUsed, s.result.EffectiveUnits)
	if reason == "" {
		return false
	}

	s.result.BudgetExceeded = true
	s.result.ExceedReason = reason
	if cb.OnBudgetExceeded != nil {
		cb.OnBudgetExceeded(progress, reason)
	}
	return true
}

func (s *claudeStreamState) handleResult(event map[string]json.RawMessage) {
	if model := extractString(event, "model"); model != "" {
		s.result.Model = model
	}

	if usage := extractUsage(extractObject(event, "usage")); hasNonZeroUsage(usage) {
		s.result.Tokens = usage
		s.result.EffectiveUnits = usage.EffectiveUnits()
	}

	if extractBool(event, "is_error") {
		s.result.IsError = true
		s.result.ErrorMessage = extractErrorMessage(event)
		return
	}

	if deliverable := extractDeliverable(event); len(deliverable) > 0 {
		s.result.Deliverable = deliverable
	}
}

func (s *claudeStreamState) sumAssistantUsage() protocol.TokenUsage {
	total := s.unnamedUsage
	for _, u := range s.seenMsgIDs {
		total = addTokenUsage(total, u)
	}
	return total
}

func checkBudget(limits catalog.Limits, turnsUsed int, effective int64) string {
	if limits.MaxEffectiveTokens > 0 && effective > limits.MaxEffectiveTokens {
		return fmt.Sprintf("effective token units %d exceeded limit %d", effective, limits.MaxEffectiveTokens)
	}
	if limits.MaxTurns > 0 && turnsUsed > limits.MaxTurns {
		return fmt.Sprintf("turns used %d exceeded limit %d", turnsUsed, limits.MaxTurns)
	}
	return ""
}

func addTokenUsage(a, b protocol.TokenUsage) protocol.TokenUsage {
	return protocol.TokenUsage{
		Input:         a.Input + b.Input,
		CacheRead:     a.CacheRead + b.CacheRead,
		CacheCreation: a.CacheCreation + b.CacheCreation,
		Output:        a.Output + b.Output,
	}
}

func hasNonZeroUsage(u protocol.TokenUsage) bool {
	return u.Input > 0 || u.CacheRead > 0 || u.CacheCreation > 0 || u.Output > 0
}

func extractUsage(obj map[string]json.RawMessage) protocol.TokenUsage {
	if len(obj) == 0 {
		return protocol.TokenUsage{}
	}
	return protocol.TokenUsage{
		Input:         extractInt64(obj, "input_tokens"),
		CacheRead:     extractInt64(obj, "cache_read_input_tokens"),
		CacheCreation: extractInt64(obj, "cache_creation_input_tokens"),
		Output:        extractInt64(obj, "output_tokens"),
	}
}

func extractDeliverable(event map[string]json.RawMessage) json.RawMessage {
	if raw, ok := event["structured_output"]; ok {
		trimmed := bytes.TrimSpace(raw)
		if len(trimmed) > 0 && !bytes.Equal(trimmed, []byte("null")) && json.Valid(trimmed) {
			return append(json.RawMessage(nil), trimmed...)
		}
	}

	resultStr := strings.TrimSpace(extractString(event, "result"))
	if strings.HasPrefix(resultStr, "{") && json.Valid([]byte(resultStr)) {
		return json.RawMessage(resultStr)
	}
	return nil
}

func extractErrorMessage(event map[string]json.RawMessage) string {
	if msg := extractString(event, "result"); msg != "" {
		return msg
	}
	if msg := extractString(event, "error"); msg != "" {
		return msg
	}
	if raw, ok := event["errors"]; ok {
		var errs []string
		if err := json.Unmarshal(raw, &errs); err == nil && len(errs) > 0 {
			return strings.Join(errs, "; ")
		}
	}
	return "claude execution failed"
}

func extractObject(obj map[string]json.RawMessage, key string) map[string]json.RawMessage {
	raw, ok := obj[key]
	if !ok {
		return nil
	}
	var nested map[string]json.RawMessage
	if err := json.Unmarshal(raw, &nested); err != nil {
		return nil
	}
	return nested
}

func extractString(obj map[string]json.RawMessage, key string) string {
	raw, ok := obj[key]
	if !ok {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return ""
	}
	return s
}

func extractInt64(obj map[string]json.RawMessage, key string) int64 {
	raw, ok := obj[key]
	if !ok {
		return 0
	}
	var n int64
	if err := json.Unmarshal(raw, &n); err != nil {
		return 0
	}
	return n
}

func extractBool(obj map[string]json.RawMessage, key string) bool {
	raw, ok := obj[key]
	if !ok {
		return false
	}
	var b bool
	if err := json.Unmarshal(raw, &b); err != nil {
		return false
	}
	return b
}
