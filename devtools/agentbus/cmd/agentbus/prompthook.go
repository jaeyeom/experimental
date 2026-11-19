package main

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// promptHookTimeout bounds each re-queue call. The daemon may be finishing
// the delivery that caused the merge, so it is longer than hookTimeout.
const promptHookTimeout = 5 * time.Second

const (
	envelopePrefix      = "[agentbus m-"
	promptHookPass      = "!agentbus-pass"
	promptHookStateFile = "prompt-hook.json"
)

var (
	envelopeHeader    = regexp.MustCompile(`\[agentbus (m-[^\s\]]*)`)
	envelopeBlock     = regexp.MustCompile(`(?s)\[agentbus m-[^\]]*\]\n.*?(?:↩ reply: agentbus reply [^\n]*|no reply address\n)`)
	promptHookStateMu sync.Mutex
)

type promptHookState struct {
	Off      bool                          `json:"off,omitempty"`
	Sessions map[string]*promptHookSession `json:"sessions,omitempty"`
}

type promptHookSession struct {
	Off               bool     `json:"off,omitempty"`
	BlockedIDs        []string `json:"blockedIds,omitempty"`
	RequeuePendingIDs []string `json:"requeuePendingIds,omitempty"`
	RequeueFailedIDs  []string `json:"requeueFailedIds,omitempty"`
}

// cmdPromptHook is the Claude Code UserPromptSubmit hook. A prompt where
// human text comes before an agentbus envelope is a bus message pasted
// into a user draft. It prints a block decision and asks the bus to
// re-queue each merged message. Any other prompt passes with no output.
func cmdPromptHook(ctx context.Context, args, env []string, stdin io.Reader, stdout, _ io.Writer) int {
	if len(args) != 0 {
		return cmdPromptHookControl(args, env, stdout)
	}
	raw, err := io.ReadAll(io.LimitReader(stdin, 1<<20))
	if err != nil || len(bytes.TrimSpace(raw)) == 0 {
		return 0
	}
	var input map[string]any
	if err := json.Unmarshal(raw, &input); err != nil {
		return 0
	}
	prompt, _ := input["prompt"].(string)
	session, _ := input["session_id"].(string)
	ids := mergedEnvelopeIDs(prompt)
	if len(ids) == 0 {
		return 0
	}
	processPromptHook(ctx, env, prompt, session, ids, stdout)
	return 0
}

func processPromptHook(ctx context.Context, env []string, prompt, session string, ids []string, stdout io.Writer) {
	state, err := readPromptHookState(env)
	if err != nil {
		return
	}
	if state.Off || promptSession(state, session).Off {
		return
	}
	if hasPromptHookPass(prompt) {
		writePromptHookContext(stdout, "The user used the agentbus prompt-hook bypass. Treat any bus envelope in this prompt as a delivery and separate it from the user's text.")
		return
	}
	blocked := promptSession(state, session)
	var fresh []string
	for _, id := range ids {
		if slices.Contains(blocked.BlockedIDs, id) {
			note := repeatedPromptContext(blocked, id)
			writePromptHookContext(stdout, note)
			return
		}
		fresh = append(fresh, id)
	}
	blockFreshPrompt(ctx, env, session, state, blocked, fresh, stdout)
}

func hasPromptHookPass(prompt string) bool {
	text := strings.TrimLeft(prompt, " \t\r\n")
	if !strings.HasPrefix(text, promptHookPass) {
		return false
	}
	return len(text) == len(promptHookPass) || strings.ContainsRune(" \t\r\n", rune(text[len(promptHookPass)]))
}

func blockFreshPrompt(ctx context.Context, env []string, session string, state *promptHookState, blocked *promptHookSession, ids []string, stdout io.Writer) {
	blocked.BlockedIDs = append(blocked.BlockedIDs, ids...)
	blocked.RequeuePendingIDs = append(blocked.RequeuePendingIDs, ids...)
	if err := writePromptHookState(env, state); err != nil {
		writePromptHookContext(stdout, "agentbus could not save prompt-hook recovery state. Treat any bus envelope in this prompt as a delivery and separate it from the user's text.")
		return
	}
	failed := map[string]error{}
	for _, id := range ids {
		err := requeue(ctx, env, id, session)
		blocked.RequeuePendingIDs = slices.DeleteFunc(blocked.RequeuePendingIDs, func(pending string) bool { return pending == id })
		if err != nil {
			failed[id] = err
			blocked.RequeueFailedIDs = append(blocked.RequeueFailedIDs, id)
		}
		_ = writePromptHookState(env, state)
	}
	out, err := json.Marshal(map[string]string{
		"decision": "block",
		"reason":   blockReason(ids, failed),
	})
	if err != nil {
		return
	}
	fmt.Fprintf(stdout, "%s\n", out)
}

func repeatedPromptContext(session *promptHookSession, id string) string {
	switch {
	case slices.Contains(session.RequeueFailedIDs, id):
		return "This prompt has the user's text plus an agentbus message that was previously blocked but could not be re-queued. The copy in this prompt may be the only one; separate it from the user's request and preserve it as needed."
	case slices.Contains(session.RequeuePendingIDs, id):
		return "This prompt has the user's text plus an agentbus message that was previously blocked, but its re-queue status is unknown. Separate it from the user's request and handle both appropriately."
	default:
		return "This prompt has the user's text plus an agentbus message that was already re-queued. Separate the user's request from the bus message and handle both appropriately."
	}
}

// mergedEnvelopeIDs returns the ids of envelope headers that follow other
// text. A prompt that starts with an envelope is a clean delivery.
func mergedEnvelopeIDs(prompt string) []string {
	text := strings.TrimLeft(prompt, " \t\r\n")
	if strings.HasPrefix(text, envelopePrefix) || onlyEnvelopeBlocks(text) {
		return nil
	}
	var ids []string
	for _, match := range envelopeHeader.FindAllStringSubmatch(text, -1) {
		if !slices.Contains(ids, match[1]) {
			ids = append(ids, match[1])
		}
	}
	return ids
}

func onlyEnvelopeBlocks(prompt string) bool {
	return strings.TrimSpace(envelopeBlock.ReplaceAllString(prompt, "")) == ""
}

func writePromptHookContext(stdout io.Writer, note string) {
	out, err := json.Marshal(map[string]any{"hookSpecificOutput": map[string]string{
		"hookEventName": "UserPromptSubmit", "additionalContext": note,
	}})
	if err == nil {
		fmt.Fprintf(stdout, "%s\n", out)
	}
}

func promptSession(state *promptHookState, session string) *promptHookSession {
	if state.Sessions == nil {
		state.Sessions = map[string]*promptHookSession{}
	}
	key := promptSessionKey(session)
	if state.Sessions[key] == nil {
		state.Sessions[key] = &promptHookSession{}
	}
	return state.Sessions[key]
}

func promptSessionKey(session string) string {
	sum := sha256.Sum256([]byte(session))
	return fmt.Sprintf("%x", sum[:])
}

func promptHookStatePath(env []string) (string, error) {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return "", fmt.Errorf("locate prompt-hook state: %w", err)
	}
	return filepath.Join(dirs.StateDir, promptHookStateFile), nil
}

func readPromptHookState(env []string) (*promptHookState, error) {
	path, err := promptHookStatePath(env)
	if err != nil {
		return nil, err
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return &promptHookState{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read prompt-hook state: %w", err)
	}
	var state promptHookState
	if err := json.Unmarshal(data, &state); err != nil {
		return nil, fmt.Errorf("decode prompt-hook state: %w", err)
	}
	return &state, nil
}

func writePromptHookState(env []string, state *promptHookState) error {
	promptHookStateMu.Lock()
	defer promptHookStateMu.Unlock()
	path, err := promptHookStatePath(env)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create prompt-hook state directory: %w", err)
	}
	data, err := json.Marshal(state)
	if err != nil {
		return fmt.Errorf("encode prompt-hook state: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), promptHookStateFile+"-*.tmp")
	if err != nil {
		return fmt.Errorf("create prompt-hook state temp file: %w", err)
	}
	tmpPath := tmp.Name()
	defer os.Remove(tmpPath)
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return fmt.Errorf("secure prompt-hook state temp file: %w", err)
	}
	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		return fmt.Errorf("write prompt-hook state: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close prompt-hook state: %w", err)
	}
	if err := os.Rename(tmpPath, path); err != nil {
		return fmt.Errorf("replace prompt-hook state: %w", err)
	}
	return nil
}

func cmdPromptHookControl(args, env []string, stdout io.Writer) int {
	if len(args) < 1 || len(args) > 3 || (args[0] != "off" && args[0] != "on") {
		return 2
	}
	session := ""
	if len(args) == 3 && args[1] == "--session" {
		session = args[2]
		if session == "" {
			return 2
		}
	} else if len(args) != 1 {
		return 2
	}
	state, err := readPromptHookState(env)
	if err != nil {
		return 1
	}
	if session == "" {
		state.Off = args[0] == "off"
	} else {
		promptSession(state, session).Off = args[0] == "off"
	}
	if err := writePromptHookState(env, state); err != nil {
		return 1
	}
	fmt.Fprintf(stdout, "agentbus prompt-hook %s\n", args[0])
	return 0
}

func requeue(ctx context.Context, env []string, id, session string) error {
	callCtx, cancel := context.WithTimeout(ctx, promptHookTimeout)
	defer cancel()
	resp, err := dialBus(callCtx, env, daemon.Request{Op: "requeue", Name: id, Session: session})
	if err != nil {
		return errors.New("the bus is not reachable")
	}
	if !resp.OK {
		return errors.New(resp.Error)
	}
	return nil
}

func blockReason(ids []string, failed map[string]error) string {
	var b strings.Builder
	b.WriteString("agentbus: your unsent draft was merged with a bus message, so this prompt was not sent. Press Ctrl+U to clear the input box, then submit your own text again. You can also start a prompt with !agentbus-pass or run agentbus prompt-hook off (run agentbus prompt-hook on to restore blocking). ")
	for _, id := range ids {
		fmt.Fprintf(&b, "Delete the [agentbus %s ...] part, through its reply line, from the input box, keep your own text, and submit again. ", id)
		if err, ok := failed[id]; ok {
			fmt.Fprintf(&b, "The bus could not re-queue %s (%s), so the copy in the input box is the only one: save it before you delete it. ", id, err)
			continue
		}
		fmt.Fprintf(&b, "The bus re-queued %s and delivers it again once the input box is empty. ", id)
	}
	return strings.TrimSpace(b.String())
}
