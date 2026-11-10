package main

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// hookTimeout bounds a bypass record. A down bus returns before this on a refused connect.
const hookTimeout = time.Second

// cmdHook records one Claude Code SendMessage and always exits 0.
// Stdout stays empty so the tool call proceeds with no decision.
func cmdHook(ctx context.Context, args, env []string, stdin io.Reader, _, _ io.Writer) int {
	if len(args) != 0 {
		return 0
	}
	raw, err := io.ReadAll(io.LimitReader(stdin, 1<<20))
	if err != nil || len(bytes.TrimSpace(raw)) == 0 {
		return 0
	}
	session, target, body, ok := parseHook(raw)
	if !ok {
		return 0
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return 0
	}
	callCtx, cancel := context.WithTimeout(ctx, hookTimeout)
	defer cancel()
	_, _ = dialDaemon(callCtx, dirs.SocketPath, daemon.Request{
		Op:      "bypass",
		Name:    target,
		Body:    body,
		Session: session,
	})
	return 0
}

// parseHook reads the Claude Code hook stdin. session_id, to/recipient/target,
// and message/content/text/body are the fields the record keeps.
func parseHook(raw []byte) (session, target, body string, ok bool) {
	var doc map[string]any
	if err := json.Unmarshal(raw, &doc); err != nil {
		return "", "", "", false
	}
	session, _ = doc["session_id"].(string)
	tool, _ := doc["tool_input"].(map[string]any)
	target = firstString(tool, "to", "recipient", "target")
	body = firstString(tool, "message", "content", "text", "body")
	return session, target, body, true
}

func firstString(fields map[string]any, keys ...string) string {
	if fields == nil {
		return ""
	}
	for _, key := range keys {
		text, _ := fields[key].(string)
		if strings.TrimSpace(text) != "" {
			return text
		}
	}
	return ""
}
