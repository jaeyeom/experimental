// Package lastturn reads when an agent last took a turn, for the agent
// kinds that record it. The idle tracker uses it to bound the idle time
// of an agent it has not seen change state. Every read is read-only,
// and a source that cannot be read reports nothing.
package lastturn

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// sessionID is a session id safe to put in a file pattern.
var sessionID = regexp.MustCompile(`^[A-Za-z0-9_-]+$`)

// ClaudeProjectsDir is where Claude Code keeps its session transcripts:
// $CLAUDE_CONFIG_DIR/projects, else ~/.claude/projects. It is "" when
// neither is set in env.
func ClaudeProjectsDir(env []string) string {
	getenv := func(key string) string {
		for _, entry := range env {
			if k, v, ok := strings.Cut(entry, "="); ok && k == key {
				return v
			}
		}
		return ""
	}
	if dir := getenv("CLAUDE_CONFIG_DIR"); dir != "" {
		return filepath.Join(dir, "projects")
	}
	if home := getenv("HOME"); home != "" {
		return filepath.Join(home, ".claude", "projects")
	}
	return ""
}

// Claude is the last turn of a Claude Code agent on this machine: the
// modification time of its session transcript,
// <projectsDir>/<encoded cwd>/<session>.jsonl. Any project directory
// matches, since session ids are unique.
func Claude(projectsDir string) func(adapter.Agent) (time.Time, bool) {
	return func(a adapter.Agent) (time.Time, bool) {
		if projectsDir == "" || a.Kind != "claude" || !sessionID.MatchString(a.SessionID) {
			return time.Time{}, false
		}
		if machine, _, _, err := hubcall.ParseHandle(a.Handle); err != nil || machine != hubcall.LocalMachine {
			return time.Time{}, false
		}
		matches, err := filepath.Glob(filepath.Join(projectsDir, "*", a.SessionID+".jsonl"))
		if err != nil {
			return time.Time{}, false
		}
		var last time.Time
		for _, path := range matches {
			if info, err := os.Stat(path); err == nil && info.ModTime().After(last) {
				last = info.ModTime()
			}
		}
		return last, !last.IsZero()
	}
}

// Turn is one user or assistant message in an agent transcript.
type Turn struct {
	Role              string
	Text              string
	IsAPIErrorMessage bool
	Model             string
}

// ClaudeTranscript reads the turns of a Claude Code agent on this machine
// from its session transcript JSONL. ok is false when the agent kind has
// no readable transcript or no transcript file is found.
func ClaudeTranscript(projectsDir string) func(adapter.Agent) ([]Turn, bool) {
	return func(a adapter.Agent) ([]Turn, bool) {
		if projectsDir == "" || a.Kind != "claude" || !sessionID.MatchString(a.SessionID) {
			return nil, false
		}
		if machine, _, _, err := hubcall.ParseHandle(a.Handle); err != nil || machine != hubcall.LocalMachine {
			return nil, false
		}
		matches, err := filepath.Glob(filepath.Join(projectsDir, "*", a.SessionID+".jsonl"))
		if err != nil || len(matches) == 0 {
			return nil, false
		}
		var bestPath string
		var bestTime time.Time
		for _, path := range matches {
			if info, err := os.Stat(path); err == nil && (bestPath == "" || info.ModTime().After(bestTime)) {
				bestPath = path
				bestTime = info.ModTime()
			}
		}
		if bestPath == "" {
			return nil, false
		}
		turns, err := ReadClaudeTurns(bestPath)
		if err != nil {
			return nil, false
		}
		return turns, true
	}
}

type claudeRecord struct {
	Type              string `json:"type"`
	IsAPIErrorMessage bool   `json:"isApiErrorMessage"`
	Model             string `json:"model"`
	Message           struct {
		Role              string          `json:"role"`
		IsAPIErrorMessage bool            `json:"isApiErrorMessage"`
		Model             string          `json:"model"`
		Content           json.RawMessage `json:"content"`
	} `json:"message"`
}

type claudeContentBlock struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// ReadClaudeTurns parses user and assistant turns from a Claude Code JSONL
// transcript file, read-only. Malformed lines are skipped.
func ReadClaudeTurns(path string) ([]Turn, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("open transcript: %w", err)
	}
	defer f.Close()
	var turns []Turn
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 0, 64*1024), 8*1024*1024)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		var rec claudeRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			continue
		}
		role := rec.Message.Role
		if role == "" {
			role = rec.Type
		}
		if role != "user" && role != "assistant" {
			continue
		}
		text := extractClaudeContent(rec.Message.Content)
		if text == "" {
			continue
		}
		model := rec.Message.Model
		if model == "" {
			model = rec.Model
		}
		turns = append(turns, Turn{
			Role:              role,
			Text:              text,
			IsAPIErrorMessage: rec.IsAPIErrorMessage || rec.Message.IsAPIErrorMessage,
			Model:             model,
		})
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("scan transcript: %w", err)
	}
	return turns, nil
}

func extractClaudeContent(raw json.RawMessage) string {
	if len(raw) == 0 {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err == nil {
		return s
	}
	var blocks []claudeContentBlock
	if err := json.Unmarshal(raw, &blocks); err == nil {
		var parts []string
		for _, b := range blocks {
			if b.Text != "" {
				parts = append(parts, b.Text)
			}
		}
		return strings.Join(parts, "\n")
	}
	return ""
}
