// Package lastturn reads when an agent last took a turn, for the agent
// kinds that record it. The idle tracker uses it to bound the idle time
// of an agent it has not seen change state. Every read is read-only,
// and a source that cannot be read reports nothing.
package lastturn

import (
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
