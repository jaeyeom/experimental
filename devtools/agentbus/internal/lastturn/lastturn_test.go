package lastturn

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

func TestClaudeIsTheTranscriptModTime(t *testing.T) {
	dir := t.TempDir()
	transcript := filepath.Join(dir, "-src-repo", "sess-1.jsonl")
	if err := os.MkdirAll(filepath.Dir(transcript), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(transcript, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	at := time.Date(2026, 11, 11, 6, 0, 0, 0, time.UTC)
	if err := os.Chtimes(transcript, at, at); err != nil {
		t.Fatal(err)
	}
	last := Claude(dir)
	claude := adapter.Agent{Handle: "herdr:local/sess-1/w1:p1", Kind: "claude", SessionID: "sess-1"}
	if got, ok := last(claude); !ok || !got.Equal(at) {
		t.Fatalf("Claude(claude) = %s, %v; want %s", got, ok, at)
	}
	for name, a := range map[string]adapter.Agent{
		"another kind":                {Handle: claude.Handle, Kind: "codex", SessionID: "sess-1"},
		"another machine":             {Handle: "herdr:mac/sess-1/w1:p1", Kind: "claude", SessionID: "sess-1"},
		"no transcript":               {Handle: "herdr:local/sess-2/w1:p1", Kind: "claude", SessionID: "sess-2"},
		"no session":                  {Handle: "herdr:local//w1:p1", Kind: "claude"},
		"a session that is a pattern": {Handle: claude.Handle, Kind: "claude", SessionID: "*"},
		"a session that is a path":    {Handle: claude.Handle, Kind: "claude", SessionID: "../-src-repo/sess-1"},
	} {
		if got, ok := last(a); ok {
			t.Errorf("%s: Claude = %s, want none", name, got)
		}
	}
}

func TestClaudeProjectsDir(t *testing.T) {
	for _, tt := range []struct {
		env  []string
		want string
	}{
		{[]string{"HOME=/home/me"}, "/home/me/.claude/projects"},
		{[]string{"HOME=/home/me", "CLAUDE_CONFIG_DIR=/cfg"}, "/cfg/projects"},
		{nil, ""},
	} {
		if got := ClaudeProjectsDir(tt.env); got != tt.want {
			t.Errorf("ClaudeProjectsDir(%v) = %q, want %q", tt.env, got, tt.want)
		}
	}
}

func TestClaudeWithoutADirKnowsNothing(t *testing.T) {
	if _, ok := Claude("")(adapter.Agent{Handle: "herdr:local/s/w1:p1", Kind: "claude", SessionID: "s"}); ok {
		t.Fatal("Claude(\"\") found a transcript")
	}
}
