package approve

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// shellRule returns a rule whose agent command is a fake: an sh script.
// Extra args are passed to the script as $1, $2, ...
func shellRule(script string, args ...string) config.ApproveRule {
	argv := append([]string{"sh", "-c", script, "fake-agent"}, args...)
	return config.ApproveRule{Name: "agent", AgentCommand: argv, AgentTimeout: 5 * time.Second}
}

func sampleInput() AgentInput {
	return AgentInput{
		Rule:  "agent",
		PR:    AgentPR{Repo: "acme/widgets", Number: 1, Title: "Tidy dashboard", HeadSHA: "h1"},
		Files: []models.File{{Path: "dash/a.go", Deletions: 3}},
		Diff:  "diff --git a/dash/a.go b/dash/a.go\n-old\n",
	}
}

func TestRunAgentDecisions(t *testing.T) {
	tests := map[string]struct {
		script       string
		wantDecision string
		wantReason   []string
	}{
		"approve":          {`echo '{"decision": "approve", "reason": "docs only"}'`, AgentApprove, []string{"docs only"}},
		"defer":            {`echo '{"decision": "defer", "reason": "touches auth"}'`, AgentDefer, []string{"touches auth"}},
		"exit 1":           {`echo boom >&2; exit 1`, AgentDefer, []string{"exit", "1", "boom"}},
		"invalid JSON":     {`echo 'not json'`, AgentDefer, []string{"JSON"}},
		"two objects":      {`echo '{"decision":"approve"}{"decision":"approve"}'`, AgentDefer, []string{"JSON"}},
		"unknown decision": {`echo '{"decision": "maybe"}'`, AgentDefer, []string{"maybe"}},
		"exit 1 approve":   {`echo '{"decision": "approve"}'; exit 1`, AgentDefer, []string{"exit"}},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got := runAgent(ExecCommand, nil, shellRule(tt.script), sampleInput())
			if got.Decision != tt.wantDecision {
				t.Fatalf("decision = %q, want %q (reason %q)", got.Decision, tt.wantDecision, got.Reason)
			}
			for _, want := range tt.wantReason {
				if !strings.Contains(got.Reason, want) {
					t.Fatalf("reason = %q, want it to mention %q", got.Reason, want)
				}
			}
		})
	}
}

func TestRunAgentMissingCommandDefers(t *testing.T) {
	rule := config.ApproveRule{Name: "agent", AgentCommand: []string{"/nonexistent/gh-approve-agent"}}
	if got := runAgent(ExecCommand, nil, rule, sampleInput()); got.Decision != AgentDefer || got.Reason == "" {
		t.Fatalf("result = %+v", got)
	}
}

func TestRunAgentTimeoutDefers(t *testing.T) {
	rule := shellRule(`sleep 30`)
	rule.AgentTimeout = 200 * time.Millisecond
	start := time.Now()
	got := runAgent(ExecCommand, nil, rule, sampleInput())
	if elapsed := time.Since(start); elapsed > 5*time.Second {
		t.Fatalf("runAgent took %v after a 200ms timeout", elapsed)
	}
	if got.Decision != AgentDefer || !strings.Contains(got.Reason, "timed out") || !strings.Contains(got.Reason, "200ms") {
		t.Fatalf("result = %+v", got)
	}
	if got.DurationMs < 200 {
		t.Fatalf("duration = %dms, want >= 200ms", got.DurationMs)
	}
}

func TestRunAgentStdinCarriesDiffAndFilesToCommand(t *testing.T) {
	path := filepath.Join(t.TempDir(), "stdin.json")
	rule := shellRule(`cat > "$1"; echo '{"decision": "approve", "reason": "ok"}'`, path)
	if got := runAgent(ExecCommand, nil, rule, sampleInput()); got.Decision != AgentApprove {
		t.Fatalf("result = %+v", got)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	var input AgentInput
	if err := json.Unmarshal(data, &input); err != nil {
		t.Fatalf("stdin %s: %v", data, err)
	}
	if input.Diff != sampleInput().Diff || len(input.Files) != 1 || input.Files[0].Path != "dash/a.go" ||
		input.PR.Repo != "acme/widgets" || input.PR.HeadSHA != "h1" || input.Rule != "agent" {
		t.Fatalf("stdin = %s", data)
	}
	for _, key := range []string{`"diff"`, `"files"`, `"pr"`, `"path"`, `"headSha"`} {
		if !strings.Contains(string(data), key) {
			t.Fatalf("stdin missing %s: %s", key, data)
		}
	}
}
