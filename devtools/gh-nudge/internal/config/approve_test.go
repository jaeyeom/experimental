package config

import (
	"fmt"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func writeApprovePkl(t *testing.T, approve string) string {
	t.Helper()
	t.Setenv("HOME", t.TempDir())
	schema, err := filepath.Abs(circleConfigPkl(t))
	if err != nil {
		t.Fatal(err)
	}
	content := fmt.Sprintf("amends %q\n\ngithub {\n  repos { \"widgets\" }\n}\n\n%s\n", schema, approve)
	return writeConfig(t, t.TempDir(), "config.pkl", content)
}

func TestLoadPklApprove(t *testing.T) {
	path := writeApprovePkl(t, `
approve {
  body = "Looks fine"
  rules {
    new {
      name = "dashboard"
      repos { "acme/*" }
      paths_all_match { "dash/**" }
      paths_none_match { ".github/**" }
      max_changed_lines = 50
      max_added_lines = 40
      max_files = 3
      pure_deletion = true
      authors_allow { "alice" }
      authors_deny { "mallory" }
      labels_required { "internal-only" }
      labels_forbidden { "wip" }
      required_approvals_by { "review-bot" }
      checks = "allow_pending"
      not_draft = false
      max_unresolved_threads = 2
      agent_command { "review-agent"; "--strict" }
      agent_timeout = 30.s
    }
    new {
      name = "defaults"
    }
  }
}
`)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	approve := cfg.Approve
	if approve.Body != "Looks fine" || !approve.SkipAlreadyApproved || len(approve.Rules) != 2 {
		t.Fatalf("approve = %+v", approve)
	}
	rule := approve.Rules[0]
	if rule.Name != "dashboard" || strings.Join(rule.Repos, ",") != "acme/*" ||
		strings.Join(rule.PathsAllMatch, ",") != "dash/**" || strings.Join(rule.PathsNoneMatch, ",") != ".github/**" {
		t.Fatalf("rule globs = %+v", rule)
	}
	if rule.MaxChangedLines == nil || *rule.MaxChangedLines != 50 || rule.MaxAddedLines == nil || *rule.MaxAddedLines != 40 ||
		rule.MaxFiles == nil || *rule.MaxFiles != 3 || !rule.PureDeletion {
		t.Fatalf("rule sizes = %+v", rule)
	}
	if strings.Join(rule.AuthorsAllow, ",") != "alice" || strings.Join(rule.AuthorsDeny, ",") != "mallory" ||
		strings.Join(rule.LabelsRequired, ",") != "internal-only" || strings.Join(rule.LabelsForbidden, ",") != "wip" ||
		strings.Join(rule.RequiredApprovalsBy, ",") != "review-bot" {
		t.Fatalf("rule lists = %+v", rule)
	}
	if rule.Checks != ChecksAllowPending || rule.NotDraft || rule.MaxUnresolvedThreads != 2 {
		t.Fatalf("rule flags = %+v", rule)
	}
	if strings.Join(rule.AgentCommand, " ") != "review-agent --strict" || rule.AgentTimeout != 30*time.Second {
		t.Fatalf("rule agent = %q %v", rule.AgentCommand, rule.AgentTimeout)
	}
	defaults := approve.Rules[1]
	if defaults.MaxChangedLines != nil || defaults.Checks != "" || !defaults.NotDraft || defaults.MaxUnresolvedThreads != 0 {
		t.Fatalf("rule defaults = %+v", defaults)
	}
	if len(defaults.AgentCommand) != 0 || defaults.AgentTimeout != DefaultAgentTimeout || DefaultAgentTimeout != 120*time.Second {
		t.Fatalf("rule agent defaults = %q %v", defaults.AgentCommand, defaults.AgentTimeout)
	}
}

func TestLoadPklApproveDefaults(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, ""))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Approve.Body != DefaultApproveBody || !cfg.Approve.SkipAlreadyApproved || len(cfg.Approve.Rules) != 0 {
		t.Fatalf("approve = %+v", cfg.Approve)
	}
}

func TestLoadPklApproveRejectsBadConfig(t *testing.T) {
	tests := map[string]struct{ approve, want string }{
		"unknown rule key":    {`approve { rules { new { name = "r"; max_lines = 3 } } }`, "max_lines"},
		"unknown approve key": {`approve { dry_run = false }`, "dry_run"},
		"bad checks":          {`approve { rules { new { name = "r"; checks = "red" } } }`, "checks"},
		"bad glob":            {`approve { rules { new { name = "r"; paths_all_match { "a/[" } } } }`, "paths_all_match"},
		"bad repo glob":       {`approve { rules { new { name = "r"; repos { "acme/[" } } } }`, "repos"},
		"duplicate name":      {`approve { rules { new { name = "r" } new { name = "r" } } }`, "twice"},
		"empty name":          {`approve { rules { new { name = "" } } }`, "name"},
		"empty agent argv0":   {`approve { rules { new { name = "r"; agent_command { "" } } } }`, "agent_command"},
		"zero agent timeout":  {`approve { rules { new { name = "r"; agent_timeout = 0.s } } }`, "agent_timeout"},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			_, err := LoadConfig(writeApprovePkl(t, tt.approve))
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("LoadConfig() error = %v, want it to mention %q", err, tt.want)
			}
		})
	}
}

func TestLoadYAMLRejectsApprove(t *testing.T) {
	path := writeConfig(t, t.TempDir(), "config.yaml", "github:\n  repos:\n    - widgets\napprove:\n  rules: []\n")
	_, err := LoadConfig(path)
	if err == nil || !strings.Contains(err.Error(), "approve") {
		t.Fatalf("LoadConfig() error = %v, want approve is Pkl only", err)
	}
}

func TestExampleConfigLoadsApproveRules(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	t.Setenv("SLACK_TOKEN", "xoxb-test")
	example := filepath.Join(filepath.Dir(circleConfigPkl(t)), "example.config.pkl")
	cfg, err := LoadConfig(example)
	if err != nil {
		t.Fatal(err)
	}
	var names []string
	for _, rule := range cfg.Approve.Rules {
		names = append(names, rule.Name)
	}
	if got := strings.Join(names, ","); got != "internal-dashboard,pure-deletion,small-outside-infra,internal-only" {
		t.Fatalf("rules = %s", got)
	}
}
