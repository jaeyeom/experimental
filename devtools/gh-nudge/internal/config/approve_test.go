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
  agent_cache_ttl = 2.d
  agent_cache_max_mb = 10
  patch_max_files = 40
  patch_max_added_lines = 900
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
      deletions_and_comments_only = true
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
      agent_cache = false
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
	if approve.AgentCacheTTL != 48*time.Hour || approve.AgentCacheMaxMB != 10 {
		t.Fatalf("approve agent cache = %v %d", approve.AgentCacheTTL, approve.AgentCacheMaxMB)
	}
	if approve.PatchMaxFiles != 40 || approve.PatchMaxAddedLines != 900 {
		t.Fatalf("approve patch cap = %d %d", approve.PatchMaxFiles, approve.PatchMaxAddedLines)
	}
	rule := approve.Rules[0]
	if rule.Name != "dashboard" || strings.Join(rule.Repos, ",") != "acme/*" ||
		strings.Join(rule.PathsAllMatch, ",") != "dash/**" || strings.Join(rule.PathsNoneMatch, ",") != ".github/**" {
		t.Fatalf("rule globs = %+v", rule)
	}
	if rule.MaxChangedLines == nil || *rule.MaxChangedLines != 50 || rule.MaxAddedLines == nil || *rule.MaxAddedLines != 40 ||
		rule.MaxFiles == nil || *rule.MaxFiles != 3 || !rule.PureDeletion || !rule.DeletionsAndCommentsOnly {
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
	if strings.Join(rule.AgentCommand, " ") != "review-agent --strict" || rule.AgentTimeout != 30*time.Second || !rule.AgentCacheDisabled {
		t.Fatalf("rule agent = %q %v", rule.AgentCommand, rule.AgentTimeout)
	}
	defaults := approve.Rules[1]
	if defaults.MaxChangedLines != nil || defaults.Checks != "" || !defaults.NotDraft || defaults.MaxUnresolvedThreads != 0 {
		t.Fatalf("rule defaults = %+v", defaults)
	}
	if len(defaults.AgentCommand) != 0 || defaults.AgentTimeout != DefaultAgentTimeout || DefaultAgentTimeout != 120*time.Second ||
		defaults.AgentCacheDisabled {
		t.Fatalf("rule agent defaults = %q %v", defaults.AgentCommand, defaults.AgentTimeout)
	}
	if defaults.Action != ActionApprove || len(defaults.Reviewers) != 0 || len(defaults.PathsAnyMatch) != 0 || defaults.CodeownersCovered || defaults.DeletionsAndCommentsOnly {
		t.Fatalf("rule hand-off defaults = %+v", defaults)
	}
}

func TestLoadPklHandoffRule(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, `
approve {
  rules {
    new {
      name = "dashboard-handoff"
      action = "handoff"
      reviewers { "colleague" }
      paths_any_match { "tools/dashboard/**" }
      authors_deny { "colleague" }
      codeowners_covered = true
    }
  }
}
`))
	if err != nil {
		t.Fatal(err)
	}
	rule := cfg.Approve.Rules[0]
	if rule.Action != ActionHandoff || strings.Join(rule.Reviewers, ",") != "colleague" ||
		strings.Join(rule.PathsAnyMatch, ",") != "tools/dashboard/**" || !rule.CodeownersCovered ||
		strings.Join(rule.AuthorsDeny, ",") != "colleague" {
		t.Fatalf("rule = %+v", rule)
	}
}

func TestLoadPklMineCommentsOnlyRule(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, `
approve {
  rules {
    new {
      name = "my-comments"
      requested_via_team = true
      mine_comments_only = true
      body = "Comment-only change in {my_files} ({my_teams}); approving for those files only. Rule {rule}."
    }
    new { name = "plain"; body = "Fine by me" }
    new { name = "defaults" }
  }
}
`))
	if err != nil {
		t.Fatal(err)
	}
	rule := cfg.Approve.Rules[0]
	if !rule.RequestedViaTeam || !rule.MineCommentsOnly || !strings.HasPrefix(rule.Body, "Comment-only change in {my_files}") {
		t.Fatalf("rule = %+v", rule)
	}
	if cfg.Approve.Rules[1].Body != "Fine by me" {
		t.Fatalf("plain = %+v", cfg.Approve.Rules[1])
	}
	defaults := cfg.Approve.Rules[2]
	if defaults.RequestedViaTeam || defaults.MineCommentsOnly || defaults.Body != "" {
		t.Fatalf("defaults = %+v", defaults)
	}
}

func TestBodyPlaceholders(t *testing.T) {
	tests := map[string]string{
		"LGTM":                        "",
		"{my_files} by {my_teams}":    "my_files,my_teams",
		"{rule} {rule}":               "rule,rule",
		"json {\"a\": 1} stays plain": "",
		"{nope}":                      "nope",
	}
	for body, want := range tests {
		if got := strings.Join(BodyPlaceholders(body), ","); got != want {
			t.Errorf("BodyPlaceholders(%q) = %q, want %q", body, got, want)
		}
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
	if cfg.Approve.AgentCacheTTL != DefaultAgentCacheTTL || DefaultAgentCacheTTL != 7*24*time.Hour ||
		cfg.Approve.AgentCacheMaxMB != DefaultAgentCacheMaxMB || DefaultAgentCacheMaxMB != 50 {
		t.Fatalf("approve agent cache = %v %d", cfg.Approve.AgentCacheTTL, cfg.Approve.AgentCacheMaxMB)
	}
	if cfg.Approve.PatchMaxFiles != DefaultPatchMaxFiles || DefaultPatchMaxFiles != 300 ||
		cfg.Approve.PatchMaxAddedLines != DefaultPatchMaxAddedLines || DefaultPatchMaxAddedLines != 5000 {
		t.Fatalf("approve patch cap = %d %d", cfg.Approve.PatchMaxFiles, cfg.Approve.PatchMaxAddedLines)
	}
	if cfg.Viewed.Enabled || cfg.Viewed.MaxMarksPerRun != DefaultMaxMarksPerRun || DefaultMaxMarksPerRun != 200 || cfg.Viewed.RemarkDismissed || len(cfg.Viewed.Rules) != 0 {
		t.Fatalf("viewed = %+v", cfg.Viewed)
	}
}

func TestLoadPklViewed(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, `
viewed {
  enabled = true
  max_marks_per_run = 5
  never_mark { "**/*.go"; "**/*.py"; "**/*.proto" }
  remark_dismissed = true
  rules {
    new { name = "locks"; repos { "acme/*" }; paths_match { "**/*.lock"; "**/go.sum" } }
    new { name = "not-mine"; codeowners_not_mine = true; paths_none_match { "docs/**" } }
  }
}
`))
	if err != nil {
		t.Fatal(err)
	}
	if !cfg.Viewed.Enabled || cfg.Viewed.MaxMarksPerRun != 5 || !cfg.Viewed.RemarkDismissed || len(cfg.Viewed.Rules) != 2 {
		t.Fatalf("viewed = %+v", cfg.Viewed)
	}
	if strings.Join(cfg.Viewed.NeverMark, ",") != "**/*.go,**/*.py,**/*.proto" {
		t.Fatalf("never_mark = %+v", cfg.Viewed.NeverMark)
	}
	locks := cfg.Viewed.Rules[0]
	if locks.Name != "locks" || strings.Join(locks.Repos, ",") != "acme/*" || strings.Join(locks.PathsMatch, ",") != "**/*.lock,**/go.sum" || locks.CodeownersNotMine {
		t.Fatalf("locks = %+v", locks)
	}
	notMine := cfg.Viewed.Rules[1]
	if !notMine.CodeownersNotMine || len(notMine.PathsMatch) != 0 || strings.Join(notMine.PathsNoneMatch, ",") != "docs/**" {
		t.Fatalf("not-mine = %+v", notMine)
	}
}

func TestLoadPklViewedExceptPathsAlias(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, `
viewed {
  rules {
    new { name = "not-mine"; codeowners_not_mine = true; except_paths { "docs/**" } }
  }
}
`))
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(cfg.Viewed.Rules[0].PathsNoneMatch, ",") != "docs/**" {
		t.Fatalf("alias = %+v", cfg.Viewed.Rules[0])
	}
}

func TestLoadPklApproveRejectsBadConfig(t *testing.T) {
	tests := map[string]struct{ approve, want string }{
		"unknown rule key":            {`approve { rules { new { name = "r"; max_lines = 3 } } }`, "max_lines"},
		"unknown approve key":         {`approve { dry_run = false }`, "dry_run"},
		"bad checks":                  {`approve { rules { new { name = "r"; checks = "red" } } }`, "checks"},
		"bad glob":                    {`approve { rules { new { name = "r"; paths_all_match { "a/[" } } } }`, "paths_all_match"},
		"bad repo glob":               {`approve { rules { new { name = "r"; repos { "acme/[" } } } }`, "repos"},
		"duplicate name":              {`approve { rules { new { name = "r" } new { name = "r" } } }`, "twice"},
		"empty name":                  {`approve { rules { new { name = "" } } }`, "name"},
		"empty agent argv0":           {`approve { rules { new { name = "r"; agent_command { "" } } } }`, "agent_command"},
		"zero agent timeout":          {`approve { rules { new { name = "r"; agent_timeout = 0.s } } }`, "agent_timeout"},
		"zero cache ttl":              {`approve { agent_cache_ttl = 0.s }`, "agent_cache_ttl"},
		"zero cache size":             {`approve { agent_cache_max_mb = 0 }`, "agent_cache_max_mb"},
		"zero patch file cap":         {`approve { patch_max_files = 0 }`, "patch_max_files"},
		"zero patch added line cap":   {`approve { patch_max_added_lines = 0 }`, "patch_max_added_lines"},
		"handoff without reviewers":   {`approve { rules { new { name = "r"; action = "handoff" } } }`, "reviewers"},
		"approve with reviewers":      {`approve { rules { new { name = "r"; reviewers { "bob" } } } }`, "reviewers"},
		"unknown action":              {`approve { rules { new { name = "r"; action = "merge" } } }`, "action"},
		"empty reviewer":              {`approve { rules { new { name = "r"; action = "handoff"; reviewers { "" } } } }`, "reviewers"},
		"bad any glob":                {`approve { rules { new { name = "r"; paths_any_match { "a/[" } } } }`, "paths_any_match"},
		"unknown handoff key":         {`approve { rules { new { name = "r"; action = "handoff"; reviewers { "bob" }; team_reviewers { "x" } } } }`, "team_reviewers"},
		"unknown viewed key":          {`viewed { nope = true }`, "nope"},
		"unknown viewed rule key":     {`viewed { rules { new { name = "r"; paths = "go.sum" } } }`, "paths"},
		"viewed rule marks nothing":   {`viewed { rules { new { name = "r" } } }`, "paths_match"},
		"duplicate viewed rule":       {`viewed { rules { new { name = "r"; paths_match { "a" } } new { name = "r"; paths_match { "b" } } } }`, "twice"},
		"bad viewed glob":             {`viewed { rules { new { name = "r"; paths_match { "a/[" } } } }`, "paths_match"},
		"bad paths_none_match glob":   {`viewed { rules { new { name = "r"; paths_match { "a" }; paths_none_match { "a/[" } } } }`, `viewed rule "r": paths_none_match`},
		"bad except_paths glob":       {`viewed { rules { new { name = "r"; codeowners_not_mine = true; except_paths { "a/[" } } } }`, `viewed rule "r": paths_none_match`},
		"bad never_mark glob":         {`viewed { never_mark { "a/[" } }`, `never_mark glob "a/["`},
		"unknown body placeholder":    {`approve { rules { new { name = "r"; mine_comments_only = true; body = "ok {nope}" } } }`, `unknown body placeholder {nope}`},
		"my_files without mine":       {`approve { rules { new { name = "r"; body = "ok {my_files}" } } }`, "mine_comments_only"},
		"my_teams without mine":       {`approve { rules { new { name = "r"; body = "ok {my_teams}" } } }`, "mine_comments_only"},
		"empty rule body":             {`approve { rules { new { name = "r"; body = "" } } }`, "body"},
		"team request on handoff":     {`approve { rules { new { name = "r"; action = "handoff"; reviewers { "bob" }; requested_via_team = true } } }`, "requested_via_team"},
		"body on handoff":             {`approve { rules { new { name = "r"; action = "handoff"; reviewers { "bob" }; body = "x" } } }`, "body"},
		"both exclusion keys":         {`viewed { rules { new { name = "r"; paths_match { "a" }; paths_none_match { "docs/**" }; except_paths { "vendor/**" } } } }`, `viewed rule "r" sets both`},
		"bad file_checks glob":        {`approve { rules { new { name = "r"; file_checks { ["a/["] = "only-deletions" } } } }`, "file_checks"},
		"zero file_checks_timeout":    {`approve { rules { new { name = "r"; file_checks_timeout = 0.s } } }`, "file_checks_timeout"},
		"file_checks specificity tie": {`approve { rules { new { name = "r"; file_checks { ["a/**"] = "only-deletions"; ["**/b"] = "only-deletions" } } } }`, "tied specificity"},
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
	if got := strings.Join(names, ","); got != "internal-dashboard,pure-deletion,comment-cleanup,small-outside-infra,internal-only,dashboard-handoff" {
		t.Fatalf("rules = %s", got)
	}
	if !cfg.Approve.Rules[2].DeletionsAndCommentsOnly || cfg.Approve.Rules[1].DeletionsAndCommentsOnly {
		t.Fatalf("comment-cleanup = %+v, pure-deletion = %+v", cfg.Approve.Rules[2], cfg.Approve.Rules[1])
	}
}

func TestLoadPklFileChecks(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	schema, err := filepath.Abs(circleConfigPkl(t))
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	content := fmt.Sprintf(`amends %q

github {
  repos { "widgets" }
}

approve {
  rules {
    new {
      name = "fc"
      file_checks {
        ["**/baseline.txt"] = "only-deletions"
        ["checkers/size/baseline.txt"] = "./check-shrink"
        ["docs/*.md"] = new Listing { "~/bin/check-docs"; "--strict" }
      }
      file_checks_timeout = 15.s
      mark_viewed_on_pass = false
    }
    new {
      name = "defaults"
    }
  }
}
`, schema)
	path := writeConfig(t, dir, "config.pkl", content)
	cfg, err := LoadConfig(path)
	if err != nil {
		t.Fatal(err)
	}
	rule := cfg.Approve.Rules[0]
	if rule.FileChecksTimeout != 15*time.Second || rule.MarkViewedOnPass {
		t.Fatalf("file_checks settings = %v %v", rule.FileChecksTimeout, rule.MarkViewedOnPass)
	}
	if got := rule.FileChecks["**/baseline.txt"]; got.Builtin != FileCheckOnlyDeletions || len(got.Argv) != 0 {
		t.Fatalf("builtin = %+v", got)
	}
	if got := rule.FileChecks["checkers/size/baseline.txt"]; len(got.Argv) != 1 || got.Argv[0] != filepath.Join(dir, "check-shrink") {
		t.Fatalf("relative script = %+v (dir=%s)", got, dir)
	}
	if got := rule.FileChecks["docs/*.md"]; len(got.Argv) != 2 || got.Argv[0] != filepath.Join(home, "bin", "check-docs") || got.Argv[1] != "--strict" {
		t.Fatalf("tilde argv = %+v", got)
	}
	defaults := cfg.Approve.Rules[1]
	if defaults.FileChecksTimeout != DefaultFileCheckTimeout || !defaults.MarkViewedOnPass {
		t.Fatalf("defaults = %v %v", defaults.FileChecksTimeout, defaults.MarkViewedOnPass)
	}
}

func TestMatchFileCheckSpecificity(t *testing.T) {
	checks := map[string]FileCheck{
		"*.txt":                      {Builtin: FileCheckOnlyDeletions},
		"*_baseline.txt":             {Builtin: FileCheckOnlyDeletions},
		"**/baseline.txt":            {Builtin: FileCheckOnlyDeletions},
		"checkers/*/baseline.txt":    {Builtin: FileCheckOnlyDeletions},
		"checkers/size/baseline.txt": {Argv: []string{"./exact"}},
	}
	tests := []struct {
		path    string
		wantPat string
	}{
		{"checkers/size/baseline.txt", "checkers/size/baseline.txt"},
		{"checkers/lint/baseline.txt", "checkers/*/baseline.txt"},
		{"other/dir/baseline.txt", "**/baseline.txt"},
		{"foo_baseline.txt", "*_baseline.txt"},
		{"notes.txt", "*.txt"},
		{"main.go", ""},
	}
	for _, tc := range tests {
		pat, _, ok := MatchFileCheck(checks, tc.path)
		if tc.wantPat == "" {
			if ok {
				t.Errorf("MatchFileCheck(%q) = %q, want no match", tc.path, pat)
			}
			continue
		}
		if !ok || pat != tc.wantPat {
			t.Errorf("MatchFileCheck(%q) = (%q, %v), want %q", tc.path, pat, ok, tc.wantPat)
		}
	}
}

func TestLoadPklGeneratedLists(t *testing.T) {
	cfg, err := LoadConfig(writeApprovePkl(t, `
generated_lists {
  ["acme/widgets"] = "tools/generated_files.yaml"
}
approve {
  rules {
    new {
      name = "gen"
      repos { "acme/widgets" }
      paths_all_match { "@generated"; "**/*.lock" }
      file_checks { ["@generated"] = "only-deletions" }
    }
  }
}
viewed {
  rules {
    new {
      name = "gen-viewed"
      repos { "acme/widgets" }
      paths_match { "@generated" }
      except_paths { "@generated" }
    }
  }
}
`))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.GeneratedLists["acme/widgets"] != "tools/generated_files.yaml" {
		t.Fatalf("GeneratedLists = %+v", cfg.GeneratedLists)
	}
}

func TestLoadPklGeneratedListsRejectsUnconfiguredRepo(t *testing.T) {
	tests := map[string]string{
		"approve paths_all_match without generated_lists": `
approve { rules { new { name = "r"; repos { "acme/widgets" }; paths_all_match { "@generated" } } } }`,
		"approve paths_any_match without generated_lists": `
approve { rules { new { name = "r"; repos { "acme/widgets" }; paths_any_match { "@generated" } } } }`,
		"approve paths_none_match without generated_lists": `
approve { rules { new { name = "r"; repos { "acme/widgets" }; paths_none_match { "@generated" } } } }`,
		"approve file_checks without generated_lists": `
approve { rules { new { name = "r"; repos { "acme/widgets" }; file_checks { ["@generated"] = "only-deletions" } } } }`,
		"approve empty repos and empty generated_lists": `
approve { rules { new { name = "r"; paths_all_match { "@generated" } } } }`,
		"approve repo missing from non-empty generated_lists": `
generated_lists { ["acme/other"] = "gen.yaml" }
approve { rules { new { name = "r"; repos { "acme/widgets" }; paths_all_match { "@generated" } } } }`,
		"viewed paths_match without generated_lists": `
viewed { rules { new { name = "r"; repos { "acme/widgets" }; paths_match { "@generated" } } } }`,
		"viewed except_paths without generated_lists": `
viewed { rules { new { name = "r"; repos { "acme/widgets" }; codeowners_not_mine = true; except_paths { "@generated" } } } }`,
	}
	for name, content := range tests {
		t.Run(name, func(t *testing.T) {
			if _, err := LoadConfig(writeApprovePkl(t, content)); err == nil {
				t.Fatalf("expected error for %s", name)
			}
		})
	}
}
