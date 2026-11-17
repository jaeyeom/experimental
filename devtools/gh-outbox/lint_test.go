package ghoutbox

import (
	"strings"
	"testing"
)

func defaultRules(t *testing.T, busNames ...string) []Rule {
	t.Helper()
	rules, err := BuildRules(LintConfig{Defaults: true, AgentbusNames: true}, busNames)
	if err != nil {
		t.Fatal(err)
	}
	return rules
}

func TestLintCatchesEachDefaultRule(t *testing.T) {
	rules := defaultRules(t, "coord/primary")
	tests := []struct {
		name, text, match, rule string
	}{
		{"decide id", "see D-123 for context", "D-123", "decide id"},
		{"bus name", "coord/primary asked for this", "coord/primary", "agentbus name"},
		{"per the user", "Per the user, rename it", "Per the user", "internal phrase"},
		{"on behalf of", "posting on behalf of Jaehyun", "on behalf of", "internal phrase"},
		{"agent", "the Agent found this", "Agent", "internal phrase"},
		{"routing", "after routing it here", "routing", "internal phrase"},
	}
	for _, tt := range tests {
		d := Draft{Body: "fine first line\n" + tt.text}
		hits := Lint(d, rules)
		if len(hits) != 1 {
			t.Errorf("%s: hits = %v, want one", tt.name, hits)
			continue
		}
		h := hits[0]
		if h.Location != "body line 2" || h.Match != tt.match || h.Rule != tt.rule {
			t.Errorf("%s: hit = %+v", tt.name, h)
		}
	}
}

func TestLintNamesLineLocation(t *testing.T) {
	d := Draft{Lines: []LineComment{
		{Path: "a.go", Line: 3, Text: "clean"},
		{Path: "b.go", Line: 9, Text: "ok\nsee D-4"},
	}}
	hits := Lint(d, defaultRules(t))
	if len(hits) != 1 || hits[0].Location != "b.go:9" {
		t.Fatalf("hits = %v, want one at b.go:9", hits)
	}
	if got := hits[0].String(); !strings.Contains(got, "b.go:9") || !strings.Contains(got, `"D-4"`) {
		t.Errorf("String = %q", got)
	}
}

func TestLintPassesCleanText(t *testing.T) {
	d := Draft{
		Body:  "This retries the write.\nThe agentic label and D-day stay.\nPaths like devtools/foo are fine.",
		Lines: []LineComment{{Path: "a.go", Line: 1, Text: "Consider a test here."}},
	}
	if hits := Lint(d, defaultRules(t, "coord/primary")); len(hits) != 0 {
		t.Errorf("hits = %v, want none", hits)
	}
}

func TestBuildRulesFromConfig(t *testing.T) {
	rules, err := BuildRules(LintConfig{
		Defaults:     false,
		Patterns:     []string{`TICKET-\d+`},
		Phrases:      []string{"secret plan"},
		AgentPattern: `\b(?:pm|ops)/[a-z-]+\b`,
	}, []string{"coord/primary"})
	if err != nil {
		t.Fatal(err)
	}
	d := Draft{Body: "TICKET-9 and the Secret Plan from pm/cost; D-1, coord/primary and agent are allowed"}
	var got []string
	for _, h := range Lint(d, rules) {
		got = append(got, h.Match)
	}
	if strings.Join(got, ",") != "TICKET-9,Secret Plan,pm/cost" {
		t.Errorf("matches = %q", got)
	}
	if _, err := BuildRules(LintConfig{Patterns: []string{"("}}, nil); err == nil {
		t.Error("bad pattern: want an error")
	}
}
