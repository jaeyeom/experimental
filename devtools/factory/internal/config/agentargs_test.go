package config

import (
	"strings"
	"testing"
)

func TestChooseAgentArgs(t *testing.T) {
	t.Parallel()
	defaults := []string{"--model", "opus"}
	rules := []AgentRule{
		{Labels: []string{"mechanical", "chore"}, AgentArgs: []string{"--model", "sonnet"}},
		{Labels: []string{"report-only"}, AgentArgs: []string{"--model", "haiku"}},
		{Labels: []string{"silent"}, AgentArgs: nil},
	}
	tests := []struct {
		name      string
		labels    []string
		flag      []string
		fromFlag  bool
		wantArgs  []string
		wantSrc   string
		wantRule  int
		wantLabel string
	}{
		{name: "default", wantArgs: defaults, wantSrc: AgentArgDefault},
		{name: "first label", labels: []string{"backend", "mechanical"}, wantArgs: []string{"--model", "sonnet"}, wantSrc: AgentArgRule, wantRule: 1, wantLabel: "mechanical"},
		{name: "second rule", labels: []string{"report-only"}, wantArgs: []string{"--model", "haiku"}, wantSrc: AgentArgRule, wantRule: 2, wantLabel: "report-only"},
		{name: "rule clears defaults", labels: []string{"silent"}, wantSrc: AgentArgRule, wantRule: 3, wantLabel: "silent"},
		{name: "flag wins", labels: []string{"mechanical"}, flag: []string{"--model", "haiku"}, fromFlag: true, wantArgs: []string{"--model", "haiku"}, wantSrc: AgentArgFlag},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := ChooseAgentArgs(tc.labels, defaults, rules, tc.flag, tc.fromFlag)
			if got.Source != tc.wantSrc || got.Rule != tc.wantRule {
				t.Fatalf("source = %s rule %d, want %s rule %d", got.Source, got.Rule, tc.wantSrc, tc.wantRule)
			}
			if !sameStrings(got.Args, tc.wantArgs) {
				t.Fatalf("args = %#v, want %#v", got.Args, tc.wantArgs)
			}
			if tc.wantLabel != "" && (len(got.Labels) == 0 || got.Labels[0] != tc.wantLabel) {
				t.Fatalf("labels = %#v", got.Labels)
			}
			got.Args = append(got.Args, "mutated")
			again := ChooseAgentArgs(tc.labels, defaults, rules, tc.flag, tc.fromFlag)
			if !sameStrings(again.Args, tc.wantArgs) {
				t.Fatalf("choice shares the config slice: %#v", again.Args)
			}
		})
	}
}

func TestFormatAgentArgs(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		choice  AgentArgChoice
		want    string
		wantErr string
	}{
		{name: "default empty", choice: AgentArgChoice{Source: AgentArgDefault}, want: "args     (default)\n"},
		{name: "default args", choice: AgentArgChoice{Args: []string{"--model", "opus"}, Source: AgentArgDefault}, want: "args     --model opus (default)\n"},
		{name: "flag", choice: AgentArgChoice{Args: []string{"--model", "sonnet"}, Source: AgentArgFlag}, want: "args     --model sonnet (flag)\n"},
		{
			name: "rule",
			choice: AgentArgChoice{
				Args: []string{"--model", "haiku"}, Source: AgentArgRule, Rule: 2, Labels: []string{"report-only"},
			},
			want: "args     --model haiku (rule 2 labels report-only)\n",
		},
		{
			name: "quoted",
			choice: AgentArgChoice{
				Args: []string{"--system", "say hi"}, Source: AgentArgFlag,
			},
			want: "args     --system \"say hi\" (flag)\n",
		},
		{name: "bad source", choice: AgentArgChoice{Source: "nope"}, wantErr: "found \"nope\""},
		{name: "empty arg", choice: AgentArgChoice{Args: []string{""}, Source: AgentArgFlag}, wantErr: "found empty"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := FormatAgentArgs(tc.choice)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if got != tc.want {
				t.Fatalf("line = %q, want %q", got, tc.want)
			}
		})
	}
}

func sameStrings(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}
