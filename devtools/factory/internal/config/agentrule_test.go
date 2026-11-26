package config

import (
	"strings"
	"testing"
)

func TestModelArg(t *testing.T) {
	tests := []struct {
		args    []string
		want    string
		wantErr string
	}{
		{args: nil, want: ""},
		{args: []string{"--model", "opus"}, want: "opus"},
		{args: []string{"--verbose", "--model=sonnet"}, want: "sonnet"},
		{args: []string{"--model"}, wantErr: "expected a value after --model"},
		{args: []string{"--model="}, wantErr: "non-empty --model"},
		{args: []string{"--model", "a", "--model", "b"}, wantErr: "expected one --model"},
	}
	for _, tc := range tests {
		got, err := ModelArg(tc.args)
		if tc.wantErr != "" {
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("ModelArg(%q) error = %v, want %q", tc.args, err, tc.wantErr)
			}
			continue
		}
		if err != nil || got != tc.want {
			t.Fatalf("ModelArg(%q) = %q, %v, want %q", tc.args, got, err, tc.want)
		}
	}
}

func TestRuleName(t *testing.T) {
	rules := []AgentRule{
		{Labels: []string{"mechanical", "chore"}},
		{Name: "big", Labels: []string{"heavy"}},
	}
	tests := []struct {
		labels []string
		flag   bool
		want   string
	}{
		{labels: []string{"chore"}, want: "mechanical"},
		{labels: []string{"heavy"}, want: "big"},
		{labels: nil, want: "default"},
		{labels: []string{"heavy"}, flag: true, want: "flag"},
	}
	for _, tc := range tests {
		got, err := ChooseAgentArgs(tc.labels, nil, rules, nil, tc.flag).RuleName()
		if err != nil || got != tc.want {
			t.Fatalf("RuleName(%v, flag=%t) = %q, %v, want %q", tc.labels, tc.flag, got, err, tc.want)
		}
	}
}
