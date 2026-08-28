package main

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/unnecessaryinterfaceassertion"
)

func TestWriteIssuesJSONMatchesConfigIssueShape(t *testing.T) {
	issues := []unnecessaryinterfaceassertion.Issue{{
		FilePath: "unnecessary/unnecessary.go",
		Line:     11,
		Column:   1,
		Severity: "warning",
		Message:  "Unnecessary interface assertion",
		ToolName: unnecessaryinterfaceassertion.ToolName,
	}}
	var buf bytes.Buffer
	if err := writeIssues(&buf, issues, true); err != nil {
		t.Fatalf("writeIssues() error = %v", err)
	}
	var decoded []unnecessaryinterfaceassertion.Issue
	if err := json.Unmarshal(buf.Bytes(), &decoded); err != nil {
		t.Fatalf("JSON = %s, unmarshal error = %v", buf.String(), err)
	}
	if len(decoded) != 1 {
		t.Fatalf("decoded %d issues, want 1", len(decoded))
	}
	if decoded[0].FilePath != "unnecessary/unnecessary.go" {
		t.Errorf("FilePath = %q", decoded[0].FilePath)
	}
	if decoded[0].Message != "Unnecessary interface assertion" {
		t.Errorf("Message = %q", decoded[0].Message)
	}
	if decoded[0].ToolName != "unnecessary-interface-assertion-linter" {
		t.Errorf("ToolName = %q", decoded[0].ToolName)
	}
}

func TestWriteIssuesText(t *testing.T) {
	issues := []unnecessaryinterfaceassertion.Issue{{
		FilePath: "unnecessary/unnecessary.go",
		Line:     11,
		Column:   1,
		Message:  "Unnecessary interface assertion",
	}}
	var buf bytes.Buffer
	if err := writeIssues(&buf, issues, false); err != nil {
		t.Fatalf("writeIssues() error = %v", err)
	}
	got := buf.String()
	if !strings.Contains(got, "unnecessary/unnecessary.go:11:1: Unnecessary interface assertion") {
		t.Errorf("text output = %q", got)
	}
}

func TestRunHelp(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"-h"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run(-h) exit = %d, want 0", code)
	}
	if !strings.Contains(stderr.String(), "USAGE:") {
		t.Errorf("help output = %q, want USAGE", stderr.String())
	}
}
