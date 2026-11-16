package main

import (
	"strings"
	"testing"
)

func TestReport(t *testing.T) {
	ta := newTestApp(t)
	ta.mustRun("add", "Ada", "--handle", "chat:ada.l")
	base := []string{
		"report", "@ada.l", "--channel", "chat", "--link", "https://chat.example/x/1",
		"--summary", "agreed to split the API change", "--by", "ops/inbox",
	}
	if out := ta.mustRun(base...); out != "1\n" {
		t.Fatalf("report printed %q", out)
	}
	code, out, errOut := ta.run(base...)
	if code != exitOK || out != "1\n" || !strings.Contains(errOut, "nothing added") {
		t.Errorf("duplicate: exit %d, out %q, err %q", code, out, errOut)
	}

	with := func(flag, value string) []string {
		args := append([]string{}, base...)
		for i := range args {
			if args[i] == flag {
				args[i+1] = value
			}
		}
		return args
	}
	without := func(flag string) []string {
		var args []string
		for i := 0; i < len(base); i++ {
			if base[i] == flag {
				i++
				continue
			}
			args = append(args, base[i])
		}
		return args
	}
	for name, args := range map[string][]string{
		"no channel": without("--channel"),
		"no link":    without("--link"),
		"no summary": without("--summary"),
		"no by":      without("--by"),
		"multi-line": with("--summary", "line one\nline two"),
		"over-long":  with("--summary", strings.Repeat("x", 201)),
		"non-url":    with("--link", "chat thread from monday"),
		"bad date":   append(append([]string{}, base...), "--date", "tomorrow"),
	} {
		if code, _, _ := ta.run(args...); code != exitUsage {
			t.Errorf("%s: exit %d, want %d", name, code, exitUsage)
		}
	}
}
