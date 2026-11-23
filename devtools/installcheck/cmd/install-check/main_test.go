package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestDryRunThenRealRun(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, "c.toml")
	state := filepath.Join(home, "state", "state.json")
	cfg := "notify_name = \"agent\"\nstate_file = \"" + state + "\"\n[[repo]]\nurl = \"https://github.com/o/r\"\nissues = [1]\n"
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}
	head := "aaaaaaaaaaaaaaaa"
	var sent []string
	runner := func(_ context.Context, name string, args ...string) ([]byte, error) {
		switch name {
		case "git":
			return []byte(head + "\trefs/heads/main\n"), nil
		case "gh":
			return []byte(`{"number":1,"title":"t","state":"CLOSED","stateReason":"COMPLETED","closedAt":"2026-11-22T12:40:00Z"}`), nil
		}
		sent = append(sent, args[2])
		return nil, nil
	}
	now := func() time.Time { return time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC) }
	env := []string{"HOME=" + home}
	call := func(args ...string) (int, string, string) {
		var out, errb bytes.Buffer
		code := run(append(args, "--config", cfgPath), &out, &errb, env, runner, now)
		return code, out.String(), errb.String()
	}

	if code, _, _ := call("run"); code != 0 { // baseline
		t.Fatalf("baseline code %d", code)
	}
	head = "bbbbbbbbbbbbbbbb"
	// Closed-before-baseline issue 1 is already reported, so nothing to send.
	if code, out, errs := call("--dry-run"); code != 0 || out != "" || len(sent) != 0 {
		t.Fatalf("code=%d out=%q err=%q sent=%v", code, out, errs, sent)
	}
	if code, out, _ := call("status"); code != 0 || !strings.Contains(out, "o/r main: aaaaaaa") {
		t.Fatalf("status code=%d out=%q", code, out)
	}
	if code, _, _ := call("bogus"); code != 2 {
		t.Fatalf("unknown command code %d", code)
	}
}
