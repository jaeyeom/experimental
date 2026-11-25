package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/secretwatch"
)

func TestMain(m *testing.M) {
	if pklExec := resolvePklExec(); pklExec != "" {
		_ = os.Setenv("PKL_EXEC", pklExec)
	}
	os.Exit(m.Run())
}

func resolvePklExec() string {
	if bin := os.Getenv("PKL_EXEC"); bin != "" {
		return bin
	}
	for _, key := range []string{"RUNFILES_DIR", "TEST_SRCDIR"} {
		root := os.Getenv(key)
		if root == "" {
			continue
		}
		matches, err := filepath.Glob(filepath.Join(root, "*pkl-cli-*", "file", "downloaded"))
		if err != nil {
			continue
		}
		for _, candidate := range matches {
			if info, err := os.Stat(candidate); err == nil && !info.IsDir() {
				return candidate
			}
		}
	}
	return ""
}

func TestCLICheckScanFollowAndMasking(t *testing.T) {
	tmp := t.TempDir()
	stateDir := filepath.Join(tmp, "state")
	cfgPath := filepath.Join(tmp, "config.pkl")
	if err := os.WriteFile(cfgPath, []byte(`
notify { "fake-notify"; "{rule}"; "{prefix}"; "{unit}"; "{cursor}" }
allowlist {
  units { "ignored.service" }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}

	now := time.Date(2026, 11, 25, 14, 0, 0, 0, time.UTC)
	secret1 := "ghp_" + strings.Repeat("m9N8", 9)
	secret2 := "xoxb-" + "123456789012-" + "XyZ987654321AbCd"

	env := []string{"HOME=" + tmp, "XDG_STATE_HOME=" + stateDir}
	var notified [][]string
	var notifiedEnv [][]string

	entries := []secretwatch.Entry{
		{Cursor: "c1", Timestamp: now.Add(time.Second), Unit: "ignored.service", Process: "test", PID: "1", Message: secret1},
		{Cursor: "c2", Timestamp: now.Add(2 * time.Second), Unit: "api.service", Process: "curl", PID: "42", Message: "Authorization: Bearer " + secret1},
	}

	rt := appRuntime{
		stream: func(_ context.Context, q secretwatch.Query, emit func(secretwatch.Entry) error) (string, error) {
			if q.Kind == secretwatch.JournalSystem {
				return "", fmt.Errorf("%w: permission denied", secretwatch.ErrJournalUnreadable)
			}
			afterFound := q.AfterCursor == ""
			var tail string
			for _, e := range entries {
				if !afterFound {
					if e.Cursor == q.AfterCursor {
						afterFound = true
					}
					continue
				}
				tail = e.Cursor
				_ = emit(e)
			}
			return tail, nil
		},
		notify: func(_ context.Context, argv, cmdEnv []string) error {
			notified = append(notified, argv)
			notifiedEnv = append(notifiedEnv, cmdEnv)
			return nil
		},
		now: func() time.Time { return now },
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"check", "--config", cfgPath}, &stdout, &stderr, env, rt)
	if code != 1 {
		t.Fatalf("check exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "system journal unreadable") {
		t.Errorf("expected unreadable system journal note in stderr: %q", stderr.String())
	}
	if len(notified) != 1 || notified[0][1] != "github-token" || notified[0][2] != "ghp_****" {
		t.Fatalf("unexpected notify argv: %+v", notified)
	}

	// Second check run sees no new entries -> exit 0.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"check", "--config", cfgPath}, &stdout, &stderr, env, rt)
	if code != 0 {
		t.Fatalf("second check exit = %d, want 0; stdout=%s stderr=%s", code, stdout.String(), stderr.String())
	}

	// One-shot scan does not change state file.
	statePath := filepath.Join(stateDir, "secretwatch", "state.json")
	stateBefore, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}
	entries = append(entries, secretwatch.Entry{
		Cursor: "c3", Timestamp: now.Add(3 * time.Second), Unit: "bot.service", Process: "bot", PID: "77", Message: secret2,
	})
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"scan", "--since", "1h", "--unit", "bot.service", "--json", "--config", cfgPath}, &stdout, &stderr, env, rt)
	if code != 1 {
		t.Fatalf("scan exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	var decoded secretwatch.Report
	if err := json.Unmarshal(stdout.Bytes(), &decoded); err != nil {
		t.Fatalf("decode scan json: %v\n%s", err, stdout.String())
	}
	if len(decoded.Findings) != 1 || decoded.Findings[0].Cursor != "c3" {
		t.Fatalf("unexpected scan findings: %+v", decoded.Findings)
	}
	stateAfter, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}
	if string(stateBefore) != string(stateAfter) {
		t.Fatalf("scan changed state.json")
	}

	// Verify strict masking across stdout, stderr, state file, and notifier argv/env.
	allHaystacks := []string{stdout.String(), stderr.String(), string(stateAfter)}
	for _, argv := range notified {
		allHaystacks = append(allHaystacks, strings.Join(argv, " "))
	}
	for _, e := range notifiedEnv {
		allHaystacks = append(allHaystacks, strings.Join(e, "\n"))
	}
	for _, h := range allHaystacks {
		assertNoSecretLeak(t, h, secret1)
		assertNoSecretLeak(t, h, secret2)
	}

	// check --follow streams remaining entry c3 and updates cursor.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"check", "--follow", "--json", "--config", cfgPath}, &stdout, &stderr, env, rt)
	if code != 1 {
		t.Fatalf("follow exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	var followFinding secretwatch.Finding
	if err := json.Unmarshal(stdout.Bytes(), &followFinding); err != nil {
		t.Fatalf("decode follow json: %v\n%s", err, stdout.String())
	}
	if followFinding.Cursor != "c3" || followFinding.MaskedPrefix != "xoxb****" {
		t.Fatalf("unexpected follow finding: %+v", followFinding)
	}
}

func TestCLIErrorExitCodes(t *testing.T) {
	tmp := t.TempDir()
	env := []string{"HOME=" + tmp}
	rt := appRuntime{
		stream: func(context.Context, secretwatch.Query, func(secretwatch.Entry) error) (string, error) {
			return "", errors.New("journalctl missing")
		},
		now: time.Now,
	}

	var out, errOut bytes.Buffer
	if code := run([]string{"scan"}, &out, &errOut, env, rt); code != 2 {
		t.Fatalf("scan without --since exit = %d, want 2", code)
	}
	out.Reset()
	errOut.Reset()
	if code := run([]string{"check", "--config", filepath.Join(tmp, "missing.pkl")}, &out, &errOut, env, rt); code != 2 {
		t.Fatalf("check missing config exit = %d, want 2", code)
	}
	out.Reset()
	errOut.Reset()
	if code := run([]string{"check"}, &out, &errOut, env, rt); code != 2 {
		t.Fatalf("check journal error exit = %d, want 2", code)
	}
}

func assertNoSecretLeak(t *testing.T, haystack, secret string) {
	t.Helper()
	runes := []rune(secret)
	for i := 0; i+5 <= len(runes); i++ {
		sub := string(runes[i : i+5])
		if strings.Contains(haystack, sub) {
			t.Fatalf("leaked >4 chars (%q) of secret in %q", sub, haystack)
		}
	}
}
