package main

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/authwatch"
)

func TestCLICheckStatusAndReportFailure(t *testing.T) {
	tmp := t.TempDir()
	cfgPath := filepath.Join(tmp, "config.toml")
	stateDir := filepath.Join(tmp, "state")
	credsPath := filepath.Join(tmp, "work", ".credentials.json")
	if err := os.MkdirAll(filepath.Dir(credsPath), 0o700); err != nil {
		t.Fatal(err)
	}

	const secretAccess = "sk-ant-oat01-CLI-SECRET-ACCESS"   //nolint:gosec // G101: test fixture
	const secretRefresh = "sk-ant-ort01-CLI-SECRET-REFRESH" //nolint:gosec // G101: test fixture

	now := time.Date(2026, 11, 18, 12, 0, 0, 0, time.UTC)
	creds := fmt.Sprintf(`{
		"claudeAiOauth": {
			"accessToken": %q,
			"refreshToken": %q,
			"expiresAt": %d,
			"refreshTokenExpiresAt": %d
		}
	}`, secretAccess, secretRefresh, now.Add(4*time.Hour).UnixMilli(), now.Add(48*time.Hour).UnixMilli())
	if err := os.WriteFile(credsPath, []byte(creds), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := fmt.Sprintf(`
notify = ["fake-notify"]
[[profiles]]
name = "work"
credentials_file = %q
`, credsPath)
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}

	env := []string{"HOME=" + tmp, "XDG_STATE_HOME=" + stateDir}
	var notifiedArgs [][]string
	rt := appRuntime{
		readStore: authwatch.DefaultStoreReader("linux", nil),
		notify: func(_ context.Context, argv, _ []string) error {
			notifiedArgs = append(notifiedArgs, argv)
			return nil
		},
		now: func() time.Time { return now },
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"check", "--config", cfgPath}, &stdout, &stderr, env, rt)
	if code != 1 {
		t.Fatalf("check exit = %d, want 1 (warn); stderr=%s", code, stderr.String())
	}
	if len(notifiedArgs) != 1 || notifiedArgs[0][1] != "work" || notifiedArgs[0][2] != "warn" {
		t.Fatalf("unexpected notify args: %v", notifiedArgs)
	}

	stdout.Reset()
	stderr.Reset()
	code = run([]string{"status", "--config", cfgPath, "--profile", "work", "--line"}, &stdout, &stderr, env, rt)
	if code != 1 {
		t.Fatalf("status exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	if !strings.HasPrefix(stdout.String(), "work:warn(") {
		t.Errorf("unexpected status --line: %q", stdout.String())
	}

	// Assert no token value in stdout, stderr, state file, or notifier argv.
	stateBytes, err := os.ReadFile(filepath.Join(stateDir, "authwatch", "state.json"))
	if err != nil {
		t.Fatal(err)
	}
	for _, haystack := range []string{stdout.String(), stderr.String(), string(stateBytes), strings.Join(notifiedArgs[0], " ")} {
		if strings.Contains(haystack, secretAccess) || strings.Contains(haystack, secretRefresh) {
			t.Fatalf("secret token leaked in output/state: %s", haystack)
		}
	}

	// report-failure subcommand.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"report-failure", "--config", cfgPath, "--profile", "work"}, &stdout, &stderr, env, rt)
	if code != 0 {
		t.Fatalf("report-failure exit = %d, stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "work failing since") {
		t.Errorf("unexpected report-failure output: %q", stdout.String())
	}
}
