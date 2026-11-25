package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestMaintainAndLedgerCLI(t *testing.T) {
	dir := t.TempDir()
	ledgerPath := filepath.Join(dir, "ledger.jsonl")
	cfgPath := filepath.Join(dir, "prsync.config")
	backendScript := filepath.Join(dir, "fake-backend.sh")

	script := `#!/bin/sh
case "$1" in
  list-sessions)
    printf '[{"id":"s1","label":"PROJ-10","status":"idle"}]\n'
    ;;
  read-idle)
    printf '{"idle":true,"has_dialog":false,"status":"idle"}\n'
    ;;
  send-prompt)
    cat >/dev/null
    printf '{"status":"done"}\n'
    ;;
esac
`
	if err := os.WriteFile(backendScript, []byte(script), 0o700); err != nil {
		t.Fatalf("write backend script: %v", err)
	}
	cfgBody := "policy_rebase=auto\nagent_backend=command\nagent_backend_cmd=" + backendScript + "\ndispatch_ledger_file=" + ledgerPath + "\n"
	if err := os.WriteFile(cfgPath, []byte(cfgBody), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}

	id := "PROJ-10"
	scanDoc := scan.Document{
		PRs: []scan.PR{{
			Repo:             "acme/widgets",
			Number:           10,
			Title:            "[PROJ-10] Rebase me",
			URL:              "https://github.com/acme/widgets/pull/10",
			State:            "open",
			Identifier:       &id,
			Base:             "main",
			Head:             "feat/rebase",
			HeadSHA:          "abc1234567",
			MergeStateStatus: "BEHIND",
			Mergeable:        "MERGEABLE",
		}},
	}
	scanBytes, err := json.Marshal(scanDoc)
	if err != nil {
		t.Fatalf("marshal scan: %v", err)
	}
	stdinFile := filepath.Join(dir, "scan.json")
	if err := os.WriteFile(stdinFile, scanBytes, 0o600); err != nil {
		t.Fatalf("write scan file: %v", err)
	}

	// Run maintain in default line-output mode.
	runWithStdin := func(args []string) (int, string, string) {
		f, err := os.Open(stdinFile)
		if err != nil {
			t.Fatalf("open stdin file: %v", err)
		}
		defer f.Close()
		oldStdin := os.Stdin
		os.Stdin = f
		defer func() { os.Stdin = oldStdin }()

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), args, &stdout, &stderr, executor.NewBasicExecutor())
		return code, stdout.String(), stderr.String()
	}

	code, out, errOut := runWithStdin([]string{"maintain", "--config", cfgPath, "--stdin"})
	if code != ExitOK {
		t.Fatalf("maintain exited %d, stderr=%q, stdout=%q", code, errOut, out)
	}
	if !strings.Contains(out, "DISPATCH") || !strings.Contains(out, "acme/widgets#10") || !strings.Contains(out, "rebase") {
		t.Fatalf("maintain stdout = %q", out)
	}

	// Second run in JSON mode must skip as already_dispatched.
	code, out, errOut = runWithStdin([]string{"maintain", "--config", cfgPath, "--stdin", "--json"})
	if code != ExitOK {
		t.Fatalf("second maintain exited %d, stderr=%q", code, errOut)
	}
	var doc policy.Document
	if err := json.Unmarshal([]byte(out), &doc); err != nil {
		t.Fatalf("unmarshal maintain JSON: %v\n%s", err, out)
	}
	if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != policy.DecisionSkip || doc.Decisions[0].Reason != "already_dispatched" {
		t.Fatalf("second run decisions = %+v", doc.Decisions)
	}

	// Query ledger CLI in both text and JSON modes.
	var stdout, stderr bytes.Buffer
	code = Execute(context.Background(), []string{"ledger", "--config", cfgPath, "--pr", "acme/widgets#10"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("ledger exited %d, stderr=%q", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "acme/widgets#10") || !strings.Contains(stdout.String(), "dispatched") {
		t.Fatalf("ledger stdout = %q", stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"ledger", "--config", cfgPath, "--json"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("ledger --json exited %d, stderr=%q", code, stderr.String())
	}
	var entries []ledger.Entry
	if err := json.Unmarshal(stdout.Bytes(), &entries); err != nil {
		t.Fatalf("unmarshal ledger JSON: %v", err)
	}
	if len(entries) != 1 || entries[0].HeadSHA != "abc1234567" || entries[0].Time.IsZero() || time.Since(entries[0].Time) > time.Hour {
		t.Fatalf("ledger entries = %+v", entries)
	}
}

func TestMaintainNonZeroExitOnAPIError(t *testing.T) {
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "prsync.config")
	// Point gh_bin at a nonexistent binary so the scan fails with an API/tool error.
	cfgBody := "gh_bin=/nonexistent/gh-binary\npolicy_rebase=auto\n"
	if err := os.WriteFile(cfgPath, []byte(cfgBody), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"maintain", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code == ExitOK {
		t.Fatalf("expected non-zero exit code on API error, got 0")
	}
}
