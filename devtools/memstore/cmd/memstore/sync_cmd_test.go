package main

import (
	"encoding/json"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func TestCLISyncPushAndPull(t *testing.T) {
	_, env := setupTestCLI(t)
	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d stderr=%s", code, errOut)
	}

	inboxDir := filepath.Join(t.TempDir(), "inbox")
	code, out, errOut := runCLI(env, "sync", "push",
		"--inbox-dir", inboxDir,
		"--id", "env_cli_1",
		"--compartment", "engineering",
		"--sensitivity", "normal",
		"--summary", "CLI sync event",
		"--json",
	)
	if code != exitOK {
		t.Fatalf("sync push exit=%d stderr=%s out=%s", code, errOut, out)
	}

	// Dry-run does not ingest or ack.
	code, out, errOut = runCLI(env, "sync", "pull",
		"--inbox-dir", inboxDir,
		"--vault", "work",
		"--profile", "work",
		"--dry-run",
		"--json",
	)
	if code != exitOK {
		t.Fatalf("sync pull --dry-run exit=%d stderr=%s", code, errOut)
	}
	var dryReport memstore.PullReport
	if err := json.Unmarshal([]byte(out), &dryReport); err != nil {
		t.Fatalf("unmarshal dry-run report: %v\nout=%s", err, out)
	}
	if dryReport.Listed != 1 || dryReport.Acked != 0 {
		t.Fatalf("dry-run report = %+v, want listed=1 acked=0", dryReport)
	}

	// Real pull ingests and acks.
	code, out, errOut = runCLI(env, "sync", "pull",
		"--inbox-dir", inboxDir,
		"--vault", "work",
		"--profile", "work",
		"--by", "sync-cron",
		"--json",
	)
	if code != exitOK {
		t.Fatalf("sync pull exit=%d stderr=%s", code, errOut)
	}
	var report memstore.PullReport
	if err := json.Unmarshal([]byte(out), &report); err != nil {
		t.Fatalf("unmarshal pull report: %v\nout=%s", err, out)
	}
	if report.Ingested != 1 || report.Acked != 1 {
		t.Fatalf("pull report = %+v, want ingested=1 acked=1", report)
	}
}
