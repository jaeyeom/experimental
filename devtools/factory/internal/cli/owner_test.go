package cli

import (
	"context"
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestOwnerReturnsMachineAndTab(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	old := time.Date(2026, 11, 1, 9, 0, 0, 0, time.UTC)
	seedDB(t, db, func(st *store.Store) {
		ctx := context.Background()
		for _, c := range []store.Claim{
			{Source: bearKey, ID: "PRJ-1", ClaimedAt: old, Machine: "laptop", TabID: "t1", WorkspaceID: "w1", State: store.Released, UpdatedAt: old},
			{Source: bearKey, ID: "PRJ-1", ClaimedAt: old.Add(time.Hour), Machine: "desk", TabID: "t7", WorkspaceID: "w2", State: store.Done, UpdatedAt: old},
		} {
			if err := st.InsertClaim(ctx, c); err != nil {
				t.Fatal(err)
			}
		}
	})

	stdout, stderr, code := runWriteCmd(t, cfg, db, "owner", "PRJ-1", "--json")
	assertExit(t, code, stdout, stderr, ExitOK)
	var got map[string]any
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("stdout is not JSON: %v\n%s", err, stdout)
	}
	if got["machine"] != "desk" || got["tab"] != "t7" || got["workspace"] != "w2" || got["state"] != "done" || got["source"] != bearKey {
		t.Fatalf("owner = %v, want the latest claim on desk, tab t7", got)
	}

	stdout, stderr, code = runWriteCmd(t, cfg, db, "owner", "PRJ-1")
	assertExit(t, code, stdout, stderr, ExitOK)
	if !strings.HasPrefix(stdout, "jira:acme PRJ-1: machine desk, lane -, tab t7, workspace w2, done since 2026-11-01T10:00:00Z") {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestOwnerNeverClaimed(t *testing.T) {
	cfg := writeSources(t, srcSpec{kind: "jira", name: "acme"})
	db := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runWriteCmd(t, cfg, db, "owner", "PRJ-9", "--json")
	assertExit(t, code, stdout, stderr, ExitOK)
	if stdout != "null\n" {
		t.Fatalf("stdout = %q, want null", stdout)
	}
	stdout, _, _ = runWriteCmd(t, cfg, db, "owner", "PRJ-9")
	if stdout != "no owner: PRJ-9 was never claimed\n" {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestTickEnqueueAndApplyConflict(t *testing.T) {
	_, stderr, code := runCmd(t, "tick", "--enqueue", "--apply")
	if code != ExitUsage || !strings.Contains(stderr, "expected --enqueue or --apply, found both") {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
}
