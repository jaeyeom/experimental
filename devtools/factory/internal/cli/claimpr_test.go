package cli

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestClaimPRRecordsHead(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedClaimPR(t, dbPath, store.Started)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommand("gh").WillSucceed(oneOpenPR, 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "claim-pr", "PRJ-1", "https://github.com/acme/widgets/pull/7", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	got := readAdoptClaim(t, dbPath)
	if len(got.PRs) != 1 {
		t.Fatalf("prs = %#v", got.PRs)
	}
	pr := got.PRs[0]
	if pr.Repo != "acme/widgets" || pr.Number != 7 || pr.Head != "feature" || pr.Title != "review me" || pr.URL != "https://github.com/acme/widgets/pull/7" {
		t.Fatalf("recorded pr = %#v", pr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestClaimPRGHFailureWritesNothing(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedClaimPR(t, dbPath, store.Started)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommand("gh").WillError(errors.New("gh down")).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "claim-pr", "PRJ-1", "acme/widgets#7", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "gh open prs") || !strings.Contains(stderr, "gh down") {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if prs := readAdoptClaim(t, dbPath).PRs; len(prs) != 0 {
		t.Fatalf("prs = %#v", prs)
	}
}

func TestClaimPRMissingClaim(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedClaimPR(t, dbPath, "")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "claim-pr", "PRJ-1", "acme/widgets#7", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "claim-pr PRJ-1: expected an active claim, found none") {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestClaimPRBadURL(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedClaimPR(t, dbPath, store.Started)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "claim-pr", "PRJ-1", "not-a-pr", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "expected owner/repo#N or a pull request URL") {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
	if prs := readAdoptClaim(t, dbPath).PRs; len(prs) != 0 {
		t.Fatalf("prs = %#v", prs)
	}
}

func seedClaimPR(t *testing.T, dbPath string, state store.ClaimState) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := rankItem("jira:acme", "PRJ-1", "Ship it", "Task", "In Progress", "", "alice", 2, false, day1, nil)
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if state == "" {
			return
		}
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: time.Now().UTC().Add(-2 * time.Hour),
			Machine: "laptop", State: state, UpdatedAt: day1,
		}); err != nil {
			t.Fatal(err)
		}
	})
}
