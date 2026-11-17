package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestScanEmitJobs(t *testing.T) {
	t.Parallel()

	ghBin, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n"+
		"job_machines=tiger\nstate_file="+filepath.Join(stateDir, "state.json")+"\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath, "--emit-jobs"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var got jobs.Document
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	want := jobs.Spec{
		Kind: "comment", Subject: "acme/widgets#123", Head: "abc123def456",
		Machine: "tiger", Affinity: "tab", Executor: "tab", Items: []string{"PRRC_widget"},
	}
	if len(got.Jobs) != 1 || !specEqual(got.Jobs[0], want) {
		t.Fatalf("jobs = %+v, want %+v", got.Jobs, want)
	}
	owners, err := jobs.LoadOwners(filepath.Join(stateDir, jobs.OwnersFile))
	if err != nil || owners["acme/widgets#123"] != "local" {
		t.Fatalf("owners = %v, %v", owners, err)
	}
}

func TestScanWithoutEmitJobsIsUnchanged(t *testing.T) {
	t.Parallel()

	ghBin, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin="+herdrBin+"\nauthor=alice\nrepos=acme/widgets\n"+
		"job_lanes=needs-linux=local\nstate_file="+filepath.Join(stateDir, "state.json")+"\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil || len(doc.PRs) != 1 {
		t.Fatalf("scan doc = %+v, %v", doc, err)
	}
	if strings.Contains(stdout.String(), `"jobs"`) || strings.Contains(stdout.String(), "labels") {
		t.Fatalf("stdout = %s", stdout.String())
	}
	if _, err := os.Stat(filepath.Join(stateDir, jobs.OwnersFile)); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("owners file stat = %v, want none without --emit-jobs", err)
	}
}

func specEqual(a, b jobs.Spec) bool {
	ra, _ := json.Marshal(a)
	rb, _ := json.Marshal(b)
	return string(ra) == string(rb)
}

// jobMock answers agentbus jobs show for job and records reports.
func jobMock(t *testing.T, job string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("gh", true)
	mock.SetAvailableCommand("agentbus", true)
	mock.ExpectCommandWithArgs("agentbus", "jobs", "show", "j-1", "--json").WillSucceed(job, 0).Build()
	return mock
}

func ghArgsContain(want string) func(context.Context, executor.ToolConfig) bool {
	return func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == "gh" && strings.Contains(strings.Join(cfg.Args, " "), want)
	}
}

func runJobCmd(t *testing.T, mock *executor.MockExecutor, extra ...string) (int, jobResult, string) {
	t.Helper()
	cfgPath := writeScanConfig(t, "gh_bin=gh\nherdr_bin=herdr\nauthor=alice\n")
	var stdout, stderr bytes.Buffer
	args := append([]string{"dispatch", "--config", cfgPath, "--job", "j-1", "--runner", "r1"}, extra...)
	code := Execute(context.Background(), args, &stdout, &stderr, mock)
	var res jobResult
	if stdout.Len() > 0 {
		if err := json.Unmarshal(stdout.Bytes(), &res); err != nil {
			t.Fatalf("json: %v\n%s", err, stdout.String())
		}
	}
	return code, res, stderr.String()
}

func agentbusCalls(mock *executor.MockExecutor) []string {
	var out []string
	for _, call := range mock.GetCallHistory() {
		if call.Config.Command == "agentbus" {
			out = append(out, strings.Join(call.Config.Args, " "))
		}
	}
	return out
}

func gitCalls(mock *executor.MockExecutor) int {
	n := 0
	for _, call := range mock.GetCallHistory() {
		if call.Config.Command == "git" {
			n++
		}
	}
	return n
}

const rebaseJobJSON = `{"id":"j-1","kind":"rebase","subject":"acme/widgets#12","head":"abc123","needs":{"machine":"turtle","affinity":"tab","executor":"cli"}}`

func TestDispatchJobCleanRebaseReportsDone(t *testing.T) {
	t.Parallel()

	mock := jobMock(t, rebaseJobJSON)
	mock.ExpectCustom(ghArgsContain("pullRequest(number:$num){id}")).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCustom(ghArgsContain("updateMethod:REBASE")).
		WillSucceed(`{"data":{"updatePullRequestBranch":{"pullRequest":{"headRefOid":"def456"}}}}`, 0).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "done", "j-1", "--runner", "r1", "--note", "rebased on GitHub to def456").
		WillSucceed("", 0).Build()

	code, res, stderr := runJobCmd(t, mock)
	if code != ExitOK || res.Result != jobDone || res.Queued != "" {
		t.Fatalf("exit = %d res = %+v stderr = %s", code, res, stderr)
	}
	if calls := agentbusCalls(mock); len(calls) != 2 || !strings.HasPrefix(calls[1], "jobs done j-1") {
		t.Fatalf("agentbus calls = %q", calls)
	}
	if n := gitCalls(mock); n != 0 {
		t.Fatalf("git calls = %d, want none: the rebase needs no checkout", n)
	}
}

func TestDispatchJobConflictQueuesOneConflictJob(t *testing.T) {
	t.Parallel()

	mock := jobMock(t, rebaseJobJSON)
	mock.ExpectCustom(ghArgsContain("pullRequest(number:$num){id}")).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCustom(ghArgsContain("updateMethod:REBASE")).
		WillSucceed(`{"errors":[{"message":"merge conflict"}]}`, 1).Build()
	mock.ExpectCommandWithArgs("agentbus", "jobs", "enqueue", "--kind", "conflict", "--subject", "acme/widgets#12",
		"--head", "abc123", "--machine", "turtle", "--affinity", "tab", "--executor", "tab", "--json").
		WillSucceed(`{"id":"j-2","state":"ready"}`, 0).Build()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == "agentbus" && len(cfg.Args) > 2 && cfg.Args[1] == "done"
	}).WillSucceed("", 0).Build()

	code, res, stderr := runJobCmd(t, mock)
	if code != ExitOK || res.Result != jobDone || res.Queued != "j-2" {
		t.Fatalf("exit = %d res = %+v stderr = %s", code, res, stderr)
	}
	enqueues := 0
	for _, call := range agentbusCalls(mock) {
		if strings.HasPrefix(call, "jobs enqueue") {
			enqueues++
		}
	}
	if enqueues != 1 {
		t.Fatalf("enqueues = %d, want 1: %q", enqueues, agentbusCalls(mock))
	}
}

func TestDispatchJobFailedRebaseReportsFail(t *testing.T) {
	t.Parallel()

	mock := jobMock(t, rebaseJobJSON)
	mock.ExpectCustom(ghArgsContain("pullRequest(number:$num){id}")).
		WillSucceed(`{"data":{"repository":{"pullRequest":{"id":"PR_1"}}}}`, 0).Build()
	mock.ExpectCustom(ghArgsContain("updateMethod:REBASE")).
		WillFail("GraphQL: Head branch was modified", 1).Build()
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == "agentbus" && len(cfg.Args) > 2 && cfg.Args[1] == "fail"
	}).WillSucceed("", 0).Build()

	code, res, stderr := runJobCmd(t, mock)
	if code != ExitOK || res.Result != jobFail || !strings.Contains(res.Note, "Head branch was modified") {
		t.Fatalf("exit = %d res = %+v stderr = %s", code, res, stderr)
	}
	calls := agentbusCalls(mock)
	if len(calls) != 2 || !strings.HasPrefix(calls[1], "jobs fail j-1 --runner r1 --note rebase on GitHub:") {
		t.Fatalf("agentbus calls = %q", calls)
	}
}

func TestDispatchJobUnknownKindReportsFail(t *testing.T) {
	t.Parallel()

	mock := jobMock(t, `{"id":"j-1","kind":"merge","subject":"acme/widgets#12","needs":{"machine":"any","executor":"cli"}}`)
	mock.ExpectCommandWithArgs("agentbus", "jobs", "fail", "j-1", "--runner", "r1", "--note", `prsync runs no "merge" job`).
		WillSucceed("", 0).Build()
	code, res, stderr := runJobCmd(t, mock)
	if code != ExitOK || res.Result != jobFail {
		t.Fatalf("exit = %d res = %+v stderr = %s", code, res, stderr)
	}
}

func TestDispatchJobReportErrorExits3(t *testing.T) {
	t.Parallel()

	mock := jobMock(t, `{"id":"j-1","kind":"merge","subject":"acme/widgets#12","needs":{"machine":"any","executor":"cli"}}`)
	mock.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		return cfg.Command == "agentbus" && len(cfg.Args) > 2 && cfg.Args[1] == "fail"
	}).WillFail("job j-1 is done; only a running, unknown, or parked job can be closed", 1).Build()
	code, _, stderr := runJobCmd(t, mock)
	if code != ExitPrecondition || !strings.Contains(stderr, "report job j-1") {
		t.Fatalf("exit = %d stderr = %s", code, stderr)
	}
}

func TestDispatchJobFlagConflicts(t *testing.T) {
	t.Parallel()

	for _, args := range [][]string{
		{"dispatch", "--job", "j-1", "--pr", "acme/widgets#1"},
		{"dispatch", "--job", "j-1", "--rebase"},
		{"dispatch", "--runner", "r1"},
	} {
		var stdout, stderr bytes.Buffer
		if code := Execute(context.Background(), args, &stdout, &stderr, executor.NewMockExecutor()); code != ExitUsage {
			t.Errorf("%q exit = %d, want %d (stderr %s)", args, code, ExitUsage, stderr.String())
		}
	}
}

// TestDispatchJobCommentPromptsTheTab runs a comment job through the
// fixture gh and herdr with a fake agentbus on PATH.
func TestDispatchJobCommentPromptsTheTab(t *testing.T) {
	bin := t.TempDir()
	log := filepath.Join(t.TempDir(), "agentbus.log")
	script := "#!/bin/sh\nprintf '%s\\n' \"$*\" >> " + log + "\n" +
		"case \"$2\" in show) printf '%s\\n' '{\"id\":\"j-1\",\"kind\":\"comment\",\"subject\":\"acme/widgets#123\",\"head\":\"abc123def456\",\"needs\":{\"machine\":\"local\",\"affinity\":\"tab\",\"executor\":\"tab\"}}';; esac\n"
	if err := os.WriteFile(filepath.Join(bin, "agentbus"), []byte(script), 0o700); err != nil { //nolint:gosec // test fake must be executable
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
	t.Setenv("JOB_RUNNER", "r9")
	ghBin, herdrBin := fixtureBins(t)
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"), "wake_before_prompt=off")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--job", "j-1"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d stderr = %s stdout = %s", code, stderr.String(), stdout.String())
	}
	var res jobResult
	if err := json.Unmarshal(stdout.Bytes(), &res); err != nil {
		t.Fatal(err)
	}
	if res.Result != jobDone || !strings.HasPrefix(res.Note, "dispatched") {
		t.Fatalf("res = %+v", res)
	}
	raw, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), "jobs done j-1 --runner r9 --note dispatched") {
		t.Fatalf("agentbus log = %s", raw)
	}
}
