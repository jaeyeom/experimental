package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/doctor"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func doctorToolOK(t *testing.T) {
	t.Helper()
	withToolTestEnv(t, &cliFakeInstaller{latest: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}})
}

const doctorTabs = `{
  "id": "cli:tab:list",
  "result": {
    "type": "tab_list",
    "tabs": [
      {"tab_id": "t-stab", "workspace_id": "w1", "label": "PRJ-STAB", "agent_status": "idle", "pane_count": 1},
      {"tab_id": "t-stall", "workspace_id": "w1", "label": "PRJ-STALL", "agent_status": "working", "pane_count": 1}
    ]
  }
}`

const doctorPRs = `{
  "data": {
    "search": {
      "pageInfo": {"hasNextPage": false, "endCursor": null},
      "nodes": [
        {
          "number": 1,
          "title": "[PRJ-HELD] held",
          "url": "https://github.com/acme/widgets/pull/1",
          "isDraft": false,
          "merged": false,
          "mergedAt": null,
          "createdAt": "2026-01-01T00:00:00Z",
          "closedAt": null,
          "headRefName": "feature",
          "repository": {"nameWithOwner": "acme/widgets"},
          "closingIssuesReferences": {"nodes": []}
        }
      ]
    }
  }
}`

func TestDoctorFixture(t *testing.T) {
	doctorToolOK(t)
	cfgPath, dbPath := doctorFixture(t)
	useHerdrGH(t, rankWorkspaces, doctorTabs, doctorPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 {
		t.Fatalf("exit = %d, want 1 (warn), stderr = %s\nstdout:\n%s", code, stderr, stdout)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	for _, want := range []string{
		"ok    binary-on-path",
		"ok    revision",
		"ok    config",
		"ok    store",
		"ok    external-commands",
		"ok    sources",
		"ok    skills",
		"warn  cycle",
		"PRJ-C1 → PRJ-C2 → PRJ-C1",
		"factory unlink PRJ-C1 PRJ-C2",
		"warn  unknown-blocker",
		"PRJ-UB is blocked by PRJ-NOPE, which was not found or is not visible",
		"check the link, or unlink",
		"warn  orphan",
		"PRJ-ORPH has no parent",
		"warn  dropped-claim",
		"PRJ-DROP is In Progress with no tab and no open PR",
		"warn  stale-tab",
		"PRJ-STAB is Done and tab PRJ-STAB is idle",
		"warn  stalled",
		"PRJ-STALL is started and older than 2h0m0s with no open PR",
		"ok    almost-ready",
		"PRJ-ALMOST is missing agent-ready",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
	for _, absent := range []string{
		"PRJ-OK", "PRJ-HELD", "PRJ-YOUNG", "EPIC-1 has no parent",
		"stale-blocker", "PRJ-SD (Done) blocks PRJ-SB", "factory unlink PRJ-SD PRJ-SB",
	} {
		if strings.Contains(stdout, absent) {
			t.Fatalf("reported %s\n%s", absent, stdout)
		}
	}
}

func TestDoctorJSON(t *testing.T) {
	doctorToolOK(t)
	cfgPath, dbPath := doctorFixture(t)
	useHerdrGH(t, rankWorkspaces, doctorTabs, doctorPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--json", "--config", cfgPath, "--db", dbPath)
	if code != 1 {
		t.Fatalf("exit = %d, want 1, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	var got toolcli.DoctorOutput
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if got.Tool != "factory" || got.Status != toolcli.StatusWarn {
		t.Fatalf("header = %+v", got)
	}
	// 7 standard/extra checks + 7 domain findings.
	if len(got.Checks) != 14 {
		t.Fatalf("checks = %d\n%s", len(got.Checks), stdout)
	}
	domain := got.Checks[7:]
	byName := make(map[string]toolcli.CheckResult, len(domain))
	for _, c := range domain {
		byName[c.Name] = c
	}
	if byName["cycle"].Status != toolcli.StatusWarn || byName["almost-ready"].Status != toolcli.StatusOK {
		t.Fatalf("ends = %#v %#v", byName["cycle"], byName["almost-ready"])
	}
	if byName["unknown-blocker"].Detail != "PRJ-UB is blocked by PRJ-NOPE, which was not found or is not visible" {
		t.Fatalf("unknown = %#v", byName["unknown-blocker"])
	}
	if strings.Contains(stdout, "factory unlink PRJ-SD") || strings.Contains(stdout, "(Done) blocks") || strings.Contains(stdout, "stale-blocker") {
		t.Fatalf("done blocker was reported:\n%s", stdout)
	}
	for _, found := range domain {
		if found.Name == "almost-ready" {
			continue
		}
		if found.Status != toolcli.StatusWarn {
			t.Fatalf("status = %#v", found)
		}
	}
}

func writeDoctorConfig(t *testing.T, sources ...nextSource) string {
	t.Helper()
	trusted := make([]nextSource, len(sources))
	for i, src := range sources {
		src.extra += "\n[source.trust]\nenvironment = \"trusted\"\n"
		trusted[i] = src
	}
	return writeNextConfig(t, trusted...)
}

func TestDoctorClean(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := "jira:acme"
	ready := []string{"agent-ready"}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		items := []source.Item{
			rankItem(src, "EPIC-1", "epic", "Epic", "To Do", "", "", 2, false, day1, nil),
			rankItem(src, "PRJ-OK", "ok", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready),
		}
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
	})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "ok    binary-on-path") {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	stdout, stderr, code = runCmd(t, "doctor", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("json exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	var got toolcli.DoctorOutput
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatal(err)
	}
	if got.Status != toolcli.StatusOK || len(got.Checks) != 7 {
		t.Fatalf("got = %+v", got)
	}
}

func TestDoctorTwoSources(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"}, nextSource{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		acme := []source.Item{rankItem("jira:acme", "PRJ-1", "a", "Task", "To Do", "", "", 2, false, day1, ready)}
		cub := []source.Item{rankItem("jira:cub", "CUB-1", "b", "Task", "To Do", "", "", 2, false, day1, ready)}
		if err := st.UpsertItems(ctx, "jira:acme", acme); err != nil {
			t.Fatal(err)
		}
		if err := st.UpsertItems(ctx, "jira:cub", cub); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:cub", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:cub", "bob"); err != nil {
			t.Fatal(err)
		}
	})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 {
		t.Fatalf("exit = %d, want 1, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "CUB-1 has no parent") || !strings.Contains(stdout, "PRJ-1 has no parent") {
		t.Fatalf("stdout:\n%s", stdout)
	}
	if strings.Index(stdout, "CUB-1") > strings.Index(stdout, "PRJ-1") {
		t.Fatalf("order:\n%s", stdout)
	}
}

func TestDoctorUsage(t *testing.T) {
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	_, stderr, code := runCmd(t, "doctor", "extra", "--config", cfgPath)
	if code != ExitUsage || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "found 1") {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
}

func TestDoctorHerdrError(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
	})
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillFail("herdr down", 1).Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 2 {
		t.Fatalf("exit = %d, want 2, stderr = %s\nstdout:\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "fail  work-items") || !strings.Contains(stdout, "herdr down") {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestDoctorMissingConfigFailsWithoutAborting(t *testing.T) {
	doctorToolOK(t)
	missing := filepath.Join(t.TempDir(), "missing.toml")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runCmd(t, "doctor", "--offline", "--config", missing, "--db", dbPath)
	if code != 2 || stderr != "" {
		t.Fatalf("code=%d stderr=%q\nstdout:\n%s", code, stderr, stdout)
	}
	for _, want := range []string{
		"ok    binary-on-path",
		"warn  revision",
		"fail  config",
		"ok    store",
		"ok    external-commands",
		"warn  sources",
		"ok    skills",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
}

func doctorFixture(t *testing.T) (string, string) {
	t.Helper()
	useSources(t, map[string]*fake.Source{"acme": {SourceName: "jira:acme", Items: map[string]source.Item{}}})
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := "jira:acme"
	ready := []string{"agent-ready"}
	items := []source.Item{
		rankItem(src, "EPIC-1", "epic", "Epic", "To Do", "", "", 2, false, day1, nil),
		rankItem(src, "PRJ-C1", "c1", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready, "PRJ-C2"),
		rankItem(src, "PRJ-C2", "c2", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready, "PRJ-C1"),
		rankItem(src, "PRJ-SD", "sd", "Task", "Done", "EPIC-1", "", 2, true, day1, ready),
		rankItem(src, "PRJ-SB", "sb", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready, "PRJ-SD"),
		rankItem(src, "PRJ-UB", "ub", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready, "PRJ-NOPE"),
		rankItem(src, "PRJ-ORPH", "orph", "Task", "To Do", "", "", 2, false, day1, ready),
		rankItem(src, "PRJ-DROP", "drop", "Task", "In Progress", "EPIC-1", "", 2, false, day1, ready),
		rankItem(src, "PRJ-STAB", "stab", "Task", "Done", "EPIC-1", "", 2, true, day1, ready),
		rankItem(src, "PRJ-STALL", "stall", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready),
		rankItem(src, "PRJ-ALMOST", "almost", "Task", "To Do", "EPIC-1", "", 2, false, day1, nil),
		rankItem(src, "PRJ-OK", "ok", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready),
		rankItem(src, "PRJ-HELD", "held", "Task", "In Progress", "EPIC-1", "", 2, false, day1, ready),
		rankItem(src, "PRJ-YOUNG", "young", "Task", "To Do", "EPIC-1", "", 2, false, day1, ready),
	}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.ReplaceExternalBlockers(ctx, src, []store.ExternalBlocker{
			{ID: "PRJ-NOPE", Status: store.ExternalNotFound, FetchedAt: day1},
		}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
		old := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)
		if err := st.InsertClaim(ctx, store.Claim{
			Source: src, ID: "PRJ-STALL", ClaimedAt: old, UpdatedAt: old,
			State: store.Started, Machine: "laptop",
		}); err != nil {
			t.Fatal(err)
		}
		young := time.Now().UTC().Add(-time.Minute)
		if err := st.InsertClaim(ctx, store.Claim{
			Source: src, ID: "PRJ-YOUNG", ClaimedAt: young, UpdatedAt: young,
			State: store.Started, Machine: "laptop",
		}); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

// lookupFake syncs PRJ-A and PRJ-B, blocked by ids outside the source query.
// X-DONE and X-OPEN answer Get only; X-GONE does not exist.
func lookupFake() *fake.Source {
	src := "jira:acme"
	ready := []string{"agent-ready"}
	return &fake.Source{
		SourceName: src,
		Account:    "alice",
		Items: map[string]source.Item{
			"EPIC-1": rankItem(src, "EPIC-1", "epic", "Epic", "To Do", "", "", 2, false, day1, nil),
			"PRJ-A":  rankItem(src, "PRJ-A", "a", "Task", "To Do", "EPIC-1", "alice", 2, false, day1, ready, "X-DONE", "X-GONE", "X-OPEN"),
			"PRJ-B":  rankItem(src, "PRJ-B", "b", "Task", "To Do", "EPIC-1", "alice", 2, false, day1, ready, "X-OPEN"),
			"X-DONE": {Source: src, ID: "X-DONE", Type: "Task", State: "Done", Done: true, Updated: day1},
			"X-OPEN": {Source: src, ID: "X-OPEN", Type: "Task", State: "In Review", Assignee: "bob", Updated: day1},
		},
		Query: func(it source.Item) bool { return !strings.HasPrefix(it.ID, "X-") },
	}
}

func getCalls(src *fake.Source) []string {
	var out []string
	for _, call := range src.Calls {
		if call.Method == "Get" {
			out = append(out, strings.Join(call.Args, ","))
		}
	}
	return out
}

// syncThenDoctor syncs backend, checks the Get calls the sync made, and runs
// doctor, which must make none of its own.
func syncThenDoctor(t *testing.T, cfgPath string, backend *fake.Source, wantGets string, doctorArgs ...string) string {
	t.Helper()
	doctorToolOK(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	useSources(t, map[string]*fake.Source{"acme": backend})
	if _, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath); code != ExitOK {
		t.Fatalf("sync exit = %d, stderr = %s", code, stderr)
	}
	if got := strings.Join(getCalls(backend), " "); got != wantGets {
		t.Fatalf("sync Get calls = %q, want %q", got, wantGets)
	}
	backend.Calls = nil
	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	args := append([]string{"doctor", "--config", cfgPath, "--db", dbPath}, doctorArgs...)
	stdout, stderr, code := runCmd(t, args...)
	if code > 1 {
		t.Fatalf("doctor exit = %d, stderr = %s\nstdout:\n%s", code, stderr, stdout)
	}
	if got := getCalls(backend); len(got) != 0 {
		t.Fatalf("doctor fetched %q", got)
	}
	return stdout
}

func TestDoctorUsesSyncedOutsideBlockers(t *testing.T) {
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	stdout := syncThenDoctor(t, cfgPath, lookupFake(), "X-DONE X-GONE X-OPEN")
	for _, want := range []string{
		"warn  unknown-blocker",
		"PRJ-A is blocked by X-GONE, which was not found or is not visible",
		"PRJ-A is blocked by X-OPEN (In Review, assignee bob), which is not in the mirror",
		"PRJ-B is blocked by X-OPEN (In Review, assignee bob), which is not in the mirror",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
	if strings.Contains(stdout, "X-DONE") {
		t.Fatalf("done blocker reported:\n%s", stdout)
	}
}

func TestDoctorBlockerLookupBound(t *testing.T) {
	for _, key := range []string{"external_blocker_lookups", "doctor_blocker_lookups"} {
		t.Run(key, func(t *testing.T) {
			cfgPath := writeDoctorConfig(t, nextSource{name: "acme", extra: key + " = 1\n"})
			stdout := syncThenDoctor(t, cfgPath, lookupFake(), "X-DONE", "--json")
			var got toolcli.DoctorOutput
			if err := json.Unmarshal([]byte(stdout), &got); err != nil {
				t.Fatalf("json: %v\n%s", err, stdout)
			}
			var unchecked []string
			for _, found := range got.Checks {
				if found.Name != doctor.CheckUnknownBlocker {
					continue
				}
				if found.Status != toolcli.StatusOK || !strings.Contains(found.Detail, "was not checked: over external_blocker_lookups (1)") {
					t.Fatalf("finding = %#v", found)
				}
				unchecked = append(unchecked, found.Detail)
			}
			if len(unchecked) != 3 {
				t.Fatalf("unchecked count = %d (%v)", len(unchecked), unchecked)
			}
		})
	}
}

func TestDoctorBlockerFetchFails(t *testing.T) {
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	backend := lookupFake()
	backend.FailOn = map[string]error{"Get": errors.New("jira GET: 401 Unauthorized")}
	stdout := syncThenDoctor(t, cfgPath, backend, "X-DONE X-GONE X-OPEN")
	want := "PRJ-A is blocked by X-DONE, which is not in the mirror and was not checked: lookup failed: jira GET: 401 Unauthorized"
	if !strings.Contains(stdout, want) || strings.Contains(stdout, "warn  unknown-blocker") {
		t.Fatalf("stdout:\n%s", stdout)
	}
}

const doctorLaneConfig = `
[[lane]]
name = "linux"
checkout = %q
max_workers = 2

[[lane]]
name = "mac"
machine = "mac1"
checkout = "/tmp/mac"
max_workers = 2

[lanes]
default = ["linux", "mac"]
`

const doctorMacTabs = `{
  "id": "cli:tab:list",
  "result": {
    "type": "tab_list",
    "tabs": [
      {"tab_id": "m-1", "workspace_id": "mw1", "label": "PRJ-MAC", "agent_status": "working", "pane_count": 1}
    ]
  }
}`

// doctorLaneDB seeds in-progress leaf items claimed on the mac lane.
func doctorLaneDB(t *testing.T) string {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := "jira:acme"
	ready := []string{"agent-ready"}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		items := []source.Item{
			rankItem(src, "EPIC-1", "epic", "Epic", "In Progress", "", "", 2, false, day1, nil),
			rankItem(src, "PRJ-MAC", "mac", "Task", "In Progress", "EPIC-1", "", 2, false, day1, ready),
			rankItem(src, "PRJ-LEAF", "leaf", "Task", "In Progress", "EPIC-1", "", 2, false, day1, ready),
		}
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
		young := time.Now().UTC().Add(-time.Minute)
		if err := st.InsertClaim(ctx, store.Claim{
			Source: src, ID: "PRJ-MAC", ClaimedAt: young, UpdatedAt: young,
			State: store.Started, Machine: "mac1", Lane: "mac",
		}); err != nil {
			t.Fatal(err)
		}
	})
	return dbPath
}

func doctorLaneMock(t *testing.T) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Build()
	useExec(t, mock)
	return mock
}

func TestDoctorRemoteLaneTabs(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	appendConfig(t, cfgPath, fmt.Sprintf(doctorLaneConfig, t.TempDir()))
	dbPath := doctorLaneDB(t)
	mock := doctorLaneMock(t)
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "tab", "list").WillSucceed(doctorMacTabs, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 {
		t.Fatalf("exit = %d, want 1, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "warn  dropped-claim") || !strings.Contains(stdout, "PRJ-LEAF is In Progress with no tab and no open PR") {
		t.Fatalf("stdout:\n%s", stdout)
	}
	if strings.Contains(stdout, "PRJ-MAC") || strings.Contains(stdout, "EPIC-1 is") {
		t.Fatalf("remote tab or parent reported:\n%s", stdout)
	}
}

func TestDoctorUnreachableLane(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	appendConfig(t, cfgPath, fmt.Sprintf(doctorLaneConfig, t.TempDir()))
	dbPath := doctorLaneDB(t)
	mock := doctorLaneMock(t)
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "workspace", "list").WillFail("ssh: connect timed out", 255).Once().Build()
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 {
		t.Fatalf("exit = %d, want 1, stderr = %s", code, stderr)
	}
	for _, want := range []string{
		"PRJ-MAC is In Progress: lane mac unreachable; tab not checked",
		"PRJ-LEAF is In Progress with no tab and no open PR",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
}

func TestDoctorClosableParentFromFinishedHistory(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := "jira:acme"
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		// Seed parent + child, mark child done (recording finished history),
		// then prune child from mirror (as a JQL excluding Done items does).
		items := []source.Item{
			rankItem(src, "EPIC-1", "epic", "Epic", "To Do", "", "", 2, false, day1, nil),
			rankItem(src, "PRJ-1", "child", "Task", "To Do", "EPIC-1", "", 2, false, day1, []string{"agent-ready"}),
		}
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		items[1].State = "Done"
		items[1].Done = true
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if _, err := st.DeleteItemsNotIn(ctx, src, map[string]bool{"EPIC-1": true}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
	})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\nstdout:\n%s", code, stderr, stdout)
	}
	for _, want := range []string{
		"ok    closable-parent",
		"EPIC-1 has all children done (PRJ-1)",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("missing %q\n%s", want, stdout)
		}
	}
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code = runCmd(t, "doctor", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("json exit = %d, stderr = %q\nstdout:\n%s", code, stderr, stdout)
	}
	var got toolcli.DoctorOutput
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	var found bool
	for _, c := range got.Checks {
		if c.Name != doctor.CheckClosableParent {
			continue
		}
		found = true
		if c.Status != toolcli.StatusOK || c.Detail != "EPIC-1 has all children done (PRJ-1)" || c.Hint != "factory close EPIC-1 --apply" {
			t.Fatalf("check = %+v", c)
		}
	}
	if !found {
		t.Fatalf("missing closable-parent check in JSON:\n%s", stdout)
	}
}

func TestDoctorAndShowMergeMode(t *testing.T) {
	doctorToolOK(t)
	cfgPath := writeDoctorConfig(t, nextSource{name: "acme"})
	appendConfig(t, cfgPath, "\n[output]\nmode = \"merge\"\n")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := "jira:acme"
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		items := []source.Item{
			rankItem(src, "EPIC-1", "epic", "Epic", "To Do", "", "", 2, false, day1, nil),
			rankItem(src, "PRJ-1", "ship it", "Task", "In Progress", "EPIC-1", "", 2, false, day1, []string{"agent-ready"}),
		}
		if err := st.UpsertItems(ctx, src, items); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, src, day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, src, "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.InsertClaim(ctx, store.Claim{
			Source:     src,
			ID:         "PRJ-1",
			ClaimedAt:  time.Now().UTC().Add(-10 * time.Minute),
			UpdatedAt:  time.Now().UTC().Add(-10 * time.Minute),
			Machine:    "laptop",
			State:      store.Started,
			OutputMode: "merge",
		}); err != nil {
			t.Fatal(err)
		}
	})

	const gitLog = "abc1234567890\x1f0000000\x1f[PRJ-1] ship it\n\x1e\n"

	// doctor sees landed commit on origin/main and does not warn dropped-claim.
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed(gitLog, 0).Once().Build()
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("doctor exit = %d, stderr = %q\nstdout:\n%s", code, stderr, stdout)
	}
	if strings.Contains(stdout, "dropped-claim") {
		t.Fatalf("unexpected dropped-claim warning:\n%s", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	// show displays output: merge and the landed commit on origin/main.
	showMock := executor.NewMockExecutor()
	showMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	showMock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(emptyWorkspaces, 0).Once().Build()
	showMock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	showMock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	showMock.ExpectCommand("git").WillSucceed(gitLog, 0).Once().Build()
	useExec(t, showMock)

	stdout, stderr, code = runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("show exit = %d, stderr = %q\nstdout:\n%s", code, stderr, stdout)
	}
	for _, want := range []string{
		"  output: merge\n",
		"Commits:\n",
		"  abc1234  [PRJ-1] ship it\n",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("show missing %q:\n%s", want, stdout)
		}
	}
	if strings.Contains(stdout, "PRs:\n") {
		t.Fatalf("show printed PRs in merge mode:\n%s", stdout)
	}
	if err := showMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func appendConfig(t *testing.T, path, text string) {
	t.Helper()
	raw, err := os.ReadFile(path) //nolint:gosec // G304: path is the temp config this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, append(raw, text...), 0o600); err != nil {
		t.Fatal(err)
	}
}
