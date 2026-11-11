package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// scopeFake is a fake herdr whose only PROJ-123 tab, w3:t2 with idle
// agent w3:p2, is on machine `on` ("local" or "m2").
func scopeFake(t *testing.T, on string) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, m := range []string{"local", "m2"} {
		f.Write(m+".tabs", `{"result":{"tabs":[]}}`)
		f.Write(m+".agents", `{"result":{"agents":[]}}`)
	}
	agent := `{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle","cwd":"/src/widgets"}`
	f.Write(on+".tabs", `{"result":{"tabs":[{"tab_id":"w3:t2","workspace_id":"w3","label":"PROJ-123","agent_status":"idle","pane_count":1}]}}`)
	f.Write(on+".agents", `{"result":{"agents":[`+agent+`]}}`)
	f.Write(on+".prompt", `{"result":{"agent":`+agent+`}}`)
	return f
}

func TestHelpShowsTwoWatcherExample(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	if code := Execute(context.Background(), []string{"--help"}, &stdout, &stderr, executor.NewBasicExecutor()); code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	for _, want := range []string{"--tab-machine local  --untabbed include", "--tab-machine linux2 --untabbed exclude --skip team-cover"} {
		if !strings.Contains(stdout.String(), want) {
			t.Fatalf("help = %s, want %q", stdout.String(), want)
		}
	}
}

func TestScopeFlagsRejectUnknownValues(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	herdrtest.Install(t)
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "machines=m2")

	tests := []struct {
		args []string
		want string
	}{
		{[]string{"scan", "--tab-machine", "m3"}, "--tab-machine"},
		{[]string{"scan", "--skip", "nudge"}, "--skip"},
		{[]string{"scan", "--untabbed", "maybe"}, "--untabbed"},
		{[]string{"dispatch", "--tab-machine", "m3"}, "--tab-machine"},
		{[]string{"dispatch", "--skip", "nudge"}, "--skip"},
		{[]string{"tabs", "--orphans", "--tab-machine", "m3"}, "--tab-machine"},
	}
	for _, tc := range tests {
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), append(tc.args, "--config", cfgPath), &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitUsage || !strings.Contains(stderr.String(), tc.want) {
			t.Errorf("%v: exit = %d, stderr = %q; want usage error naming %s", tc.args, code, stderr.String(), tc.want)
		}
		if stdout.Len() != 0 {
			t.Errorf("%v: stdout = %q, want empty", tc.args, stdout.String())
		}
	}
}

func TestScanTabMachineFromConfig(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	scopeFake(t, "local")
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "machines=m2", "tab_machine=m2")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatal(err)
	}
	if len(doc.PRs) != 1 || doc.PRs[0].OutOfScope != "tab on local" {
		t.Fatalf("prs = %+v, want #123 out of scope (tab on local)", doc.PRs)
	}

	stdout.Reset()
	code = Execute(context.Background(), []string{"scan", "--config", cfgPath, "--tab-machine", "local"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if strings.Contains(stdout.String(), "out_of_scope") {
		t.Fatalf("--tab-machine local must override the config: %s", stdout.String())
	}
}

func TestDispatchTabMachineSkipsOtherMachines(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := scopeFake(t, "local")
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "machines=m2", "machine_timeout_ms=2000")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go", "--tab-machine", "m2"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedOutOfScope || got.Results[0].Detail != "tab on local" {
		t.Fatalf("results = %+v, want skipped_out_of_scope (tab on local)", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "agent prompt") {
		t.Fatalf("argv = %s, want no prompt", argv)
	}
}

func TestDispatchTabMachinePromptsItsMachine(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := scopeFake(t, "m2")
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "machines=m2", "machine_timeout_ms=2000")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--config", cfgPath, "--go", "--tab-machine", "m2", "--untabbed", "exclude", "--skip", "team-cover"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched || got.Results[0].Machine != "m2" {
		t.Fatalf("results = %+v, want dispatched on m2", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); !strings.Contains(argv, "--machine m2 agent prompt w3:p2 ") {
		t.Fatalf("argv = %s, want the prompt on m2", argv)
	}
}

func TestTabsTabMachineConsidersOnlyThatMachine(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := scopeFake(t, "m2")
	t.Setenv("JIRA_FAKE_FAIL", "")
	t.Setenv("JIRA_FAKE_STRICT", "")
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin=herdr\njira_bin="+jiraFakeBin(t)+"\nauthor=alice\nmachines=m2\nmachine_timeout_ms=2000\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"tabs", "--orphans", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	var orphans scan.OrphanDocument
	if err := json.Unmarshal(stdout.Bytes(), &orphans); err != nil {
		t.Fatal(err)
	}
	if len(orphans.OrphanTabs) != 0 {
		t.Fatalf("orphan_tabs = %+v, want none: without --tab-machine only local tabs count", orphans.OrphanTabs)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "--machine") {
		t.Fatalf("argv = %s, want no remote call", argv)
	}

	stdout.Reset()
	code = Execute(context.Background(), []string{"tabs", "--orphans", "--close-merged", "--go", "--tab-machine", "m2", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var closed scan.CloseDocument
	if err := json.Unmarshal(stdout.Bytes(), &closed); err != nil {
		t.Fatal(err)
	}
	if len(closed.Results) != 1 || closed.Results[0].Action != scan.ActionClosed || closed.Results[0].TabID != "w3:t2" || closed.Results[0].Machine != "m2" {
		t.Fatalf("results = %+v, want closed w3:t2 on m2", closed.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); !strings.Contains(argv, "--machine m2 tab close w3:t2") {
		t.Fatalf("argv = %s, want the close on m2", argv)
	}
}
