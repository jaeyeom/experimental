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

// TestScanThenDispatchOnAnotherMachine scans with machines=m2, where the
// only PROJ-123 tab lives, then sends the scan to dispatch --go.
func TestScanThenDispatchOnAnotherMachine(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := herdrtest.Install(t)
	f.Write("local.tabs", `{"result":{"tabs":[]}}`)
	f.Write("local.agents", `{"result":{"agents":[]}}`)
	f.Write("m2.tabs", `{"result":{"tabs":[{"tab_id":"w3:t2","workspace_id":"w3","label":"PROJ-123","agent_status":"idle","pane_count":1}]}}`)
	agent := `{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle"}`
	f.Write("m2.agents", `{"result":{"agents":[`+agent+`]}}`)
	f.Write("m2.prompt", `{"result":{"agent":`+agent+`}}`)
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "machines=m2", "machine_timeout_ms=2000")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"scan", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("scan exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var doc scan.Document
	if err := json.Unmarshal(stdout.Bytes(), &doc); err != nil {
		t.Fatalf("scan json: %v\n%s", err, stdout.String())
	}
	if len(doc.PRs) == 0 || doc.PRs[0].Tab == nil || doc.PRs[0].Tab.Machine != "m2" {
		t.Fatalf("scan prs = %+v, want PROJ-123 on m2", doc.PRs)
	}
	if !strings.Contains(stdout.String(), `"machine": "m2"`) {
		t.Fatalf("scan JSON = %s, want tab machine m2", stdout.String())
	}
	f.Argv()

	restore := swapStdin(t, stdout.String())
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go", "--pr", "acme/widgets#123"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("dispatch exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched || got.Results[0].Machine != "m2" {
		t.Fatalf("dispatch results = %+v, want dispatched on m2", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); !strings.Contains(argv, "--machine m2 agent prompt w3:p2 ") {
		t.Fatalf("argv = %s, want the prompt on m2", argv)
	}
}

// TestGateWatchesOtherMachines runs `prsync gate` with machines=m2: a
// working agent there holds the gate, and so does m2 being down.
func TestGateWatchesOtherMachines(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := herdrtest.Install(t)
	f.Write("local.agents", `{"result":{"agents":[]}}`)
	f.Write("m2.agents", `{"result":{"agents":[{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"working"}]}}`)
	cfgPath := writeScanConfig(t, "gh_bin="+ghBin+"\nherdr_bin=herdr\nauthor=alice\nrepos=acme/widgets\nmachines=m2\nmachine_timeout_ms=2000\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	got := decodeGate(t, stdout.Bytes())
	if code != ExitUnsafe || len(got.Busy) != 1 || got.Busy[0].Machine != "m2" || got.Busy[0].Signal != dispatch.SignalWorking {
		t.Fatalf("exit = %d, gate = %+v, stderr=%q; want m2 working to hold", code, got, stderr.String())
	}

	f.Write("m2.down", "")
	stdout.Reset()
	code = Execute(context.Background(), []string{"gate", "--config", cfgPath}, &stdout, &stderr, executor.NewBasicExecutor())
	got = decodeGate(t, stdout.Bytes())
	if code != ExitUnsafe || len(got.Busy) != 1 || got.Busy[0].Signal != dispatch.SignalMachineUnreachable || got.Busy[0].Detail != "host unreachable" {
		t.Fatalf("exit = %d, gate = %+v; want m2 unreachable to hold", code, got)
	}
}

// TestGateTabMachineGatesOnThatMachine runs `prsync gate --tab-machine
// mac`: it reads mac's agents against mac's checkout, never the local
// session, and needs machine_checkouts for mac.
func TestGateTabMachineGatesOnThatMachine(t *testing.T) {
	ghBin, _ := fixtureBins(t)
	f := herdrtest.Install(t)
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w1:p1","tab_id":"w1:t1","agent":"codex","agent_status":"working"}]}}`)
	f.Write("mac.agents", `{"result":{"agents":[{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"working","cwd":"/Users/me/src/repo/pkg"}]}}`)
	base := "gh_bin=" + ghBin + "\nherdr_bin=herdr\nauthor=alice\nrepos=acme/widgets\nmachines=mac\nmachine_timeout_ms=2000\nconcurrency_cwd=checkout\n"
	run := func(cfgPath string, args ...string) (int, string, string) {
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), append([]string{"gate", "--config", cfgPath}, args...), &stdout, &stderr, executor.NewBasicExecutor())
		return code, stdout.String(), stderr.String()
	}

	code, _, stderr := run(writeScanConfig(t, base), "--tab-machine", "mac")
	if code != ExitUsage || !strings.Contains(stderr, "machine_checkouts") || !strings.Contains(stderr, "mac") {
		t.Fatalf("exit = %d, stderr = %q; want a machine_checkouts error for mac", code, stderr)
	}

	cfgPath := writeScanConfig(t, base+"machine_checkouts=mac=/Users/me/src/repo\n")
	code, stdout, stderr := run(cfgPath, "--tab-machine", "mac")
	got := decodeGate(t, []byte(stdout))
	if code != ExitUnsafe || len(got.Busy) != 1 || got.Busy[0].Machine != "mac" || got.Busy[0].Signal != dispatch.SignalWorking {
		t.Fatalf("exit = %d, gate = %+v, stderr=%q; want only mac's working tab", code, got, stderr)
	}
	if got.Machine != "mac" || got.LeaseKey != "mac:/Users/me/src/repo" {
		t.Fatalf("gate = %s, want machine and lease_key", stdout)
	}
	if !strings.Contains(stdout, `"lease_key": "mac:/Users/me/src/repo"`) || !strings.Contains(stdout, "checkout not inspected on mac") {
		t.Fatalf("gate JSON = %s, want lease_key and the note", stdout)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "\nagent list") || strings.HasPrefix(argv, "agent list") {
		t.Fatalf("argv = %s, want no local agent list", argv)
	}

	f.Write("mac.down", "")
	code, stdout, _ = run(cfgPath, "--tab-machine", "mac")
	if got := decodeGate(t, []byte(stdout)); code != ExitUnsafe || got.Reason != "machine mac unreachable" {
		t.Fatalf("exit = %d, gate = %s; want unsafe machine mac unreachable", code, stdout)
	}

	if code, _, stderr := run(cfgPath, "--tab-machine", "m9"); code != ExitUsage || !strings.Contains(stderr, "--tab-machine") {
		t.Fatalf("exit = %d, stderr = %q; want a --tab-machine usage error", code, stderr)
	}
}
