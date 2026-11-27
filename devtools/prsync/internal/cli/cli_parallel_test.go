package cli

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

func setupGitRepo(t *testing.T, dirty bool) string {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not installed")
	}
	dir := t.TempDir()
	git := func(args ...string) {
		t.Helper()
		cmd := exec.Command("git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: test git helper
		cmd.Env = append(os.Environ(), "GIT_CONFIG_GLOBAL="+os.DevNull, "GIT_CONFIG_NOSYSTEM=1")
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	git("init", "-q")
	git("remote", "add", "origin", "git@github.com:acme/widgets.git")
	writeFile(t, filepath.Join(dir, "a.txt"), "a\n")
	git("add", "a.txt")
	git("-c", "user.name=t", "-c", "user.email=t@example.com", "-c", "commit.gpgsign=false", "commit", "-q", "-m", "init")
	if dirty {
		writeFile(t, filepath.Join(dir, "a.txt"), "dirty\n")
	}
	t.Chdir(dir)
	return dir
}

func installFakeAgentbus(t *testing.T, guarded bool, statusJSONPath string) string {
	t.Helper()
	binDir := t.TempDir()
	guardedVal := "false"
	if guarded {
		guardedVal = "true"
	}
	script := fmt.Sprintf(`#!/bin/sh
set -eu
case "${1-} ${2-} ${3-}" in
"lease status --guarded")
	printf '{"guarded": %s}\n'
	exit 0
	;;
"lease status --json")
	cat %q
	exit 0
	;;
"lease status --all")
	printf '[]\n'
	exit 0
	;;
*)
	printf 'fake agentbus unexpected: %%s\n' "$*" >&2
	exit 1
	;;
esac
`, guardedVal, statusJSONPath)
	writeExec(t, filepath.Join(binDir, "agentbus"), script)
	t.Setenv("PATH", binDir+string(os.PathListSeparator)+os.Getenv("PATH"))
	t.Setenv("PRSYNC_FAKE_AGENTBUS", "1")
	return binDir
}

func installConcurrentFakeHerdr(t *testing.T, stateDir string) string {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "herdr")
	script := fmt.Sprintf(`#!/bin/sh
set -eu
STATE=%q
mkdir -p "$STATE"
if [ "${1-}" = "--version" ]; then
	printf 'herdr 0.8.1\n'
	exit 0
fi
case "${1-} ${2-}" in
"agent list")
	printf '{"result":{"agents":['
	sep=""
	for i in 1 2 3 4 5; do
		printf '%%s{"pane_id":"w2:p%%d","tab_id":"w2:t%%d","agent":"codex","agent_status":"idle"}' "$sep" "$i" "$i"
		sep=","
	done
	printf ']}}\n'
	exit 0
	;;
"agent prompt")
	pane=$3
	printf '%%s\n' "$pane" >> "$STATE/started"
	while [ ! -f "$STATE/release-$pane" ]; do
		sleep 0.01
	done
	printf '{"result":{"agent":{"pane_id":"%%s","agent_status":"idle"}}}\n' "$pane"
	exit 0
	;;
*)
	printf 'fake herdr unexpected: %%s\n' "$*" >&2
	exit 1
	;;
esac
`, stateDir)
	writeExec(t, bin, script)
	return bin
}

func parallelScanDoc(n int) scan.Document {
	prs := make([]scan.PR, 0, n)
	for i := 1; i <= n; i++ {
		id := fmt.Sprintf("PROJ-%d", i)
		pane := fmt.Sprintf("w2:p%d", i)
		prs = append(prs, scan.PR{
			Repo:        "acme/widgets",
			Number:      i,
			Title:       fmt.Sprintf("[%s] PR %d", id, i),
			URL:         fmt.Sprintf("https://github.com/acme/widgets/pull/%d", i),
			Head:        fmt.Sprintf("fix-%d", i),
			HeadSHA:     fmt.Sprintf("sha%d", i),
			Base:        "main",
			State:       "open",
			Identifier:  &id,
			Unaddressed: true,
			BlockingComments: []scan.Comment{{
				ThreadID:  fmt.Sprintf("T%d", i),
				CommentID: fmt.Sprintf("C%d", i),
				Author:    "rev",
				Path:      "a.go",
				URL:       fmt.Sprintf("https://github.com/acme/widgets/pull/%d#r1", i),
				Body:      "fix",
			}},
			Tab: &scan.Tab{
				TabID: fmt.Sprintf("w2:t%d", i), PaneID: &pane,
				WorkspaceID: "w2", Label: id, AgentStatus: "idle",
			},
		})
	}
	return scan.Document{
		GeneratedAt: "2026-01-01T09:00:00Z",
		Author:      "alice",
		Repos:       []string{"acme/widgets"},
		PRs:         prs,
	}
}

func writeExec(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o700); err != nil {
		t.Fatal(err)
	}
}

func writeAtomicFile(t *testing.T, path, body string) {
	t.Helper()
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Rename(tmp, path); err != nil {
		t.Fatal(err)
	}
}

func TestDispatchParallel3With5PRsFakeHerdrAndAgentbus(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, _ := fixtureBins(t)
	checkout := setupGitRepo(t, false)
	leaseStatus := filepath.Join(t.TempDir(), "lease.json")
	writeFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"free","holder":null}`+"\n", checkout))
	installFakeAgentbus(t, true, leaseStatus)

	herdrState := t.TempDir()
	herdrBin := installConcurrentFakeHerdr(t, herdrState)
	statePath := filepath.Join(t.TempDir(), "state.json")
	ledgerPath := filepath.Join(t.TempDir(), "ledger.jsonl")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath,
		"gate_lease=required",
		"dispatch_ledger_file="+ledgerPath,
	)

	raw := mustScanJSON(t, parallelScanDoc(5))
	restore := swapStdin(t, string(raw))
	defer restore()

	type cliOut struct {
		code   int
		stdout string
		stderr string
	}
	done := make(chan cliOut, 1)
	go func() {
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{
			"dispatch", "--stdin", "--config", cfgPath, "--go", "--parallel", "3",
		}, &stdout, &stderr, executor.NewBasicExecutor())
		done <- cliOut{code: code, stdout: stdout.String(), stderr: stderr.String()}
	}()

	startedFile := filepath.Join(herdrState, "started")
	deadline := time.Now().Add(5 * time.Second)
	var firstThree []string
	for time.Now().Before(deadline) {
		data, err := os.ReadFile(startedFile) //nolint:gosec // test file
		if err == nil {
			lines := strings.Fields(string(data))
			if len(lines) >= 3 {
				firstThree = lines
				break
			}
		}
		time.Sleep(10 * time.Millisecond)
	}
	if len(firstThree) != 3 {
		t.Fatalf("expected exactly 3 started initially, got %v", firstThree)
	}
	time.Sleep(40 * time.Millisecond)
	data, _ := os.ReadFile(startedFile) //nolint:gosec // test file
	if lines := strings.Fields(string(data)); len(lines) != 3 {
		t.Fatalf("4th PR started before any of first 3 finished: %v", lines)
	}

	// Dirty the repo AND have dispatched firstThree[1] hold the lease before releasing the first pane,
	// verifying that a dirty checkout held by a dispatched pane does not block PR #4!
	writeFile(t, filepath.Join(checkout, "a.txt"), "dirty while dispatched holds lease\n")
	writeAtomicFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"held","holder":{"handle":"herdr:local/default/%s"}}`+"\n", checkout, firstThree[1]))

	// Release first pane; verify 4th PR begins immediately.
	writeFile(t, filepath.Join(herdrState, "release-"+firstThree[0]), "ok\n")
	deadline = time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		data, _ := os.ReadFile(startedFile) //nolint:gosec // test file
		if len(strings.Fields(string(data))) >= 4 {
			break
		}
		time.Sleep(10 * time.Millisecond)
	}
	data, _ = os.ReadFile(startedFile) //nolint:gosec // test file
	if lines := strings.Fields(string(data)); len(lines) != 4 {
		t.Fatalf("after releasing 1st, started = %v, want 4", lines)
	}

	// Release all remaining panes.
	for i := 1; i <= 5; i++ {
		writeFile(t, filepath.Join(herdrState, "release-w2:p"+strconv.Itoa(i)), "ok\n")
	}

	out := <-done
	if out.code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q stdout=%q", out.code, out.stderr, out.stdout)
	}
	doc := decodeDispatch(t, []byte(out.stdout))
	if len(doc.Results) != 5 {
		t.Fatalf("results len = %d, want 5", len(doc.Results))
	}
	for i, r := range doc.Results {
		if r.Action != dispatch.ActionDispatched || r.Number != i+1 {
			t.Fatalf("result[%d] = %+v, want dispatched #%d", i, r, i+1)
		}
		if !strings.Contains(r.RenderedPrompt, "agentbus lease claim") || !strings.Contains(r.RenderedPrompt, "agentbus lease release") {
			t.Fatalf("result[%d] prompt missing lease instruction: %q", i, r.RenderedPrompt)
		}
	}
	if doc.Summary == nil || doc.Summary.MaxInFlight != 3 || doc.Summary.Finished != 5 || doc.Summary.TimedOut != 0 || doc.Summary.Blocked != 0 {
		t.Fatalf("summary = %+v, want max_in_flight=3 finished=5", doc.Summary)
	}
	entries, err := (&ledger.Store{Path: ledgerPath}).Load()
	if err != nil || len(entries) != 5 {
		t.Fatalf("ledger entries = %d (%v), want 5", len(entries), err)
	}
}

func TestDispatchParallelRefusesWithoutLeaseAndAllowsDirtyUnderDispatchedLease(t *testing.T) {
	t.Run("parallel without gate_lease=required fails", func(t *testing.T) {
		ghBin, herdrBin := fixtureBins(t)
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"))
		raw := mustScanJSON(t, parallelScanDoc(1))
		restore := swapStdin(t, string(raw))
		defer restore()

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go", "--parallel", "3"}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitUsage {
			t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
		}
		if !strings.Contains(stderr.String(), "gate_lease") {
			t.Fatalf("stderr = %q", stderr.String())
		}
	})

	t.Run("parallel with unguarded checkout refuses", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		ghBin, herdrBin := fixtureBins(t)
		checkout := setupGitRepo(t, false)
		leaseStatus := filepath.Join(t.TempDir(), "lease.json")
		writeFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"free","holder":null}`+"\n", checkout))
		installFakeAgentbus(t, false, leaseStatus)

		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"), "gate_lease=required")
		raw := mustScanJSON(t, parallelScanDoc(1))
		restore := swapStdin(t, string(raw))
		defer restore()

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go", "--parallel", "3"}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitPrecondition {
			t.Fatalf("exit = %d, want %d, stderr=%q stdout=%q", code, ExitPrecondition, stderr.String(), stdout.String())
		}
		if !strings.Contains(stderr.String(), "not guarded") {
			t.Fatalf("stderr = %q", stderr.String())
		}
	})

	t.Run("dirty checkout with dispatched lease holder does not block and free dirty blocks", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		ghBin, _ := fixtureBins(t)
		checkout := setupGitRepo(t, true)
		leaseStatus := filepath.Join(t.TempDir(), "lease.json")
		installFakeAgentbus(t, true, leaseStatus)

		herdrState := t.TempDir()
		herdrBin := installConcurrentFakeHerdr(t, herdrState)
		writeFile(t, filepath.Join(herdrState, "release-w2:p1"), "ok\n")
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state.json"), "gate_lease=required")

		// 1. Free + dirty -> refuses immediately with ExitUnsafe.
		writeFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"free","holder":null}`+"\n", checkout))
		raw := mustScanJSON(t, parallelScanDoc(1))
		restore := swapStdin(t, string(raw))
		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go", "--parallel", "2"}, &stdout, &stderr, executor.NewBasicExecutor())
		restore()
		if code != ExitUnsafe {
			t.Fatalf("free dirty exit = %d, want %d, stderr=%q stdout=%q", code, ExitUnsafe, stderr.String(), stdout.String())
		}
		doc := decodeDispatch(t, stdout.Bytes())
		if len(doc.Results) != 1 || doc.Results[0].Action != dispatch.ActionGateUnsafe {
			t.Fatalf("free dirty results = %+v, want gate_unsafe", doc.Results)
		}

		// 2. Non-dispatched lease holder -> blocks and times out gate.
		writeFile(t, leaseStatus, fmt.Sprintf(`{"checkout":"local:%s","state":"held","holder":{"handle":"herdr:local/default/w9:p9","name":"pm/other"}}`+"\n", checkout))
		cfgShort := writeLiveConfig(t, ghBin, herdrBin, filepath.Join(t.TempDir(), "state2.json"), "gate_lease=required", "gate_timeout_ms=20")
		restore = swapStdin(t, string(raw))
		stdout.Reset()
		stderr.Reset()
		code = Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgShort, "--go", "--parallel", "2"}, &stdout, &stderr, executor.NewBasicExecutor())
		restore()
		if code != ExitGateTimeout {
			t.Fatalf("external lease holder exit = %d, want %d, stderr=%q stdout=%q", code, ExitGateTimeout, stderr.String(), stdout.String())
		}
	})

	t.Run("parallel 1 output identical and omits summary", func(t *testing.T) {
		ghBin, herdrBin := fixtureBins(t)
		statePath := filepath.Join(t.TempDir(), "state.json")
		cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
		raw := mustScanJSON(t, stdinEligibleDoc())
		restore := swapStdin(t, string(raw))
		defer restore()

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go", "--parallel", "1"}, &stdout, &stderr, executor.NewBasicExecutor())
		if code != ExitOK {
			t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
		}
		if strings.Contains(stdout.String(), `"summary"`) {
			t.Fatalf("parallel=1 included summary field:\n%s", stdout.String())
		}
		doc := decodeDispatch(t, stdout.Bytes())
		if strings.Contains(doc.Results[0].RenderedPrompt, "agentbus lease claim") {
			t.Fatalf("parallel=1 included lease instruction: %q", doc.Results[0].RenderedPrompt)
		}
	})
}
