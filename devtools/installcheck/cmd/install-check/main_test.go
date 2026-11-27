package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/installcheck"
)

func TestMainHelperProcess(t *testing.T) {
	if os.Getenv("GO_WANT_HELPER_PROCESS") != "1" {
		return
	}
	code, _ := strconv.Atoi(os.Getenv("HELPER_EXIT_CODE"))
	os.Exit(code)
}

func testExitError(t *testing.T, code int) error {
	t.Helper()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(exe, "-test.run=^TestMainHelperProcess$") //nolint:gosec // G204: self-invocation for ExitError test
	cmd.Env = append(os.Environ(),
		"GO_WANT_HELPER_PROCESS=1",
		"HELPER_EXIT_CODE="+strconv.Itoa(code),
	)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run helper: %w", err)
	}
	return nil
}

func TestDryRunThenRealRun(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, "c.toml")
	state := filepath.Join(home, "state", "state.json")
	cfg := "notify_name = \"agent\"\nstate_file = \"" + state + "\"\n[[repo]]\nurl = \"https://github.com/o/r\"\nissues = [1]\n"
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}
	head := "aaaaaaaaaaaaaaaa"
	var sent []string
	rt := appRuntime{
		run: func(_ context.Context, name string, args ...string) ([]byte, error) {
			switch name {
			case "git":
				return []byte(head + "\trefs/heads/main\n"), nil
			case "gh":
				return []byte(`{"number":1,"title":"t","state":"CLOSED","stateReason":"COMPLETED","closedAt":"2026-11-22T12:40:00Z"}`), nil
			}
			sent = append(sent, args[2])
			return nil, testExitError(t, 3) // agentbus held exit 3 still counts as sent
		},
		now: func() time.Time { return time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC) },
	}
	env := []string{"HOME=" + home}
	call := func(args ...string) (int, string, string) {
		var out, errb bytes.Buffer
		code := run(append(args, "--config", cfgPath), &out, &errb, env, rt)
		return code, out.String(), errb.String()
	}

	if code, _, _ := call("run"); code != 0 { // baseline
		t.Fatalf("baseline code %d", code)
	}
	head = "bbbbbbbbbbbbbbbb"
	// Closed-before-baseline issue 1 is already reported, so nothing to send.
	if code, out, errs := call("--dry-run"); code != 0 || out != "" || len(sent) != 0 {
		t.Fatalf("code=%d out=%q err=%q sent=%v", code, out, errs, sent)
	}
	if code, out, _ := call("status"); code != 0 || !strings.Contains(out, "o/r main: aaaaaaa") {
		t.Fatalf("status code=%d out=%q", code, out)
	}
	if code, _, _ := call("bogus"); code != 2 {
		t.Fatalf("unknown command code %d", code)
	}
}

func TestStandaloneCommandAndGHBinAndJSON(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, "c.toml")
	cfg := `notify_command = ["/usr/local/bin/my-notify", "--tag", "ic"]
gh_bin = "/opt/bin/gh-throttle"
state_file = "~/state/state.json"

[[repo]]
url = "https://github.com/o/r"
issues = [1]
`
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}
	head := "aaaaaaaaaaaaaaaa"
	issueState := "OPEN"
	var ghUsed []string
	var notifiedArgv, notifiedEnv []string
	rt := appRuntime{
		run: func(_ context.Context, name string, _ ...string) ([]byte, error) {
			if name == "git" {
				return []byte(head + "\trefs/heads/main\n"), nil
			}
			ghUsed = append(ghUsed, name)
			return []byte(`{"number":1,"title":"ship it","state":"` + issueState + `","stateReason":"COMPLETED","closedAt":"2026-11-22T12:40:00Z"}`), nil
		},
		notify: func(_ context.Context, argv, env []string) error {
			notifiedArgv = argv
			notifiedEnv = env
			return nil
		},
		now: func() time.Time { return time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC) },
	}
	env := []string{"HOME=" + home, "GH_BIN=/ignored/when/config/set"}
	call := func(args ...string) (int, string, string) {
		var out, errb bytes.Buffer
		code := run(append(args, "--config", cfgPath), &out, &errb, env, rt)
		return code, out.String(), errb.String()
	}

	// Baseline.
	if code, _, errb := call("run"); code != 0 {
		t.Fatalf("baseline code=%d stderr=%s", code, errb)
	}
	if !slices.Equal(ghUsed, []string{"/opt/bin/gh-throttle"}) {
		t.Fatalf("ghUsed = %v, want [/opt/bin/gh-throttle]", ghUsed)
	}

	// Move head and close issue -> dry-run --json.
	head = "bbbbbbbbbbbbbbbb"
	issueState = "CLOSED"
	code, out, errb := call("run", "--dry-run", "--json")
	if code != 0 || len(notifiedArgv) != 0 {
		t.Fatalf("dry-run json code=%d err=%s notified=%v", code, errb, notifiedArgv)
	}
	var dryOut runJSONOutput
	if err := json.Unmarshal([]byte(out), &dryOut); err != nil {
		t.Fatalf("decode dry-run json: %v\n%s", err, out)
	}
	if !dryOut.DryRun || dryOut.Sent || dryOut.NotifyMode != "command" || dryOut.GHBin != "/opt/bin/gh-throttle" || !strings.Contains(dryOut.Message, "o/r#1 ship it") {
		t.Fatalf("unexpected dryOut: %+v", dryOut)
	}

	// Real run --json executes notify_command without agentbus.
	code, out, errb = call("run", "--json")
	if code != 0 {
		t.Fatalf("run json code=%d err=%s", code, errb)
	}
	var runOut runJSONOutput
	if err := json.Unmarshal([]byte(out), &runOut); err != nil {
		t.Fatalf("decode run json: %v\n%s", err, out)
	}
	if runOut.DryRun || !runOut.Sent || len(notifiedArgv) != 4 || notifiedArgv[0] != "/usr/local/bin/my-notify" {
		t.Fatalf("unexpected runOut=%+v argv=%v", runOut, notifiedArgv)
	}
	if !slices.Contains(notifiedEnv, "INSTALL_CHECK_KIND=landed") {
		t.Fatalf("missing INSTALL_CHECK_KIND in env: %v", notifiedEnv)
	}

	// status --json.
	code, out, errb = call("status", "--json")
	if code != 0 {
		t.Fatalf("status json code=%d err=%s", code, errb)
	}
	var rep installcheck.StatusReport
	if err := json.Unmarshal([]byte(out), &rep); err != nil {
		t.Fatalf("decode status json: %v\n%s", err, out)
	}
	if rep.NotifyMode != "command" || rep.GHBin != "/opt/bin/gh-throttle" || len(rep.Repos) != 1 || rep.Repos[0].Head != "bbbbbbbbbbbbbbbb" {
		t.Fatalf("unexpected status report: %+v", rep)
	}
}

func TestStdoutModeAndCLIValidation(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, "c.toml")
	cfg := `notify_mode = "stdout"
[[repo]]
url = "https://github.com/o/r"
issues = [1]
`
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o600); err != nil {
		t.Fatal(err)
	}
	head := "aaaaaaaaaaaaaaaa"
	issueState := "OPEN"
	var ghBins []string
	rt := appRuntime{
		run: func(_ context.Context, name string, _ ...string) ([]byte, error) {
			if name == "git" {
				return []byte(head + "\trefs/heads/main\n"), nil
			}
			ghBins = append(ghBins, name)
			return []byte(`{"number":1,"title":"done","state":"` + issueState + `","stateReason":"COMPLETED"}`), nil
		},
		now: func() time.Time { return time.Date(2026, 11, 22, 13, 0, 0, 0, time.UTC) },
	}
	env := []string{"HOME=" + home, "GH_BIN=/bin/gh-throttle"}
	call := func(args ...string) (int, string, string) {
		var out, errb bytes.Buffer
		code := run(args, &out, &errb, env, rt)
		return code, out.String(), errb.String()
	}

	if code, out, _ := call("help"); code != 0 || !strings.Contains(out, "usage: install-check") {
		t.Fatalf("help code=%d out=%q", code, out)
	}
	if code, out, _ := call("--help"); code != 0 || !strings.Contains(out, "usage: install-check") {
		t.Fatalf("--help code=%d out=%q", code, out)
	}
	if code, _, _ := call("run", "--config", cfgPath, "extra"); code != 2 {
		t.Fatalf("run positional arg code=%d, want 2", code)
	}
	if code, _, _ := call("status", "--config", cfgPath, "extra"); code != 2 {
		t.Fatalf("status positional arg code=%d, want 2", code)
	}
	// Empty GH_BIN env fails with exit code 2.
	var emptyOut, emptyErr bytes.Buffer
	if code := run([]string{"run", "--config", cfgPath}, &emptyOut, &emptyErr, []string{"HOME=" + home, "GH_BIN="}, rt); code != 2 {
		t.Fatalf("empty GH_BIN code=%d, want 2 (stderr=%s)", code, emptyErr.String())
	}

	// Baseline then land -> stdout receives the message directly.
	if code, _, errb := call("run", "--config", cfgPath); code != 0 {
		t.Fatalf("baseline code=%d err=%s", code, errb)
	}
	if !slices.Equal(ghBins, []string{"/bin/gh-throttle"}) {
		t.Fatalf("GH_BIN not used: %v", ghBins)
	}
	head = "bbbbbbbbbbbbbbbb"
	issueState = "CLOSED"
	code, out, errb := call("run", "--config", cfgPath)
	if code != 0 || !strings.Contains(out, "o/r#1 done") || errb != "" {
		t.Fatalf("stdout mode code=%d out=%q err=%q", code, out, errb)
	}

	// Stdout mode with --json still emits valid JSON only.
	head = "cccccccccccccccc"
	// Re-verify with fresh state via dry-run=false on new repo? Already reported #1, so test quiet json.
	code, out, _ = call("run", "--config", cfgPath, "--json")
	if code != 0 {
		t.Fatalf("run --json code=%d", code)
	}
	var parsed runJSONOutput
	if err := json.Unmarshal([]byte(out), &parsed); err != nil {
		t.Fatalf("invalid json: %v\n%s", err, out)
	}
}
