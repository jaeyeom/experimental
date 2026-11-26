package cli

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

func TestMain(m *testing.M) {
	// The default hold file is under the home directory. Keep CLI tests off
	// the real one so a local hold cannot change gate results.
	dir, err := os.MkdirTemp("", "factory-home-")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	prev, had := os.LookupEnv("HOME")
	if err := os.Setenv("HOME", dir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	// A GH_BIN in the developer's shell would change which binary the
	// mocks see. Tests that need it set it with t.Setenv.
	// Likewise a FACTORY_ITEM_SOURCE from running inside a factory worker.
	if err := os.Unsetenv("FACTORY_ITEM_SOURCE"); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := os.Unsetenv("NO_COLOR"); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := os.Unsetenv("GH_BIN"); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	// Starts in these tests run against a mock that does not expect
	// agentbus wake. Wake tests set newWaker themselves.
	newWaker = func(executor.Executor) func(context.Context, string, string) (bool, error) { return nil }
	startSleep = func(context.Context, time.Duration) error { return nil }
	// List commands narrow to the source whose cwd holds the current
	// directory. Run them from a directory inside no test source's cwd;
	// scope tests set processEnv themselves.
	processEnv = func() config.Env {
		env := config.OSEnv()
		env.Getwd = func() (string, error) { return dir, nil }
		return env
	}
	code := m.Run()
	if had {
		_ = os.Setenv("HOME", prev)
	} else {
		_ = os.Unsetenv("HOME")
	}
	_ = os.RemoveAll(dir)
	os.Exit(code)
}

func TestHoldAndReleaseHold(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	cfgPath, _ := writeStartConfig(t, "", "")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, fmt.Errorf("unexpected command"))
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "hold", "--config", cfgPath, "--owner", "ada", "--reason", "long job")
	path := filepath.Join(home, ".local", "state", "factory", "hold")
	if code != ExitOK || stderr != "" || stdout != path+"\n" {
		t.Fatalf("hold exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	body, err := os.ReadFile(path) //nolint:gosec // G304: path is the temp hold file this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if string(body) != "owner: ada\nlong job\n" {
		t.Fatalf("body = %q", body)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("hold executions = %d, want 0", len(mock.Executions()))
	}

	stdout, stderr, code = runCmd(t, "release-hold", "--config", cfgPath)
	if code != ExitOK || stderr != "" || stdout != path+"\n" {
		t.Fatalf("release exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("stat after release = %v, want not exist", err)
	}
	stdout, stderr, code = runCmd(t, "release-hold", "--config", cfgPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "expected hold file "+path+", found none") {
		t.Fatalf("second release exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
}

func TestHoldConfiguredPath(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	path := filepath.Join(home, "holds", "job")
	cfgPath, _ := writeStartConfig(t, "[gate]\nhold_files = [\"~/holds/job\"]\n", "")
	stdout, stderr, code := runCmd(t, "hold", "--config", cfgPath, "--reason", "fixing")
	if code != ExitOK || stderr != "" || stdout != path+"\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	body, err := os.ReadFile(path) //nolint:gosec // G304: path is the temp hold file this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if string(body) != "fixing\n" {
		t.Fatalf("body = %q", body)
	}
}

func TestHoldRejectsManyFiles(t *testing.T) {
	a := filepath.Join(t.TempDir(), "a")
	b := filepath.Join(t.TempDir(), "b")
	cfgPath, _ := writeStartConfig(t, fmt.Sprintf("[gate]\nhold_files = [%q, %q]\n", a, b), "")
	stdout, stderr, code := runCmd(t, "hold", "--config", cfgPath)
	if code != ExitError || stdout != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "expected one path in gate.hold_files, found 2") || !strings.Contains(stderr, a) || !strings.Contains(stderr, b) {
		t.Fatalf("stderr = %q", stderr)
	}
	if _, err := os.Stat(a); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("stat a = %v", err)
	}
}

func TestHoldUsage(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "args", args: []string{"hold", "extra"}, want: "hold: expected no arguments, found 1"},
		{name: "empty owner", args: []string{"hold", "--owner", ""}, want: "hold: expected a non-empty owner, found empty"},
		{name: "blank reason", args: []string{"hold", "--reason", "  "}, want: "hold: expected a non-empty reason, found empty"},
		{name: "owner line", args: []string{"hold", "--reason", "fixing\nowner: ada"}, want: `hold: expected a reason, found an owner line "owner: ada"`},
		{name: "release args", args: []string{"release-hold", "extra"}, want: "release-hold: expected no arguments, found 1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stdout, stderr, code := runCmd(t, tc.args...)
			if code != ExitUsage || stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, tc.want) {
				t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
			}
		})
	}
}

func TestGateHoldFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "hold")
	if err := os.WriteFile(path, []byte("owner: ada\nfixing the gate\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfgPath, _ := writeGateConfig(t, fmt.Sprintf("[gate]\nhold_files = [%q]\n", path), gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "hold "+path+": owner ada: fixing the gate\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	if err := os.Remove(path); err != nil {
		t.Fatal(err)
	}
	mock = gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code = runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("after release exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateStaleHold(t *testing.T) {
	path := filepath.Join(t.TempDir(), "hold")
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	old := time.Now().Add(-2 * time.Hour)
	if err := os.Chtimes(path, old, old); err != nil {
		t.Fatal(err)
	}
	cfgPath, _ := writeGateConfig(t, fmt.Sprintf("[gate]\nhold_files = [%q]\nhold_ttl = \"1s\"\n", path), gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.HasPrefix(stdout, "hold "+path+" is stale (age ") || !strings.Contains(stdout, "is stale") {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateHoldTab(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "[gate]\nhold_tabs = [\"coord\"]\n", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "coord", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "hold tab coord (w1:t1) is idle\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	mock = gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "coord", status: "done"}), "", "", nil)
	stdout, stderr, code = runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("done exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartProceedsAfterReleaseHold(t *testing.T) {
	path := filepath.Join(t.TempDir(), "hold")
	cfgPath, cwd := writeStartConfig(t, fmt.Sprintf("[gate]\nhold_files = [%q]\n", path), "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})

	stdout, stderr, code := runCmd(t, "hold", "--config", cfgPath, "--owner", "ada", "--reason", "long job")
	if code != ExitOK || stderr != "" || stdout != path+"\n" {
		t.Fatalf("hold exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code = runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused || !strings.HasSuffix(stdout, "factory start PRJ-1: refused, nothing changed: start PRJ-1: gate: hold "+path+": owner ada: long job\n") || !strings.Contains(stderr, "gate: hold "+path+": owner ada: long job") {
		t.Fatalf("start exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)

	stdout, stderr, code = runCmd(t, "release-hold", "--config", cfgPath)
	if code != ExitOK || stderr != "" || stdout != path+"\n" {
		t.Fatalf("release exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	mock = startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code = runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != startPlan(cwd) {
		t.Fatalf("start after release exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestTickHoldRefuses(t *testing.T) {
	path := filepath.Join(t.TempDir(), "hold")
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	cfgPath, cwd := writeStartConfig(t, fmt.Sprintf("[gate]\nhold_files = [%q]\n", path), "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := tickMock(t, emptyTabs, false, cwd)
	stdout, stderr, code := runCmd(t, "tick", "--config", cfgPath, "--db", dbPath)
	want := "blocked  yes\n" + startPlan(cwd) + "gate: hold " + path + "\n"
	if code != ExitOK || stderr != "" || stdout != want {
		t.Fatalf("exit = %d, stdout =\n%s\nwant =\n%s\nstderr = %s", code, stdout, want, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}
