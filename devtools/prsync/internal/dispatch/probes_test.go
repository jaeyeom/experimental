package dispatch

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestParsePS(t *testing.T) {
	t.Parallel()

	out := "    1     0 /sbin/launchd\n" +
		"  100    50 /usr/local/bin/claude\n" +
		" 4242   100 sh -c bazel build //...\n" +
		"\n" +
		"  777     1\n"
	got := parsePS(out)
	if len(got) != 4 {
		t.Fatalf("parsePS() = %+v, want 4 rows", got)
	}
	job := got[2]
	if job.PID != 4242 || job.PPID != 100 || job.Name != "sh" || !slices.Equal(job.Argv, []string{"sh", "-c", "bazel", "build", "//..."}) {
		t.Fatalf("row = %+v", job)
	}
	if got[1].Name != "claude" {
		t.Fatalf("name = %q, want base of argv[0]", got[1].Name)
	}
	if got[3].PID != 777 || got[3].Name != "" || len(got[3].Argv) != 0 {
		t.Fatalf("row without args = %+v", got[3])
	}
}

func TestParseLsofCwd(t *testing.T) {
	t.Parallel()

	if got := parseLsofCwd("p4242\nfcwd\nn/src/repo dir\n"); got != "/src/repo dir" {
		t.Fatalf("parseLsofCwd() = %q", got)
	}
	if got := parseLsofCwd("p4242\n"); got != "" {
		t.Fatalf("parseLsofCwd() = %q, want empty", got)
	}
}

func TestProcFSTable(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	cwd := t.TempDir()
	writeProc(t, root, "100", "100 (my (odd) name) S 50 100 100 0", "claude\x00--resume\x00", cwd)
	writeProc(t, root, "4242", "4242 (sh) S 100 4242 100 0", "sh\x00-c\x00bazel build //...\x00", "")
	mkdirAll(t, filepath.Join(root, "self"))
	writeTestFile(t, filepath.Join(root, "meminfo"), "x")

	table := procFS{root: root}
	got, err := table.List(context.Background())
	if err != nil {
		t.Fatalf("List() unexpected error: %v", err)
	}
	slices.SortFunc(got, func(a, b Process) int { return a.PID - b.PID })
	if len(got) != 2 {
		t.Fatalf("List() = %+v, want 2", got)
	}
	if got[0].Name != "my (odd) name" || got[0].PPID != 50 || !slices.Equal(got[0].Argv, []string{"claude", "--resume"}) {
		t.Fatalf("row = %+v", got[0])
	}
	if got[1].PPID != 100 || !slices.Equal(got[1].Argv, []string{"sh", "-c", "bazel build //..."}) {
		t.Fatalf("row = %+v", got[1])
	}
	gotCwd, err := table.Cwd(context.Background(), 100)
	if err != nil || gotCwd != cwd {
		t.Fatalf("Cwd() = %q, %v; want %q", gotCwd, err, cwd)
	}
}

func writeProc(t *testing.T, root, pid, stat, cmdline, cwd string) {
	t.Helper()
	dir := filepath.Join(root, pid)
	mkdirAll(t, dir)
	writeTestFile(t, filepath.Join(dir, "stat"), stat)
	writeTestFile(t, filepath.Join(dir, "cmdline"), cmdline)
	if cwd != "" {
		if err := os.Symlink(cwd, filepath.Join(dir, "cwd")); err != nil {
			t.Fatal(err)
		}
	}
}

func TestPSTableUsesExecutor(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("ps", "-A", "-o", "pid=,ppid=,args=").WillSucceed("  1 0 init\n", 0).Build()
	mock.ExpectCommandWithArgs("lsof", "-a", "-p", "1", "-d", "cwd", "-Fn").WillSucceed("p1\nfcwd\nn/\n", 0).Build()
	table := psTable{exec: mock}
	got, err := table.List(context.Background())
	if err != nil || len(got) != 1 || got[0].Name != "init" {
		t.Fatalf("List() = %+v, %v", got, err)
	}
	cwd, err := table.Cwd(context.Background(), 1)
	if err != nil || cwd != "/" {
		t.Fatalf("Cwd() = %q, %v", cwd, err)
	}
}

func TestDecideCLI(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("decide", "list", "--json").
		WillSucceed(`[{"id":"D-17","question":"ship?","state":"open","notifyPane":"w3:p2","unread":true}]`, 0).Build()
	got, err := DecideCLI{Exec: mock, Bin: "decide"}.OpenItems(context.Background())
	if err != nil {
		t.Fatalf("OpenItems() unexpected error: %v", err)
	}
	if len(got) != 1 || got[0] != (DecideItem{ID: "D-17", State: "open", NotifyPane: "w3:p2"}) {
		t.Fatalf("OpenItems() = %+v", got)
	}
}

func TestDecideCLIMissing(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("decide", "list", "--json").
		WillError(&executor.ExecutableNotFoundError{Command: "decide"}).Build()
	_, err := DecideCLI{Exec: mock, Bin: "decide"}.OpenItems(context.Background())
	if !errors.Is(err, ErrDecideMissing) {
		t.Fatalf("OpenItems() error = %v, want ErrDecideMissing", err)
	}
}

func TestDecideCLIFailure(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("decide", "list", "--json").WillFail("ledger locked", 1).Build()
	_, err := DecideCLI{Exec: mock, Bin: "decide"}.OpenItems(context.Background())
	if err == nil || errors.Is(err, ErrDecideMissing) {
		t.Fatalf("OpenItems() error = %v, want a non-missing failure", err)
	}
}
