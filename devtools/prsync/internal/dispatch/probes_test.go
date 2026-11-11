package dispatch

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestProcFSTable(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	cwd := t.TempDir()
	// starttime (field 22) is in 1/100 s since boot; uptime is 1000.5s.
	writeProc(t, root, "100", "100 (my (odd) name) S 50 100 100 0", "claude\x00--resume\x00", cwd)
	writeProc(t, root, "4242", "4242 (sh) S 100 4242 100 0 -1 4194560 0 0 0 0 0 0 0 0 20 0 1 0 97050 0", "sh\x00-c\x00bazel build //...\x00", "")
	writeTestFile(t, filepath.Join(root, "uptime"), "1000.50 900.00\n")
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
	if got[1].Age != 30*time.Second {
		t.Fatalf("age = %s, want 30s (uptime 1000.5s less start 970.5s)", got[1].Age)
	}
	if got[0].Age >= 0 {
		t.Fatalf("age = %s, want unknown without a start time", got[0].Age)
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
	mock.ExpectCommandWithArgs("ps", "-A", "-o", "pid=,ppid=,rss=,etime=,args=").WillSucceed("  1 0 8 00:05 init\n", 0).Build()
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
	mock.ExpectCommandWithArgs("decide", "list", "--json", "--state", "open").
		WillSucceed(`[{"id":"D-17","question":"ship?","state":"open","notifyPane":"w3:p2","unread":true,"createdAt":"2026-11-08T12:00:00Z"}]`, 0).Build()
	mock.ExpectCommandWithArgs("decide", "list", "--json", "--state", "answered").
		WillSucceed(`[{"id":"D-18","state":"answered","notifyPane":"w3:p3","createdAt":"2026-11-08T12:00:00Z","answeredAt":"2026-11-10T09:30:00Z"}]`, 0).Build()
	got, err := DecideCLI{Exec: mock, Bin: "decide"}.Items(context.Background())
	if err != nil {
		t.Fatalf("Items() unexpected error: %v", err)
	}
	created := time.Date(2026, 11, 8, 12, 0, 0, 0, time.UTC)
	want := []DecideItem{
		{ID: "D-17", State: "open", NotifyPane: "w3:p2", CreatedAt: created},
		{ID: "D-18", State: "answered", NotifyPane: "w3:p3", CreatedAt: created, AnsweredAt: time.Date(2026, 11, 10, 9, 30, 0, 0, time.UTC)},
	}
	if len(got) != 2 || !got[0].CreatedAt.Equal(want[0].CreatedAt) || got[0].ID != "D-17" ||
		got[1].ID != "D-18" || got[1].State != "answered" || !got[1].AnsweredAt.Equal(want[1].AnsweredAt) {
		t.Fatalf("Items() = %+v, want %+v", got, want)
	}
}

func TestDecideCLIMissing(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("decide", "list", "--json", "--state", "open").
		WillError(&executor.ExecutableNotFoundError{Command: "decide"}).Build()
	_, err := DecideCLI{Exec: mock, Bin: "decide"}.Items(context.Background())
	if !errors.Is(err, ErrDecideMissing) {
		t.Fatalf("Items() error = %v, want ErrDecideMissing", err)
	}
}

func TestDecideCLIFailure(t *testing.T) {
	t.Parallel()

	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("decide", "list", "--json", "--state", "open").WillFail("ledger locked", 1).Build()
	_, err := DecideCLI{Exec: mock, Bin: "decide"}.Items(context.Background())
	if err == nil || errors.Is(err, ErrDecideMissing) {
		t.Fatalf("Items() error = %v, want a non-missing failure", err)
	}
}
