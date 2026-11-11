package procinfo

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestParsePS(t *testing.T) {
	t.Parallel()

	out := "    1     0  1024 42-20:59:28 /sbin/launchd\n" +
		"  100    50 409600 01:02:03 /usr/local/bin/claude\n" +
		" 4242   100   512 02:13 sh -c bazel build //...\n" +
		"\n" +
		"  777     1     0 bogus\n" +
		"  778     1 x 00:01 bad-rss\n"
	got := ParsePS(out)
	if len(got) != 5 {
		t.Fatalf("ParsePS() = %+v, want 5 rows", got)
	}
	job := got[2]
	if job.PID != 4242 || job.PPID != 100 || job.Name != "sh" || !slices.Equal(job.Argv, []string{"sh", "-c", "bazel", "build", "//..."}) {
		t.Fatalf("row = %+v", job)
	}
	if job.Age != 2*time.Minute+13*time.Second {
		t.Fatalf("age = %s, want 2m13s", job.Age)
	}
	if job.RSS != 512*1024 {
		t.Fatalf("rss = %d, want 512 KiB in bytes", job.RSS)
	}
	if got[0].Age != 42*24*time.Hour+20*time.Hour+59*time.Minute+28*time.Second || got[1].Age != time.Hour+2*time.Minute+3*time.Second {
		t.Fatalf("ages = %s %s", got[0].Age, got[1].Age)
	}
	if got[1].Name != "claude" || got[1].RSS != 409600*1024 {
		t.Fatalf("row = %+v, want base of argv[0] and rss", got[1])
	}
	if got[3].PID != 777 || got[3].Name != "" || len(got[3].Argv) != 0 || got[3].Age >= 0 {
		t.Fatalf("row without args = %+v", got[3])
	}
	if got[4].PID != 778 || got[4].RSS != 0 || got[4].Name != "bad-rss" {
		t.Fatalf("row with a bad rss = %+v", got[4])
	}
}

func TestPSRunsOnePS(t *testing.T) {
	t.Parallel()

	var calls []string
	table := PS{Run: func(_ context.Context, name string, args ...string) (string, error) {
		calls = append(calls, name+" "+strings.Join(args, " "))
		return "  1 0 8 00:05 init\n", nil
	}}
	got, err := table.List(context.Background())
	if err != nil || len(got) != 1 || got[0].Name != "init" || got[0].RSS != 8*1024 {
		t.Fatalf("List() = %+v, %v", got, err)
	}
	if want := []string{"ps -A -o pid=,ppid=,rss=,etime=,args="}; !slices.Equal(calls, want) {
		t.Fatalf("calls = %q, want %q", calls, want)
	}
}

func TestProcFS(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	// starttime (field 22) is in 1/100 s since boot; uptime is 1000.5s.
	// rss (field 24) is in pages.
	writeProc(t, root, "100", "100 (my (odd) name) S 50 100 100 0", "claude\x00--resume\x00")
	writeProc(t, root, "4242", "4242 (sh) S 100 4242 100 0 -1 4194560 0 0 0 0 0 0 0 0 20 0 1 0 97050 1000 3 0", "sh\x00-c\x00bazel build //...\x00")
	writeFile(t, filepath.Join(root, "uptime"), "1000.50 900.00\n")
	if err := os.MkdirAll(filepath.Join(root, "self"), 0o755); err != nil {
		t.Fatal(err)
	}

	got, err := ProcFS{Root: root, PageSize: 4096}.List(context.Background())
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
	if got[0].Age >= 0 || got[0].RSS != 0 {
		t.Fatalf("row = %+v, want unknown age and no rss without the fields", got[0])
	}
	if got[1].PPID != 100 || !slices.Equal(got[1].Argv, []string{"sh", "-c", "bazel build //..."}) {
		t.Fatalf("row = %+v", got[1])
	}
	if got[1].Age != 30*time.Second {
		t.Fatalf("age = %s, want 30s (uptime 1000.5s less start 970.5s)", got[1].Age)
	}
	if got[1].RSS != 3*4096 {
		t.Fatalf("rss = %d, want 3 pages", got[1].RSS)
	}
}

func writeProc(t *testing.T, root, pid, stat, cmdline string) {
	t.Helper()
	dir := filepath.Join(root, pid)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(dir, "stat"), stat)
	writeFile(t, filepath.Join(dir, "cmdline"), cmdline)
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}
