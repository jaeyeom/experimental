package gate

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
)

func TestHoldFile(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	path := filepath.Join(t.TempDir(), "hold")
	other := filepath.Join(t.TempDir(), "other")

	t.Run("missing", func(t *testing.T) {
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK", got)
		}
	})

	t.Run("empty", func(t *testing.T) {
		writeHold(t, path, "")
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		want := []string{"hold " + path}
		assertReasons(t, got, want)
	})

	t.Run("owner and reason", func(t *testing.T) {
		writeHold(t, path, "owner: ada\r\n\r\nfixing the gate\r\nstill running\r\n")
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		assertReasons(t, got, []string{"hold " + path + ": owner ada: fixing the gate; still running"})
	})

	t.Run("reason only", func(t *testing.T) {
		writeHold(t, path, "fixing the gate\n")
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		assertReasons(t, got, []string{"hold " + path + ": fixing the gate"})
	})

	t.Run("two files", func(t *testing.T) {
		writeHold(t, path, "owner: ada\n")
		got := holdCheck(t, Inputs{HoldFiles: []string{other, path}}, now)
		assertReasons(t, got, []string{"hold " + path + ": owner ada"})
	})

	t.Run("empty owner", func(t *testing.T) {
		writeHold(t, path, "owner:\nfixing\n")
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		assertReasons(t, got, []string{"hold " + path + ": expected a non-empty owner, found empty"})
	})

	t.Run("two owners", func(t *testing.T) {
		writeHold(t, path, "owner: ada\nowner: bob\n")
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		assertReasons(t, got, []string{"hold " + path + ": expected one owner line, found 2"})
	})

	t.Run("directory", func(t *testing.T) {
		dir := filepath.Join(t.TempDir(), "dirhold")
		if err := os.Mkdir(dir, 0o700); err != nil {
			t.Fatal(err)
		}
		got := holdCheck(t, Inputs{HoldFiles: []string{dir}}, now)
		assertReasons(t, got, []string{"hold " + dir + ": expected a file, found a directory"})
	})

	t.Run("empty path", func(t *testing.T) {
		got := holdCheck(t, Inputs{HoldFiles: []string{""}}, now)
		assertReasons(t, got, []string{"gate.hold_files: expected a non-empty path, found empty"})
	})

	t.Run("removed", func(t *testing.T) {
		writeHold(t, path, "")
		if err := os.Remove(path); err != nil {
			t.Fatal(err)
		}
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK after remove", got)
		}
	})
}

func TestHoldFileStale(t *testing.T) {
	path := filepath.Join(t.TempDir(), "hold")
	writeHold(t, path, "owner: ada\nfixing\n")
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	mod := info.ModTime()
	hour := time.Hour

	t.Run("equal age is not stale", func(t *testing.T) {
		got := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &hour}, mod.Add(time.Hour))
		assertReasons(t, got, []string{"hold " + path + ": owner ada: fixing"})
	})

	t.Run("older than ttl", func(t *testing.T) {
		ttl := time.Hour - time.Nanosecond
		got := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &ttl}, mod.Add(time.Hour))
		want := "hold " + path + " is stale (age " + time.Hour.String() + "): owner ada: fixing"
		assertReasons(t, got, []string{want})
	})

	t.Run("unset ttl ignores age", func(t *testing.T) {
		got := holdCheck(t, Inputs{HoldFiles: []string{path}}, mod.Add(1000*time.Hour))
		assertReasons(t, got, []string{"hold " + path + ": owner ada: fixing"})
	})

	t.Run("future file is not stale", func(t *testing.T) {
		got := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &hour}, mod.Add(-time.Minute))
		assertReasons(t, got, []string{"hold " + path + ": owner ada: fixing"})
	})

	t.Run("zero ttl", func(t *testing.T) {
		zero := time.Duration(0)
		fresh := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &zero}, mod)
		assertReasons(t, fresh, []string{"hold " + path + ": owner ada: fixing"})
		aged := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &zero}, mod.Add(time.Second))
		want := "hold " + path + " is stale (age " + time.Second.String() + "): owner ada: fixing"
		assertReasons(t, aged, []string{want})
	})

	t.Run("zero clock", func(t *testing.T) {
		in := Inputs{
			HoldFiles: []string{path},
			HoldTTL:   &hour,
			GitStatus: func(context.Context, string) (string, error) { return "", nil },
		}
		d := config.Defaults{MaxInFlight: 3}
		got := Check(context.Background(), in, "/src", d)
		assertReasons(t, got, []string{"hold " + path + ": gate.hold_ttl: expected a clock, found the zero time"})
	})

	t.Run("negative ttl", func(t *testing.T) {
		neg := -time.Second
		got := holdCheck(t, Inputs{HoldFiles: []string{path}, HoldTTL: &neg}, mod.Add(time.Hour))
		assertReasons(t, got, []string{
			"gate.hold_ttl: expected a non-negative duration, found -1s",
			"hold " + path + ": owner ada: fixing",
		})
	})
}

func TestHoldTabs(t *testing.T) {
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name string
		in   Inputs
		want []string
	}{
		{
			name: "missing",
			in:   Inputs{HoldTabs: []string{"coord"}},
		},
		{
			name: "done",
			in: Inputs{
				HoldTabs: []string{"coord"},
				Tabs:     []herdr.Tab{{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusDone}},
			},
		},
		{
			name: "idle",
			in: Inputs{
				HoldTabs: []string{"coord"},
				Tabs:     []herdr.Tab{{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusIdle}},
			},
			want: []string{"hold tab coord (w1:t1) is idle"},
		},
		{
			name: "blocked and unknown",
			in: Inputs{
				HoldTabs: []string{"a", "b"},
				Tabs: []herdr.Tab{
					{ID: "w1:t1", Label: "a", AgentStatus: herdr.AgentStatusBlocked},
					{ID: "w1:t2", Label: "b", AgentStatus: herdr.AgentStatusUnknown},
				},
			},
			want: []string{
				"hold tab a (w1:t1) is blocked",
				"hold tab b (w1:t2) is unknown",
			},
		},
		{
			name: "working is also a working agent",
			in: Inputs{
				HoldTabs: []string{"PRJ-2"},
				Tabs:     []herdr.Tab{{ID: "w1:t2", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking}},
				Panes:    []herdr.Pane{{ID: "w1:p2", TabID: "w1:t2", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking, Cwd: "/src"}},
			},
			want: []string{
				"hold tab PRJ-2 (w1:t2) is working",
				"agent PRJ-2 (w1:t2) is working",
			},
		},
		{
			name: "ignore tab does not clear a hold",
			in: Inputs{
				HoldTabs:   []string{"coord"},
				IgnoreTabs: []string{"coord"},
				Tabs:       []herdr.Tab{{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusWorking}},
			},
			want: []string{"hold tab coord (w1:t1) is working"},
		},
		{
			name: "caller pane does not clear a hold",
			in: Inputs{
				HoldTabs:     []string{"coord"},
				CallerPaneID: "w1:p1",
				Tabs:         []herdr.Tab{{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusIdle}},
				Panes:        []herdr.Pane{{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusIdle}},
			},
			want: []string{"hold tab coord (w1:t1) is idle"},
		},
		{
			name: "done sibling does not clear the live one",
			in: Inputs{
				HoldTabs: []string{"coord"},
				Tabs: []herdr.Tab{
					{ID: "w1:t1", Label: "coord", AgentStatus: herdr.AgentStatusDone},
					{ID: "w2:t2", Label: "coord", AgentStatus: herdr.AgentStatusIdle},
				},
			},
			want: []string{"hold tab coord (w2:t2) is idle"},
		},
		{
			name: "bad status",
			in: Inputs{
				HoldTabs: []string{"coord"},
				Tabs:     []herdr.Tab{{ID: "w1:t1", Label: "coord", AgentStatus: "asleep"}},
			},
			want: []string{`hold tab coord (w1:t1): expected agent_status one of idle, working, blocked, done, unknown, found "asleep"`},
		},
		{
			name: "empty label",
			in:   Inputs{HoldTabs: []string{""}},
			want: []string{"gate.hold_tabs: expected a non-empty tab label, found empty"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			assertReasons(t, holdCheck(t, tc.in, now), tc.want)
		})
	}
}

func TestPrepareHold(t *testing.T) {
	owner, reason, err := PrepareHold(" ada ", "  fixing \n\n still running \n")
	if err != nil {
		t.Fatal(err)
	}
	if owner != "ada" || reason != "fixing\nstill running" {
		t.Fatalf("owner = %q reason = %q", owner, reason)
	}
	if _, _, err := PrepareHold("ada\n", "fixing"); err == nil || err.Error() != "hold: expected a single-line owner, found a newline" {
		t.Fatalf("newline owner error = %v", err)
	}
	if _, _, err := PrepareHold("   ", ""); err == nil || err.Error() != "hold: expected a non-empty owner, found empty" {
		t.Fatalf("blank owner error = %v", err)
	}
	if _, _, err := PrepareHold("", " \n "); err == nil || err.Error() != "hold: expected a non-empty reason, found empty" {
		t.Fatalf("blank reason error = %v", err)
	}
	if _, _, err := PrepareHold("", "fixing\nowner: ada"); err == nil || err.Error() != `hold: expected a reason, found an owner line "owner: ada"` {
		t.Fatalf("owner line error = %v", err)
	}
}

func TestWriteAndRemoveHoldFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state", "hold")
	if err := WriteHoldFile(path, "ada", "fixing"); err != nil {
		t.Fatal(err)
	}
	body, err := os.ReadFile(path) //nolint:gosec // G304: path is the temp hold file this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if string(body) != "owner: ada\nfixing\n" {
		t.Fatalf("body = %q", body)
	}
	now := time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC)
	assertReasons(t, holdCheck(t, Inputs{HoldFiles: []string{path}}, now), []string{"hold " + path + ": owner ada: fixing"})
	if err := RemoveHoldFile(path); err != nil {
		t.Fatal(err)
	}
	got := holdCheck(t, Inputs{HoldFiles: []string{path}}, now)
	if !got.OK || got.Reasons != nil {
		t.Fatalf("Check() = %+v, want OK", got)
	}
	if err := RemoveHoldFile(path); err == nil || err.Error() != "release-hold: expected hold file "+path+", found none" {
		t.Fatalf("second remove error = %v", err)
	}
	if err := WriteHoldFile("", "", ""); err == nil || err.Error() != "hold: expected a path, found empty" {
		t.Fatalf("empty path error = %v", err)
	}
	if err := WriteHoldFile(path, "", ""); err != nil {
		t.Fatal(err)
	}
	body, err = os.ReadFile(path) //nolint:gosec // G304: path is the temp hold file this test just wrote.
	if err != nil {
		t.Fatal(err)
	}
	if len(body) != 0 {
		t.Fatalf("empty hold body = %q", body)
	}
}

func holdCheck(t *testing.T, in Inputs, now time.Time) Result {
	t.Helper()
	in.Now = now
	in.LastStartedAt = now.Add(-time.Hour)
	in.GitStatus = func(context.Context, string) (string, error) { return "", nil }
	d := config.Defaults{MaxInFlight: 3, MinStartInterval: 10 * time.Minute}
	return Check(context.Background(), in, "/src", d)
}

func assertReasons(t *testing.T, got Result, want []string) {
	t.Helper()
	if want == nil {
		if !got.OK || got.Reasons != nil {
			t.Fatalf("Check() = %+v, want OK", got)
		}
		return
	}
	if got.OK || len(got.Reasons) != len(want) {
		t.Fatalf("Check() = %+v, want reasons %q", got, want)
	}
	for i := range want {
		if got.Reasons[i] != want[i] {
			t.Fatalf("Check() = %+v, want reasons %q", got, want)
		}
	}
}

func writeHold(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}
