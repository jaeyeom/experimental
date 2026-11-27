package main

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestDiagnoseHerdr(t *testing.T) {
	t.Parallel()
	mismatch := herdrFacts{
		GOOS: "linux", UID: 1000, PID: 42,
		Started: "2026-11-15T08:19:33Z", Supervisor: "systemd",
		DaemonPATH: "/usr/bin:/bin", CallerHerdr: "/home/u/.local/bin/herdr",
		ExecNotFound: true, Checked: true,
	}
	mac := mismatch
	mac.GOOS = "darwin"
	mac.UID = 501
	mac.Supervisor = "launchd"
	both := herdrFacts{Checked: true, DaemonPATH: "/usr/bin:/bin"}
	pinned := mismatch
	pinned.DaemonHerdr = "/opt/herdr"
	pinned.DaemonHerdrOK = true

	cases := []struct {
		name  string
		in    herdrFacts
		want  []string
		empty bool
	}{
		{
			name: "caller has herdr and the daemon path does not",
			in:   mismatch,
			want: []string{
				"agentbusd (pid 42) was started at 2026-11-15T08:19:33Z by systemd with a PATH that does not include /home/u/.local/bin, where herdr is.",
				"fix: run `agentbus install` from a login shell, then: systemctl --user restart agentbus",
			},
		},
		{
			name: "macos restart command",
			in:   mac,
			want: []string{
				"by launchd with a PATH that does not include /home/u/.local/bin, where herdr is.",
				"fix: run `agentbus install` from a login shell, then: launchctl kickstart -k gui/501/com.agentbus.agentbusd",
			},
		},
		{
			name: "neither side has herdr",
			in:   both,
			want: []string{"herdr is not installed or not on the login PATH."},
		},
		{name: "pinned executable", in: pinned, empty: true},
		{
			name: "exec not found without a start time",
			in:   herdrFacts{ExecNotFound: true, DaemonPATH: "/usr/bin:/bin"},
			want: []string{
				"herdr: executable file not found in $PATH",
				"unit PATH=/usr/bin:/bin AGENTBUS_HERDR=",
				"fix: re-run `agentbus install` from a shell where `which herdr` works, or set `herdr_path`.",
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := diagnoseHerdr(tc.in)
			if tc.empty {
				if got != "" {
					t.Fatalf("diagnose = %q", got)
				}
				return
			}
			for _, want := range tc.want {
				if !strings.Contains(got, want) {
					t.Errorf("missing %q\n%s", want, got)
				}
			}
			if tc.name == "neither side has herdr" && strings.Contains(got, "fix:") {
				t.Fatalf("not-installed diagnosis has a fix line:\n%s", got)
			}
		})
	}
}

func TestDiagnoseSandboxAndDaemonBinary(t *testing.T) {
	t.Parallel()
	socket := "/run/user/1000/agentbus/tunnel.sock"
	settings := "/home/u/.claude/settings.json"
	got := diagnoseSandbox(settings, socket, []string{"/run/herdr/herdr.sock"}, true)
	for _, want := range []string{socket, settings, "fix: run setup-claude-sandbox"} {
		if !strings.Contains(got, want) {
			t.Errorf("sandbox missing %q\n%s", want, got)
		}
	}
	if diagnoseSandbox(settings, socket, []string{socket}, true) != "" {
		t.Fatal("listed socket was reported missing")
	}
	if diagnoseSandbox(settings, socket, nil, false) != "" {
		t.Fatal("missing settings file was reported")
	}

	restart := "systemctl --user restart agentbus"
	old := diagnoseDaemon(daemonFacts{
		Checked: true, Running: true, PID: 9,
		RunningBuild: "abc", InstalledBuild: "def",
		InstalledPath: "/home/u/go/bin/agentbusd", Restart: restart,
	})
	for _, want := range []string{"pid 9", "abc", "def", "/home/u/go/bin/agentbusd", restart} {
		if !strings.Contains(old, want) {
			t.Errorf("old binary missing %q\n%s", want, old)
		}
	}
	down := diagnoseDaemon(daemonFacts{Checked: true, Restart: restart})
	if !strings.Contains(down, "agentbusd is not running.") || !strings.Contains(down, restart) {
		t.Fatalf("not running = %q", down)
	}
	if diagnoseDaemon(daemonFacts{Checked: true, Running: true, RunningBuild: "abc", InstalledBuild: "abc"}) != "" {
		t.Fatal("matching build was reported")
	}
}

func TestBinaryBuildIsEmptyForAMissingFile(t *testing.T) {
	t.Parallel()
	if binaryBuild(filepath.Join(t.TempDir(), "no-such-agentbusd")) != "" {
		t.Fatal("missing binary reported a build")
	}
}

func TestRunningExeBuildMatchesThisProcess(t *testing.T) {
	t.Parallel()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	got := runningExe(context.Background(), os.Getpid(), "")
	if got == "" {
		t.Fatal("runningExe returned no path for this process")
	}
	if binaryBuild(got) != binaryBuild(exe) {
		t.Fatalf("running %q build %q, this process %q build %q", got, binaryBuild(got), exe, binaryBuild(exe))
	}
}
