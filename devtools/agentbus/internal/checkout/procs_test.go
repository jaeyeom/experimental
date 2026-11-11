package checkout

import (
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestBlockersIgnoresShellsAgentsEditorsAndTheAdoptersTree(t *testing.T) {
	root := "/home/u/src/repo"
	procs := []Proc{
		{PID: 1, Name: "launchd", Cwd: "/"},
		{PID: 10, PPID: 1, Name: "claude", Cwd: root},        // adopter's agent
		{PID: 11, PPID: 10, Name: "zsh", Cwd: root},          // adopter's shell
		{PID: 12, PPID: 11, Name: "make", Cwd: root},         // a sibling, not an ancestor, of self
		{PID: 13, PPID: 11, Name: "agentbus", Cwd: root},     // self
		{PID: 20, PPID: 1, Name: "-zsh", Cwd: root + "/sub"}, // login shell
		{PID: 21, PPID: 1, Name: "nvim", Cwd: root},          // editor
		{PID: 22, PPID: 1, Name: "node", Args: []string{"node", "/usr/lib/node_modules/@anthropic-ai/claude-code/cli.js"}, Cwd: root},
		{PID: 23, PPID: 1, Name: "node", Args: []string{"node", "server.js"}, Cwd: root},
		{PID: 24, PPID: 1, Name: "go", Cwd: root + "/devtools"},
		{PID: 25, PPID: 1, Name: "bazel", Cwd: "/home/u/src/repo2"}, // a sibling directory
		{PID: 26, PPID: 1, Name: "python3", Cwd: "/tmp"},
		{PID: 27, PPID: 1, Name: "less", Cwd: root},
	}
	got := Blockers(root, procs, DefaultIgnoreProcs, 13)
	var pids []int
	for _, p := range got {
		pids = append(pids, p.PID)
	}
	if want := []int{12, 23, 24}; !slices.Equal(pids, want) {
		t.Fatalf("blockers = %v, want %v", pids, want)
	}
	if label := got[0].Label(); label != "make (pid 12)" {
		t.Fatalf("label = %q", label)
	}

	// A configured list replaces the default.
	got = Blockers(root, procs, []string{"make", "go", "node"}, 13)
	pids = nil
	for _, p := range got {
		pids = append(pids, p.PID)
	}
	if want := []int{20, 21, 27}; !slices.Equal(pids, want) {
		t.Fatalf("blockers with a custom list = %v, want %v", pids, want)
	}
}

func TestProcfsReadsCwdParentAndName(t *testing.T) {
	dir := t.TempDir()
	mk := func(pid, cwd, stat, cmdline, comm string) {
		t.Helper()
		base := filepath.Join(dir, pid)
		if err := os.MkdirAll(base, 0o755); err != nil {
			t.Fatal(err)
		}
		if cwd != "" {
			if err := os.Symlink(cwd, filepath.Join(base, "cwd")); err != nil {
				t.Fatal(err)
			}
		}
		for name, text := range map[string]string{"stat": stat, "cmdline": cmdline, "comm": comm} {
			if err := os.WriteFile(filepath.Join(base, name), []byte(text), 0o600); err != nil {
				t.Fatal(err)
			}
		}
	}
	mk("42", "/home/u/src/repo", "42 (my (odd) prog) S 7 42 42 0", "/usr/bin/sleep\x0060\x00", "sleep\n")
	mk("43", "/tmp", "43 (kworker) S 2 0 0 0", "", "kworker/0:1\n")
	mk("44", "", "44 (secret) S 1 0 0 0", "secret\x00", "secret\n")
	mk("self", "/", "", "", "")
	procs, err := procfs(dir)
	if err != nil {
		t.Fatal(err)
	}
	slices.SortFunc(procs, func(a, b Proc) int { return a.PID - b.PID })
	if len(procs) != 2 {
		t.Fatalf("procs = %+v", procs)
	}
	if p := procs[0]; p.PID != 42 || p.PPID != 7 || p.Name != "sleep" || p.Cwd != "/home/u/src/repo" || !slices.Equal(p.Args, []string{"/usr/bin/sleep", "60"}) {
		t.Fatalf("proc 42 = %+v", p)
	}
	if p := procs[1]; p.Name != "kworker/0:1" || p.PPID != 2 {
		t.Fatalf("proc 43 = %+v", p)
	}
}

// startIn starts argv with its working directory in dir and stops it
// when the test ends.
func startIn(t *testing.T, dir string, argv ...string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command(argv[0], argv[1:]...) //nolint:gosec // G204: test helper
	cmd.Dir = dir
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	cmd.Stdout, cmd.Stderr = io.Discard, io.Discard
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		stdin.Close()
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
	})
	return cmd
}

func TestListProcsFindsASleepInTheCheckoutButNotAnInteractiveShell(t *testing.T) {
	if _, err := exec.LookPath("sleep"); err != nil {
		t.Skip("sleep is not installed")
	}
	root, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	sub := filepath.Join(root, "sub")
	if err := os.Mkdir(sub, 0o755); err != nil {
		t.Fatal(err)
	}
	sleep := startIn(t, sub, "sleep", "60")
	var shell *exec.Cmd
	if _, err := exec.LookPath("bash"); err == nil {
		shell = startIn(t, root, "bash", "--noprofile", "--norc", "-i")
	}

	deadline := time.Now().Add(5 * time.Second)
	for {
		procs, err := ListProcs()
		if err != nil {
			t.Fatal(err)
		}
		blockers := Blockers(root, procs, DefaultIgnoreProcs, os.Getpid())
		i := slices.IndexFunc(blockers, func(p Proc) bool { return p.PID == sleep.Process.Pid })
		if i >= 0 {
			if blockers[i].Name != "sleep" {
				t.Fatalf("sleep is named %q", blockers[i].Name)
			}
			if shell != nil {
				if slices.ContainsFunc(blockers, func(p Proc) bool { return p.PID == shell.Process.Pid }) {
					t.Fatalf("interactive bash blocks: %+v", blockers)
				}
				// The shell is listed, just ignored.
				if !slices.ContainsFunc(procs, func(p Proc) bool { return p.PID == shell.Process.Pid && p.Name == "bash" }) {
					t.Fatalf("bash (pid %d) is not listed", shell.Process.Pid)
				}
			}
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("sleep (pid %d) in %s is not a blocker: %+v", sleep.Process.Pid, sub, blockers)
		}
		time.Sleep(50 * time.Millisecond)
	}
}
