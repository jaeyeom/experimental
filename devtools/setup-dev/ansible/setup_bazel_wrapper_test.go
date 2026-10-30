package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// termuxBazelWrapperScript returns the bash script installed by the
// "Create bazel wrapper script for Termux" task.
func termuxBazelWrapperScript(t *testing.T) string {
	t.Helper()
	path := filepath.Join(ansiblePlaybookDir(t), "setup-bazel.yml")
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	const startMark = "\n          #!/bin/bash\n"
	start := strings.Index(string(body), startMark)
	if start < 0 {
		t.Fatal("Termux bazel wrapper shebang not found in setup-bazel.yml")
	}
	return dedentTermuxBazelWrapper(string(body)[start+1:])
}

func dedentTermuxBazelWrapper(raw string) string {
	const prefix = "          "
	var b strings.Builder
	lines := strings.Split(raw, "\n")
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			b.WriteByte('\n')
			continue
		}
		if !strings.HasPrefix(line, prefix) {
			break
		}
		b.WriteString(strings.TrimPrefix(line, prefix))
		b.WriteByte('\n')
	}
	return b.String()
}

// runTermuxBazelWrapper executes the installed wrapper with a stub pd.
// TERMUX_CWD is injected so the test does not need the Termux home directory.
func runTermuxBazelWrapper(t *testing.T, script, cwd string) (workDir, stderr string) {
	t.Helper()
	if _, err := exec.LookPath("bash"); err != nil {
		t.Fatal("bash is required to run the Termux bazel wrapper")
	}
	dir := t.TempDir()
	stub := `#!/bin/sh
has_termux_home=0
prev=
for arg in "$@"; do
  if [ "$arg" = "--termux-home" ]; then
    has_termux_home=1
  fi
  if [ "$prev" = "--work-dir" ]; then
    if [ "$has_termux_home" -ne 1 ]; then
      printf '%s\n' "missing --termux-home" >&2
      exit 1
    fi
    printf '%s\n' "$arg"
    exit 0
  fi
  prev=$arg
done
printf '%s\n' "pd stub missing --work-dir" >&2
exit 1
`
	pdPath := filepath.Join(dir, "pd")
	writeExecutable(t, pdPath, stub)
	const cwdAssign = "TERMUX_CWD=$(pwd)"
	if !strings.Contains(script, cwdAssign) {
		t.Fatalf("wrapper does not assign TERMUX_CWD from pwd:\n%s", script)
	}
	script = strings.Replace(script, cwdAssign, `TERMUX_CWD="$TERMUX_CWD_UNDER_TEST"`, 1)

	cmd := exec.Command("bash", "-c", script)
	cmd.Env = []string{
		"PATH=" + dir,
		"HOME=" + dir,
		"DEBIAN_USER=builder",
		"TERMUX_CWD_UNDER_TEST=" + cwd,
	}
	var stdout, stderrBuf bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderrBuf
	if err := cmd.Run(); err != nil {
		t.Fatalf("wrapper cwd %q: %v\nstderr: %s\nstdout: %s", cwd, err, stderrBuf.String(), stdout.String())
	}
	return strings.TrimSpace(stdout.String()), stderrBuf.String()
}

func TestTermuxBazelWrapperWorkDir(t *testing.T) {
	script := termuxBazelWrapperScript(t)
	const home = "/data/data/com.termux/files/home"
	tests := []struct {
		name string
		cwd  string
		want string
		warn bool
	}{
		{
			name: "termux home",
			cwd:  home,
			want: home,
		},
		{
			name: "directory under termux home",
			cwd:  home + "/src/experimental",
			want: home + "/src/experimental",
		},
		{
			name: "outside termux home",
			cwd:  "/data/data/com.termux/files/usr",
			want: "/home/builder",
			warn: true,
		},
		{
			name: "path with home as a prefix but not the home directory",
			cwd:  home + "-backup/src",
			want: "/home/builder",
			warn: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, stderr := runTermuxBazelWrapper(t, script, tc.cwd)
			if got != tc.want {
				t.Errorf("work-dir = %q, want %q", got, tc.want)
			}
			warned := strings.Contains(stderr, "outside Termux home")
			if tc.warn && !warned {
				t.Errorf("stderr = %q, want outside-home warning", stderr)
			}
			if !tc.warn && warned {
				t.Errorf("stderr = %q, want no warning", stderr)
			}
		})
	}
}
