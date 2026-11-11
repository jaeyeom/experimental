// Package sshtest puts a fake ssh, and fake hub tools it runs, on PATH
// for tests. It never contacts a real host.
package sshtest

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// sshScript answers `ssh -G` from $dir/port and otherwise records argv
// and runs the last argument with sh, as a remote login shell would.
const sshScript = `#!/bin/sh
d=%DIR%
if [ "$1" = "-G" ]; then
  echo "hostname 127.0.0.1"
  echo "port $(cat "$d/port" 2>/dev/null || echo 9)"
  exit 0
fi
printf '%s\n' "$@" > "$d/argv"
for a; do cmd=$a; done
if [ -e "$d/hang" ]; then exec sleep 30; fi
if [ -e "$d/down" ]; then sleep 1; echo "ssh: connect to host hub port 22: Operation timed out" >&2; exit 255; fi
if [ -e "$d/locked" ]; then echo "user@hub: Permission denied (publickey)." >&2; exit 255; fi
exec sh -c "$cmd"
`

// toolScript records its arguments, stdin, AGENTBUS_AS_HANDLE, and its
// whole environment, sleeps for $dir/<tool>.sleep seconds when that file
// exists, then prints $dir/<tool>.stdout and $dir/<tool>.stderr and
// exits with $dir/<tool>.code.
const toolScript = `#!/bin/sh
d=%DIR%
t=%TOOL%
printf '%s\n' "$@" > "$d/$t.args"
cat > "$d/$t.stdin"
printf '%s' "$AGENTBUS_AS_HANDLE" > "$d/$t.handle"
env > "$d/$t.env"
[ -e "$d/$t.sleep" ] && sleep "$(cat "$d/$t.sleep")"
[ -e "$d/$t.stdout" ] && cat "$d/$t.stdout"
[ -e "$d/$t.stderr" ] && cat "$d/$t.stderr" >&2
exit "$(cat "$d/$t.code" 2>/dev/null || echo 0)"
`

// Fake is an ssh on PATH controlled by files in its directory:
//
//   - hang makes ssh hang before the remote command starts.
//   - down makes ssh fail after a second with a connect timeout.
//   - locked makes ssh fail with a publickey error.
//   - port is the SSH port `ssh -G` reports for the probe.
//
// Otherwise ssh runs its last argument with sh.
type Fake struct {
	t   testing.TB
	dir string
}

// Install puts a fake ssh first on PATH for the rest of the test.
func Install(t testing.TB) *Fake {
	t.Helper()
	dir := t.TempDir()
	f := &Fake{t: t, dir: dir}
	f.writeExec("ssh", strings.ReplaceAll(sshScript, "%DIR%", dir))
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return f
}

// InstallTool adds a fake hub command named tool, such as agentbus, that
// the fake ssh runs. See toolScript for what it records and prints.
func (f *Fake) InstallTool(tool string) {
	f.t.Helper()
	body := strings.ReplaceAll(strings.ReplaceAll(toolScript, "%DIR%", f.dir), "%TOOL%", tool)
	f.writeExec(tool, body)
}

func (f *Fake) writeExec(name, body string) {
	f.t.Helper()
	if err := os.WriteFile(filepath.Join(f.dir, name), []byte(body), 0o755); err != nil { //nolint:gosec // test executable
		f.t.Fatal(err)
	}
}

// Write creates or replaces one control file.
func (f *Fake) Write(name, body string) {
	f.t.Helper()
	if err := os.WriteFile(filepath.Join(f.dir, name), []byte(body), 0o600); err != nil {
		f.t.Fatal(err)
	}
}

// Read returns one recorded file, or "" when it does not exist.
func (f *Fake) Read(name string) string {
	f.t.Helper()
	data, err := os.ReadFile(filepath.Join(f.dir, name))
	if errors.Is(err, os.ErrNotExist) {
		return ""
	}
	if err != nil {
		f.t.Fatal(err)
	}
	return string(data)
}

// Lines returns one recorded file split into lines.
func (f *Fake) Lines(name string) []string {
	f.t.Helper()
	text := f.Read(name)
	if text == "" {
		return nil
	}
	return strings.Split(strings.TrimSuffix(text, "\n"), "\n")
}

// Reachable makes the probe find a listening SSH port.
func (f *Fake) Reachable() {
	f.t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		f.t.Fatal(err)
	}
	f.t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			conn.Close()
		}
	}()
	f.Write("port", strconv.Itoa(ln.Addr().(*net.TCPAddr).Port))
}

// Unreachable makes the probe find a closed SSH port.
func (f *Fake) Unreachable() {
	f.t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		f.t.Fatal(err)
	}
	port := ln.Addr().(*net.TCPAddr).Port
	ln.Close()
	f.Write("port", strconv.Itoa(port))
}
