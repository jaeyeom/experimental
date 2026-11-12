// Package herdrtest puts a fake herdr executable on PATH for tests.
package herdrtest

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// script records argv in $dir/argv and answers from files in $dir.
// See Fake for the files it reads.
const script = `#!/bin/sh
d=%DIR%
printf '%s\n' "$*" >> "$d/argv"
if [ "$1" = "--version" ]; then echo "herdr 0.8.1"; exit 0; fi
m=local
if [ "$1" = "--machine" ]; then m="$2"; shift 2; fi
if [ "$1 $2" = "machine status" ]; then
  if [ -e "$d/$3.down" ] || [ -e "$d/$3.status-down" ]; then echo '[{"id":"id-'"$3"'","label":"'"$3"'","status":"unreachable","error":"connect failed"}]'; exit 1; fi
  echo '[{"id":"id-'"$3"'","label":"'"$3"'","status":"reachable","error":null}]'
  exit 0
fi
if [ -e "$d/$m.down" ]; then echo "ssh: connect to host $m port 22: Connection refused" >&2; exit 255; fi
if [ -e "$d/$m.locked" ]; then echo "user@$m: Permission denied (publickey)." >&2; exit 255; fi
if [ -e "$d/$m.hang" ]; then sleep 30; exit 0; fi
if [ -e "$d/$m.eperm" ]; then echo "Error: connect to herdr socket: Operation not permitted (os error 1)" >&2; exit 1; fi
case "$1 $2" in
"agent list") cat "$d/$m.agents" ;;
"tab list")
  if [ -e "$d/$m.tabs" ]; then cat "$d/$m.tabs"; exit 0; fi
  echo '{"result":{"tabs":[{"tab_id":"w2:t1","label":"'"$m"'-tab"}]}}' ;;
"tab close") echo '{"result":{}}' ;;
"pane list")
  if [ -e "$d/$m.panes" ]; then cat "$d/$m.panes"; exit 0; fi
  echo '{"result":{"panes":[]}}' ;;
"workspace list")
  if [ -e "$d/$m.workspaces" ]; then cat "$d/$m.workspaces"; exit 0; fi
  echo '{"result":{"workspaces":[]}}' ;;
"workspace focus"|"tab focus")
  if [ -e "$d/$m.focus-refused" ]; then echo '{"error":{"code":"unsupported"}}' >&2; exit 1; fi
  echo '{"result":{}}' ;;
"agent prompt"|"agent wait")
  if [ -e "$d/$m.prompt-error" ]; then echo '{"error":{"code":"pane_not_found"}}' >&2; exit 1; fi
  if [ -e "$d/$m.prompt" ]; then cat "$d/$m.prompt"; exit 0; fi
  echo '{"result":{}}' ;;
"agent read") cat "$d/$m.screen" ;;
"pane send-text"|"pane send-keys"|"pane run")
  if [ "$2" = "send-keys" ] && [ -e "$d/$m.after-keys.$3" ]; then mv "$d/$m.after-keys.$3" "$d/$m.process-info.$3"; fi
  echo '{"result":{}}' ;;
"pane process-info")
  if [ "$3" = "--pane" ] && [ -e "$d/$m.process-info.$4" ]; then cat "$d/$m.process-info.$4"; exit 0; fi
  if [ -e "$d/$m.process-info" ]; then cat "$d/$m.process-info"; exit 0; fi
  echo '{"error":{"code":"pane_not_found"}}' >&2; exit 1 ;;
*) echo "unexpected $*" >&2; exit 2 ;;
esac
`

// Fake is a herdr on PATH whose machines answer from files. For machine
// m ("local" without --machine):
//
//   - m.agents is the `agent list` output, and m.screen the `agent read`
//     output.
//   - m.tabs is the `tab list` output. Without it, m has one tab w2:t1,
//     labelled "<m>-tab". `tab close` always succeeds.
//   - m.panes is the `pane list` output, and m.workspaces the
//     `workspace list` output. Without them, both lists are empty.
//   - `workspace focus` and `tab focus` succeed unless m.focus-refused
//     exists, which makes them fail with a herdr error.
//   - m.prompt is the `agent prompt` and `agent wait` output. Without it,
//     both print an empty result.
//   - m.process-info.<pane> is the `pane process-info --pane <pane>`
//     output for that pane, and m.process-info the output for every
//     other pane. Without either, `pane process-info` fails with a
//     herdr error.
//   - `pane send-text`, `pane send-keys`, and `pane run` succeed. After
//     `pane send-keys <pane> ...`, m.after-keys.<pane>, when it exists,
//     becomes m.process-info.<pane>, as when the keys made the pane's
//     foreground program exit.
//   - m.down makes m refuse connections and `machine status m` fail.
//   - m.status-down makes only `machine status m` fail.
//   - m.hang makes every call to m hang.
//   - m.locked makes m fail with a publickey error.
//   - m.eperm makes every call to m fail as herdr does when a sandbox
//     denies its socket: "Operation not permitted".
//   - m.prompt-error makes `agent prompt` and `agent wait` fail with a
//     herdr error.
//
// `herdr --version` prints "herdr 0.8.1".
type Fake struct {
	t   testing.TB
	dir string
}

// Install puts a fake herdr first on PATH for the rest of the test.
func Install(t testing.TB) *Fake {
	t.Helper()
	dir := t.TempDir()
	body := strings.ReplaceAll(script, "%DIR%", dir)
	if err := os.WriteFile(filepath.Join(dir, "herdr"), []byte(body), 0o755); err != nil { //nolint:gosec // test executable
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return &Fake{t: t, dir: dir}
}

// Write creates or replaces one control file.
func (f *Fake) Write(name, body string) {
	f.t.Helper()
	if err := os.WriteFile(filepath.Join(f.dir, name), []byte(body), 0o600); err != nil {
		f.t.Fatal(err)
	}
}

// Remove deletes one control file.
func (f *Fake) Remove(name string) {
	f.t.Helper()
	if err := os.Remove(filepath.Join(f.dir, name)); err != nil {
		f.t.Fatal(err)
	}
}

// Argv returns the recorded invocations, oldest first, and clears them.
func (f *Fake) Argv() []string {
	f.t.Helper()
	path := filepath.Join(f.dir, "argv")
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		f.t.Fatal(err)
	}
	if err := os.Remove(path); err != nil {
		f.t.Fatal(err)
	}
	return strings.Split(strings.TrimSuffix(string(data), "\n"), "\n")
}
