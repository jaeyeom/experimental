package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// fixedNow is 2026-03-10 18:30 in Seoul, 09:30 UTC.
var fixedNow = time.Date(2026, 3, 10, 9, 30, 0, 0, time.UTC)

// testApp is an app with a fresh, initialized data directory and a fixed
// clock.
type testApp struct {
	t       *testing.T
	env     map[string]string
	dataDir string
}

func newTestApp(t *testing.T) *testApp {
	t.Helper()
	root := t.TempDir()
	ta := &testApp{
		t: t,
		env: map[string]string{
			"HOME":            root,
			"XDG_CONFIG_HOME": filepath.Join(root, "config"),
			"XDG_DATA_HOME":   root,
		},
	}
	ta.dataDir = filepath.Join(root, "contacts")
	ta.mustRun("init")
	return ta
}

// run runs one command and returns its exit code, stdout and stderr.
func (ta *testApp) run(args ...string) (int, string, string) {
	var out, errOut bytes.Buffer
	a := &app{
		stdout: &out,
		stderr: &errOut,
		getenv: func(k string) string { return ta.env[k] },
		now:    func() time.Time { return fixedNow },
	}
	code := a.run(args)
	return code, out.String(), errOut.String()
}

func (ta *testApp) mustRun(args ...string) string {
	ta.t.Helper()
	code, out, errOut := ta.run(args...)
	if code != 0 {
		ta.t.Fatalf("contacts %s: exit %d\nstdout: %s\nstderr: %s", strings.Join(args, " "), code, out, errOut)
	}
	return out
}

func TestInit(t *testing.T) {
	ta := newTestApp(t)
	if _, err := os.Stat(filepath.Join(ta.dataDir, "contacts.db")); err != nil {
		t.Fatalf("database not created: %v", err)
	}
	ta.mustRun("init")
}

func TestUnknownCommand(t *testing.T) {
	ta := newTestApp(t)
	if code, _, _ := ta.run("nope"); code != exitUsage {
		t.Errorf("exit = %d, want %d", code, exitUsage)
	}
}
