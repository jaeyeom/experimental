package main

import (
	"bytes"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

var updateGolden *bool

func TestMain(m *testing.M) {
	updateGolden = flag.Bool("update", false, "rewrite testdata golden files and the README walkthrough output")
	flag.Parse()
	os.Exit(m.Run())
}

// fixedNow is the tests' clock: 2026-03-10 09:30 UTC.
func fixedNow() time.Time { return time.Date(2026, 3, 10, 9, 30, 0, 0, time.UTC) }

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
		now:    fixedNow,
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

// golden compares got, with the data directory replaced by $DATA, to
// testdata/<name>, rewriting the file under -update.
func (ta *testApp) golden(name, got string) {
	t := ta.t
	t.Helper()
	got = strings.ReplaceAll(got, ta.dataDir, "$DATA")
	path := filepath.Join("testdata", name)
	if *updateGolden {
		if err := os.MkdirAll("testdata", 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(got), 0o600); err != nil {
			t.Fatal(err)
		}
		return
	}
	want, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read golden (run with -update to create): %v", err)
	}
	if got != string(want) {
		t.Errorf("%s mismatch\n--- got ---\n%s\n--- want ---\n%s", name, got, want)
	}
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
