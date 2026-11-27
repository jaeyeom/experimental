//go:build darwin || linux

package tui

import (
	"bytes"
	"context"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/creack/pty"
)

// drainPTYUntil reads from r until EOF and closes ready once needle
// appears in the drained bytes.
func drainPTYUntil(r io.Reader, needle string) <-chan struct{} {
	ready := make(chan struct{})
	go func() {
		var buf bytes.Buffer
		tmp := make([]byte, 4096)
		signaled := false
		for {
			n, err := r.Read(tmp)
			if n > 0 && !signaled {
				buf.Write(tmp[:n])
				if strings.Contains(buf.String(), needle) {
					signaled = true
					close(ready)
					buf.Reset()
				}
			}
			if err != nil {
				if !signaled {
					close(ready)
				}
				return
			}
		}
	}()
	return ready
}

// writeTTYEditor writes an editor that reads one line from its stdin,
// as a terminal editor reads keys, and saves it as the draft.
func writeTTYEditor(t *testing.T, dir, log string) string {
	t.Helper()
	body := "#!/bin/sh\n" +
		"printf 'editor\\n' >> " + shellQuote(log) + "\n" +
		"file=\n" +
		"for a in \"$@\"; do file=$a; done\n" +
		"IFS= read -r line\n" +
		"printf '%s' \"$line\" > \"$file\"\n"
	path := filepath.Join(dir, "ed-tty")
	if err := os.WriteFile(path, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake editor must be executable
		t.Fatal(err)
	}
	return path
}

// TestEditorUnderPty runs the editor through a real program whose input
// and output are a pseudo-terminal, on a local inbox and a hub client.
// One editor exits at once and one reads from the terminal.
func TestEditorUnderPty(t *testing.T) {
	for _, hub := range []bool{false, true} {
		for _, mode := range []string{"exit", "tty"} {
			name := mode + " local"
			if hub {
				name = mode + " hub"
			}
			t.Run(name, func(t *testing.T) {
				tmp := t.TempDir()
				t.Setenv("TMPDIR", tmp)
				var m *Model
				if hub {
					m, _, _, _ = hubModel(t)
					m.busRun = func(context.Context, []string) ([]byte, error) {
						return nil, errors.New("agentbus is not run in tests")
					}
				} else {
					m, _, _ = editorFixture(t)
				}
				log := filepath.Join(t.TempDir(), "editor.log")
				var script, want string
				if mode == "tty" {
					script, want = writeTTYEditor(t, t.TempDir(), log), "typed"
				} else {
					script, want = writeEditorScript(t, t.TempDir(), log, "replace"), "replaced"
				}
				t.Setenv("VISUAL", script)
				t.Setenv("EDITOR", "")
				press(t, m, "o0draft")

				master, slave, err := pty.Open()
				if err != nil {
					t.Skipf("no pty: %v", err)
				}
				defer master.Close()
				defer slave.Close()
				ready := drainPTYUntil(master, "reply:")

				p := newProgram(m, slave, slave, nil)
				done := make(chan error, 1)
				go func() {
					_, err := p.Run()
					done <- err
				}()
				select {
				case <-ready:
				case <-time.After(5 * time.Second):
					p.Kill()
					t.Fatal("the program did not render before ctrl+g")
				}
				if _, err := master.Write([]byte{0x07}); err != nil { // Ctrl+G
					t.Fatal(err)
				}
				if mode == "tty" {
					waitFile(t, log)
					if _, err := master.Write([]byte("typed\n")); err != nil {
						t.Fatal(err)
					}
				}
				waitEditorDone(t, log, tmp)
				p.Quit()
				select {
				case err := <-done:
					if err != nil {
						t.Fatal(err)
					}
				case <-time.After(5 * time.Second):
					p.Kill()
					t.Fatal("the program did not stop after the editor")
				}
				if m.reply != want || m.editor != nil {
					t.Fatalf("composer %q, want %q", m.reply, want)
				}
				if left, _ := filepath.Glob(filepath.Join(tmp, "decide-*.md")); len(left) != 0 {
					t.Fatalf("temp files left: %s", strings.Join(left, " "))
				}
			})
		}
	}
}

func waitFile(t *testing.T, path string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for {
		if _, err := os.Stat(path); err == nil {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("%s never appeared", path)
		}
		time.Sleep(2 * time.Millisecond)
	}
}

func TestEditorUsesTerminal(t *testing.T) {
	for _, tc := range []struct {
		prog string
		args []string
		want bool
	}{
		{"/usr/bin/vi", nil, true},
		{"/opt/homebrew/bin/emacsclient", nil, false},
		{"emacsclient", []string{"--alternate-editor="}, false},
		{"emacsclient", []string{"-t"}, true},
		{"emacsclient", []string{"-nw"}, true},
		{"emacsclient", []string{"-c"}, true},
		{"emacsclient", []string{"--create-frame"}, true},
	} {
		if got := editorUsesTerminal(tc.prog, tc.args); got != tc.want {
			t.Errorf("%s %v: got %v want %v", tc.prog, tc.args, got, tc.want)
		}
	}
}

// TestGUIEditorGetsNoTerminal runs an editor named emacsclient that
// fails when its stdin is a terminal, as a GUI client opens a text frame
// there. It must still start and return its text under a pty.
func TestGUIEditorGetsNoTerminal(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)
	m, _, _ := editorFixture(t)
	log := filepath.Join(t.TempDir(), "editor.log")
	bin := t.TempDir()
	body := "#!/bin/sh\n" +
		"[ -t 0 ] && exit 1\n" +
		"printf 'editor\\n' >> " + shellQuote(log) + "\n" +
		"for a in \"$@\"; do file=$a; done\n" +
		"printf 'gui' > \"$file\"\n"
	if err := os.WriteFile(filepath.Join(bin, "emacsclient"), []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake editor must be executable
		t.Fatal(err)
	}
	t.Setenv("VISUAL", filepath.Join(bin, "emacsclient"))
	t.Setenv("EDITOR", "")
	press(t, m, "o0draft")
	master, slave, err := pty.Open()
	if err != nil {
		t.Skipf("no pty: %v", err)
	}
	defer master.Close()
	defer slave.Close()
	ready := drainPTYUntil(master, "reply:")
	p := newProgram(m, slave, slave, nil)
	done := make(chan error, 1)
	go func() {
		_, err := p.Run()
		done <- err
	}()
	select {
	case <-ready:
	case <-time.After(5 * time.Second):
		p.Kill()
		t.Fatal("the program did not render before ctrl+g")
	}
	if _, err := master.Write([]byte{0x07}); err != nil {
		t.Fatal(err)
	}
	waitEditorDone(t, log, tmp)
	p.Quit()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		p.Kill()
		t.Fatal("the program did not stop after the editor")
	}
	if m.reply != "gui" {
		t.Fatalf("composer %q", m.reply)
	}
}
