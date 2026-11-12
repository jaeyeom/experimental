package tui

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"reflect"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestCopyFallsBackAcrossClipboardTools(t *testing.T) {
	var calls []string
	err := copyWith("hi", func(name string, args []string, text string) error {
		calls = append(calls, name)
		if text != "hi" {
			t.Fatalf("text %q", text)
		}
		if name == "xclip" && len(args) != 2 {
			t.Fatalf("xclip args %v", args)
		}
		if name != "pbcopy" {
			return fmt.Errorf("missing %s", name)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(calls, ",") != "xclip,wl-copy,pbcopy" {
		t.Fatalf("calls %v", calls)
	}

	calls = nil
	err = copyWith("hi", func(name string, _ []string, _ string) error {
		calls = append(calls, name)
		if name == "xclip" {
			return nil
		}
		return fmt.Errorf("should not run")
	})
	if err != nil || strings.Join(calls, ",") != "xclip" {
		t.Fatalf("err %v calls %v", err, calls)
	}
}

func TestClickAndCopyWhileMouseCaptureIsOn(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	link := "https://example.com/" + strings.Repeat("p", 80) + "/tailtoken"
	mustCreate(t, led, &decide.Item{
		Question: "see the spec",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{
			{Author: "ada", Text: "see the spec"},
			{Author: "ada", Text: "latest note"},
		},
		Links:     []string{link, "https://example.test/second"},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 40
	m.height = 24
	if !m.mouseOn {
		t.Fatal("mouse capture starts off")
	}
	press(t, m, "o")
	view := m.View()
	full := "\x1b]8;;" + link + "\x1b\\"
	wrapped := false
	for _, line := range strings.Split(view, "\n") {
		vis := visibleLine(line)
		if strings.Contains(vis, "https://example.com/") && !strings.Contains(vis, "tailtoken") && strings.Contains(line, full) {
			wrapped = true
		}
	}
	if !wrapped {
		t.Fatalf("OSC 8 did not keep the full URL on a wrapped line\n%s", view)
	}
	if !strings.Contains(view, "mouse on") || !strings.Contains(m.helpText(), "shift+drag selects text") {
		t.Fatalf("footer or help missing mouse guidance\n%s", view)
	}

	open := &fakeOpener{}
	m.runOpen = open.run
	row := screenRow(view, "https://example.com/")
	if row == 0 {
		t.Fatalf("link row missing\n%s", view)
	}
	line := strings.Split(view, "\n")[row-1]
	col := visibleCol(line, "https://example.com/")
	if col == 0 {
		t.Fatalf("link column missing\n%s", line)
	}
	before := m.scroll
	shift := tea.MouseMsg{
		X:      col - 1,
		Y:      row - 1,
		Shift:  true,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionMotion,
	}
	next, cmd := m.Update(shift)
	m = next.(*Model)
	runCmd(t, m, cmd)
	if len(open.calls) != 0 || m.scroll != before {
		t.Fatalf("shift+drag consumed opened %v scroll %d", open.calls, m.scroll)
	}

	next, cmd = m.Update(tea.MouseMsg{
		X:      col - 1,
		Y:      row - 1,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionPress,
	})
	m = next.(*Model)
	runCmd(t, m, cmd)
	if opened := open.urls(); len(opened) != 1 || opened[0] != link {
		t.Fatalf("click opened %#v", opened)
	}

	ctrl := tea.MouseMsg{
		X:      col - 1,
		Y:      row - 1,
		Ctrl:   true,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionPress,
	}
	next, cmd = m.Update(ctrl)
	m = next.(*Model)
	runCmd(t, m, cmd)
	if opened := open.urls(); len(opened) != 2 || opened[1] != link {
		t.Fatalf("ctrl+click opened %#v", opened)
	}

	press(t, m, "l")
	if len(open.calls) != 2 || !m.linkPick {
		t.Fatalf("several links status %q opened %#v", m.status, open.calls)
	}
	press(t, m, "2")
	finishOpen(t, m)
	if opened := open.urls(); len(opened) != 3 || opened[2] != "https://example.test/second" {
		t.Fatalf("picked link %#v", opened)
	}

	var out bytes.Buffer
	m.out = &out
	m.copyClip = func(string) error { return fmt.Errorf("clipboard unavailable") }
	press(t, m, "L")
	got, ok := osc52Text(out.String())
	if m.status != "copied" || !ok || got != "https://example.test/second" {
		t.Fatalf("copy link status %q ok %v text %q osc %q", m.status, ok, got, out.String())
	}

	out.Reset()
	press(t, m, "Y")
	got, ok = osc52Text(out.String())
	if m.status != "copied" || !ok || got != "latest note" {
		t.Fatalf("copy message status %q ok %v text %q", m.status, ok, got)
	}

	next, cmd = m.Update(runeKey('M'))
	m = next.(*Model)
	if m.mouseOn || cmd == nil || !reflect.DeepEqual(cmd(), tea.DisableMouse()) {
		t.Fatalf("mouse on %v cmd %#v", m.mouseOn, cmd)
	}
	if !strings.Contains(m.View(), "mouse off") {
		t.Fatalf("footer did not show mouse off\n%s", m.View())
	}
	n := len(open.calls)
	scroll := m.scroll
	next, _ = m.Update(tea.MouseMsg{
		X:      col - 1,
		Y:      row - 1,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionMotion,
	})
	m = next.(*Model)
	if len(open.calls) != n || m.scroll != scroll {
		t.Fatalf("plain drag while capture is off changed the inbox")
	}

	next, cmd = m.Update(runeKey('M'))
	m = next.(*Model)
	if !m.mouseOn || cmd == nil || !reflect.DeepEqual(cmd(), tea.EnableMouseCellMotion()) {
		t.Fatalf("mouse restore on %v", m.mouseOn)
	}
}

func TestYCopiesTheQuestionWhenTheThreadIsEmpty(t *testing.T) {
	m := New(t.TempDir())
	m.mode = modeDetail
	m.detail = &decide.Item{Question: "just a question"}
	var out bytes.Buffer
	m.out = &out
	m.copyClip = func(string) error { return nil }
	press(t, m, "Y")
	got, ok := osc52Text(out.String())
	if m.status != "copied" || !ok || got != "just a question" {
		t.Fatalf("status %q ok %v text %q", m.status, ok, got)
	}
}

func TestLOpensTheOnlyLink(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "one link",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		Links:     []string{"https://example.test/only"},
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 80
	m.height = 24
	open := &fakeOpener{}
	m.runOpen = open.run
	press(t, m, "ol")
	finishOpen(t, m)
	if opened := open.urls(); len(opened) != 1 || opened[0] != "https://example.test/only" {
		t.Fatalf("opened %q status %q", opened, m.status)
	}
}

func (m *Model) helpText() string {
	m.help = true
	text := m.View()
	m.help = false
	return text
}

func osc52Text(out string) (string, bool) {
	const prefix = "\x1b]52;c;"
	i := strings.LastIndex(out, prefix)
	if i < 0 {
		return "", false
	}
	rest := out[i+len(prefix):]
	end := strings.IndexAny(rest, "\a\x1b")
	if end < 0 {
		return "", false
	}
	raw, err := base64.StdEncoding.DecodeString(rest[:end])
	if err != nil {
		return "", false
	}
	return string(raw), true
}
