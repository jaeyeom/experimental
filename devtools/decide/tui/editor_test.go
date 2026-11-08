package tui

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestComposerExternalEditor(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)

	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "edit me",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{{Author: "ada", Text: "edit me"}},
	})
	m := New(dir)
	m.width = 80
	m.height = 20
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	log := filepath.Join(t.TempDir(), "editor.log")
	script := writeEditorScript(t, t.TempDir(), log, "replace")
	t.Setenv("VISUAL", script+" --flag")
	t.Setenv("EDITOR", "unused-editor")
	m.suspend, m.resume = logHooks(log)

	press(t, m, "o0draft")
	if !m.replyFocus || m.reply != "draft" {
		t.Fatalf("composer %q focus %v", m.reply, m.replyFocus)
	}
	footer := m.View()
	for _, needle := range []string{"ctrl+x ctrl+e", "ctrl+g", "? help"} {
		if !strings.Contains(footer, needle) {
			t.Fatalf("footer missing %q\n%s", needle, footer)
		}
	}

	press(t, m, "\x1b?")
	help := m.View()
	if !strings.Contains(help, "ctrl+x ctrl+e or ctrl+g edits the draft") {
		t.Fatalf("help missing the editor key\n%s", help)
	}
	press(t, m, "?0")

	finishEditor(t, m, editorKeys(m, tea.KeyCtrlX, tea.KeyCtrlE))
	assertEditorLog(t, log, []string{"suspend", script, "--flag", "editor", "resume"})
	md := markdownArg(t, log)
	if _, err := os.Stat(md); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("temp file %s err %v", md, err)
	}
	if m.reply != "replaced" || !strings.Contains(m.View(), "replaced") {
		t.Fatalf("composer %q\n%s", m.reply, m.View())
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil || len(got.Messages) != 1 || got.Messages[0].Text != "edit me" {
		t.Fatalf("editor sent the draft: %+v", got)
	}

	os.Remove(log)
	m.reply = "draft"
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	assertEditorLog(t, log, []string{"suspend", "editor", "resume"})
	if m.reply != "replaced" {
		t.Fatalf("ctrl+g composer %q", m.reply)
	}
	got, err = led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || len(got.Messages) != 1 {
		t.Fatalf("ctrl+g sent the draft: %+v", got)
	}
}

func TestComposerEditorKeepsText(t *testing.T) {
	m, _, _ := editorFixture(t)
	log := filepath.Join(t.TempDir(), "editor.log")
	bin := t.TempDir()
	script := writeEditorScript(t, bin, log, "dirty")
	t.Setenv("VISUAL", script)
	t.Setenv("EDITOR", "")
	m.suspend, m.resume = logHooks(log)
	press(t, m, "o0draft")
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	if m.reply != "draft" {
		t.Fatalf("nonzero editor changed %q", m.reply)
	}
	if _, err := os.Stat(markdownArg(t, log)); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("dirty temp file remains")
	}

	os.Remove(log)
	script = writeEditorScript(t, bin, log, "keep")
	t.Setenv("VISUAL", script)
	m.reply = "draft"
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlX, tea.KeyCtrlE))
	if m.reply != "draft" {
		t.Fatalf("unchanged editor changed %q", m.reply)
	}
	if _, err := os.Stat(markdownArg(t, log)); !errors.Is(err, fs.ErrNotExist) {
		t.Fatal("unchanged temp file remains")
	}
}

func TestComposerEditorProgram(t *testing.T) {
	m, _, _ := editorFixture(t)
	log := filepath.Join(t.TempDir(), "editor.log")
	bin := t.TempDir()
	script := writeEditorScript(t, bin, log, "replace")
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", script+" --wait")
	m.suspend, m.resume = logHooks(log)
	press(t, m, "o0draft")
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	assertEditorLog(t, log, []string{script, "--wait"})
	if m.reply != "replaced" {
		t.Fatalf("EDITOR composer %q", m.reply)
	}

	os.Remove(log)
	nano := writeEditorScript(t, bin, log, "replace")
	nanoPath := filepath.Join(bin, "nano")
	if err := os.Rename(nano, nanoPath); err != nil {
		t.Fatal(err)
	}
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", "")
	t.Setenv("PATH", bin)
	m.reply = "draft"
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	body, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(body), "nano") || m.reply != "replaced" {
		t.Fatalf("nano log %s composer %q", body, m.reply)
	}
}

func TestComposerEditorMissingNano(t *testing.T) {
	m, led, id := editorFixture(t)
	bin := t.TempDir()
	t.Setenv("TMPDIR", t.TempDir())
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", "")
	t.Setenv("PATH", bin)
	var suspended bool
	m.suspend = func() { suspended = true }
	m.resume = func() { suspended = true }
	press(t, m, "o0draft")
	cmd := editorKeys(m, tea.KeyCtrlG)
	if cmd != nil || m.editor != nil || suspended {
		t.Fatal("missing nano started an editor")
	}
	if m.status != "editor not found" || m.reply != "draft" || !m.replyFocus {
		t.Fatalf("status %q reply %q focus %v", m.status, m.reply, m.replyFocus)
	}
	if !strings.Contains(m.View(), "editor not found") || !strings.Contains(m.View(), "draft") {
		t.Fatalf("composer left the screen\n%s", m.View())
	}
	entries, err := os.ReadDir(os.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), "decide-") {
			t.Fatalf("temp file remains: %s", entry.Name())
		}
	}
	got, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen || got.Answer != nil {
		t.Fatalf("missing nano changed the item: %+v", got)
	}
}

func TestComposerEditorDropsOtherKeys(t *testing.T) {
	m, led, id := editorFixture(t)
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", "")
	t.Setenv("PATH", t.TempDir())
	var suspended bool
	m.suspend = func() { suspended = true }
	press(t, m, "o0draft")
	editorKeys(m, tea.KeyCtrlX)
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'z'}})
	if m.reply != "draft" || m.editor != nil || suspended || m.editPrefix {
		t.Fatalf("ctrl+x then z reply %q editor %v prefix %v", m.reply, m.editor != nil, m.editPrefix)
	}
	editorKeys(m, tea.KeyCtrlE)
	if m.editor != nil {
		t.Fatal("ctrl+e alone opened the editor")
	}
	editorKeys(m, tea.KeyCtrlX)
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	got, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	if m.reply != "draft" || got.State != decide.StateOpen || len(got.Messages) != 1 {
		t.Fatalf("swallowed enter sent reply %q state %s", m.reply, got.State)
	}
}

func TestComposeNoteAndFreeTextEditor(t *testing.T) {
	m, led, _ := editorFixture(t)
	log := filepath.Join(t.TempDir(), "editor.log")
	script := writeEditorScript(t, t.TempDir(), log, "replace")
	t.Setenv("VISUAL", script)
	t.Setenv("EDITOR", "")
	m.suspend, m.resume = logHooks(log)

	m.focusAnswer()
	m.draft = "free"
	m.free.SetValue("free")
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	if m.draft != "replaced" || m.free.Value() != "replaced" {
		t.Fatalf("free text %q field %q", m.draft, m.free.Value())
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 || items[0].Answer != nil {
		t.Fatalf("free text sent: %+v", items)
	}

	os.Remove(log)
	m.answerFocus = false
	m.Targets = []Target{{Name: "ada", Pane: "pane-1"}}
	press(t, m, "u")
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "c\thi")
	if !m.composing || m.composeFocus != focusText || m.note != "hi" {
		t.Fatalf("compose focus %d note %q", m.composeFocus, m.note)
	}
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlX, tea.KeyCtrlE))
	if m.note != "replaced" || m.noteField.Value() != "replaced" || !m.composing {
		t.Fatalf("note %q composing %v", m.note, m.composing)
	}
	items, err = led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 {
		t.Fatalf("compose sent: %d items", len(items))
	}
}

func editorFixture(t *testing.T) (*Model, *decide.Ledger, string) {
	t.Helper()
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "edit me",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{{Author: "ada", Text: "edit me"}},
	})
	m := New(dir)
	m.width = 80
	m.height = 20
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	return m, led, item.ID
}

func editorKeys(m *Model, keys ...tea.KeyType) tea.Cmd {
	var cmd tea.Cmd
	for _, key := range keys {
		_, cmd = m.Update(tea.KeyMsg{Type: key})
	}
	return cmd
}

func finishEditor(t *testing.T, m *Model, cmd tea.Cmd) {
	t.Helper()
	if cmd == nil || m.editor == nil {
		t.Fatal("editor did not start")
	}
	err := m.editor.Run()
	m.Update(editorDoneMsg{err: err})
}

func logHooks(log string) (func(), func()) {
	write := func(line string) {
		f, err := os.OpenFile(log, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
		if err != nil {
			return
		}
		_, _ = f.WriteString(line + "\n")
		_ = f.Close()
	}
	return func() { write("suspend") }, func() { write("resume") }
}

func writeEditorScript(t *testing.T, dir, log, mode string) string {
	t.Helper()
	body := "#!/bin/sh\n" +
		"printf '%s\\n' \"$0\" >> " + shellQuote(log) + "\n" +
		"for a in \"$@\"; do printf '%s\\n' \"$a\" >> " + shellQuote(log) + "; done\n" +
		"printf 'editor\\n' >> " + shellQuote(log) + "\n" +
		"file=\n" +
		"for a in \"$@\"; do file=$a; done\n"
	switch mode {
	case "replace":
		body += "printf 'replaced' > \"$file\"\n"
	case "dirty":
		body += "printf 'changed' > \"$file\"\nexit 1\n"
	default:
		body += "exit 0\n"
	}
	path := filepath.Join(dir, "ed-"+mode)
	if err := os.WriteFile(path, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake editor must be executable
		t.Fatal(err)
	}
	return path
}

func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", "'\\''") + "'"
}

func assertEditorLog(t *testing.T, log string, needles []string) {
	t.Helper()
	body, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	text := string(body)
	last := 0
	for _, needle := range needles {
		i := strings.Index(text[last:], needle)
		if i < 0 {
			t.Fatalf("log missing %q after %d\n%s", needle, last, text)
		}
		last += i + len(needle)
	}
}

func markdownArg(t *testing.T, log string) string {
	t.Helper()
	body, err := os.ReadFile(log)
	if err != nil {
		t.Fatal(err)
	}
	var found string
	for _, line := range strings.Split(string(body), "\n") {
		if strings.HasSuffix(line, ".md") {
			found = line
		}
	}
	if found == "" {
		t.Fatalf("no markdown path\n%s", body)
	}
	return found
}
