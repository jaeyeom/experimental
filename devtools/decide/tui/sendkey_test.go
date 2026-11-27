package tui

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// sendResult is what a send left behind: the stored item, the pane
// commands run, and whether a composer is still focused.
func sendResult(t *testing.T, m *Model, led *decide.Ledger, id string, sends int) string {
	t.Helper()
	it, err := led.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	var msgs []string
	for _, msg := range it.Messages {
		msgs = append(msgs, msg.Text)
	}
	answer := ""
	if it.Answer != nil {
		answer = it.Answer.Value
	}
	_, focused := m.composerKind()
	return fmt.Sprintf("state=%s outcome=%q answer=%q messages=%q sends=%d focused=%v composing=%v",
		it.State, it.Outcome, answer, msgs, sends, focused, m.composing)
}

// typeDraft types "hi" into the focused composer and returns a counter
// of the pane commands the model runs.
func typeDraft(t *testing.T, m *Model) *int {
	t.Helper()
	sends := new(int)
	m.runSend = func([]string) error {
		*sends++
		return nil
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("hi")})
	return sends
}

func TestSendKeysSendFromEveryComposer(t *testing.T) {
	t.Parallel()
	// The reference is what the composer already does to send: Tab then
	// Enter in the note and reply composers, Enter in the others.
	reference := map[composerKind][]tea.KeyMsg{
		composerReply:   {{Type: tea.KeyTab}, {Type: tea.KeyEnter}},
		composerAnswer:  {{Type: tea.KeyEnter}},
		composerOutcome: {{Type: tea.KeyEnter}},
		composerNote:    {{Type: tea.KeyTab}, {Type: tea.KeyEnter}},
	}
	sendKeys := map[string]tea.KeyMsg{
		"Alt+Enter": {Type: tea.KeyEnter, Alt: true},
		"C-s":       {Type: tea.KeyCtrlS},
	}
	for kind, ref := range reference {
		m, led, item := composerModel(t, kind)
		sends := typeDraft(t, m)
		before := sendResult(t, m, led, item.ID, *sends)
		for _, k := range ref {
			sendKey(t, m, k)
		}
		want := sendResult(t, m, led, item.ID, *sends)
		if want == before {
			t.Fatalf("composer %v: reference keys did not send: %s", kind, want)
		}
		for name, k := range sendKeys {
			m, led, item := composerModel(t, kind)
			sends := typeDraft(t, m)
			sendKey(t, m, k)
			if got := sendResult(t, m, led, item.ID, *sends); got != want {
				t.Errorf("composer %v %s:\n got %s\nwant %s", kind, name, got, want)
			}
		}
	}
}

func TestEnterAndCtrlJDoNotSendNoteOrReply(t *testing.T) {
	t.Parallel()
	for _, kind := range []composerKind{composerNote, composerReply} {
		for name, k := range map[string]tea.KeyMsg{
			"Enter": {Type: tea.KeyEnter},
			"C-j":   {Type: tea.KeyCtrlJ},
		} {
			m, led, item := composerModel(t, kind)
			sends := typeDraft(t, m)
			sendKey(t, m, k)
			_, text := m.composerField()
			if *sends != 0 || text == nil || *text != "hi\n" {
				t.Errorf("%v %s: sends=%d, want a newline and no send", kind, name, *sends)
			}
			if got, ok := m.composerKind(); !ok || got != kind {
				t.Errorf("%v: %s left the composer", kind, name)
			}
			if got, err := led.Get(item.ID); err != nil || len(got.Messages) != 1 {
				t.Errorf("%v %s: messages %+v err %v", kind, name, got.Messages, err)
			}
		}
	}
}

func TestReplySendKeysSendMultiLineText(t *testing.T) {
	t.Parallel()
	for name, keys := range map[string][]tea.KeyMsg{
		"Alt+Enter":  {{Type: tea.KeyEnter, Alt: true}},
		"C-s":        {{Type: tea.KeyCtrlS}},
		"Tab [Send]": {{Type: tea.KeyTab}, {Type: tea.KeyEnter}},
	} {
		m, led, item := composerModel(t, composerReply)
		press(t, m, "abc")
		sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
		press(t, m, "def")
		for _, k := range keys {
			sendKey(t, m, k)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		last := got.Messages[len(got.Messages)-1]
		if last.Text != "abc\ndef" || m.reply != "" || m.replyFocus || m.replySend {
			t.Errorf("%s: last %q reply %q focus %v send %v", name, last.Text, m.reply, m.replyFocus, m.replySend)
		}
	}
}

func TestCtrlSOutsideComposerDoesNothing(t *testing.T) {
	t.Parallel()
	m, led, item := composerModel(t, composerReply)
	sends := typeDraft(t, m)
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEsc})
	before := sendResult(t, m, led, item.ID, *sends)
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlS})
	if got := sendResult(t, m, led, item.ID, *sends); got != before {
		t.Fatalf("C-s with no composer focused: %s, want %s", got, before)
	}
}

func TestComposerHintsListSendKeys(t *testing.T) {
	t.Parallel()
	m, _, _ := composerModel(t, composerNote)
	m.width = 160
	m.height = 30
	m.status = ""
	footer := visibleLine(lineContaining(m.View(), "C-k kill"))
	if strings.Contains(footer, "Ctrl+Enter / Alt+Enter send") {
		t.Fatalf("note footer still duplicates send button hint: %q", footer)
	}
	if !strings.Contains(visibleLine(m.View()), "[Send (^⏎)]") {
		t.Fatalf("note view missing [Send (^⏎)]\n%s", visibleLine(m.View()))
	}

	m, _, _ = composerModel(t, composerReply)
	m.height = 60
	press(t, m, "\x1b?")
	help := visibleLine(m.View())
	if !strings.Contains(help, "Ctrl+Enter, Alt+Enter or C-s sends the draft") {
		t.Fatalf("help missing the send keys\n%s", help)
	}
}

// plainEnterSend reports a hint that plain Enter sends, which the note
// composer must not show. "Alt+Enter send" does not count.
func plainEnterSend(view string) bool {
	return strings.Contains(strings.ReplaceAll(view, "Alt+Enter send", ""), "Enter send")
}

func TestKeyDebugPath(t *testing.T) {
	t.Parallel()
	for v, want := range map[string]string{
		"":               "",
		"0":              "",
		"1":              filepath.Join(os.TempDir(), "decide-keys.log"),
		"/tmp/keys.log":  "/tmp/keys.log",
		"relative/k.log": "relative/k.log",
	} {
		if got := keyDebugPath(v); got != want {
			t.Errorf("keyDebugPath(%q) = %q, want %q", v, got, want)
		}
	}
}

func TestKeyDebugLogsKeyMessages(t *testing.T) {
	t.Parallel()
	m, _, _ := composerModel(t, composerNote)
	var log bytes.Buffer
	m.keyLog = &log
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter, Alt: true})
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlJ})
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("é")})
	got := log.String()
	for _, want := range []string{
		`key type=13 (enter) alt=true runes="" str="alt+enter"`,
		`key type=10 (ctrl+j) alt=false runes="" str="ctrl+j"`,
		`key type=-1 (runes) alt=false runes="é" str="é"`,
	} {
		if !strings.Contains(got, want+"\n") {
			t.Errorf("key log missing %q\n%s", want, got)
		}
	}
}

// TestRunKeyDebugLogsCtrlEnter feeds the Ctrl+Enter sequences some
// terminals send through a real program. The log shows the bytes the
// input reader turned into Alt+Enter, and the key bubbletea then saw.
func TestRunKeyDebugLogsCtrlEnter(t *testing.T) {
	logPath := filepath.Join(t.TempDir(), "keys.log")
	t.Setenv("DECIDE_KEYDEBUG", logPath)
	r, w := io.Pipe()
	var out bytes.Buffer
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{Dir: t.TempDir(), In: r, Out: &out, Interval: time.Hour})
	}()
	if _, err := w.Write([]byte("\x1b[13;5u\x1b[27;5;13~q")); err != nil {
		t.Fatal(err)
	}
	if err := w.Close(); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("inbox did not quit")
	}
	body, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		`ctrl+enter bytes="\x1b[13;5u" as alt+enter`,
		`ctrl+enter bytes="\x1b[27;5;13~" as alt+enter`,
		`key type=13 (enter) alt=true runes="" str="alt+enter"`,
		`key type=-1 (runes) alt=false runes="q" str="q"`,
	} {
		if !strings.Contains(string(body), want) {
			t.Errorf("key log missing %q\n%s", want, body)
		}
	}
	if strings.Contains(string(body), "unknown") {
		t.Errorf("Ctrl+Enter reached bubbletea as an unknown sequence\n%s", body)
	}
}
