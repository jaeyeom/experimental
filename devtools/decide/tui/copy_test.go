package tui

import (
	"bytes"
	"encoding/base64"
	"fmt"
	"strings"
	"testing"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestCopyMenuStoredText(t *testing.T) {
	now := time.Date(2026, 11, 11, 12, 0, 0, 0, time.UTC)
	first := decide.Message{
		Author: "ada",
		Text:   "first\nline",
		Time:   time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	}
	second := decide.Message{
		Author: "bea",
		By:     "ops/bot",
		Text:   "second note",
		Time:   time.Date(2026, 11, 11, 11, 30, 0, 0, time.UTC),
	}
	question := "what now?\nkeep this line"
	m := New(t.TempDir())
	m.mode = modeDetail
	m.UTC = true
	m.width = 80
	m.height = 24
	m.Now = func() time.Time { return now }
	m.detail = &decide.Item{
		ID:       "D-123",
		Question: question,
		Messages: []decide.Message{first, second},
		Answer:   &decide.Answer{Value: "ship it"},
	}
	var out bytes.Buffer
	var clipped string
	m.out = &out
	m.copyClip = func(text string) error {
		clipped = text
		return nil
	}

	if !strings.Contains(m.helpText(), "Y copy menu") || !strings.Contains(m.helpText(), "V plain view") {
		t.Fatalf("help missing copy keys\n%s", m.helpText())
	}

	press(t, m, "Y")
	if !m.copyMenu || m.status != "" || out.Len() != 0 {
		t.Fatalf("menu open %v status %q out %q", m.copyMenu, m.status, out.String())
	}
	if !strings.Contains(m.View(), "m  message") {
		t.Fatalf("menu view\n%s", m.View())
	}
	quit, err := m.Key('z')
	if quit || err != nil || !m.copyMenu || m.status != "unknown key, ? for help" {
		t.Fatalf("unknown quit %v err %v menu %v status %q", quit, err, m.copyMenu, m.status)
	}
	quit, err = m.Key('\x1b')
	if quit || err != nil || m.copyMenu || out.Len() != 0 {
		t.Fatalf("esc quit %v err %v menu %v out %q", quit, err, m.copyMenu, out.String())
	}

	wantThread := "ada 2026-11-01 00:00\nfirst\nline\n\nops/bot 11:30 · 30m ago\nsecond note"
	checks := []struct {
		keys string
		kind string
		text string
	}{
		{"Ym", "message", "first\nline"},
		{"Yq", "question", question},
		{"Yt", "thread", wantThread},
		{"Yi", "id", "D-123"},
		{"Ya", "answer", "ship it"},
	}
	for _, tc := range checks {
		out.Reset()
		clipped = ""
		press(t, m, tc.keys)
		assertCleanCopy(t, tc.kind, tc.text, m.status, out.String(), clipped)
		if m.copyMenu {
			t.Fatalf("%s left the menu open", tc.keys)
		}
	}

	out.Reset()
	clipped = ""
	press(t, m, "nYm")
	assertCleanCopy(t, "message", "second note", m.status, out.String(), clipped)

	m.message = 9
	out.Reset()
	clipped = ""
	press(t, m, "Ym")
	assertCleanCopy(t, "message", "second note", m.status, out.String(), clipped)

	m.detail.Answer = nil
	out.Reset()
	press(t, m, "Ya")
	if m.status != "no answer" || out.Len() != 0 {
		t.Fatalf("unanswered status %q out %q", m.status, out.String())
	}

	m.detail.Answer = &decide.Answer{}
	out.Reset()
	press(t, m, "Ya")
	if m.status != "nothing to copy" || out.Len() != 0 {
		t.Fatalf("empty answer status %q out %q", m.status, out.String())
	}

	m.detail.Messages[0].Text = ""
	m.message = 0
	out.Reset()
	press(t, m, "Ym")
	if m.status != "nothing to copy" || out.Len() != 0 {
		t.Fatalf("empty message status %q out %q", m.status, out.String())
	}
}

func TestClipboardModes(t *testing.T) {
	big := strings.Repeat("a", osc52MaxBytes+1)
	exact := strings.Repeat("b", osc52MaxBytes)
	cases := []struct {
		name   string
		mode   string
		text   string
		out    bool
		clip   bool
		fail   bool
		status string
	}{
		{name: "auto", mode: decide.ClipboardAuto, text: "note", out: true, clip: true},
		{name: "empty is auto", text: "note", out: true, clip: true},
		{name: "osc52", mode: decide.ClipboardOSC52, text: "note", out: true},
		{name: "command", mode: decide.ClipboardCommand, text: "note", clip: true},
		{name: "off", mode: decide.ClipboardOff, text: "note", status: "copy is disabled"},
		{name: "auto over limit", mode: decide.ClipboardAuto, text: big, clip: true},
		{name: "auto over limit no command", mode: decide.ClipboardAuto, text: big, fail: true, status: "text is too large"},
		{name: "auto at limit", mode: decide.ClipboardAuto, text: exact, out: true, clip: true},
		{name: "osc52 over limit", mode: decide.ClipboardOSC52, text: big, status: "text is too large"},
		{name: "command over limit", mode: decide.ClipboardCommand, text: big, clip: true},
		{name: "auto command fails", mode: decide.ClipboardAuto, text: "note", out: true, clip: true, fail: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			m, out, calls := clipboardFixture(t, tc.text)
			m.clipboard = tc.mode
			if tc.fail {
				m.copyClip = func(string) error { return fmt.Errorf("clipboard unavailable") }
			}
			press(t, m, "Ym")
			got, ok := osc52Text(out.String())
			if tc.out {
				if !ok || got != tc.text || !strings.Contains(out.String(), osc52Sequence(tc.text)) {
					t.Fatalf("osc ok %v text %q out %q", ok, got, out.String())
				}
			} else if out.Len() != 0 {
				t.Fatalf("wrote %q", out.String())
			}
			if tc.clip && !tc.fail && *calls != 1 {
				t.Fatalf("clip calls %d", *calls)
			}
			if !tc.clip && !tc.fail && *calls != 0 {
				t.Fatalf("clip calls %d", *calls)
			}
			want := tc.status
			if want == "" {
				want = fmt.Sprintf("copied message (%d chars)", utf8.RuneCountInString(tc.text))
			}
			if m.status != want {
				t.Fatalf("status %q, want %q", m.status, want)
			}
		})
	}

	m, out, calls := clipboardFixture(t, "note")
	m.out = nil
	m.clipboard = decide.ClipboardOSC52
	press(t, m, "Ym")
	if m.status != "could not write OSC 52" || out.Len() != 0 || *calls != 0 {
		t.Fatalf("status %q out %d calls %d", m.status, out.Len(), *calls)
	}

	m, out, _ = clipboardFixture(t, "note")
	m.clipboard = "paste"
	press(t, m, "Ym")
	if out.Len() != 0 || !strings.Contains(m.status, "ui.clipboard") || !strings.Contains(m.status, "paste") {
		t.Fatalf("status %q out %q", m.status, out.String())
	}

	m = New(t.TempDir())
	m.mode = modeDetail
	m.detail = &decide.Item{
		ID:       "D-1",
		Question: "q",
		Links:    []string{"https://example.test/only"},
	}
	var linkOut bytes.Buffer
	var linkCalls int
	var linkText string
	m.out = &linkOut
	m.copyClip = func(got string) error {
		linkCalls++
		linkText = got
		return nil
	}
	m.clipboard = decide.ClipboardOff
	press(t, m, "L")
	if m.status != "copy is disabled" || linkOut.Len() != 0 || linkCalls != 0 {
		t.Fatalf("link off status %q out %q calls %d", m.status, linkOut.String(), linkCalls)
	}
	m.clipboard = decide.ClipboardCommand
	press(t, m, "L")
	if m.status != "copied" || linkOut.Len() != 0 || linkCalls != 1 || linkText != "https://example.test/only" {
		t.Fatalf("link command status %q out %q calls %d text %q", m.status, linkOut.String(), linkCalls, linkText)
	}
}

func TestPlainViewGolden(t *testing.T) {
	now := time.Date(2026, 11, 11, 0, 0, 0, 0, time.UTC)
	long := strings.Repeat("x", 50)
	m := New(t.TempDir())
	m.mode = modeDetail
	m.UTC = true
	m.Now = func() time.Time { return now }
	m.width = 40
	m.height = 12
	m.detail = &decide.Item{
		ID:       "D-123",
		Question: "why so long?",
		Messages: []decide.Message{
			{Author: "ada", Text: "line one\n" + long, Time: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)},
			{By: "ops/bot", Text: "second", Time: time.Date(2026, 11, 1, 1, 0, 0, 0, time.UTC)},
		},
	}
	press(t, m, "V")
	if !m.plain {
		t.Fatal("plain view off")
	}
	view := m.View()
	if strings.ContainsAny(view, "│─┌┐└┘") {
		t.Fatalf("plain view has a border\n%s", view)
	}
	if !strings.Contains(view, long) {
		t.Fatalf("long line was wrapped or cut\n%s", view)
	}
	checkGolden(t, "plain_view_40.golden", view)

	press(t, m, "V")
	if m.plain {
		t.Fatal("second V left plain view on")
	}
	m.width = 80
	m.height = 24
	if !strings.Contains(m.View(), "┌") {
		t.Fatal("framed view missing its border")
	}

	m.detail.Messages = nil
	m.width = 40
	m.height = 12
	press(t, m, "V")
	plainQ := m.View()
	if !strings.Contains(plainQ, "why so long?") || strings.ContainsAny(plainQ, "│─┌┐└┘") {
		t.Fatalf("question view\n%s", plainQ)
	}
	press(t, m, "u")
	if m.plain || m.copyMenu || m.mode != modeList {
		t.Fatalf("back plain %v menu %v mode %v", m.plain, m.copyMenu, m.mode)
	}
}

func clipboardFixture(t *testing.T, text string) (*Model, *bytes.Buffer, *int) {
	t.Helper()
	m := New(t.TempDir())
	m.mode = modeDetail
	m.detail = &decide.Item{
		ID:       "D-1",
		Question: "q",
		Messages: []decide.Message{{Author: "ada", Text: text}},
	}
	var out bytes.Buffer
	calls := 0
	m.out = &out
	m.copyClip = func(got string) error {
		calls++
		if got != text {
			t.Fatalf("clipped %q", got)
		}
		return nil
	}
	return m, &out, &calls
}

func assertCleanCopy(t *testing.T, kind, text, status, out, clipped string) {
	t.Helper()
	want := fmt.Sprintf("copied %s (%d chars)", kind, utf8.RuneCountInString(text))
	got, ok := osc52Text(out)
	if status != want || !ok || got != text || clipped != text {
		t.Fatalf("%s status %q ok %v osc %q clip %q", kind, status, ok, got, clipped)
	}
	if !strings.Contains(out, osc52Sequence(text)) {
		t.Fatalf("%s sequence %q", kind, out)
	}
	for _, bad := range []string{"│", "─", "┌", "┐", "└", "┘", "\x1b"} {
		if strings.Contains(text, bad) {
			t.Fatalf("%s copied %q", kind, bad)
		}
	}
}

func osc52Sequence(text string) string {
	return "\x1b]52;c;" + base64.StdEncoding.EncodeToString([]byte(text)) + "\x07"
}
