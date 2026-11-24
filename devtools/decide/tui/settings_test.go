package tui

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func setupSettingsModel(t *testing.T, configJSON string) (*Model, string) {
	t.Helper()
	t.Setenv("NO_COLOR", "1")
	xdg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", xdg)
	cfgPath := filepath.Join(xdg, "decide", "config.json")
	if configJSON != "" {
		if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(cfgPath, []byte(configJSON), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	dir := t.TempDir()
	m := New(dir)
	m.width, m.height = 180, 30
	m.Machine = "laptop"
	m.Version = "v1.2.3"
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	return m, cfgPath
}

func TestSettingsChordOpensViewAndAppearsInHelp(t *testing.T) {
	m, cfgPath := setupSettingsModel(t, "{}\n")

	// ? help lists g , settings.
	press(t, m, "?")
	help := visibleLine(m.View())
	if !strings.Contains(help, ", settings") {
		t.Fatalf("help missing ', settings':\n%s", help)
	}
	press(t, m, "?")

	// g , opens settings view.
	press(t, m, "g,")
	if m.mode != modeSettings {
		t.Fatalf("mode = %d, want modeSettings (%d)", m.mode, modeSettings)
	}
	view := visibleLine(m.View())
	for _, want := range []string{
		"Settings",
		"ui.answerAction",
		"stay (default)",
		"ui.openAt",
		"new (default)",
		"ui.utc",
		"false (default)",
		"messageAvatars",
		"true (default)",
		"hyperlinks",
		"ui.asciiKeys",
		"ui.orgTree",
		"ui.clipboard",
		"auto (default)",
		"ui.fillColumn",
		"72 (default)",
		cfgPath,
		"machine: laptop",
		"version: v1.2.3",
	} {
		if !strings.Contains(view, want) {
			t.Errorf("settings view missing %q:\n%s", want, view)
		}
	}
}

func TestSettingsToggleBoolWritesFileKeepsUnknownAndOneBak(t *testing.T) {
	orig := "{\n  \"unknownKeep\": \"yes\",\n  \"ui\": {\n    \"utc\": false\n  }\n}\n"
	m, cfgPath := setupSettingsModel(t, orig)

	press(t, m, "g,")
	// Move to ui.utc (index 2: answerAction=0, openAt=1, utc=2).
	press(t, m, "jj")
	// Toggle with space.
	press(t, m, " ")

	if !m.UTC {
		t.Fatalf("m.UTC = false after toggle, want true")
	}
	data, err := os.ReadFile(cfgPath)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), `"unknownKeep": "yes"`) || !strings.Contains(string(data), `"utc": true`) {
		t.Fatalf("config after toggle:\n%s", string(data))
	}
	bak, err := os.ReadFile(cfgPath + ".bak")
	if err != nil {
		t.Fatalf("missing .bak: %v", err)
	}
	if string(bak) != orig {
		t.Fatalf(".bak = %q, want %q", string(bak), orig)
	}
	// Non-default value no longer shows (default).
	view := visibleLine(m.View())
	for _, line := range strings.Split(view, "\n") {
		if strings.Contains(line, "ui.utc") && strings.Contains(line, "(default)") {
			t.Fatalf("non-default ui.utc still shows (default): %q", line)
		}
	}

	// Second toggle must not overwrite .bak.
	press(t, m, "\r")
	if m.UTC {
		t.Fatalf("m.UTC = true after second toggle, want false")
	}
	bak2, err := os.ReadFile(cfgPath + ".bak")
	if err != nil {
		t.Fatal(err)
	}
	if string(bak2) != orig {
		t.Fatalf(".bak overwritten on second toggle: %q", string(bak2))
	}
}

func TestSettingsInvalidConfigShowsErrorAndLeavesFileUnchanged(t *testing.T) {
	bad := `{"ui": {invalid`
	m, cfgPath := setupSettingsModel(t, bad)

	press(t, m, "g,")
	press(t, m, "jj ") // try toggling ui.utc

	if m.UTC {
		t.Fatal("m.UTC changed despite invalid config.json")
	}
	if m.status == "" {
		t.Fatal("expected error in m.status for invalid config.json")
	}
	got, err := os.ReadFile(cfgPath)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != bad {
		t.Fatalf("invalid config.json was modified: %q", string(got))
	}
}

func TestSettingsAnswerActionTakesEffectLive(t *testing.T) {
	m, _ := setupSettingsModel(t, "{}\n")
	led := openLedger(t, m.Dir)
	mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "y", Label: "Yes"}, {Key: "n", Label: "No"}},
	})
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Open settings, cycle ui.answerAction (index 0) from stay -> list.
	press(t, m, "g,\r")
	if m.answerAction != decide.AnswerActionList {
		t.Fatalf("m.answerAction = %q, want %q", m.answerAction, decide.AnswerActionList)
	}

	// Return to list, open the item, answer option 1 -> should return to list immediately!
	press(t, m, "u\r1")
	if m.mode != modeList {
		t.Fatalf("after answering with live ui.answerAction=list, mode = %d, want modeList (%d)", m.mode, modeList)
	}
}

func TestSettingsEnumLeftRightAndNumberInlineEdit(t *testing.T) {
	m, cfgPath := setupSettingsModel(t, "{}\n")
	press(t, m, "g,")

	// Left on ui.answerAction (stay) wraps to edit.
	m.Update(tea.KeyMsg{Type: tea.KeyLeft})
	if m.answerAction != decide.AnswerActionEdit {
		t.Fatalf("left from stay = %q, want edit", m.answerAction)
	}
	// Right wraps back to stay.
	m.Update(tea.KeyMsg{Type: tea.KeyRight})
	if m.answerAction != decide.AnswerActionStay {
		t.Fatalf("right from edit = %q, want stay", m.answerAction)
	}

	// Jump to last setting (ui.fillColumn) with End.
	m.Update(tea.KeyMsg{Type: tea.KeyEnd})
	// Press Enter to start inline edit, backspace twice, type 80, press Enter.
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if !m.settingsEditing {
		t.Fatal("expected settingsEditing = true")
	}
	m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
	m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'8'}})
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'0'}})
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})

	if m.settingsEditing {
		t.Fatal("expected settingsEditing = false after enter")
	}
	if m.fillColumn != 80 {
		t.Fatalf("m.fillColumn = %d, want 80", m.fillColumn)
	}
	cfg, err := decide.LoadConfigAt(cfgPath)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.UI.FillColumn != 80 {
		t.Fatalf("saved fillColumn = %d, want 80", cfg.UI.FillColumn)
	}
}

func TestSettingsHubModeHeaderAndWebFooter(t *testing.T) {
	m, _ := setupSettingsModel(t, "{}\n")
	m.Hub = "dragon"
	m.WebPublicURL = "https://decide.example.com"

	press(t, m, "g,")
	view := visibleLine(m.View())
	for _, want := range []string{
		"edits this machine's config",
		"hub:     dragon",
		"web:     https://decide.example.com",
	} {
		if !strings.Contains(view, want) {
			t.Errorf("hub settings view missing %q:\n%s", want, view)
		}
	}
}

func TestSettingsExternalEditorReloadsConfig(t *testing.T) {
	m, cfgPath := setupSettingsModel(t, "{}\n")

	// Write a tiny editor script that sets ui.utc=true in the edited file.
	script := filepath.Join(t.TempDir(), "edit.sh")
	body := "#!/bin/sh\nprintf '{\"ui\":{\"utc\":true,\"answerAction\":\"edit\"}}\\n' > \"$1\"\n"
	if err := os.WriteFile(script, []byte(body), 0o700); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	t.Setenv("VISUAL", "")
	t.Setenv("EDITOR", script)

	press(t, m, "g,")
	_, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'e'}})
	if cmd == nil || m.configEditor == nil {
		t.Fatal("expected configEditor to start from pressing e in settings")
	}
	err := m.configEditor.Run()
	m.Update(configEditorDoneMsg{err: err})

	if !m.UTC || m.answerAction != decide.AnswerActionEdit {
		t.Fatalf("after $EDITOR reload: UTC=%v answerAction=%q, want true/edit", m.UTC, m.answerAction)
	}
	if _, err := os.Stat(cfgPath); err != nil {
		t.Fatalf("config.json missing after edit: %v", err)
	}

	// If $EDITOR writes invalid JSON, show error and keep previous live settings.
	badScript := filepath.Join(t.TempDir(), "bad.sh")
	if err := os.WriteFile(badScript, []byte("#!/bin/sh\nprintf '{bad' > \"$1\"\n"), 0o700); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	t.Setenv("EDITOR", badScript)
	_, cmd = m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'e'}})
	if cmd == nil || m.configEditor == nil {
		t.Fatal("expected configEditor to start on second e")
	}
	err = m.configEditor.Run()
	m.Update(configEditorDoneMsg{err: err})
	if m.status == "" || m.status == "reloaded config.json" {
		t.Fatalf("expected parse error in status, got %q", m.status)
	}
	if !m.UTC || m.answerAction != decide.AnswerActionEdit {
		t.Fatalf("live settings changed after invalid edit: UTC=%v answerAction=%q", m.UTC, m.answerAction)
	}
}
