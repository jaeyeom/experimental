package tui

import (
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
	"github.com/muesli/termenv"
)

// pickConfig points the config at a temp dir holding data, or nothing
// when data is empty, and returns the config path.
func pickConfig(t *testing.T, data string) string {
	t.Helper()
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if data == "" {
		return path
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(data), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

// opsItem opens one item owned by ops/x at 100x40.
func opsItem(t *testing.T, rules ...decide.AvatarRule) *Model {
	t.Helper()
	t.Setenv("NO_COLOR", "")
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "ops/x",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Now = func() time.Time { return avatarNow }
	m.Avatars = rules
	m.avatarProfile = termenv.Ascii
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 40
	press(t, m, "o")
	return m
}

// pickEntry moves the picker cursor to entry and presses Enter.
func pickEntry(t *testing.T, m *Model, entry string) {
	t.Helper()
	i := slices.Index(avatarPickEntries(), entry)
	if i < 0 {
		t.Fatalf("no entry %q", entry)
	}
	m.avatarCursor = i
	press(t, m, "\r")
}

func TestAvatarPickerShowsEveryFigure(t *testing.T) {
	pickConfig(t, "")
	m := opsItem(t, decide.AvatarRule{Match: "ops/x", Builtin: "owl"})
	m.height = 0
	press(t, m, "A")
	if !m.avatarPick {
		t.Fatalf("A did not open the picker, status %q", m.status)
	}
	view := visibleLine(m.View())
	for _, id := range append(avatar.BuiltinIDs(), "default", "none") {
		if !strings.Contains(view, " "+id) && !strings.Contains(view, "*"+id) {
			t.Fatalf("picker has no %q\n%s", id, view)
		}
	}
	if !strings.Contains(view, ">*owl") {
		t.Fatalf("owl is not marked current under the cursor\n%s", view)
	}
	owl, err := avatar.Builtin("owl", avatarCols, avatarRows, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	for _, row := range owl {
		if !strings.Contains(view, row) {
			t.Fatalf("picker has no owl row %q\n%s", row, view)
		}
	}
}

func TestAvatarPickerPicksOwl(t *testing.T) {
	path := pickConfig(t, `{"other": [3, 1]}`)
	m := opsItem(t)
	press(t, m, "A")
	pickEntry(t, m, "owl")
	if m.avatarPick {
		t.Fatal("picker still open")
	}
	if m.status != "avatar for ops/x: owl (saved to config)" {
		t.Fatalf("status %q", m.status)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	want := "{\n  \"other\": [\n    3,\n    1\n  ],\n  \"avatars\": [\n    {\n      \"match\": \"ops/x\",\n      \"builtin\": \"owl\"\n    }\n  ]\n}\n"
	if string(data) != want {
		t.Fatalf("config =\n%s", data)
	}
	if _, err := os.Stat(path + ".bak"); err != nil {
		t.Fatalf("no .bak: %v", err)
	}
	owl, _ := avatar.Builtin("owl", avatarCols, avatarRows, termenv.Ascii)
	if got := m.avatarLines("ops/x"); !slices.Equal(got, owl) {
		t.Fatalf("avatar after pick = %q", got)
	}
	if !strings.Contains(visibleLine(m.View()), owl[1]) {
		t.Fatalf("item view does not show the owl\n%s", visibleLine(m.View()))
	}
}

func TestAvatarPickerNoneHides(t *testing.T) {
	pickConfig(t, `{"avatars": [{"match": "ops/x", "builtin": "owl"}]}`)
	m := opsItem(t, decide.AvatarRule{Match: "ops/x", Builtin: "owl"})
	press(t, m, "A")
	pickEntry(t, m, "none")
	if m.hasAvatar("ops/x") || m.avatarLines("ops/x") != nil {
		t.Fatal("avatar still shown after none")
	}
	press(t, m, "A")
	if got := avatarPickEntries()[m.avatarCursor]; got != "none" {
		t.Fatalf("cursor opens on %q, want none", got)
	}
}

func TestAvatarPickerDefaultFallsBack(t *testing.T) {
	path := pickConfig(t, `{"avatars": [{"match": "ops/x", "builtin": "owl"}, {"match": "ops/*", "builtin": "cat"}]}`)
	m := opsItem(t, decide.AvatarRule{Match: "ops/x", Builtin: "owl"}, decide.AvatarRule{Match: "ops/*", Builtin: "cat"})
	press(t, m, "A")
	pickEntry(t, m, "default")
	if r, _ := decide.MatchAvatar(m.Avatars, "ops/x"); r.Builtin != "cat" {
		t.Fatalf("ops/x matches %+v after default", r)
	}
	data, _ := os.ReadFile(path)
	if strings.Contains(string(data), `"ops/x"`) {
		t.Fatalf("exact rule kept:\n%s", data)
	}
}

func TestAvatarPickerBrokenConfig(t *testing.T) {
	broken := `{"avatars": [`
	path := pickConfig(t, broken)
	m := opsItem(t)
	press(t, m, "A")
	pickEntry(t, m, "owl")
	if !strings.HasPrefix(m.status, "avatar for ops/x: ") || !strings.Contains(m.status, "parse decide config") {
		t.Fatalf("status %q", m.status)
	}
	if data, _ := os.ReadFile(path); string(data) != broken {
		t.Fatalf("config changed to %q", data)
	}
	if m.hasAvatar("ops/x") {
		t.Fatal("avatar shown after a failed save")
	}
}

func TestAvatarPickerEscCancels(t *testing.T) {
	path := pickConfig(t, "")
	m := opsItem(t)
	press(t, m, "A")
	press(t, m, "l\x1b")
	if m.avatarPick || m.mode != modeDetail {
		t.Fatalf("picker %v mode %d", m.avatarPick, m.mode)
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("esc wrote the config: %v", err)
	}
}

func TestAvatarPickerMovesAndScrolls(t *testing.T) {
	pickConfig(t, "")
	m := opsItem(t)
	m.height = 20
	press(t, m, "A")
	perRow := m.avatarPerRow()
	if perRow < 2 {
		t.Fatalf("perRow %d", perRow)
	}
	press(t, m, "l")
	m.Update(tea.KeyMsg{Type: tea.KeyRight})
	if m.avatarCursor != 2 {
		t.Fatalf("cursor %d after l and right", m.avatarCursor)
	}
	press(t, m, "j")
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	if m.avatarCursor != 2+2*perRow {
		t.Fatalf("cursor %d after j and down", m.avatarCursor)
	}
	press(t, m, "kh")
	m.Update(tea.KeyMsg{Type: tea.KeyUp})
	m.Update(tea.KeyMsg{Type: tea.KeyLeft})
	if m.avatarCursor != 0 {
		t.Fatalf("cursor %d after moving back", m.avatarCursor)
	}
	entries := avatarPickEntries()
	last := entries[len(entries)-1]
	view := m.View()
	if lines := viewLines(view); len(lines) > m.height {
		t.Fatalf("view is %d lines, height %d", len(lines), m.height)
	}
	if strings.Contains(visibleLine(view), " "+last) {
		t.Fatalf("last entry %q fits at height 20; the test needs scrolling", last)
	}
	for range len(entries) / perRow {
		press(t, m, "j")
	}
	press(t, m, strings.Repeat("l", perRow))
	view = visibleLine(m.View())
	if !strings.Contains(view, ">"+" "+last) && !strings.Contains(view, "> "+last) {
		t.Fatalf("cursor on %q is not visible\n%s", entries[m.avatarCursor], view)
	}
	if strings.Contains(view, " default") {
		t.Fatalf("first row still shown after scrolling\n%s", view)
	}
}

func TestHubPickerSavesOnTheHub(t *testing.T) {
	path := pickConfig(t, `{"zeta": true}`)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	hub := &hubAvatars{settings: decide.DisplaySettings{Keys: map[string]json.RawMessage{
		"avatars": json.RawMessage(`[{"match":"ops/x","builtin":"owl"}]`),
	}}}
	m := opsItem(t)
	m.backend = hub
	press(t, m, "A")
	pickEntry(t, m, "owl")
	if hub.name != "ops/x" || hub.choice != "owl" {
		t.Fatalf("set-avatar name %q choice %q", hub.name, hub.choice)
	}
	if m.status != "avatar for ops/x: owl (saved on hub)" {
		t.Fatalf("status %q", m.status)
	}
	if len(m.Avatars) != 1 || m.Avatars[0].Builtin != "owl" {
		t.Fatalf("avatars = %+v", m.Avatars)
	}
	if got, err := os.ReadFile(path); err != nil || string(got) != string(before) {
		t.Fatalf("client config = %s, %v", got, err)
	}
}

func TestHubPickerNotesALocalRule(t *testing.T) {
	pickConfig(t, `{"avatars":[{"match":"ops/x","builtin":"cat"}]}`)
	local, err := decide.LoadConfigFile()
	if err != nil {
		t.Fatal(err)
	}
	hub := &hubAvatars{settings: decide.DisplaySettings{Keys: map[string]json.RawMessage{
		"avatars": json.RawMessage(`[{"match":"ops/x","builtin":"owl"}]`),
	}}}
	m := opsItem(t, decide.AvatarRule{Match: "ops/x", Builtin: "cat"})
	m.backend = hub
	m.localConfig = local
	press(t, m, "A")
	pickEntry(t, m, "owl")
	if hub.name != "ops/x" || hub.choice != "owl" {
		t.Fatalf("set-avatar name %q choice %q", hub.name, hub.choice)
	}
	if m.status != "avatar for ops/x: owl (saved on hub; a local rule for ops/x overrides it here)" {
		t.Fatalf("status %q", m.status)
	}
	if len(m.Avatars) != 1 || m.Avatars[0].Builtin != "cat" {
		t.Fatalf("avatars = %+v", m.Avatars)
	}
}

func TestHubPickerKeepsAvatarsWhenTheHubIsOld(t *testing.T) {
	pickConfig(t, "")
	hub := &hubAvatars{err: errors.New(`hub me@hub: unknown op "set-avatar"; update decide on the hub`)}
	m := opsItem(t, decide.AvatarRule{Match: "ops/x", Builtin: "cat"})
	m.backend = hub
	press(t, m, "A")
	pickEntry(t, m, "owl")
	if !strings.Contains(m.status, "update decide on the hub") {
		t.Fatalf("status %q", m.status)
	}
	if len(m.Avatars) != 1 || m.Avatars[0].Builtin != "cat" {
		t.Fatalf("avatars = %+v", m.Avatars)
	}
}

type hubAvatars struct {
	*LocalBackend
	name     string
	choice   string
	settings decide.DisplaySettings
	err      error
}

func (h *hubAvatars) SetAvatar(name, choice string) (decide.DisplaySettings, error) {
	h.name, h.choice = name, choice
	return h.settings, h.err
}

func TestSaveAvatarRejectsUnknownID(t *testing.T) {
	path := pickConfig(t, "")
	if _, err := SaveAvatar("ops/x", "nope"); err == nil {
		t.Fatal("no error for an unknown id")
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("config written: %v", err)
	}
}
