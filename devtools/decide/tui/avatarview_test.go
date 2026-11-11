package tui

import (
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
	"github.com/muesli/termenv"
)

// updateGolden is -update, registered in TestMain.
var updateGolden *bool

var avatarNow = time.Date(2026, 11, 11, 0, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

// checkGolden compares got with testdata/name. -update rewrites the file.
func checkGolden(t *testing.T, name, got string) {
	t.Helper()
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
		t.Fatal(err)
	}
	if got != string(want) {
		t.Fatalf("view differs from %s\ngot:\n%s\nwant:\n%s", path, got, want)
	}
}

// avatarItemModel opens one item owned by a/x at width columns. setup runs
// before the first view.
func avatarItemModel(t *testing.T, width int, setup func(*Model)) *Model {
	t.Helper()
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question:  "ship it?",
		Owner:     "a/x",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC),
	})
	m := New(dir)
	m.Now = func() time.Time { return avatarNow }
	if setup != nil {
		setup(m)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = width
	m.height = 30
	press(t, m, "o")
	return m
}

// avatarComposeModel opens compose with recipients a/x, coord/primary, and
// none.
func avatarComposeModel(t *testing.T, setup func(*Model)) *Model {
	t.Helper()
	m := New(t.TempDir())
	m.Now = func() time.Time { return avatarNow }
	m.Targets = []Target{
		{Name: "a/x", Pane: "w1:p1"},
		{Name: "coord/primary", Pane: "w1:p2"},
		{Name: "none", Pane: "w1:p3"},
	}
	if setup != nil {
		setup(m)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 30
	press(t, m, "c")
	return m
}

func TestAvatarPlainGoldens(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	checkGolden(t, "item_plain_100.golden", avatarItemModel(t, 100, nil).View())
	checkGolden(t, "item_plain_50.golden", avatarItemModel(t, 50, nil).View())
	checkGolden(t, "compose_plain_100.golden", avatarComposeModel(t, nil).View())
}

// writeAvatar writes a 10x6 green text avatar of ch and returns its path.
func writeAvatar(t *testing.T, ch string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), ch+".txt")
	row := "\x1b[32m" + strings.Repeat(ch, avatarCols) + "\x1b[0m\n"
	if err := os.WriteFile(path, []byte(strings.Repeat(row, avatarRows)), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func withAvatars(rules ...decide.AvatarRule) func(*Model) {
	return func(m *Model) {
		m.Avatars = rules
		m.avatarProfile = termenv.ANSI
	}
}

func viewLines(view string) []string {
	return strings.Split(view, "\n")
}

func TestItemViewAvatarBesideHeader(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	m := avatarItemModel(t, 100, withAvatars(decide.AvatarRule{Match: "a/x", File: a}))
	view := m.View()
	checkGolden(t, "item_avatar_100.golden", view)
	lines := viewLines(view)
	// Row 0 is the frame and row 1 the item box top.
	for i := 2; i < 2+avatarRows; i++ {
		vis := strings.TrimPrefix(visibleLine(lines[i]), "││")
		if !strings.HasPrefix(vis, strings.Repeat("A", avatarCols)+" ") {
			t.Fatalf("row %d does not start with the avatar\n%s", i, visibleLine(view))
		}
	}
	if !strings.Contains(visibleLine(lines[3]), "AAAAAAAAAA     From: a/x") {
		t.Fatalf("fields are not right of the avatar\n%s", visibleLine(view))
	}
}

func TestItemViewWithoutAvatarIsUnchanged(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	checkGolden(t, "item_plain_100.golden", avatarItemModel(t, 100, withAvatars(decide.AvatarRule{Match: "b/*", File: a})).View())
	checkGolden(t, "item_plain_50.golden", avatarItemModel(t, 50, withAvatars(decide.AvatarRule{Match: "a/x", File: a})).View())
}

func TestItemViewAvatarDefault(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	m := avatarItemModel(t, 100, func(m *Model) {
		m.AvatarDefault = true
		m.avatarProfile = termenv.ANSI
	})
	want := avatar.ForName("a/x", avatarCols, avatarRows, termenv.ANSI)
	assertAvatarRows(t, m.View(), want)
}

// assertAvatarRows fails unless every row of av shows in view, in order,
// each followed by the gutter.
func assertAvatarRows(t *testing.T, view string, av []string) {
	t.Helper()
	lines := viewLines(view)
	for i, row := range av {
		if !strings.Contains(lines[2+i], row+" ") {
			t.Fatalf("avatar row %d %q missing\n%s", i, row, view)
		}
	}
}

func builtinLines(t *testing.T, id string, profile termenv.Profile) []string {
	t.Helper()
	lines, err := avatar.Builtin(id, avatarCols, avatarRows, profile)
	if err != nil {
		t.Fatal(err)
	}
	return lines
}

func TestItemViewAvatarBuiltinRule(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	m := avatarItemModel(t, 100, func(m *Model) {
		withAvatars(decide.AvatarRule{Match: "a/x", Builtin: "owl"})(m)
		m.AvatarDefault = true
	})
	assertAvatarRows(t, m.View(), builtinLines(t, "owl", termenv.ANSI))
}

func TestItemViewAvatarBuiltinPrecedence(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	exactBuiltin := avatarItemModel(t, 100, withAvatars(
		decide.AvatarRule{Match: "a/*", File: a},
		decide.AvatarRule{Match: "a/x", Builtin: "owl"},
	))
	assertAvatarRows(t, exactBuiltin.View(), builtinLines(t, "owl", termenv.ANSI))

	exactFile := avatarItemModel(t, 100, withAvatars(
		decide.AvatarRule{Match: "a/*", Builtin: "owl"},
		decide.AvatarRule{Match: "a/x", File: a},
	))
	if view := exactFile.View(); !strings.Contains(view, "AAAAAAAAAA") {
		t.Fatalf("exact file rule lost to a glob built-in\n%s", view)
	}

	firstGlob := avatarItemModel(t, 100, withAvatars(
		decide.AvatarRule{Match: "a/*", Builtin: "crab"},
		decide.AvatarRule{Match: "*", Builtin: "owl"},
	))
	assertAvatarRows(t, firstGlob.View(), builtinLines(t, "crab", termenv.ANSI))
}

func TestItemViewAvatarBuiltinUnknown(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	m := avatarItemModel(t, 100, func(m *Model) {
		withAvatars(decide.AvatarRule{Match: "a/x", Builtin: "no-such-figure"})(m)
		m.AvatarDefault = true
	})
	got := m.View()
	status := screenRow(got, "avatar for a/x:")
	if status == 0 {
		t.Fatalf("no avatar error in the status line\n%s", visibleLine(got))
	}
	if !strings.Contains(got, "no-such-figure") {
		t.Fatalf("status does not name the id\n%s", visibleLine(got))
	}
	want := viewLines(avatarItemModel(t, 100, nil).View())
	for i, line := range viewLines(got) {
		if i != status-1 && line != want[i] {
			t.Fatalf("row %d differs\ngot:  %q\nwant: %q", i, line, want[i])
		}
	}
	m.status = ""
	if strings.Contains(m.View(), "avatar for") {
		t.Fatal("the avatar error showed twice")
	}
}

func TestItemViewAvatarDefaultMonochrome(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	m := avatarItemModel(t, 100, func(m *Model) {
		m.AvatarDefault = true
		m.avatarProfile = termenv.TrueColor
	})
	plain := avatar.ForName("a/x", avatarCols, avatarRows, termenv.Ascii)
	assertAvatarRows(t, m.View(), plain)
	for _, row := range plain {
		if strings.Contains(row, "\x1b") {
			t.Fatalf("NO_COLOR avatar row has escapes: %q", row)
		}
	}

	hidden := avatarItemModel(t, 100, func(m *Model) {
		m.AvatarDefault = true
		m.AvatarMono = decide.AvatarMonoHide
	})
	if strings.Contains(hidden.View(), plain[2]) {
		t.Fatalf("avatarMono hide still shows the built-in under NO_COLOR\n%s", hidden.View())
	}

	t.Setenv("NO_COLOR", "")
	ascii := avatarItemModel(t, 100, func(m *Model) {
		m.AvatarDefault = true
		m.AvatarMono = decide.AvatarMonoHide
		m.avatarProfile = termenv.Ascii
	})
	checkGolden(t, "item_plain_100.golden", ascii.View())
}

func TestItemViewAvatarMonochrome(t *testing.T) {
	a := writeAvatar(t, "A")
	rule := decide.AvatarRule{Match: "a/x", File: a}
	t.Setenv("NO_COLOR", "1")
	view := avatarItemModel(t, 100, withAvatars(rule)).View()
	assertPlainAvatar(t, view)

	ascii := avatarItemModel(t, 100, func(m *Model) {
		withAvatars(rule)(m)
		m.avatarProfile = termenv.Ascii
	})
	t.Setenv("NO_COLOR", "")
	assertPlainAvatar(t, ascii.View())

	hidden := avatarItemModel(t, 100, func(m *Model) {
		withAvatars(rule)(m)
		m.avatarProfile = termenv.Ascii
		m.AvatarMono = decide.AvatarMonoHide
	})
	checkGolden(t, "item_plain_100.golden", hidden.View())
}

func assertPlainAvatar(t *testing.T, view string) {
	t.Helper()
	lines := viewLines(view)
	for i := 2; i < 2+avatarRows; i++ {
		at := strings.Index(lines[i], "AAAAAAAAAA")
		if at < 0 || lines[i][at:at+avatarCols+1] != "AAAAAAAAAA " || strings.Contains(lines[i][at:], "A\x1b") {
			t.Fatalf("avatar row %d has escapes: %q", i, lines[i])
		}
		if prefix := lines[i][:at]; !strings.HasSuffix(prefix, "│\x1b[0m") {
			t.Fatalf("avatar row %d has escapes before it: %q", i, prefix)
		}
	}
}

func TestItemViewMissingAvatarFile(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	missing := filepath.Join(t.TempDir(), "gone.png")
	m := avatarItemModel(t, 100, withAvatars(decide.AvatarRule{Match: "a/x", File: missing}))
	got := viewLines(m.View())
	want := viewLines(avatarItemModel(t, 100, nil).View())
	status := screenRow(strings.Join(got, "\n"), "avatar for a/x:")
	if status == 0 {
		t.Fatalf("no avatar error in the status line\n%s", visibleLine(strings.Join(got, "\n")))
	}
	if n := strings.Count(visibleLine(got[status-1]), "avatar for"); n != 1 {
		t.Fatalf("status shows %d avatar errors: %q", n, got[status-1])
	}
	for i := range want {
		if i != status-1 && got[i] != want[i] {
			t.Fatalf("row %d differs\ngot:  %q\nwant: %q", i, got[i], want[i])
		}
	}
	m.status = ""
	if strings.Contains(m.View(), "avatar for") {
		t.Fatal("the avatar error showed twice")
	}
}

func TestComposeAvatarFollowsSelection(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	c := writeAvatar(t, "C")
	m := avatarComposeModel(t, withAvatars(
		decide.AvatarRule{Match: "a/x", File: a},
		decide.AvatarRule{Match: "coord/*", File: c},
	))
	view := m.View()
	checkGolden(t, "compose_avatar_a.golden", view)
	nameCol := visibleCol(viewLines(view)[screenRow(view, "coord/primary")-1], "coord/primary")

	m.pickerArrow(1)
	view = m.View()
	checkGolden(t, "compose_avatar_coord.golden", view)
	if strings.Contains(view, "AAAA") || !strings.Contains(view, "CCCC") {
		t.Fatalf("avatar did not follow the selection\n%s", visibleLine(view))
	}

	m.pickerArrow(1)
	view = m.View()
	if strings.Contains(view, "AAAA") || strings.Contains(view, "CCCC") {
		t.Fatalf("a recipient with no avatar shows one\n%s", visibleLine(view))
	}
	if got := visibleCol(viewLines(view)[screenRow(view, "coord/primary")-1], "coord/primary"); got != nameCol {
		t.Fatalf("picker shifted from column %d to %d\n%s", nameCol, got, visibleLine(view))
	}
	if screenRow(view, "[Send]") != screenRow(checkView(t, "compose_avatar_a.golden"), "[Send]") {
		t.Fatalf("picker box height changed\n%s", visibleLine(view))
	}

	for _, r := range "coord" {
		m.pickerKey(r)
	}
	if view = m.View(); !strings.Contains(view, "CCCC") {
		t.Fatalf("filtered selection shows no avatar\n%s", visibleLine(view))
	}
}

func checkView(t *testing.T, name string) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join("testdata", name))
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func TestComposeWithoutAvatarIsUnchanged(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	m := avatarComposeModel(t, withAvatars(decide.AvatarRule{Match: "nobody", File: a}))
	checkGolden(t, "compose_plain_100.golden", m.View())
}

func TestComposeClickWithAvatarSelectsRow(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	m := avatarComposeModel(t, withAvatars(decide.AvatarRule{Match: "*", File: a}))
	view := m.View()
	row := screenRow(view, "coord/primary")
	col := visibleCol(viewLines(view)[row-1], "coord/primary")
	if err := m.Click(col, row); err != nil {
		t.Fatal(err)
	}
	if tg, ok := m.selectedTarget(); !ok || tg.Name != "coord/primary" {
		t.Fatalf("click selected %+v", tg)
	}
	view = m.View()
	row = screenRow(view, "none")
	if err := m.Click(4, row); err != nil {
		t.Fatal(err)
	}
	if tg, _ := m.selectedTarget(); tg.Name != "coord/primary" {
		t.Fatalf("a click on the avatar selected %q", tg.Name)
	}
	if err := m.Click(visibleCol(viewLines(view)[row-1], "none"), row); err != nil {
		t.Fatal(err)
	}
	if tg, _ := m.selectedTarget(); tg.Name != "none" {
		t.Fatalf("click selected %q, want none", tg.Name)
	}
}

// countFS reads the local file system and counts opens.
type countFS struct{ opens int }

func (f *countFS) Stat(name string) (fs.FileInfo, error) {
	info, err := os.Stat(name)
	if err != nil {
		return nil, fmt.Errorf("stat: %w", err)
	}
	return info, nil
}

func (f *countFS) Open(name string) (io.ReadCloser, error) {
	f.opens++
	file, err := os.Open(name)
	if err != nil {
		return nil, fmt.Errorf("open: %w", err)
	}
	return file, nil
}

func TestComposeAvatarUsesCache(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	a := writeAvatar(t, "A")
	c := writeAvatar(t, "C")
	files := &countFS{}
	m := avatarComposeModel(t, func(m *Model) {
		withAvatars(
			decide.AvatarRule{Match: "a/x", File: a},
			decide.AvatarRule{Match: "coord/*", File: c},
		)(m)
		m.avatarRender = avatar.NewRenderer(files)
	})
	for range 3 {
		m.pickerArrow(1)
		m.View()
		m.pickerArrow(-1)
		m.View()
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		m.View()
	}
	if files.opens != 2 {
		t.Fatalf("opened avatar files %d times, want 2", files.opens)
	}
}
