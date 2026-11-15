package avatar

import (
	"strings"
	"testing"

	"github.com/muesli/termenv"
)

func TestPixelMapHalfBlocks(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	text := "" +
		"palette\n" +
		"C 106 215 229\n" +
		"K 20 20 20\n" +
		"map\n" +
		".C.C......\n" +
		"..CK......\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n" +
		"..........\n"
	cells, err := decodePixelMap(text)
	if err != nil {
		t.Fatal(err)
	}
	if len(cells) != 6 {
		t.Fatalf("%d rows, want 6", len(cells))
	}
	for _, row := range cells {
		if len(row) != 10 {
			t.Fatalf("row has %d cells, want 10", len(row))
		}
	}
	if cells[0][0].ch != ' ' || cells[0][0].fg != nil || cells[0][0].bg != nil {
		t.Fatalf("transparent cell = %+v", cells[0][0])
	}
	if cells[0][1].ch != '▀' || cells[0][1].fg == nil || *cells[0][1].fg != (rgb{106, 215, 229}) || cells[0][1].bg != nil {
		t.Fatalf("top-only cell = %+v", cells[0][1])
	}
	if cells[0][2].ch != '▄' || cells[0][2].fg == nil || *cells[0][2].fg != (rgb{106, 215, 229}) || cells[0][2].bg != nil {
		t.Fatalf("bottom-only cell = %+v", cells[0][2])
	}
	wantFG, wantBG := rgb{106, 215, 229}, rgb{20, 20, 20}
	if cells[0][3].ch != '▀' || cells[0][3].fg == nil || cells[0][3].bg == nil || *cells[0][3].fg != wantFG || *cells[0][3].bg != wantBG {
		t.Fatalf("pair cell = %+v", cells[0][3])
	}

	lines, err := renderPixelMap(text, 10, 6, termenv.TrueColor)
	if err != nil {
		t.Fatal(err)
	}
	got := parseRender(lines)
	if got[0][1].r != '▀' || got[0][1].hasBG || got[0][1].fg == nil {
		t.Fatalf("truecolor top-only = %+v", got[0][1])
	}
	if got[0][2].r != '▄' || got[0][2].hasBG {
		t.Fatalf("truecolor bottom-only = %+v", got[0][2])
	}
	if got[0][3].r != '▀' || !got[0][3].hasBG || got[0][3].fg == nil {
		t.Fatalf("truecolor pair = %+v", got[0][3])
	}

	plain, err := renderPixelMap(text, 10, 6, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(strings.Join(plain, ""), "\x1b") {
		t.Fatalf("no-colour render has escapes: %q", plain)
	}
	vis := []rune(visibleLines(strings.Join(plain, "\n"))[0])
	if vis[0] != ' ' || vis[1] != '▀' || vis[2] != '▄' || vis[3] != '█' {
		t.Fatalf("no-colour row = %q", string(vis))
	}
}

func TestPixelMapRejectsBadFiles(t *testing.T) {
	palette := "palette\nC 106 215 229\nmap\n"
	body := strings.Repeat(".CCCCCCCC.\n", 12)
	cases := []struct {
		name string
		text string
		want string
	}{
		{name: "size", text: palette + ".CCCCCCCC.\n", want: "1 rows, want 12"},
		{name: "letter", text: palette + strings.Replace(body, ".CCCCCCCC.", ".CQC.CCCC.", 1), want: "unknown letter"},
		{name: "enclosed", text: palette + enclosedHole(), want: "enclosed transparent pixel at 2,3"},
		{name: "gap", text: "palette\nC 1 2 3\ngaps\n0 0\nmap\n" + strings.Repeat("..........\n", 12), want: "not an enclosed transparent pixel"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := decodePixelMap(tc.text)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v, want %q", err, tc.want)
			}
		})
	}
	hole := "palette\nC 106 215 229\ngaps\n2 3\nmap\n" + enclosedHole()
	if _, err := decodePixelMap(hole); err != nil {
		t.Fatal(err)
	}
}

func enclosedHole() string {
	rows := []string{
		"..........",
		".CCCCCCCC.",
		".CC.CCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		".CCCCCCCC.",
		"..........",
	}
	return strings.Join(rows, "\n") + "\n"
}

func TestHandAndGopherArePixelMaps(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	for _, id := range []string{"hand", "gopher"} {
		if _, ok := pixelSource(id); !ok {
			t.Fatalf("%s is not a pixel map", id)
		}
		if _, ok := builtinSource(id); ok {
			t.Fatalf("%s still has an ANSI figure", id)
		}
		lines, err := Builtin(id, builtinCols, builtinRows, termenv.TrueColor)
		if err != nil {
			t.Fatalf("%s: %v", id, err)
		}
		checkShape(t, lines, builtinCols, builtinRows)
		if !strings.Contains(strings.Join(lines, ""), "\x1b[38;2;") {
			t.Errorf("%s has no truecolor", id)
		}
		plain, err := Builtin(id, builtinCols, builtinRows, termenv.Ascii)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(strings.Join(plain, ""), "\x1b") {
			t.Errorf("%s no-colour has escapes", id)
		}
	}
}
