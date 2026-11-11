package avatar

import (
	"bytes"
	"image"
	"image/color"
	"image/png"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

const reset = "\x1b[0m"

func checkShape(t *testing.T, lines []string, cols, rows int) {
	t.Helper()
	if len(lines) != rows {
		t.Fatalf("got %d lines, want %d: %q", len(lines), rows, lines)
	}
	for i, l := range lines {
		if w := lipgloss.Width(l); w != cols {
			t.Errorf("line %d width = %d, want %d: %q", i, w, cols, l)
		}
	}
}

func writeFile(t *testing.T, name string, data []byte) string {
	t.Helper()
	p := filepath.Join(t.TempDir(), name)
	if err := os.WriteFile(p, data, 0o600); err != nil {
		t.Fatal(err)
	}
	return p
}

func pngBytes(t *testing.T, img image.Image) []byte {
	t.Helper()
	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		t.Fatal(err)
	}
	return buf.Bytes()
}

func halfImage(w, h int) *image.NRGBA {
	img := image.NewNRGBA(image.Rect(0, 0, w, h))
	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			c := color.NRGBA{0, 0, 0, 255}
			if x >= w/2 {
				c = color.NRGBA{255, 255, 255, 255}
			}
			img.SetNRGBA(x, y, c)
		}
	}
	return img
}

func newRenderer() *Renderer { return NewRenderer(nil) }

func TestTextFixture(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	text := "\x1b[31mred line that is far too long\x1b[0m\nshort\n\x1b[1;32m日本語テキスト\n"
	p := writeFile(t, "a.ans", []byte(text))
	lines, err := newRenderer().Render(p, 10, 6, termenv.TrueColor)
	if err != nil {
		t.Fatal(err)
	}
	checkShape(t, lines, 10, 6)
	for i, l := range lines {
		if !strings.HasSuffix(l, reset) {
			t.Errorf("line %d does not end with reset: %q", i, l)
		}
	}
	if !strings.HasPrefix(lines[0], "\x1b[31mred line t") {
		t.Errorf("line 0 = %q", lines[0])
	}
	// 日本語テキスト is 14 cells; 10 cells keeps five wide runes.
	if got := stripANSI(lines[2]); got != "日本語テキ" {
		t.Errorf("line 2 = %q, want 5 wide runes", got)
	}
}

func TestTextWideRuneAtBoundary(t *testing.T) {
	p := writeFile(t, "a.txt", []byte("abcdefghi日\n"))
	lines, err := newRenderer().Render(p, 10, 1, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	if lines[0] != "abcdefghi " {
		t.Errorf("got %q, want wide rune dropped and padded", lines[0])
	}
}

func TestTextStripsEscapes(t *testing.T) {
	p := writeFile(t, "a.ans", []byte("\x1b[31mred\x1b[0m\n"))
	t.Run("ascii", func(t *testing.T) {
		lines, err := newRenderer().Render(p, 10, 6, termenv.Ascii)
		if err != nil {
			t.Fatal(err)
		}
		checkShape(t, lines, 10, 6)
		for _, l := range lines {
			if strings.Contains(l, "\x1b") {
				t.Errorf("escape in %q", l)
			}
		}
		if lines[0] != "red       " {
			t.Errorf("line 0 = %q", lines[0])
		}
	})
	t.Run("NO_COLOR", func(t *testing.T) {
		t.Setenv("NO_COLOR", "1")
		lines, err := newRenderer().Render(p, 10, 6, termenv.TrueColor)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(strings.Join(lines, ""), "\x1b") {
			t.Errorf("escape in %q", lines)
		}
	})
}

func rampIndex(t *testing.T, ch rune) int {
	t.Helper()
	i := strings.IndexRune(ramp, ch)
	if i < 0 {
		t.Fatalf("%q is not a ramp character", ch)
	}
	return i
}

func TestImageHalves(t *testing.T) {
	p := writeFile(t, "half.png", pngBytes(t, halfImage(20, 20)))
	lines, err := newRenderer().Render(p, 10, 6, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	checkShape(t, lines, 10, 6)
	mid := []rune(lines[3])
	for x := 0; x < 5; x++ {
		if i := rampIndex(t, mid[x]); i > len(ramp)/4 {
			t.Errorf("left cell %d = %q, want dark", x, mid[x])
		}
	}
	for x := 5; x < 10; x++ {
		if i := rampIndex(t, mid[x]); i < 3*len(ramp)/4 {
			t.Errorf("right cell %d = %q, want light", x, mid[x])
		}
	}
}

func TestImageLetterbox(t *testing.T) {
	p := writeFile(t, "wide.png", pngBytes(t, halfImage(40, 10)))
	lines, err := newRenderer().Render(p, 10, 6, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	checkShape(t, lines, 10, 6)
	blank := strings.Repeat(" ", 10)
	if lines[0] != blank || lines[5] != blank {
		t.Errorf("want blank top and bottom rows, got %q", lines)
	}
	filled := 0
	for _, l := range lines {
		if l != blank {
			filled++
		}
	}
	if filled == 0 || filled >= 6 {
		t.Errorf("filled rows = %d: %q", filled, lines)
	}
}

func TestImageTransparent(t *testing.T) {
	img := image.NewNRGBA(image.Rect(0, 0, 2, 2))
	img.SetNRGBA(0, 0, color.NRGBA{255, 255, 255, 255})
	img.SetNRGBA(1, 0, color.NRGBA{255, 255, 255, 255})
	p := writeFile(t, "t.png", pngBytes(t, img))
	lines, err := newRenderer().Render(p, 1, 1, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	// The cell covers the opaque top row and the transparent bottom row.
	if lines[0] == " " {
		t.Errorf("half-opaque cell is blank")
	}
	transparent := image.NewNRGBA(image.Rect(0, 0, 4, 4))
	p = writeFile(t, "clear.png", pngBytes(t, transparent))
	lines, err = newRenderer().Render(p, 2, 1, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	if lines[0] != "  " {
		t.Errorf("transparent image = %q, want spaces", lines[0])
	}
}

func TestImageProfiles(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	img := image.NewNRGBA(image.Rect(0, 0, 4, 4))
	for y := 0; y < 4; y++ {
		for x := 0; x < 4; x++ {
			img.SetNRGBA(x, y, color.NRGBA{200, 100, 50, 255})
		}
	}
	p := writeFile(t, "c.png", pngBytes(t, img))
	tests := []struct {
		profile termenv.Profile
		want    string
	}{
		{termenv.TrueColor, "38;2;200;100;50"},
		{termenv.ANSI256, "38;5;"},
	}
	for _, tt := range tests {
		lines, err := newRenderer().Render(p, 4, 2, tt.profile)
		if err != nil {
			t.Fatal(err)
		}
		checkShape(t, lines, 4, 2)
		if all := strings.Join(lines, "\n"); !strings.Contains(all, tt.want) {
			t.Errorf("profile %v: want %q in %q", tt.profile, tt.want, all)
		}
	}
	lines, err := newRenderer().Render(p, 4, 2, termenv.Ascii)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(strings.Join(lines, ""), "\x1b") {
		t.Errorf("ascii output has escapes: %q", lines)
	}
}

func TestWebP(t *testing.T) {
	p, err := filepath.Abs("testdata/half.webp")
	if err != nil {
		t.Fatal(err)
	}
	lines, err := newRenderer().Render(p, 10, 6, termenv.TrueColor)
	if err != nil {
		t.Fatal(err)
	}
	checkShape(t, lines, 10, 6)
	if !strings.Contains(strings.Join(lines, ""), "38;2;255;0;0") {
		t.Errorf("want red right half in %q", lines)
	}
}

type countingFS struct {
	opens int
	mtime time.Time
	data  []byte
}

type fakeInfo struct {
	fs.FileInfo
	mtime time.Time
	size  int64
}

func (i fakeInfo) ModTime() time.Time { return i.mtime }
func (i fakeInfo) Size() int64        { return i.size }

func (c *countingFS) Stat(string) (fs.FileInfo, error) {
	return fakeInfo{mtime: c.mtime, size: int64(len(c.data))}, nil
}

func (c *countingFS) Open(string) (io.ReadCloser, error) {
	c.opens++
	return io.NopCloser(bytes.NewReader(c.data)), nil
}

func TestCache(t *testing.T) {
	cfs := &countingFS{mtime: time.Unix(1, 0), data: pngBytes(t, halfImage(20, 20))}
	r := NewRenderer(cfs)
	for i := 0; i < 2; i++ {
		if _, err := r.Render("/x/a.png", 10, 6, termenv.Ascii); err != nil {
			t.Fatal(err)
		}
	}
	if cfs.opens != 1 {
		t.Errorf("opens = %d, want 1", cfs.opens)
	}
	cfs.mtime = time.Unix(2, 0)
	if _, err := r.Render("/x/a.png", 10, 6, termenv.Ascii); err != nil {
		t.Fatal(err)
	}
	if cfs.opens != 2 {
		t.Errorf("opens after mtime change = %d, want 2", cfs.opens)
	}
	if _, err := r.Render("/x/a.png", 8, 6, termenv.Ascii); err != nil {
		t.Fatal(err)
	}
	if cfs.opens != 3 {
		t.Errorf("opens after size change = %d, want 3", cfs.opens)
	}
}

func TestErrors(t *testing.T) {
	r := newRenderer()
	if _, err := r.Render(filepath.Join(t.TempDir(), "missing.png"), 10, 6, termenv.Ascii); err == nil {
		t.Error("missing file: want error")
	}
	p := writeFile(t, "bad.png", []byte("not a png"))
	if _, err := r.Render(p, 10, 6, termenv.Ascii); err == nil {
		t.Error("corrupt png: want error")
	}
	p = writeFile(t, "a.jpg", []byte("x"))
	if _, err := r.Render(p, 10, 6, termenv.Ascii); err == nil {
		t.Error("unsupported image: want error")
	}
}
