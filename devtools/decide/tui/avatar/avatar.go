// Package avatar renders a text or image file as a small block of colored
// terminal text for the decide TUI.
//
// Text files are used as is, with ANSI SGR colors. PNG and WEBP images are
// downsampled to one character per cell. The character comes from a
// luminance ramp and the color is the cell's average color.
package avatar

import (
	"bytes"
	"fmt"
	"image"
	"image/png"
	"io"
	"io/fs"
	"math"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
	"golang.org/x/image/webp"
)

// ramp orders characters from dark to light. Space is left out so an
// opaque cell is never blank; only transparent cells become spaces.
const ramp = ".'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

const sgrReset = "\x1b[0m"

// cellAspect is a terminal cell's height divided by its width.
const cellAspect = 2

// FS is the file access a Renderer needs.
type FS interface {
	Stat(name string) (fs.FileInfo, error)
	Open(name string) (io.ReadCloser, error)
}

type osFS struct{}

func (osFS) Stat(name string) (fs.FileInfo, error) {
	info, err := os.Stat(name)
	if err != nil {
		return nil, fmt.Errorf("stat avatar: %w", err)
	}
	return info, nil
}

func (osFS) Open(name string) (io.ReadCloser, error) {
	f, err := os.Open(name) //nolint:gosec // G304: the avatar path is operator config
	if err != nil {
		return nil, fmt.Errorf("open avatar: %w", err)
	}
	return f, nil
}

type cacheKey struct {
	path       string
	mtime      time.Time
	size       int64
	cols, rows int
	profile    termenv.Profile
}

// Renderer renders avatars and caches the results in memory.
type Renderer struct {
	fs    FS
	mu    sync.Mutex
	cache map[cacheKey][]string
}

// NewRenderer returns a Renderer that reads files through fsys. A nil fsys
// reads the local file system.
func NewRenderer(fsys FS) *Renderer {
	if fsys == nil {
		fsys = osFS{}
	}
	return &Renderer{fs: fsys, cache: map[cacheKey][]string{}}
}

var defaultRenderer = NewRenderer(nil)

// Render renders path with a shared in-memory cache. See Renderer.Render.
func Render(path string, cols, rows int, profile termenv.Profile) ([]string, error) {
	return defaultRenderer.Render(path, cols, rows, profile)
}

// Render returns exactly rows lines, each exactly cols cells wide. PNG and
// WEBP files are images; other image types are errors; any other file is
// text. With the Ascii profile or NO_COLOR set, the output has no escapes.
// A result is cached by path, mtime, size, cols, rows, and profile.
func (r *Renderer) Render(path string, cols, rows int, profile termenv.Profile) ([]string, error) {
	if cols <= 0 || rows <= 0 {
		return nil, fmt.Errorf("avatar size %dx%d", cols, rows)
	}
	profile = effectiveProfile(profile)
	info, err := r.fs.Stat(path)
	if err != nil {
		return nil, err //nolint:wrapcheck // FS implementations add context
	}
	key := cacheKey{path, info.ModTime(), info.Size(), cols, rows, profile}
	r.mu.Lock()
	lines, ok := r.cache[key]
	r.mu.Unlock()
	if ok {
		return lines, nil
	}
	lines, err = r.render(path, cols, rows, profile)
	if err != nil {
		return nil, err
	}
	r.mu.Lock()
	r.cache[key] = lines
	r.mu.Unlock()
	return lines, nil
}

func (r *Renderer) render(path string, cols, rows int, profile termenv.Profile) ([]string, error) {
	ext := strings.ToLower(filepath.Ext(path))
	switch ext {
	case ".jpg", ".jpeg", ".gif", ".bmp", ".tif", ".tiff", ".ico", ".heic", ".avif", ".svg":
		return nil, fmt.Errorf("avatar %s: unsupported image type %s", path, ext)
	}
	f, err := r.fs.Open(path)
	if err != nil {
		return nil, err //nolint:wrapcheck // FS implementations add context
	}
	defer f.Close()
	data, err := io.ReadAll(f)
	if err != nil {
		return nil, fmt.Errorf("read avatar %s: %w", path, err)
	}
	var img image.Image
	switch ext {
	case ".png":
		img, err = png.Decode(bytes.NewReader(data))
	case ".webp":
		img, err = webp.Decode(bytes.NewReader(data))
	default:
		if !utf8.Valid(data) {
			return nil, fmt.Errorf("avatar %s: not a text file", path)
		}
		return renderText(string(data), cols, rows, profile), nil
	}
	if err != nil {
		return nil, fmt.Errorf("decode avatar %s: %w", path, err)
	}
	return renderImage(img, cols, rows, profile), nil
}

func effectiveProfile(p termenv.Profile) termenv.Profile {
	if os.Getenv("NO_COLOR") != "" || p > termenv.Ascii {
		return termenv.Ascii
	}
	return p
}

func renderText(text string, cols, rows int, profile termenv.Profile) []string {
	color := profile != termenv.Ascii
	src := strings.Split(strings.TrimSuffix(strings.ReplaceAll(text, "\r\n", "\n"), "\n"), "\n")
	out := make([]string, rows)
	for i := range out {
		line := ""
		if i < len(src) {
			line = src[i]
		}
		out[i] = fitLine(line, cols, color)
	}
	return out
}

// fitLine cuts s to cols display cells, keeping SGR sequences when color is
// set, and pads it. A colored line ends with a reset.
func fitLine(s string, cols int, color bool) string {
	var b strings.Builder
	width := 0
	for i := 0; i < len(s); {
		if s[i] == 0x1b {
			seq, n := escape(s[i:])
			i += n
			if color && strings.HasSuffix(seq, "m") {
				b.WriteString(seq)
			}
			continue
		}
		r, n := utf8.DecodeRuneInString(s[i:])
		i += n
		if r == '\t' {
			r = ' '
		}
		if r < 0x20 || r == 0x7f {
			continue
		}
		w := lipgloss.Width(string(r))
		if width+w > cols {
			break
		}
		b.WriteRune(r)
		width += w
	}
	b.WriteString(strings.Repeat(" ", cols-width))
	if color {
		b.WriteString(sgrReset)
	}
	return b.String()
}

// escape returns the escape sequence at the start of s and its length. Only
// a CSI sequence is returned; other sequences are skipped with an empty seq.
func escape(s string) (string, int) {
	if len(s) < 2 {
		return "", len(s)
	}
	switch s[1] {
	case '[':
		for j := 2; j < len(s); j++ {
			if s[j] >= 0x40 && s[j] <= 0x7e {
				return s[:j+1], j + 1
			}
		}
		return "", len(s)
	case ']':
		for j := 2; j < len(s); j++ {
			if s[j] == 0x07 {
				return "", j + 1
			}
			if s[j] == 0x1b && j+1 < len(s) && s[j+1] == '\\' {
				return "", j + 2
			}
		}
		return "", len(s)
	}
	return "", 2
}

// stripANSI removes escape sequences from s.
func stripANSI(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); {
		if s[i] == 0x1b {
			_, n := escape(s[i:])
			i += n
			continue
		}
		b.WriteByte(s[i])
		i++
	}
	return b.String()
}

type cell struct {
	opaque  bool
	r, g, b uint8
}

func renderImage(img image.Image, cols, rows int, profile termenv.Profile) []string {
	bounds := img.Bounds()
	w, h := bounds.Dx(), bounds.Dy()
	blank := strings.Repeat(" ", cols)
	out := make([]string, rows)
	for i := range out {
		out[i] = blank
	}
	if w == 0 || h == 0 {
		return out
	}
	// scale is the pixel width of one cell; a cell is cellAspect times as tall.
	scale := math.Max(float64(w)/float64(cols), float64(h)/float64(cellAspect*rows))
	cw := clamp(int(math.Round(float64(w)/scale)), 1, cols)
	ch := clamp(int(math.Round(float64(h)/(cellAspect*scale))), 1, rows)
	left, top := (cols-cw)/2, (rows-ch)/2
	for cy := 0; cy < ch; cy++ {
		cells := make([]cell, cw)
		for cx := 0; cx < cw; cx++ {
			cells[cx] = average(img,
				bounds.Min.X+cx*w/cw, bounds.Min.X+(cx+1)*w/cw,
				bounds.Min.Y+cy*h/ch, bounds.Min.Y+(cy+1)*h/ch)
		}
		out[top+cy] = strings.Repeat(" ", left) + shadeRow(cells, profile) + strings.Repeat(" ", cols-left-cw)
	}
	return out
}

func clamp(v, lo, hi int) int {
	return max(lo, min(v, hi))
}

// average returns the alpha-weighted mean color of the pixels in
// [x0,x1) x [y0,y1). A region with no alpha is not opaque.
func average(img image.Image, x0, x1, y0, y1 int) cell {
	x1, y1 = max(x1, x0+1), max(y1, y0+1)
	var sr, sg, sb, sa uint64
	for y := y0; y < y1; y++ {
		for x := x0; x < x1; x++ {
			r, g, b, a := img.At(x, y).RGBA() // premultiplied, 16-bit
			sr += uint64(r)
			sg += uint64(g)
			sb += uint64(b)
			sa += uint64(a)
		}
	}
	if sa == 0 {
		return cell{}
	}
	return cell{
		opaque: true,
		r:      uint8(sr * 255 / sa), //nolint:gosec // G115: premultiplied sum never exceeds alpha sum
		g:      uint8(sg * 255 / sa), //nolint:gosec // G115: premultiplied sum never exceeds alpha sum
		b:      uint8(sb * 255 / sa), //nolint:gosec // G115: premultiplied sum never exceeds alpha sum
	}
}

func shadeRow(cells []cell, profile termenv.Profile) string {
	var b strings.Builder
	color := profile != termenv.Ascii
	last := ""
	for _, c := range cells {
		if !c.opaque {
			b.WriteByte(' ')
			continue
		}
		if color {
			seq := profile.Color(fmt.Sprintf("#%02x%02x%02x", c.r, c.g, c.b)).Sequence(false)
			if seq != last {
				b.WriteString("\x1b[" + seq + "m")
				last = seq
			}
		}
		b.WriteByte(shade(c))
	}
	if color && last != "" {
		b.WriteString(sgrReset)
	}
	return b.String()
}

func shade(c cell) byte {
	lum := (0.2126*float64(c.r) + 0.7152*float64(c.g) + 0.0722*float64(c.b)) / 255
	i := int(lum * float64(len(ramp)-1))
	return ramp[clamp(i, 0, len(ramp)-1)]
}
