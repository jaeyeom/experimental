package avatar

import (
	"fmt"
	"math"
	"strings"
	"unicode/utf8"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

// rgb is one 24-bit color.
type rgb struct{ r, g, b uint8 }

// figCell is one source cell and the colors it renders with.
type figCell struct {
	ch     rune
	fg, bg *rgb
}

// gapAll figures are an outline with an opening. Every space enclosed on
// a row is a deliberate gap, so the terminal background stays.
var gapAll = map[string]bool{
	"bishop":     true,
	"glasses":    true,
	"headphones": true,
}

// deliberateGaps are enclosed spaces that stay unpainted: a gap between
// parts of a figure, not the surface inside it. Keys are row, col.
var deliberateGaps = map[string]map[[2]int]bool{
	"bee":        gap(0, 3, 0, 4, 0, 5, 0, 6, 1, 4, 1, 5),
	"beer-mug":   gap(0, 4, 3, 8),
	"book":       gap(0, 4, 0, 5),
	"cactus":     gap(0, 6, 0, 7, 1, 2, 1, 3, 2, 6),
	"cat":        gap(0, 3, 0, 4, 0, 5, 0, 6, 4, 8, 5, 4, 5, 5),
	"crab":       gap(1, 2, 1, 4, 1, 5, 1, 7, 5, 1, 5, 3, 5, 4, 5, 5, 5, 6, 5, 8),
	"dog":        gap(0, 3, 0, 4, 0, 5, 0, 6),
	"fox":        gap(0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7),
	"frog":       gap(0, 4, 0, 5),
	"jester":     gap(0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7),
	"king":       gap(0, 3, 0, 6),
	"lighthouse": gap(0, 2, 0, 7),
	"mushroom":   gap(1, 3, 1, 7, 2, 3, 2, 6),
	"octopus":    gap(5, 4, 5, 5),
	"owl":        gap(0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 5, 3, 5, 6),
	"peasant":    gap(0, 2, 0, 3, 2, 2, 3, 2, 5, 2, 5, 5, 5, 6),
	"robot":      gap(5, 4, 5, 5),
	"snail":      gap(0, 6, 0, 8, 1, 8),
	"teapot":     gap(2, 1, 3, 8),
	"whale":      gap(0, 3, 1, 4, 1, 5, 1, 6),
	"wizard-hat": gap(2, 7),
}

// gap packs row, col pairs into a set.
func gap(coords ...int) map[[2]int]bool {
	set := make(map[[2]int]bool, len(coords)/2)
	for i := 0; i+1 < len(coords); i += 2 {
		set[[2]int{coords[i], coords[i+1]}] = true
	}
	return set
}

// interiorGap reports whether the enclosed space at row, col is a
// deliberate gap that stays unpainted.
func interiorGap(id string, row, col int) bool {
	if gapAll[id] {
		return true
	}
	return deliberateGaps[id][[2]int{row, col}]
}

// renderBuiltin draws a built-in figure. Enclosed spaces get a background
// so the inside of the figure is not the terminal's own background. With
// no color those spaces become a block. Queen and king use their eye
// color as the face fill.
func renderBuiltin(id, text string, cols, rows int, profile termenv.Profile) []string {
	parsed := parseColored(text)
	applyInterior(id, parsed)
	out := make([]string, rows)
	for i := range out {
		var row []figCell
		if i < len(parsed) {
			row = parsed[i]
		}
		out[i] = emitRow(row, cols, profile)
	}
	return out
}

func parseColored(text string) [][]figCell {
	src := strings.Split(strings.TrimSuffix(strings.ReplaceAll(text, "\r\n", "\n"), "\n"), "\n")
	out := make([][]figCell, len(src))
	for i, line := range src {
		out[i] = parseColoredLine(line)
	}
	return out
}

func parseColoredLine(s string) []figCell {
	var fg *rgb
	var figCells []figCell
	for i := 0; i < len(s); {
		if s[i] == 0x1b {
			seq, n := escape(s[i:])
			i += n
			fg = applyFG(fg, seq)
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
		c := figCell{ch: r}
		if fg != nil {
			copied := *fg
			c.fg = &copied
		}
		figCells = append(figCells, c)
	}
	return figCells
}

func applyFG(fg *rgb, seq string) *rgb {
	if !strings.HasPrefix(seq, "\x1b[") || !strings.HasSuffix(seq, "m") {
		return fg
	}
	params := seq[2 : len(seq)-1]
	if params == "" || params == "0" {
		return nil
	}
	parts := strings.Split(params, ";")
	for i := 0; i+4 < len(parts); i++ {
		if parts[i] != "38" || parts[i+1] != "2" {
			continue
		}
		c, ok := atoiRGB(parts[i+2], parts[i+3], parts[i+4])
		if ok {
			return &c
		}
	}
	return fg
}

func atoiRGB(rs, gs, bs string) (rgb, bool) {
	ri, errR := atoi255(rs)
	gi, errG := atoi255(gs)
	bi, errB := atoi255(bs)
	if errR != nil || errG != nil || errB != nil {
		return rgb{}, false
	}
	return rgb{ri, gi, bi}, true
}

func atoi255(s string) (uint8, error) {
	n := 0
	if s == "" {
		return 0, fmt.Errorf("empty component")
	}
	for _, r := range s {
		if r < '0' || r > '9' {
			return 0, fmt.Errorf("bad component %q", s)
		}
		n = n*10 + int(r-'0')
		if n > 255 {
			return 0, fmt.Errorf("component %q out of range", s)
		}
	}
	return uint8(n), nil
}

func applyInterior(id string, rows [][]figCell) {
	tone, hasTone := faceTone(id, rows)
	for y, row := range rows {
		left, right, ok := span(row)
		if !ok {
			continue
		}
		var fills []rgb
		for x := left + 1; x < right; x++ {
			if row[x].ch != ' ' || interiorGap(id, y, x) {
				continue
			}
			bg, ok := spaceFill(row, x, tone, hasTone)
			if !ok {
				continue
			}
			row[x].bg = rgbPtr(bg)
			fills = append(fills, bg)
		}
		rowBG, okBG := tone, hasTone
		if !okBG {
			rowBG, okBG = majority(fills)
		}
		if !okBG && row[left].fg != nil {
			rowBG, okBG = *row[left].fg, true
		}
		if !okBG {
			continue
		}
		filled := len(fills) > 0
		for x := left + 1; x < right; x++ {
			if !glyphOnFill(row[x].ch, filled, hasTone) {
				continue
			}
			row[x].bg = rgbPtr(rowBG)
			if row[x].fg == nil || needsInk(row[x].ch, *row[x].fg, rowBG) {
				row[x].fg = rgbPtr(inkFor(rowBG))
			}
		}
	}
}

func rgbPtr(c rgb) *rgb {
	return &c
}

func span(row []figCell) (left, right int, ok bool) {
	left = -1
	for i, c := range row {
		if c.ch == ' ' {
			continue
		}
		if left < 0 {
			left = i
		}
		right = i
		ok = true
	}
	return left, right, ok
}

// faceTone is the eye color of the queen and the king, used as the fill
// behind the whole face. Other figures keep each cell's own color.
func faceTone(id string, rows [][]figCell) (rgb, bool) {
	if id != "queen" && id != "king" {
		return rgb{}, false
	}
	counts := map[rgb]int{}
	var order []rgb
	for _, row := range rows {
		for _, c := range row {
			if c.ch != 'o' || c.fg == nil {
				continue
			}
			if counts[*c.fg] == 0 {
				order = append(order, *c.fg)
			}
			counts[*c.fg]++
		}
	}
	if len(order) == 0 {
		return rgb{}, false
	}
	best := order[0]
	for _, c := range order[1:] {
		if counts[c] > counts[best] {
			best = c
		}
	}
	return best, true
}

func spaceFill(row []figCell, x int, tone rgb, hasTone bool) (rgb, bool) {
	if hasTone {
		return tone, true
	}
	if row[x].fg != nil {
		return *row[x].fg, true
	}
	for d := 1; d < len(row); d++ {
		if x-d >= 0 && row[x-d].ch != ' ' && row[x-d].fg != nil {
			return *row[x-d].fg, true
		}
		if x+d < len(row) && row[x+d].ch != ' ' && row[x+d].fg != nil {
			return *row[x+d].fg, true
		}
	}
	return rgb{}, false
}

func majority(colors []rgb) (rgb, bool) {
	if len(colors) == 0 {
		return rgb{}, false
	}
	counts := map[rgb]int{}
	best, n := colors[0], 0
	for _, c := range colors {
		counts[c]++
		if counts[c] > n {
			best, n = c, counts[c]
		}
	}
	return best, true
}

// glyphOnFill reports whether r is drawn on top of the row fill.
// A row with a painted interior does this for every glyph except a full
// block. A face with a tone also does it for eyes and mouths where the
// row has no space to fill.
func glyphOnFill(r rune, filled, hasTone bool) bool {
	if r == ' ' || r == '█' {
		return false
	}
	if filled {
		return true
	}
	if !hasTone {
		return false
	}
	switch r {
	case 'o', 'O', '▀', '▄', '─', '═', '(', ')', '/', '\\', '+', '-', '_', '*':
		return true
	default:
		return false
	}
}

func needsInk(r rune, fg, bg rgb) bool {
	if contrast(fg, bg) >= 2 {
		return false
	}
	return r == 'o' || r == 'O' || contrast(fg, bg) < 1.25
}

func inkFor(bg rgb) rgb {
	best := rgb{100, 60, 40}
	bestD := math.MaxFloat64
	found := false
	target := scale(bg, 0.45)
	if relLum(bg) < 0.2 {
		target = scale(bg, 1.8)
	}
	for _, r := range []uint8{40, 70, 100, 130, 160, 190, 220} {
		for _, g := range []uint8{40, 70, 100, 130, 160, 190, 220} {
			for _, b := range []uint8{30, 60, 90, 120, 150, 180, 210} {
				c := rgb{r, g, b}
				blk, wht := contrastBW(c)
				if blk < 2 || wht < 2 || contrast(c, bg) < 2 {
					continue
				}
				d := dist(c, target)
				if d < bestD {
					best, bestD, found = c, d, true
				}
			}
		}
	}
	if !found {
		return rgb{100, 60, 40}
	}
	return best
}

func scale(c rgb, f float64) rgb {
	conv := func(v uint8) uint8 {
		n := int(math.Round(float64(v) * f))
		if n < 0 {
			return 0
		}
		if n > 255 {
			return 255
		}
		return uint8(n)
	}
	return rgb{conv(c.r), conv(c.g), conv(c.b)}
}

func dist(a, b rgb) float64 {
	dr := float64(a.r) - float64(b.r)
	dg := float64(a.g) - float64(b.g)
	db := float64(a.b) - float64(b.b)
	return dr*dr + dg*dg + db*db
}

func relLum(c rgb) float64 {
	lin := func(v uint8) float64 {
		x := float64(v) / 255
		if x <= 0.03928 {
			return x / 12.92
		}
		return math.Pow((x+0.055)/1.055, 2.4)
	}
	return 0.2126*lin(c.r) + 0.7152*lin(c.g) + 0.0722*lin(c.b)
}

func contrast(a, b rgb) float64 {
	ha, hb := relLum(a), relLum(b)
	if ha < hb {
		ha, hb = hb, ha
	}
	return (ha + 0.05) / (hb + 0.05)
}

func contrastBW(c rgb) (black, white float64) {
	l := relLum(c)
	return (l + 0.05) / 0.05, 1.05 / (l + 0.05)
}

func emitRow(row []figCell, cols int, profile termenv.Profile) string {
	color := profile != termenv.Ascii
	var b strings.Builder
	width := 0
	lastFG, lastBG := "", ""
	open := false
	reset := func() {
		if !open {
			return
		}
		b.WriteString(sgrReset)
		open = false
		lastFG, lastBG = "", ""
	}
	for _, c := range row {
		if width >= cols {
			break
		}
		r := c.ch
		if !color && c.bg != nil && r == ' ' {
			r = '█'
		}
		w := lipgloss.Width(string(r))
		if w == 0 || width+w > cols {
			continue
		}
		if color {
			fg, bg := colorSeq(profile, c.fg, false), colorSeq(profile, c.bg, true)
			if fg != lastFG || bg != lastBG {
				reset()
				if fg != "" {
					b.WriteString("\x1b[" + fg + "m")
					open = true
				}
				if bg != "" {
					b.WriteString("\x1b[" + bg + "m")
					open = true
				}
				lastFG, lastBG = fg, bg
			}
		}
		b.WriteRune(r)
		width += w
	}
	reset()
	if width < cols {
		b.WriteString(strings.Repeat(" ", cols-width))
	}
	return b.String()
}

func colorSeq(profile termenv.Profile, c *rgb, bg bool) string {
	if c == nil {
		return ""
	}
	// True color stays on the source components. termenv's hex conversion
	// rounds some of them.
	if profile == termenv.TrueColor {
		prefix := "38"
		if bg {
			prefix = "48"
		}
		return fmt.Sprintf("%s;2;%d;%d;%d", prefix, c.r, c.g, c.b)
	}
	return profile.Color(fmt.Sprintf("#%02x%02x%02x", c.r, c.g, c.b)).Sequence(bg)
}
