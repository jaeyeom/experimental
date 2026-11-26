// Package chart draws small text charts for factory stats: stacked bars,
// sparklines, braille line charts, and vertical bars. Every function is
// pure and returns plain strings, so it is easy to golden-test. Color is
// applied by the caller through Style.
package chart

import (
	"math"
	"strings"
)

// ANSI colors from a small palette that reads on dark and light terminals.
const (
	Green   = "32"
	Yellow  = "33"
	Blue    = "34"
	Magenta = "35"
	Cyan    = "36"
	Red     = "31"
	Gray    = "90"
)

// Style says whether to color and whether to stay within ASCII.
type Style struct {
	Color bool
	ASCII bool
}

// Paint wraps s in the ANSI color code when Color is set. Empty s stays empty.
func (s Style) Paint(code, text string) string {
	if !s.Color || code == "" || text == "" {
		return text
	}
	return "\x1b[" + code + "m" + text + "\x1b[0m"
}

// Part is one segment of a stacked bar.
type Part struct {
	Value float64
	Color string // ANSI color code, or empty for none.
	// Fill is the ASCII fill rune used in ASCII mode, such as '#' or '='.
	Fill rune
	// Shade, such as '▓' or '▒', tells the part apart when there is no
	// color. It is drawn in whole cells. Zero uses blocks and eighths.
	Shade rune
}

// eighths are the partial cells from one eighth to a full block.
var eighths = []rune("▏▎▍▌▋▊▉█")

// Stack draws parts side by side, sized against scale (the largest total in
// the table) in at most width cells, and returns one unpainted segment per
// part so the caller can color each. A part above zero always gets at least
// one cell while width allows. With color, or without a Shade, a part is
// full blocks ending in an eighth-block partial cell. Without color a part
// with a Shade is whole cells of that shade, and ASCII uses its Fill.
func Stack(parts []Part, scale float64, width int, style Style) []string {
	out := make([]string, len(parts))
	if width <= 0 || scale <= 0 {
		return out
	}
	units := make([]int, len(parts)) // Eighths of a cell.
	cells := make([]int, len(parts))
	for i, p := range parts {
		if p.Value <= 0 {
			continue
		}
		units[i] = max(1, int(math.Round(p.Value/scale*float64(width*8))))
		cells[i] = (units[i] + 7) / 8
	}
	fit(units, cells, width)
	for i, p := range parts {
		out[i] = segment(units[i], cells[i], p, style)
	}
	return out
}

// fit shrinks the widest parts one cell at a time until the cells fit in
// width, keeping one cell for each part that has any while it can.
func fit(units, cells []int, width int) {
	for sum(cells) > width {
		widest := -1
		for i, c := range cells {
			if c > 1 && (widest < 0 || c > cells[widest]) {
				widest = i
			}
		}
		if widest < 0 {
			// More parts than cells: drop parts from the end.
			for i := len(cells) - 1; i >= 0 && sum(cells) > width; i-- {
				cells[i], units[i] = 0, 0
			}
			return
		}
		cells[widest]--
		units[widest] = cells[widest] * 8
	}
}

func sum(xs []int) int {
	n := 0
	for _, x := range xs {
		n += x
	}
	return n
}

// segment draws one part: n cells holding units eighths.
func segment(units, n int, p Part, style Style) string {
	switch {
	case n <= 0:
		return ""
	case style.ASCII:
		fill := p.Fill
		if fill == 0 {
			fill = '#'
		}
		return strings.Repeat(string(fill), n)
	case !style.Color && p.Shade != 0:
		return strings.Repeat(string(p.Shade), n)
	}
	full, rest := units/8, units%8
	s := strings.Repeat("█", full)
	if rest > 0 {
		s += string(eighths[rest-1])
	}
	return s
}

// sparks are the eight sparkline levels.
var sparks = []rune("▁▂▃▄▅▆▇█")

// asciiSparks are the ASCII sparkline levels.
var asciiSparks = []rune("_.-=*#")

// Sparkline draws one character per value, scaled from 0 to the maximum.
// All zero values draw the lowest level.
func Sparkline(values []float64, style Style) string {
	levels := sparks
	if style.ASCII {
		levels = asciiSparks
	}
	maxV := 0.0
	for _, v := range values {
		maxV = math.Max(maxV, v)
	}
	var b strings.Builder
	for _, v := range values {
		i := 0
		if maxV > 0 && v > 0 {
			i = int(math.Round(v / maxV * float64(len(levels)-1)))
		}
		b.WriteRune(levels[i])
	}
	return b.String()
}

// VBars draws values as vertical bars, one column per value, height rows
// tall, scaled to the maximum. Row 0 is the top.
func VBars(values []float64, height int, style Style) []string {
	if height <= 0 || len(values) == 0 {
		return nil
	}
	maxV := 0.0
	for _, v := range values {
		maxV = math.Max(maxV, v)
	}
	full, part := "█", []rune("▁▂▃▄▅▆▇")
	if style.ASCII {
		full, part = "#", []rune("........")
	}
	rows := make([]string, height)
	for r := range height {
		var b strings.Builder
		// Eighths this row's floor is above the baseline.
		floor := (height - 1 - r) * 8
		for _, v := range values {
			e := 0
			if maxV > 0 {
				e = int(math.Round(v / maxV * float64(height*8)))
			}
			switch {
			case e >= floor+8:
				b.WriteString(full)
			case e > floor:
				b.WriteRune(part[min(e-floor-1, len(part)-1)])
			default:
				b.WriteByte(' ')
			}
		}
		rows[r] = b.String()
	}
	return rows
}

// Braille draws each series as a line on a canvas of w by h cells, two by
// four dots per cell. Every series shares the y scale from 0 to the
// largest value. Row 0 is the top. In ASCII mode each cell is '*' where
// any dot is set. dotted, when true for a series, draws every other x.
func Braille(series [][]float64, dotted []bool, w, h int, style Style) []string {
	if w <= 0 || h <= 0 {
		return nil
	}
	dw, dh := w*2, h*4
	grid := make([][]bool, dh)
	for i := range grid {
		grid[i] = make([]bool, dw)
	}
	maxV := 0.0
	for _, s := range series {
		for _, v := range s {
			maxV = math.Max(maxV, v)
		}
	}
	for si, s := range series {
		if len(s) == 0 {
			continue
		}
		skip := si < len(dotted) && dotted[si]
		for x := range dw {
			if skip && x%2 == 1 {
				continue
			}
			v := sample(s, x, dw)
			y := 0
			if maxV > 0 {
				y = int(math.Round(v / maxV * float64(dh-1)))
			}
			grid[dh-1-y][x] = true
		}
	}
	rows := make([]string, h)
	for r := range h {
		var b strings.Builder
		for c := range w {
			b.WriteRune(cell(grid, r*4, c*2, style.ASCII))
		}
		rows[r] = b.String()
	}
	return rows
}

// sample is series s read at dot x of dw, linearly interpolated.
func sample(s []float64, x, dw int) float64 {
	if len(s) == 1 || dw == 1 {
		return s[0]
	}
	pos := float64(x) / float64(dw-1) * float64(len(s)-1)
	i := int(pos)
	if i >= len(s)-1 {
		return s[len(s)-1]
	}
	f := pos - float64(i)
	return s[i]*(1-f) + s[i+1]*f
}

// cell is the braille rune for the 2x4 dots at grid[top][left].
func cell(grid [][]bool, top, left int, ascii bool) rune {
	// Dot bits in braille order: column 0 rows 0-2, column 1 rows 0-2,
	// then row 3 of each column.
	bits := [4][2]rune{{0x01, 0x08}, {0x02, 0x10}, {0x04, 0x20}, {0x40, 0x80}}
	var code rune
	for dy := range 4 {
		for dx := range 2 {
			if grid[top+dy][left+dx] {
				code |= bits[dy][dx]
			}
		}
	}
	if ascii {
		if code == 0 {
			return ' '
		}
		return '*'
	}
	if code == 0 {
		return ' '
	}
	return 0x2800 + code
}
