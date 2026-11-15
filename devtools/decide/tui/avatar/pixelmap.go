package avatar

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/muesli/termenv"
)

const (
	pixelCols = 10
	pixelRows = 12
)

// pixelSource returns the pixel-map text for id.
func pixelSource(id string) (string, bool) {
	if strings.ContainsAny(id, "/.") {
		return "", false
	}
	data, err := builtinFS.ReadFile("builtin/" + id + ".px")
	if err != nil {
		return "", false
	}
	return string(data), true
}

// renderPixelMap draws a 10x12 pixel map as 10x6 half-block cells.
func renderPixelMap(text string, cols, rows int, profile termenv.Profile) ([]string, error) {
	cells, err := decodePixelMap(text)
	if err != nil {
		return nil, err
	}
	if profile == termenv.Ascii {
		for _, row := range cells {
			for i, c := range row {
				if c.fg != nil && c.bg != nil {
					c.ch = '█'
				}
				c.fg, c.bg = nil, nil
				row[i] = c
			}
		}
	}
	out := make([]string, rows)
	for i := range out {
		var row []figCell
		if i < len(cells) {
			row = cells[i]
		}
		out[i] = emitRow(row, cols, profile)
	}
	return out, nil
}

// decodePixelMap parses a pixel map into 6 rows of 10 cells.
// Two transparent pixels are a space. One opaque pixel is ▀ or ▄ with
// only a foreground. Two opaque pixels are ▀ with a foreground and a
// background. The top pixel is the foreground.
func decodePixelMap(text string) ([][]figCell, error) {
	pal, gaps, pix, err := parsePixelFile(text)
	if err != nil {
		return nil, err
	}
	if err := checkPixelGaps(pix, gaps); err != nil {
		return nil, err
	}
	cells := make([][]figCell, pixelRows/2)
	for y := range cells {
		cells[y] = make([]figCell, pixelCols)
		for x := range cells[y] {
			cells[y][x] = pixelCell(pix[y*2][x], pix[y*2+1][x], pal)
		}
	}
	return cells, nil
}

func pixelCell(top, bot rune, pal map[rune]rgb) figCell {
	topC, topOn := pal[top]
	botC, botOn := pal[bot]
	switch {
	case topOn && botOn:
		return figCell{ch: '▀', fg: rgbPtr(topC), bg: rgbPtr(botC)}
	case topOn:
		return figCell{ch: '▀', fg: rgbPtr(topC)}
	case botOn:
		return figCell{ch: '▄', fg: rgbPtr(botC)}
	default:
		return figCell{ch: ' '}
	}
}

func parsePixelFile(text string) (map[rune]rgb, map[[2]int]bool, [][]rune, error) {
	pal := map[rune]rgb{}
	gaps := map[[2]int]bool{}
	var pix [][]rune
	section := ""
	for n, line := range pixelLines(text) {
		if skipPixelLine(line) {
			continue
		}
		if name, ok := pixelSection(line); ok {
			section = name
			continue
		}
		if err := addPixelEntry(section, line, n, pal, gaps, &pix); err != nil {
			return nil, nil, nil, err
		}
	}
	if len(pix) != pixelRows {
		return nil, nil, nil, fmt.Errorf("pixel map: %d rows, want %d", len(pix), pixelRows)
	}
	return pal, gaps, pix, nil
}

func pixelLines(text string) []string {
	lines := strings.Split(strings.ReplaceAll(text, "\r\n", "\n"), "\n")
	if len(lines) > 0 && lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	return lines
}

func skipPixelLine(line string) bool {
	return line == "" || strings.HasPrefix(line, "#")
}

func pixelSection(line string) (string, bool) {
	switch line {
	case "palette", "gaps", "map":
		return line, true
	default:
		return "", false
	}
}

func addPixelEntry(section, line string, n int, pal map[rune]rgb, gaps map[[2]int]bool, pix *[][]rune) error {
	switch section {
	case "palette":
		return addPalette(line, pal)
	case "gaps":
		return addGap(line, gaps)
	case "map":
		row, err := parsePixelRow(line, len(*pix), pal)
		if err != nil {
			return err
		}
		*pix = append(*pix, row)
		return nil
	default:
		return fmt.Errorf("pixel map: line %d is outside a section", n+1)
	}
}

func addPalette(line string, pal map[rune]rgb) error {
	letter, c, err := parsePalette(line)
	if err != nil {
		return err
	}
	if _, ok := pal[letter]; ok {
		return fmt.Errorf("pixel map: duplicate letter %q", string(letter))
	}
	pal[letter] = c
	return nil
}

func addGap(line string, gaps map[[2]int]bool) error {
	y, x, err := parseGap(line)
	if err != nil {
		return err
	}
	gaps[[2]int{y, x}] = true
	return nil
}

func parsePalette(line string) (rune, rgb, error) {
	fields := strings.Fields(line)
	if len(fields) != 4 || utf8.RuneCountInString(fields[0]) != 1 {
		return 0, rgb{}, fmt.Errorf("pixel map: palette line %q wants a letter and three components", line)
	}
	letter, _ := utf8.DecodeRuneInString(fields[0])
	if letter == '.' {
		return 0, rgb{}, fmt.Errorf("pixel map: '.' is transparent and cannot be a palette letter")
	}
	comps := [3]uint8{}
	for i := range comps {
		v, err := strconv.ParseUint(fields[i+1], 10, 8)
		if err != nil {
			return 0, rgb{}, fmt.Errorf("pixel map: palette letter %q has a bad component %q", string(letter), fields[i+1])
		}
		comps[i] = uint8(v)
	}
	return letter, rgb{comps[0], comps[1], comps[2]}, nil
}

func parseGap(line string) (int, int, error) {
	fields := strings.Fields(line)
	if len(fields) != 2 {
		return 0, 0, fmt.Errorf("pixel map: gap line %q wants a row and a column", line)
	}
	y, errY := strconv.Atoi(fields[0])
	x, errX := strconv.Atoi(fields[1])
	if errY != nil || errX != nil || y < 0 || x < 0 || y >= pixelRows || x >= pixelCols {
		return 0, 0, fmt.Errorf("pixel map: gap %q is outside the 10x12 map", line)
	}
	return y, x, nil
}

func parsePixelRow(line string, y int, pal map[rune]rgb) ([]rune, error) {
	if utf8.RuneCountInString(line) != pixelCols {
		return nil, fmt.Errorf("pixel map: row %d has %d pixels, want %d", y, utf8.RuneCountInString(line), pixelCols)
	}
	row := []rune(line)
	for x, r := range row {
		if r == '.' {
			continue
		}
		if _, ok := pal[r]; !ok {
			return nil, fmt.Errorf("pixel map: unknown letter %q at %d,%d", string(r), y, x)
		}
	}
	return row, nil
}

func checkPixelGaps(pix [][]rune, gaps map[[2]int]bool) error {
	got := map[[2]int]bool{}
	for _, p := range enclosedPixels(pix) {
		got[p] = true
		if !gaps[p] {
			return fmt.Errorf("pixel map: enclosed transparent pixel at %d,%d is not a gap", p[0], p[1])
		}
	}
	for p := range gaps {
		if !got[p] {
			return fmt.Errorf("pixel map: gap %d,%d is not an enclosed transparent pixel", p[0], p[1])
		}
	}
	return nil
}

// enclosedPixels lists transparent pixels that cannot reach the border,
// in row-major order. Neighbours share an edge.
func enclosedPixels(pix [][]rune) [][2]int {
	h, w := len(pix), pixelCols
	seen := make([]bool, h*w)
	markReachable(pix, seen)
	var out [][2]int
	for y := range h {
		for x := range w {
			if pix[y][x] == '.' && !seen[y*w+x] {
				out = append(out, [2]int{y, x})
			}
		}
	}
	return out
}

func markReachable(pix [][]rune, seen []bool) {
	h, w := len(pix), pixelCols
	var stack [][2]int
	for y := range h {
		for x := range w {
			if y == 0 || x == 0 || y == h-1 || x == w-1 {
				pushDot(&stack, seen, pix, y, x)
			}
		}
	}
	for len(stack) > 0 {
		p := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		pushDot(&stack, seen, pix, p[0]-1, p[1])
		pushDot(&stack, seen, pix, p[0]+1, p[1])
		pushDot(&stack, seen, pix, p[0], p[1]-1)
		pushDot(&stack, seen, pix, p[0], p[1]+1)
	}
}

func pushDot(stack *[][2]int, seen []bool, pix [][]rune, y, x int) {
	h, w := len(pix), pixelCols
	if y < 0 || x < 0 || y >= h || x >= w {
		return
	}
	if pix[y][x] != '.' || seen[y*w+x] {
		return
	}
	seen[y*w+x] = true
	*stack = append(*stack, [2]int{y, x})
}
