package avatar

import (
	"fmt"
	"hash/fnv"
	"math"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

const (
	builtinCols = 10
	builtinRows = 6
)

// visibleLines is text split into lines with SGR stripped.
func visibleLines(text string) []string {
	return strings.Split(strings.TrimSuffix(stripANSI(text), "\n"), "\n")
}

// checkVisibleShape fails unless text is rows lines of exactly cols cells
// each once its escapes are stripped.
func checkVisibleShape(t *testing.T, label, text string, cols, rows int) {
	t.Helper()
	lines := visibleLines(text)
	if len(lines) != rows {
		t.Fatalf("%s: %d lines, want %d: %q", label, len(lines), rows, lines)
	}
	for i, l := range lines {
		if w := lipgloss.Width(l); w != cols {
			t.Errorf("%s line %d: %d cells, want %d: %q", label, i, w, cols, l)
		}
	}
}

func TestBuiltinIDs(t *testing.T) {
	ids := BuiltinIDs()
	if len(ids) < 20 {
		t.Fatalf("%d built-ins, want at least 20", len(ids))
	}
	seen := map[string]bool{}
	for _, id := range ids {
		if seen[id] {
			t.Errorf("duplicate id %q", id)
		}
		seen[id] = true
		if !regexp.MustCompile(`^[a-z]+(-[a-z]+)*$`).MatchString(id) {
			t.Errorf("id %q is not lowercase words joined by -", id)
		}
	}
	if !slices.IsSorted(ids) {
		t.Errorf("ids are not sorted: %q", ids)
	}
}

// source is the embedded text of the built-in id.
func source(t *testing.T, id string) string {
	t.Helper()
	text, ok := builtinSource(id)
	if !ok {
		t.Fatalf("no source for %q", id)
	}
	return text
}

func TestBuiltinSourceShape(t *testing.T) {
	for _, id := range BuiltinIDs() {
		checkVisibleShape(t, id, source(t, id), builtinCols, builtinRows)
	}
}

func TestBuiltinRenderShape(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	for _, id := range BuiltinIDs() {
		lines, err := Builtin(id, builtinCols, builtinRows, termenv.TrueColor)
		if err != nil {
			t.Fatal(err)
		}
		checkShape(t, lines, builtinCols, builtinRows)
		if !strings.Contains(strings.Join(lines, ""), "\x1b[38;2;") {
			t.Errorf("%s has no truecolor escapes", id)
		}
		if got, want := stripANSI(strings.Join(lines, "\n")), strings.Join(visibleLines(source(t, id)), "\n"); got != want {
			t.Errorf("%s render differs from its source\ngot:\n%s\nwant:\n%s", id, got, want)
		}
	}
}

// TestBuiltinLayoutsDiffer is the not-just-recolors check: with SGR
// stripped, no two figures share a glyph layout or a silhouette.
func TestBuiltinLayoutsDiffer(t *testing.T) {
	glyphs := map[string]string{}
	shapes := map[string]string{}
	for _, id := range BuiltinIDs() {
		layout := stripANSI(source(t, id))
		if other, ok := glyphs[layout]; ok {
			t.Errorf("%s and %s have the same glyph layout", id, other)
		}
		glyphs[layout] = id
		shape := silhouette(layout)
		if other, ok := shapes[shape]; ok {
			t.Errorf("%s and %s have the same silhouette", id, other)
		}
		shapes[shape] = id
	}
}

func TestSilhouette(t *testing.T) {
	if silhouette("ab \n c") != silhouette("xy \n z") {
		t.Error("a recolor or glyph swap changed the silhouette")
	}
	if silhouette("ab \n c") == silhouette("a b\n c") {
		t.Error("a moved cell kept the silhouette")
	}
}

// silhouette marks each cell of layout as ink or blank.
func silhouette(layout string) string {
	var b strings.Builder
	for _, r := range layout {
		switch r {
		case ' ', '\n':
			b.WriteRune(r)
		default:
			b.WriteByte('#')
		}
	}
	return b.String()
}

var sgrPattern = regexp.MustCompile("\x1b\\[([0-9;]*)m")

// TestBuiltinColors checks that every figure reads on both a white and a
// black background: SGR is only a reset or a truecolor foreground, with
// no background fill or reverse video, and every color has at least 2:1
// contrast against both white and black.
func TestBuiltinColors(t *testing.T) {
	for _, id := range BuiltinIDs() {
		text := source(t, id)
		if n := strings.Count(text, "\x1b"); n != len(sgrPattern.FindAllString(text, -1)) {
			t.Errorf("%s has escapes that are not SGR", id)
		}
		for _, m := range sgrPattern.FindAllStringSubmatch(text, -1) {
			params := m[1]
			if params == "0" || params == "" {
				continue
			}
			rgb, err := truecolorFG(params)
			if err != nil {
				t.Errorf("%s: %v", id, err)
				continue
			}
			l := relativeLuminance(rgb)
			onBlack, onWhite := (l+0.05)/0.05, 1.05/(l+0.05)
			if onBlack < 2 || onWhite < 2 {
				t.Errorf("%s: color %v contrast %.2f on black, %.2f on white; want at least 2 on both", id, rgb, onBlack, onWhite)
			}
		}
	}
}

func truecolorFG(params string) ([3]int, error) {
	parts := strings.Split(params, ";")
	if len(parts) != 5 || parts[0] != "38" || parts[1] != "2" {
		return [3]int{}, fmt.Errorf("SGR %q is not a truecolor foreground", params)
	}
	var rgb [3]int
	for i := range rgb {
		v, err := strconv.Atoi(parts[i+2])
		if err != nil || v < 0 || v > 255 {
			return [3]int{}, fmt.Errorf("SGR %q has a bad component", params)
		}
		rgb[i] = v
	}
	return rgb, nil
}

func relativeLuminance(rgb [3]int) float64 {
	lin := func(v int) float64 {
		c := float64(v) / 255
		if c <= 0.03928 {
			return c / 12.92
		}
		return math.Pow((c+0.055)/1.055, 2.4)
	}
	return 0.2126*lin(rgb[0]) + 0.7152*lin(rgb[1]) + 0.0722*lin(rgb[2])
}

func TestBuiltinNoColor(t *testing.T) {
	for _, id := range BuiltinIDs() {
		t.Setenv("NO_COLOR", "1")
		lines, err := Builtin(id, builtinCols, builtinRows, termenv.TrueColor)
		if err != nil {
			t.Fatal(err)
		}
		if s := strings.Join(lines, ""); strings.Contains(s, "\x1b") {
			t.Errorf("%s under NO_COLOR has escapes: %q", id, s)
		}
		t.Setenv("NO_COLOR", "")
		ascii, err := Builtin(id, builtinCols, builtinRows, termenv.Ascii)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Join(ascii, "\n") != strings.Join(lines, "\n") {
			t.Errorf("%s: Ascii and NO_COLOR differ", id)
		}
	}
}

func TestBuiltinUnknown(t *testing.T) {
	for _, id := range []string{"no-such-figure", "", "../builtin/owl", "owl.txt"} {
		if _, err := Builtin(id, builtinCols, builtinRows, termenv.TrueColor); err == nil {
			t.Errorf("Builtin(%q): want error", id)
		}
	}
}

func TestPickBuiltin(t *testing.T) {
	ids := BuiltinIDs()
	known := map[string]bool{}
	for _, id := range ids {
		known[id] = true
	}
	// Golden: generation-1 names keep the #647 pick (FNV-1a 32 of the
	// name, mod 24, over the sorted generation-1 ids); the others moved
	// to a generation-2 figure.
	golden := map[string]string{
		"coord/primary":   "snail",
		"pm-1":            "planet",
		"agent-5":         "mushroom",
		"agent-0":         "king",
		"agent-3":         "jester",
		"coord/secondary": "king",
	}
	for _, name := range []string{"coord/primary", "pm-1", "agent-5"} {
		if g := pickGeneration(name); g != 0 {
			t.Errorf("pickGeneration(%q) = %d, want 0", name, g)
		}
	}
	for _, name := range []string{"agent-0", "agent-3", "coord/secondary"} {
		if g := pickGeneration(name); g != 1 {
			t.Errorf("pickGeneration(%q) = %d, want 1", name, g)
		}
	}
	for name, want := range golden {
		if got := PickBuiltin(name); got != want {
			t.Errorf("PickBuiltin(%q) = %q, want %q", name, got, want)
		}
	}
	used := map[string]bool{}
	for i := range 50 {
		name := fmt.Sprintf("agent-%d", i)
		id := PickBuiltin(name)
		if !known[id] {
			t.Fatalf("PickBuiltin(%q) = %q, not a built-in", name, id)
		}
		if PickBuiltin(name) != id {
			t.Fatalf("PickBuiltin(%q) is not stable", name)
		}
		used[id] = true
	}
	if len(used) < 15 {
		t.Errorf("50 names used %d figures, want at least 15", len(used))
	}
}

// generation1 is the #647 set, which must never change.
var generation1 = []string{
	"bee", "bun", "cactus", "cat", "crab", "fox", "frog", "ghost",
	"glasses", "headphones", "knight-helmet", "lighthouse", "mushroom",
	"octopus", "owl", "penguin", "planet", "robot", "rocket", "snail",
	"teapot", "tree", "whale", "wizard-hat",
}

func TestGenerations(t *testing.T) {
	if !slices.Equal(builtinGenerations[0], generation1) {
		t.Errorf("generation 1 = %q, want %q", builtinGenerations[0], generation1)
	}
	want2 := []string{
		"banker", "beer-mug", "bishop", "book", "cook", "dog", "general",
		"hammer", "jester", "king", "merchant", "peasant", "physician",
		"pirate", "queen", "scholar", "spy", "witch",
	}
	if len(builtinGenerations) < 2 || !slices.Equal(builtinGenerations[1], want2) {
		t.Fatalf("generations = %q, want generation 2 %q", builtinGenerations, want2)
	}
	var all []string
	for g, ids := range builtinGenerations {
		if len(ids) == 0 {
			t.Errorf("generation %d is empty", g+1)
		}
		for _, id := range ids {
			if _, ok := builtinSource(id); !ok {
				t.Errorf("generation %d id %q has no figure file", g+1, id)
			}
		}
		all = append(all, ids...)
	}
	slices.Sort(all)
	entries, err := builtinFS.ReadDir("builtin")
	if err != nil {
		t.Fatal(err)
	}
	var files []string
	for _, e := range entries {
		files = append(files, strings.TrimSuffix(e.Name(), ".txt"))
	}
	if !slices.Equal(all, files) {
		t.Errorf("generation ids %q differ from figure files %q", all, files)
	}
	if !slices.Equal(BuiltinIDs(), all) {
		t.Errorf("BuiltinIDs = %q, want every generation's ids sorted", BuiltinIDs())
	}
}

// legacyPick is the #647 rule: FNV-1a 32 of name mod 24 over the sorted
// generation-1 ids.
func legacyPick(name string) string {
	h := fnv.New32a()
	_, _ = h.Write([]byte(name))
	return generation1[h.Sum32()%24]
}

func TestPickBuiltinKeepsGeneration1(t *testing.T) {
	newIDs := map[string]bool{}
	for _, ids := range builtinGenerations[1:] {
		for _, id := range ids {
			newIDs[id] = true
		}
	}
	stayed := 0
	moved := map[string]bool{}
	for i := range 200 {
		name := fmt.Sprintf("agent-%d", i)
		got := PickBuiltin(name)
		if pickGeneration(name) == 0 {
			stayed++
			if want := legacyPick(name); got != want {
				t.Errorf("PickBuiltin(%q) = %q, want the #647 pick %q", name, got, want)
			}
			continue
		}
		if !newIDs[got] {
			t.Errorf("PickBuiltin(%q) = %q moved to an old figure", name, got)
		}
		moved[got] = true
	}
	if stayed < 100 {
		t.Errorf("%d of 200 names kept their figure, want at least 100", stayed)
	}
	if len(moved) < 12 {
		t.Errorf("moved names use %d new figures, want at least 12", len(moved))
	}
}

// TestDrawGenerationAppendOnly checks that adding a generation only moves
// names into it: a name that draws an older generation draws the same
// one without the newer generations.
func TestDrawGenerationAppendOnly(t *testing.T) {
	sizes := []int{24, 18, 30, 7}
	for i := range 2000 {
		name := fmt.Sprintf("name-%d", i)
		full := drawGeneration(sizes, name)
		for k := full + 1; k < len(sizes); k++ {
			if got := drawGeneration(sizes[:k], name); got != full {
				t.Fatalf("%q draws %d of %d generations but %d of %d", name, full, len(sizes), got, k)
			}
		}
	}
}

// TestPickGenerationWeights checks each generation draws about its share
// of names.
func TestPickGenerationWeights(t *testing.T) {
	counts := make([]int, len(builtinGenerations))
	const n = 20000
	for i := range n {
		counts[pickGeneration(fmt.Sprintf("name-%d", i))]++
	}
	total := 0
	for _, ids := range builtinGenerations {
		total += len(ids)
	}
	for g, ids := range builtinGenerations {
		want := float64(n*len(ids)) / float64(total)
		if math.Abs(float64(counts[g])-want) > 0.03*n {
			t.Errorf("generation %d drew %d of %d names, want about %.0f", g+1, counts[g], n, want)
		}
	}
}

func TestForName(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	got := ForName("coord/primary", builtinCols, builtinRows, termenv.TrueColor)
	want, err := Builtin(PickBuiltin("coord/primary"), builtinCols, builtinRows, termenv.TrueColor)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Errorf("ForName differs from the picked built-in")
	}
}
