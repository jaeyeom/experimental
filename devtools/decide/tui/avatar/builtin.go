package avatar

import (
	"crypto/sha256"
	"embed"
	"encoding/binary"
	"fmt"
	"hash/fnv"
	"slices"
	"strings"

	"github.com/muesli/termenv"
)

// builtinFS holds the built-in figures, one 10x6 ANSI text file per id.
//
//go:embed builtin/*.txt
var builtinFS embed.FS

// builtinGenerations lists the built-in ids in the order they were added.
// A generation never changes once added; new figures go in a new
// generation at the end, so names that drew an older one keep their pick.
var builtinGenerations = [][]string{
	{
		"bee", "bun", "cactus", "cat", "crab", "fox", "frog", "ghost",
		"glasses", "headphones", "knight-helmet", "lighthouse", "mushroom",
		"octopus", "owl", "penguin", "planet", "robot", "rocket", "snail",
		"teapot", "tree", "whale", "wizard-hat",
	},
	{
		"banker", "beer-mug", "bishop", "book", "cook", "dog", "general",
		"hammer", "jester", "king", "merchant", "peasant", "physician",
		"pirate", "queen", "scholar", "spy", "witch",
	},
}

// BuiltinIDs returns the built-in figure ids in sorted order.
func BuiltinIDs() []string {
	var ids []string
	for _, gen := range builtinGenerations {
		ids = append(ids, gen...)
	}
	slices.Sort(ids)
	return ids
}

// builtinSource returns the text of the built-in figure id.
func builtinSource(id string) (string, bool) {
	if strings.ContainsAny(id, "/.") {
		return "", false
	}
	data, err := builtinFS.ReadFile("builtin/" + id + ".txt")
	if err != nil {
		return "", false
	}
	return string(data), true
}

// Builtin renders the built-in figure id fit to cols x rows. An unknown id
// is an error.
func Builtin(id string, cols, rows int, profile termenv.Profile) ([]string, error) {
	text, ok := builtinSource(id)
	if !ok {
		return nil, fmt.Errorf("unknown built-in avatar %q", id)
	}
	return renderText(text, cols, rows, effectiveProfile(profile)), nil
}

// PickBuiltin returns the built-in id for name. pickGeneration chooses a
// generation, weighted by size; then the FNV-1a 32-bit hash of name, mod
// that generation's size, indexes its ids. The pick depends only on name
// and the generations.
func PickBuiltin(name string) string {
	ids := builtinGenerations[pickGeneration(name)]
	h := fnv.New32a()
	_, _ = h.Write([]byte(name))
	return ids[h.Sum32()%uint32(len(ids))] //nolint:gosec // G115: a generation is a few dozen figures
}

// pickGeneration is the index of the generation name draws.
func pickGeneration(name string) int {
	sizes := make([]int, len(builtinGenerations))
	for i, gen := range builtinGenerations {
		sizes[i] = len(gen)
	}
	return drawGeneration(sizes, name)
}

// drawGeneration picks a generation for name, each with probability
// proportional to its size. From the newest down, generation g takes name
// when a hash of (g, name), mod the total size of generations 0..g, falls
// in g's share. Generation g's test ignores later generations, so adding
// one moves names only into it: about new/(old+new) of them.
func drawGeneration(sizes []int, name string) int {
	total := 0
	for _, n := range sizes {
		total += n
	}
	for g := len(sizes) - 1; g > 0; g-- {
		sum := sha256.Sum256(fmt.Appendf(nil, "decide avatar generation %d\x00%s", g, name))
		draw := binary.BigEndian.Uint64(sum[:8]) % uint64(total) //nolint:gosec // G115: total is positive
		total -= sizes[g]
		if draw >= uint64(total) { //nolint:gosec // G115: total is positive
			return g
		}
	}
	return 0
}

// ForName renders the built-in PickBuiltin chooses for name.
func ForName(name string, cols, rows int, profile termenv.Profile) []string {
	lines, err := Builtin(PickBuiltin(name), cols, rows, profile)
	if err != nil {
		panic(err) // PickBuiltin returns an embedded id
	}
	return lines
}
