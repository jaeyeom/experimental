package avatar

import (
	"embed"
	"fmt"
	"hash/fnv"
	"strings"

	"github.com/muesli/termenv"
)

// builtinFS holds the built-in figures, one 10x6 ANSI text file per id.
//
//go:embed builtin/*.txt
var builtinFS embed.FS

// BuiltinIDs returns the built-in figure ids in sorted order.
func BuiltinIDs() []string {
	entries, err := builtinFS.ReadDir("builtin")
	if err != nil {
		panic(err) // the directory is embedded
	}
	ids := make([]string, len(entries))
	for i, e := range entries {
		ids[i] = strings.TrimSuffix(e.Name(), ".txt")
	}
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

// PickBuiltin returns the built-in id for name: the FNV-1a 32-bit hash of
// name, mod the number of built-ins, indexes the sorted ids. The pick
// depends only on name and the set.
func PickBuiltin(name string) string {
	ids := BuiltinIDs()
	h := fnv.New32a()
	_, _ = h.Write([]byte(name))
	return ids[h.Sum32()%uint32(len(ids))] //nolint:gosec // G115: the set is a few dozen figures
}

// ForName renders the built-in PickBuiltin chooses for name.
func ForName(name string, cols, rows int, profile termenv.Profile) []string {
	lines, err := Builtin(PickBuiltin(name), cols, rows, profile)
	if err != nil {
		panic(err) // PickBuiltin returns an embedded id
	}
	return lines
}
