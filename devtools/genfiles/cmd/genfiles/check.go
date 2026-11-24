package main

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/pkg/genfiles"
)

const linguistAttr = "linguist-generated"

// checkRepo checks a generated-files list against the tracked files and the
// root .gitattributes text. It returns one line per problem, the number of
// patterns, and the number of tracked files the list matches.
func checkRepo(list []byte, attrs string, tracked []string) (problems []string, patterns, matched int) {
	entries, err := genfiles.Parse(list)
	if err != nil {
		return strings.Split(err.Error(), "\n"), 0, 0
	}
	used := make([]bool, len(entries))
	for _, path := range tracked {
		hit := false
		for i, e := range entries {
			if !genfiles.Match(e.Pattern, path) {
				continue
			}
			used[i] = true
			if !hit && unsafeName(path) {
				problems = append(problems, fmt.Sprintf(
					"unsafe file name %q matched by %q: a path segment starts with \"-\" or the path has a control character; rename the file",
					path, e.Pattern))
			}
			hit = true
		}
		if hit {
			matched++
		}
	}
	for i, e := range entries {
		if !used[i] {
			problems = append(problems, fmt.Sprintf(
				"stale pattern %q (line %d): no tracked file matches; remove the entry if the files are gone",
				e.Pattern, e.Line))
		}
	}
	return append(problems, checkAttributes(entries, attrs)...), len(entries), matched
}

// unsafeName reports a path a program could misread even with no shell: a
// segment that looks like an option, or a control character such as newline.
func unsafeName(path string) bool {
	for _, seg := range strings.Split(path, "/") {
		if strings.HasPrefix(seg, "-") {
			return true
		}
	}
	return strings.IndexFunc(path, unicode.IsControl) >= 0
}

// checkAttributes requires .gitattributes to set linguist-generated on
// exactly the listed patterns, once each. Lines that unset it are allowed,
// for handwritten files a listed glob also matches.
func checkAttributes(entries []genfiles.Entry, attrs string) []string {
	listed := map[string]bool{}
	for _, e := range entries {
		listed[e.Pattern] = true
	}
	var problems []string
	marked := map[string]bool{}
	for i, line := range strings.Split(attrs, "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || strings.HasPrefix(fields[0], "#") || strings.HasPrefix(fields[0], "[attr]") {
			continue
		}
		if !setsGenerated(fields[1:]) {
			continue
		}
		pattern := fields[0]
		switch {
		case marked[pattern]:
			problems = append(problems, fmt.Sprintf(
				".gitattributes:%d: %q is marked %s more than once", i+1, pattern, linguistAttr))
		case !listed[pattern]:
			problems = append(problems, fmt.Sprintf(
				".gitattributes:%d: %q is marked %s but is not in the list; add an entry or drop the attribute",
				i+1, pattern, linguistAttr))
		}
		marked[pattern] = true
	}
	for _, e := range entries {
		if !marked[e.Pattern] {
			problems = append(problems, fmt.Sprintf(
				".gitattributes: no %q line for a listed pattern", e.Pattern+" "+linguistAttr))
		}
	}
	return problems
}

func setsGenerated(attrs []string) bool {
	set := false
	for _, a := range attrs {
		switch a {
		case linguistAttr, linguistAttr + "=true":
			set = true
		case "-" + linguistAttr, "!" + linguistAttr, linguistAttr + "=false":
			set = false
		}
	}
	return set
}
