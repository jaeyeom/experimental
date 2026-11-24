package main

import (
	"strings"
	"testing"
)

const goodList = `- pattern: Cargo.lock
  run: [[cargo, generate-lockfile]]
- pattern: "docs/*.md"
`

const goodAttrs = `# comment
[attr]binary -diff -merge -text
Cargo.lock -merge linguist-generated=true
docs/*.md linguist-generated
docs/hand.md linguist-generated=false
docs/other.md -linguist-generated
`

func TestCheckRepoClean(t *testing.T) {
	got, patterns, matched := checkRepo([]byte(goodList), goodAttrs, []string{"Cargo.lock", "sub/Cargo.lock", "docs/a.md", "README.md"})
	if len(got) != 0 {
		t.Fatalf("problems = %q, want none", got)
	}
	if patterns != 2 || matched != 3 {
		t.Errorf("patterns, matched = %d, %d, want 2, 3", patterns, matched)
	}
}

func TestCheckRepoProblems(t *testing.T) {
	tests := []struct {
		name, list, attrs string
		tracked           []string
		want              string
	}{
		{"parse error", "- pattern: x\n  foo: 1\n", "", nil, `line 2: entry 1: unknown key "foo"`},
		{"stale pattern", goodList, goodAttrs, []string{"Cargo.lock"}, `stale pattern "docs/*.md" (line 3): no tracked file matches`},
		{"leading dash", goodList, goodAttrs, []string{"-x/Cargo.lock", "docs/a.md"}, `unsafe file name "-x/Cargo.lock" matched by "Cargo.lock"`},
		{"newline", goodList, goodAttrs, []string{"a/b\nc/Cargo.lock", "docs/a.md"}, `unsafe file name "a/b\nc/Cargo.lock"`},
		{"attr missing", goodList, "Cargo.lock linguist-generated\n", []string{"Cargo.lock", "docs/a.md"}, `.gitattributes: no "docs/*.md linguist-generated" line for a listed pattern`},
		{"attr extra", goodList, goodAttrs + "x.json linguist-generated=true\n", []string{"Cargo.lock", "docs/a.md"}, `.gitattributes:7: "x.json" is marked linguist-generated but is not in the list`},
		{"attr duplicate", goodList, goodAttrs + "Cargo.lock linguist-generated\n", []string{"Cargo.lock", "docs/a.md"}, `.gitattributes:7: "Cargo.lock" is marked linguist-generated more than once`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, _, _ := checkRepo([]byte(tt.list), tt.attrs, tt.tracked)
			joined := strings.Join(got, "\n")
			if !strings.Contains(joined, tt.want) {
				t.Errorf("problems = %q, want one containing %q", got, tt.want)
			}
		})
	}
}
