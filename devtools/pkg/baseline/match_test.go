package baseline

import "testing"

func TestNormalizeTarget(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		want    string
		wantErr bool
	}{
		{name: "simple", input: "devtools/pkg/baseline/match.go", want: "devtools/pkg/baseline/match.go"},
		{name: "leading dot slash", input: "./a/b.go", want: "a/b.go"},
		{name: "trailing slash", input: "a/b/", want: "a/b"},
		{name: "redundant slashes and dot", input: "a//./b/c", want: "a/b/c"},
		{name: "glob pattern", input: "**/testdata/**", want: "**/testdata/**"},
		{name: "empty", input: "", wantErr: true},
		{name: "dot", input: ".", wantErr: true},
		{name: "dot slash", input: "./", wantErr: true},
		{name: "absolute", input: "/etc/passwd", wantErr: true},
		{name: "parent traversal", input: "../a/b", wantErr: true},
		{name: "mid parent traversal", input: "a/../b", wantErr: true},
		{name: "backslash", input: `a\b\c`, wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := NormalizeTarget(tc.input)
			if (err != nil) != tc.wantErr {
				t.Fatalf("NormalizeTarget(%q) error = %v, wantErr %v", tc.input, err, tc.wantErr)
			}
			if got != tc.want {
				t.Errorf("NormalizeTarget(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}

func TestIsGlob(t *testing.T) {
	tests := []struct {
		pattern string
		want    bool
	}{
		{pattern: "a/b/c.go", want: false},
		{pattern: "a/*/c.go", want: true},
		{pattern: "a/?.go", want: true},
		{pattern: "a/[ab].go", want: true},
		{pattern: "**/testdata/**", want: true},
	}
	for _, tc := range tests {
		if got := IsGlob(tc.pattern); got != tc.want {
			t.Errorf("IsGlob(%q) = %v, want %v", tc.pattern, got, tc.want)
		}
	}
}

func TestMatchGlob(t *testing.T) {
	tests := []struct {
		name    string
		pattern string
		target  string
		want    bool
		wantErr bool
	}{
		{name: "exact", pattern: "a/b/c.go", target: "a/b/c.go", want: true},
		{name: "single star within segment", pattern: "a/*.go", target: "a/main.go", want: true},
		{name: "single star does not cross slash", pattern: "a/*.go", target: "a/b/main.go", want: false},
		{name: "question mark", pattern: "a/b?.go", target: "a/bc.go", want: true},
		{name: "question mark no match slash", pattern: "a?c", target: "a/c", want: false},
		{name: "char class", pattern: "a/[bc]*.go", target: "a/bar.go", want: true},
		{name: "char class miss", pattern: "a/[bc]*.go", target: "a/dog.go", want: false},
		{name: "double star zero segments", pattern: "**/testdata/**", target: "testdata/foo.go", want: true},
		{name: "double star multiple segments", pattern: "**/testdata/**", target: "a/b/testdata/c/d.go", want: true},
		{name: "double star trailing zero segments", pattern: "**/testdata/**", target: "a/b/testdata", want: true},
		{name: "double star middle zero segments", pattern: "a/**/b", target: "a/b", want: true},
		{name: "double star middle many segments", pattern: "a/**/b", target: "a/x/y/z/b", want: true},
		{name: "double star root wildcard", pattern: "**/*.go", target: "main.go", want: true},
		{name: "double star nested wildcard", pattern: "**/*.go", target: "a/b/main.go", want: true},
		{name: "double star all", pattern: "**", target: "a/b/c", want: true},
		{name: "consecutive double stars", pattern: "**/**/b", target: "a/b", want: true},
		{name: "mismatch with double star", pattern: "a/**/b", target: "a/c/d", want: false},
		{name: "invalid bracket", pattern: "a/**/[bad", target: "x/y", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := MatchGlob(tc.pattern, tc.target)
			if (err != nil) != tc.wantErr {
				t.Fatalf("MatchGlob(%q, %q) error = %v, wantErr %v", tc.pattern, tc.target, err, tc.wantErr)
			}
			if got != tc.want {
				t.Errorf("MatchGlob(%q, %q) = %v, want %v", tc.pattern, tc.target, got, tc.want)
			}
		})
	}
}

func TestResolvePrecedence(t *testing.T) {
	f := &File{
		Version: CurrentVersion,
		Checker: "test",
		Entries: map[string]Entry{
			"devtools/pkg/a.go":   {Reason: "exact"},
			"devtools/pkg/*.go":   {Reason: "longest-prefix"},
			"devtools/**/*.go":    {Reason: "shorter-prefix"},
			"devtools/pkg/?*":     {Reason: "tie-first-lex"},
			"devtools/pkg/[a-z]*": {Reason: "tie-second-lex"},
		},
	}

	t.Run("exact beats glob", func(t *testing.T) {
		key, entry, ok := f.Resolve("devtools/pkg/a.go")
		if !ok || key != "devtools/pkg/a.go" || entry.Reason != "exact" {
			t.Errorf("Resolve exact = (%q, %+v, %v), want exact", key, entry, ok)
		}
	})

	t.Run("longest non-wildcard prefix wins", func(t *testing.T) {
		// "devtools/pkg/*.go" has prefix "devtools/pkg/" (13 chars).
		// "devtools/**/*.go" has prefix "devtools/" (9 chars).
		// "devtools/pkg/?*" and "devtools/pkg/[a-z]*" also have 13-char prefix;
		// lexicographically "*" (0x2a) < "?" (0x3f) < "[" (0x5b), so "devtools/pkg/*.go" wins!
		key, entry, ok := f.Resolve("devtools/pkg/b.go")
		if !ok || key != "devtools/pkg/*.go" || entry.Reason != "longest-prefix" {
			t.Errorf("Resolve glob = (%q, %+v, %v), want devtools/pkg/*.go", key, entry, ok)
		}
	})

	t.Run("lexicographical tie breaker", func(t *testing.T) {
		// "devtools/pkg/?*" vs "devtools/pkg/[a-z]*" on target without ".go" suffix.
		// Both have prefix "devtools/pkg/" (13 chars); '?' < '[' lexicographically.
		key, entry, ok := f.Resolve("devtools/pkg/README")
		if !ok || key != "devtools/pkg/?*" || entry.Reason != "tie-first-lex" {
			t.Errorf("Resolve tie = (%q, %+v, %v), want devtools/pkg/?*", key, entry, ok)
		}
	})

	t.Run("no match", func(t *testing.T) {
		key, _, ok := f.Resolve("other/file.txt")
		if ok {
			t.Errorf("Resolve miss = (%q, %v), want ok=false", key, ok)
		}
	})

	t.Run("nil file", func(t *testing.T) {
		var nilFile *File
		if _, _, ok := nilFile.Resolve("a/b"); ok {
			t.Error("nil File.Resolve() = true, want false")
		}
	})
}
