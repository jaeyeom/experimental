package scan

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCODEOWNERSFixtureLastMatch(t *testing.T) {
	t.Parallel()

	body, err := os.ReadFile(filepath.Join("testdata", "fixtures", "CODEOWNERS"))
	if err != nil {
		t.Fatal(err)
	}
	rules := parseCODEOWNERS(string(body))
	tests := []struct {
		file  string
		login string
		team  string
	}{
		{file: "README.md", team: "acme/everyone"},
		{file: "src/foo/bar.go", team: "acme/foo-team"},
		{file: "src/foo/nested/x.go", team: "acme/foo-team"},
		{file: "src/foo/special.go", login: "alice"},
	}
	for _, tc := range tests {
		t.Run(tc.file, func(t *testing.T) {
			t.Parallel()
			rule, ok := lastCodeOwner(rules, tc.file)
			if !ok || len(rule.owners) != 1 {
				t.Fatalf("owners of %s = %+v, ok %v", tc.file, rule.owners, ok)
			}
			if rule.owners[0].login != tc.login || rule.owners[0].team != tc.team {
				t.Fatalf("owner = %+v, want login %q team %q", rule.owners[0], tc.login, tc.team)
			}
		})
	}
}

func TestCODEOWNERSGitHubPatterns(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		pattern string
		file    string
		want    bool
	}{
		{name: "star basename", pattern: "*.js", file: "dir/a.js", want: true},
		{name: "star misses other suffix", pattern: "*.js", file: "dir/a.ts", want: false},
		{name: "docs star one level", pattern: "docs/*", file: "docs/getting-started.md", want: true},
		{name: "docs star not nested", pattern: "docs/*", file: "docs/build-app/troubleshooting.md", want: false},
		{name: "unanchored directory anywhere", pattern: "apps/", file: "src/apps/main.go", want: true},
		{name: "unanchored directory misses prefix", pattern: "apps/", file: "applications/main.go", want: false},
		{name: "anchored directory includes nested", pattern: "/docs/", file: "docs/a/b.md", want: true},
		{name: "anchored directory is root only", pattern: "/docs/", file: "src/docs/a.md", want: false},
		{name: "doublestar under foo", pattern: "/src/foo/**", file: "src/foo/a/b.go", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := codeownersMatch(tc.pattern, tc.file); got != tc.want {
				t.Fatalf("codeownersMatch(%q, %q) = %v, want %v", tc.pattern, tc.file, got, tc.want)
			}
		})
	}
}

func TestParseCODEOWNERSSkipsSectionsAndComments(t *testing.T) {
	t.Parallel()

	rules := parseCODEOWNERS("# comment\n[Dev]\n*.go @bob # inline\n")
	if len(rules) != 1 || rules[0].pattern != "*.go" || rules[0].owners[0].login != "bob" {
		t.Fatalf("rules = %+v", rules)
	}
}
