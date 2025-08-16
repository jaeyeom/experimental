package codeowners

import (
	"strings"
	"testing"
)

func TestSectionBasedOwners(t *testing.T) {
	const codeowners = `
*                                                                   @myorg/root-reviewers
package.json                                                        @myorg/package-reviewers
/myorg/api/**                                                       @myorg/api-reviewers
/myorg/api/**/*.proto                                               @myorg/public-api-proto-reviewers
/myorg/cloud/**                                                     @myorg/cloud-reviewers
/myorg/device/**                                                    @myorg/universe-msa-reviewers

## READABILITY
**/*.go                         @myorg/go-readability-reviewers
`

	testCases := []struct {
		name string
		file string
		want []string
	}{
		{
			name: "package.json",
			file: "package.json",
			want: []string{"@myorg/package-reviewers"},
		},
		{
			name: "myorg/api/foo.go",
			file: "myorg/api/foo.go",
			want: []string{"@myorg/api-reviewers", "@myorg/go-readability-reviewers"},
		},
		{
			name: "myorg/api/foo.proto",
			file: "myorg/api/foo.proto",
			want: []string{"@myorg/public-api-proto-reviewers"},
		},
		{
			name: "myorg/cloud/bar.txt",
			file: "myorg/cloud/bar.txt",
			want: []string{"@myorg/cloud-reviewers"},
		},
		{
			name: "myorg/device/baz.txt",
			file: "myorg/device/baz.txt",
			want: []string{"@myorg/universe-msa-reviewers"},
		},
		{
			name: "foo.go",
			file: "foo.go",
			want: []string{"@myorg/root-reviewers", "@myorg/go-readability-reviewers"},
		},
		{
			name: "unknown.txt",
			file: "unknown.txt",
			want: []string{"@myorg/root-reviewers"},
		},
	}

	owners := ParseSections(strings.NewReader(codeowners))

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got := owners.OwnersFor(tc.file)
			// Convert Owner interface to strings for comparison
			gotStrings := make([]string, len(got))
			for i, owner := range got {
				gotStrings[i] = owner.String()
			}
			if !equalUnordered(gotStrings, tc.want) {
				t.Errorf("file %q: got owners %v, want %v", tc.file, gotStrings, tc.want)
			}
		})
	}
}

func TestParseOwner(t *testing.T) {
	testCases := []struct {
		input    string
		expected Owner
	}{
		{
			input:    "@username",
			expected: User{Name: "username"},
		},
		{
			input:    "@myorg/team",
			expected: Team{Org: "myorg", Name: "team"},
		},
		{
			input:    "user@example.com",
			expected: Email{Address: "user@example.com"},
		},
		{
			input:    "  @spaced  ",
			expected: User{Name: "spaced"},
		},
		{
			input:    "username", // without @
			expected: User{Name: "username"},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.input, func(t *testing.T) {
			got := ParseOwner(tc.input)
			if got.String() != tc.expected.String() {
				t.Errorf("ParseOwner(%q): got %v (%T), want %v (%T)",
					tc.input, got, got, tc.expected, tc.expected)
			}

			// Also test that the types match
			switch expected := tc.expected.(type) {
			case User:
				if user, ok := got.(User); !ok || user.Name != expected.Name {
					t.Errorf("ParseOwner(%q): expected User{Name: %q}, got %v",
						tc.input, expected.Name, got)
				}
			case Team:
				if team, ok := got.(Team); !ok || team.Org != expected.Org || team.Name != expected.Name {
					t.Errorf("ParseOwner(%q): expected Team{Org: %q, Name: %q}, got %v",
						tc.input, expected.Org, expected.Name, got)
				}
			case Email:
				if email, ok := got.(Email); !ok || email.Address != expected.Address {
					t.Errorf("ParseOwner(%q): expected Email{Address: %q}, got %v",
						tc.input, expected.Address, got)
				}
			}
		})
	}
}

func TestMatchPattern(t *testing.T) {
	testCases := []struct {
		pattern string
		file    string
		want    bool
		desc    string
	}{
		// Exact matches
		{"foo.go", "foo.go", true, "exact match"},
		{"foo.go", "bar.go", false, "exact non-match"},
		{"dir/foo.go", "dir/foo.go", true, "exact path match"},
		{"dir/foo.go", "dir/bar.go", false, "exact path non-match"},

		// Simple glob patterns
		{"*.go", "foo.go", true, "simple extension match"},
		{"*.go", "foo.txt", false, "simple extension non-match"},
		{"*.go", "dir/foo.go", false, "extension at root only - should not match subdirs"},
		{"foo.*", "foo.go", true, "simple prefix match"},
		{"foo.*", "bar.go", false, "simple prefix non-match"},

		// Directory patterns with **
		{"**/*.go", "foo.go", true, "** extension at root"},
		{"**/*.go", "dir/foo.go", true, "** extension in subdir"},
		{"**/*.go", "dir/subdir/foo.go", true, "** extension in deep subdir"},
		{"**/*.go", "foo.txt", false, "** extension non-match"},

		// Directory prefix patterns
		{"dir/**", "dir/foo.go", true, "directory prefix match"},
		{"dir/**", "dir/subdir/foo.go", true, "directory prefix deep match"},
		{"dir/**", "other/foo.go", false, "directory prefix non-match"},
		{"dir/**", "dir", false, "directory itself doesn't match **"},

		// Complex patterns with directory and extension
		{"myorg/api/**/*.proto", "myorg/api/foo.proto", true, "complex pattern match"},
		{"myorg/api/**/*.proto", "myorg/api/v1/foo.proto", true, "complex pattern with subdir"},
		{"myorg/api/**/*.proto", "myorg/api/v1/v2/foo.proto", true, "complex pattern with deep subdir"},
		{"myorg/api/**/*.proto", "myorg/other/foo.proto", false, "complex pattern wrong dir"},
		{"myorg/api/**/*.proto", "myorg/api/foo.go", false, "complex pattern wrong extension"},
		{"myorg/api/**/*.proto", "other/api/foo.proto", false, "complex pattern wrong prefix"},

		// Edge cases with leading/trailing slashes
		{"/foo.go", "foo.go", true, "leading slash pattern"},
		{"foo.go", "/foo.go", true, "leading slash file"},
		{"/dir/foo.go", "/dir/foo.go", true, "both leading slashes"},
		{"/dir/foo.go", "dir/foo.go", true, "pattern leading slash only"},

		// Multiple ** patterns
		{"**/src/**/*.go", "src/foo.go", true, "multiple ** - direct"},
		{"**/src/**/*.go", "proj/src/foo.go", true, "multiple ** - with prefix"},
		{"**/src/**/*.go", "proj/src/util/foo.go", true, "multiple ** - with subdir"},
		{"**/src/**/*.go", "proj/test/foo.go", false, "multiple ** - wrong middle"},

		// Patterns with single *
		{"foo*.go", "foo.go", true, "single * suffix"},
		{"foo*.go", "foobar.go", true, "single * with content"},
		{"foo*.go", "bar.go", false, "single * non-match"},
		{"*foo.go", "testfoo.go", true, "single * prefix"},
		{"*foo.go", "foo.go", true, "single * empty prefix"},
		{"*foo.go", "bar.go", false, "single * prefix non-match"},

		// Question mark patterns
		{"foo?.go", "foox.go", true, "question mark match"},
		{"foo?.go", "foo.go", false, "question mark requires char"},
		{"foo?.go", "fooxx.go", false, "question mark single char only"},

		// Mixed patterns
		{"test/**/foo*.go", "test/foo.go", true, "mixed ** and *"},
		{"test/**/foo*.go", "test/util/foobar.go", true, "mixed ** and * with subdir"},
		{"test/**/foo*.go", "test/util/bar.go", false, "mixed ** and * non-match"},

		// Empty and special cases
		{"", "", true, "both empty"},
		{"", "foo.go", false, "empty pattern, non-empty file"},
		{"foo.go", "", false, "non-empty pattern, empty file"},

		// Case sensitivity (should be case sensitive)
		{"Foo.go", "foo.go", false, "case sensitive - different case"},
		{"foo.go", "Foo.go", false, "case sensitive - different case reverse"},

		// Patterns without globs that should not match subdirs
		{"foo.go", "dir/foo.go", false, "no glob should not match subdir"},
		{"dir/foo.go", "dir/subdir/foo.go", false, "specific path should not match deeper"},

		// Real-world CODEOWNERS patterns
		{"*", "anything.txt", true, "global pattern"},
		{"package.json", "package.json", true, "specific file"},
		{"*.md", "README.md", true, "markdown files"},
		{"docs/**", "docs/readme.txt", true, "docs directory"},
		{"src/**/*.ts", "src/components/Button.ts", true, "TypeScript in src"},
		{".github/**", ".github/workflows/ci.yml", true, "GitHub directory"},
	}

	for _, tc := range testCases {
		t.Run(tc.desc, func(t *testing.T) {
			got := matchPattern(tc.pattern, tc.file)
			if got != tc.want {
				t.Errorf("matchPattern(%q, %q) = %v, want %v",
					tc.pattern, tc.file, got, tc.want)
			}
		})
	}
}

// equalUnordered checks if two slices contain the same elements, order-independent.
func equalUnordered(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	m := make(map[string]int)
	for _, s := range a {
		m[s]++
	}
	for _, s := range b {
		m[s]--
	}
	for _, v := range m {
		if v != 0 {
			return false
		}
	}
	return true
}
