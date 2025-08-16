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
