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
			if !equalUnordered(got, tc.want) {
				t.Errorf("file %q: got owners %v, want %v", tc.file, got, tc.want)
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
