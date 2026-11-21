package version

import "testing"

func TestResolvePrecedence(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		override   string
		pkgVersion string
		buildVer   string
		buildOK    bool
		want       string
	}{
		{
			name:       "override wins over package and build info",
			override:   "v1.2.3",
			pkgVersion: "v0.1.0",
			buildVer:   "v0.0.1",
			buildOK:    true,
			want:       "v1.2.3",
		},
		{
			name:       "package version wins over build info",
			override:   "",
			pkgVersion: "v0.2.0",
			buildVer:   "v0.1.0",
			buildOK:    true,
			want:       "v0.2.0",
		},
		{
			name:       "dev package version falls back to build info",
			override:   "",
			pkgVersion: "dev",
			buildVer:   "v0.3.0",
			buildOK:    true,
			want:       "v0.3.0",
		},
		{
			name:       "devel build info falls back to (devel)",
			override:   "",
			pkgVersion: "dev",
			buildVer:   "(devel)",
			buildOK:    true,
			want:       "(devel)",
		},
		{
			name:       "missing build info falls back to (devel)",
			override:   "",
			pkgVersion: "dev",
			buildVer:   "",
			buildOK:    false,
			want:       "(devel)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := resolve(tc.override, tc.pkgVersion, func() (string, bool) {
				return tc.buildVer, tc.buildOK
			})
			if got != tc.want {
				t.Fatalf("resolve(%q, %q, ...) = %q, want %q", tc.override, tc.pkgVersion, got, tc.want)
			}
		})
	}
}

func TestResolveDoesNotReturnEmpty(t *testing.T) {
	t.Parallel()

	if got := Resolve(); got == "" {
		t.Fatal("Resolve() returned empty string")
	}
	if got := ResolveWithOverride(""); got == "" {
		t.Fatal("ResolveWithOverride(\"\") returned empty string")
	}
}
