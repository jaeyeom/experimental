package version

import (
	"runtime/debug"
	"testing"
)

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
	if got := Format("mytool"); got == "" {
		t.Fatal("Format(\"mytool\") returned empty string")
	}
}

func TestReadInfoAndFormat(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		override   string
		pkgVersion string
		buildInfo  *debug.BuildInfo
		buildOK    bool
		tool       string
		want       string
		wantModule string
		wantTime   string
	}{
		{
			name:       "module pseudo-version and clean vcs revision",
			pkgVersion: "dev",
			buildOK:    true,
			buildInfo: &debug.BuildInfo{
				Main: debug.Module{
					Path:    "github.com/jaeyeom/experimental",
					Version: "v0.0.0-20261127050000-abcdef123456",
				},
				Settings: []debug.BuildSetting{
					{Key: "vcs.revision", Value: "abcdef1234567890"},
					{Key: "vcs.time", Value: "2026-11-27T05:00:00Z"},
					{Key: "vcs.modified", Value: "false"},
				},
			},
			tool:       "org-lint",
			want:       "org-lint v0.0.0-20261127050000-abcdef123456 (abcdef1234567890)",
			wantModule: "github.com/jaeyeom/experimental",
			wantTime:   "2026-11-27T05:00:00Z",
		},
		{
			name:       "dirty vcs revision appends -dirty",
			pkgVersion: "v0.1.0",
			buildOK:    true,
			buildInfo: &debug.BuildInfo{
				Main: debug.Module{Path: "example.com/mod", Version: "(devel)"},
				Settings: []debug.BuildSetting{
					{Key: "vcs.revision", Value: "deadbeef"},
					{Key: "vcs.modified", Value: "true"},
				},
			},
			tool:       "agentbusd",
			want:       "agentbusd v0.1.0 (deadbeef-dirty)",
			wantModule: "example.com/mod",
		},
		{
			name:       "missing vcs revision uses unknown",
			pkgVersion: "dev",
			buildOK:    false,
			tool:       "box-renderer",
			want:       "box-renderer (devel) (unknown)",
		},
		{
			name:       "empty tool omits tool prefix",
			override:   "v1.0.0",
			pkgVersion: "dev",
			buildOK:    true,
			buildInfo: &debug.BuildInfo{
				Settings: []debug.BuildSetting{
					{Key: "vcs.revision", Value: "1234567"},
				},
			},
			tool: "",
			want: "v1.0.0 (1234567)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			info := readInfo(tc.override, tc.pkgVersion, func() (*debug.BuildInfo, bool) {
				return tc.buildInfo, tc.buildOK
			})
			if got := info.Format(tc.tool); got != tc.want {
				t.Fatalf("Format(%q) = %q, want %q", tc.tool, got, tc.want)
			}
			if info.ModulePath != tc.wantModule {
				t.Fatalf("ModulePath = %q, want %q", info.ModulePath, tc.wantModule)
			}
			if info.BuildTime != tc.wantTime {
				t.Fatalf("BuildTime = %q, want %q", info.BuildTime, tc.wantTime)
			}
		})
	}
}
