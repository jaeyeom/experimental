// Package version resolves the monorepo binary version from link-time
// -ldflags -X overrides or runtime/debug.ReadBuildInfo.
package version

import "runtime/debug"

// Version is the monorepo version. Overridable at link time with
// -X github.com/jaeyeom/experimental/devtools/pkg/version.Version.
var Version = "dev" //nolint:pkgvar // set with -ldflags -X

// Resolve returns the effective version string:
//  1. Version if set via -ldflags -X (not "" or "dev")
//  2. debug.ReadBuildInfo Main.Version if available (and not "" or "(devel)")
//  3. "(devel)" otherwise.
func Resolve() string {
	return resolve("", Version, readMainVersion)
}

// ResolveWithOverride returns the effective version string, preferring
// a non-empty per-binary override (not "" or "dev") before falling back
// to Version and runtime/debug.ReadBuildInfo.
func ResolveWithOverride(override string) string {
	return resolve(override, Version, readMainVersion)
}

func resolve(override, pkgVersion string, readBuild func() (string, bool)) string {
	if isExplicit(override) {
		return override
	}
	if isExplicit(pkgVersion) {
		return pkgVersion
	}
	if readBuild != nil {
		if v, ok := readBuild(); ok && isExplicit(v) && v != "(devel)" {
			return v
		}
	}
	return "(devel)"
}

func isExplicit(v string) bool {
	return v != "" && v != "dev"
}

func readMainVersion() (string, bool) {
	bi, ok := debug.ReadBuildInfo()
	if !ok {
		return "", false
	}
	return bi.Main.Version, true
}
