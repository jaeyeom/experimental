// Package version holds the prsync version string.
package version

import pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"

// Version is the prsync version. Overridable at link time with
// -X github.com/jaeyeom/experimental/devtools/prsync/internal/version.Version.
var Version = "" //nolint:pkgvar // set with -ldflags -X

// Resolve returns the effective prsync version string.
func Resolve() string {
	return pkgversion.ResolveWithOverride(Version)
}
