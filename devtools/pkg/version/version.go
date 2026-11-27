// Package version resolves the monorepo binary version from link-time
// -ldflags -X overrides or runtime/debug.ReadBuildInfo.
package version

import "runtime/debug"

// Version is the monorepo version. Overridable at link time with
// -X github.com/jaeyeom/experimental/devtools/pkg/version.Version.
var Version = "dev" //nolint:pkgvar // set with -ldflags -X

// Info describes the resolved module version and VCS build metadata.
type Info struct {
	ModulePath  string
	Version     string
	VCSRevision string
	VCSModified bool
	BuildTime   string
}

// Revision returns the VCS revision with a "-dirty" suffix when modified,
// or "unknown" when no VCS revision is recorded.
func (i Info) Revision() string {
	if i.VCSRevision == "" {
		return "unknown"
	}
	if i.VCSModified {
		return i.VCSRevision + "-dirty"
	}
	return i.VCSRevision
}

// String formats the version and VCS revision as "<version> (<revision>)".
func (i Info) String() string {
	return i.Version + " (" + i.Revision() + ")"
}

// Format formats "<tool> <version> (<revision>)" when tool is non-empty,
// or "<version> (<revision>)" when tool is empty.
func (i Info) Format(tool string) string {
	if tool == "" {
		return i.String()
	}
	return tool + " " + i.String()
}

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

// ReadInfo returns the resolved version and build metadata from -ldflags -X
// and runtime/debug.ReadBuildInfo.
func ReadInfo() Info {
	return ReadInfoWithOverride("")
}

// ReadInfoWithOverride returns the resolved version and build metadata,
// preferring a non-empty per-binary override before Version and BuildInfo.
func ReadInfoWithOverride(override string) Info {
	return readInfo(override, Version, debug.ReadBuildInfo)
}

// Format returns "<tool> <version> (<revision>)" for the current binary.
func Format(tool string) string {
	return ReadInfo().Format(tool)
}

func readInfo(override, pkgVersion string, readBuild func() (*debug.BuildInfo, bool)) Info {
	var bi *debug.BuildInfo
	var ok bool
	if readBuild != nil {
		bi, ok = readBuild()
	}
	mainVersion := func() (string, bool) {
		if !ok || bi == nil {
			return "", false
		}
		return bi.Main.Version, true
	}
	info := Info{
		Version: resolve(override, pkgVersion, mainVersion),
	}
	if !ok || bi == nil {
		return info
	}
	info.ModulePath = bi.Main.Path
	for _, s := range bi.Settings {
		switch s.Key {
		case "vcs.revision":
			info.VCSRevision = s.Value
		case "vcs.time":
			info.BuildTime = s.Value
		case "vcs.modified":
			info.VCSModified = s.Value == "true"
		}
	}
	return info
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
