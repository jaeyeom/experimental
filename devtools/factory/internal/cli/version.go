package cli

import (
	"fmt"
	"runtime/debug"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
)

const (
	devVersion    = "dev"
	unknownCommit = "unknown"
)

// version and commit are set with -ldflags -X.
// An empty value, dev, or unknown falls back to build info.
var (
	version = devVersion    //nolint:pkgvar // set with -ldflags -X
	commit  = unknownCommit //nolint:pkgvar // set with -ldflags -X
)

type versionInfo struct {
	Version string `json:"version"`
	Commit  string `json:"commit"`
}

type vcsInfo struct {
	revision string
	time     string
	modified string
}

func newVersionCmd() *cobra.Command {
	var asJSON bool
	cmd := &cobra.Command{
		Use:           "version",
		Short:         "Print version information",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("version: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return factoryTool().RunVersion(asJSON, cmd.OutOrStdout())
		},
	}
	cmd.Flags().BoolVar(&asJSON, "json", false, "output JSON")
	return cmd
}

func effectiveLDVersion() string {
	if version != "" && version != devVersion {
		return version
	}
	if pkgversion.Version != "" && pkgversion.Version != devVersion {
		return pkgversion.Version
	}
	return version
}

func currentToolBuildInfo() toolcli.BuildInfo {
	if testToolBuildInfo != nil {
		return testToolBuildInfo()
	}
	ldVer := effectiveLDVersion()
	info, ok := debug.ReadBuildInfo()
	resolved, err := resolveVersion(ldVer, commit, info, ok)
	if err != nil {
		return toolcli.BuildInfo{Version: ldVer, Commit: commit}
	}
	out := toolcli.BuildInfo{
		Version: resolved.Version,
		Commit:  resolved.Commit,
	}
	if ok && info != nil {
		out.ModulePath = info.Main.Path
		if vcs, err := readVCS(info); err == nil {
			out.VCSRevision = vcs.revision
			out.VCSModified = vcs.modified == "true"
			out.BuildTime = vcs.time
		}
	}
	if out.VCSRevision == "" && resolved.Commit != "" && resolved.Commit != unknownCommit {
		out.VCSRevision = resolved.Commit
	}
	return out
}

func currentVersion() (versionInfo, error) {
	info, ok := debug.ReadBuildInfo()
	return resolveVersion(effectiveLDVersion(), commit, info, ok)
}

// resolveVersion prefers ldflags. dev, unknown, and empty fall back to
// ReadBuildInfo: the module version when it is set, otherwise vcs.time and
// vcs.revision. vcs.modified true appends +dirty. The commit is vcs.revision.
func resolveVersion(ldVersion, ldCommit string, info *debug.BuildInfo, ok bool) (versionInfo, error) {
	out := versionInfo{Version: ldVersion, Commit: ldCommit}
	if !ok || info == nil {
		return out, nil
	}
	vcs, err := readVCS(info)
	if err != nil {
		return versionInfo{}, err
	}
	if out.Version == devVersion || out.Version == "" {
		out.Version = versionFromBuild(info.Main.Version, vcs)
	}
	if out.Commit == unknownCommit || out.Commit == "" {
		out.Commit = commitFromBuild(vcs)
	}
	return out, nil
}

func readVCS(info *debug.BuildInfo) (vcsInfo, error) {
	var vcs vcsInfo
	for _, setting := range info.Settings {
		switch setting.Key {
		case "vcs.revision":
			vcs.revision = setting.Value
		case "vcs.time":
			vcs.time = setting.Value
		case "vcs.modified":
			vcs.modified = setting.Value
		}
	}
	switch vcs.modified {
	case "", "true", "false":
		return vcs, nil
	default:
		return vcsInfo{}, fmt.Errorf("version: expected vcs.modified true or false, found %q", vcs.modified)
	}
}

func versionFromBuild(mainVersion string, vcs vcsInfo) string {
	if mainVersion != "" && mainVersion != "(devel)" {
		return withDirty(mainVersion, vcs.modified)
	}
	if vcs.revision == "" && vcs.time == "" {
		return devVersion
	}
	ver := shortRev(vcs.revision)
	switch {
	case vcs.time != "" && vcs.revision != "":
		ver = vcs.time + "-" + shortRev(vcs.revision)
	case vcs.time != "":
		ver = vcs.time
	}
	return withDirty(ver, vcs.modified)
}

func commitFromBuild(vcs vcsInfo) string {
	if vcs.revision == "" {
		return unknownCommit
	}
	return withDirty(vcs.revision, vcs.modified)
}

func shortRev(rev string) string {
	const n = 12
	if len(rev) > n {
		return rev[:n]
	}
	return rev
}

func withDirty(value, modified string) string {
	if modified == "true" {
		return value + "+dirty"
	}
	return value
}
