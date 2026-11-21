package toolcli

import (
	"flag"
	"fmt"
	"io"
)

// VersionOutput is the JSON payload for `<tool> version --json`.
type VersionOutput struct {
	Tool        string `json:"tool"`
	ModulePath  string `json:"modulePath"`
	Version     string `json:"version"`
	VCSRevision string `json:"vcsRevision,omitempty"`
	VCSModified bool   `json:"vcsModified,omitempty"`
	BuildTime   string `json:"buildTime,omitempty"`
}

func (t *Tool) cmdVersion(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("version", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		return failMsg(stderr, "version does not take positional arguments", 2)
	}

	bi := t.buildInfo()
	out := VersionOutput{
		Tool:        t.Name,
		ModulePath:  bi.ModulePath,
		Version:     bi.Version,
		VCSRevision: bi.VCSRevision,
		VCSModified: bi.VCSModified,
		BuildTime:   bi.BuildTime,
	}

	if *jsonOut {
		if err := writeJSON(stdout, out); err != nil {
			return failErr(stderr, err, 2)
		}
		return 0
	}

	rev := out.VCSRevision
	if rev == "" {
		rev = "unknown"
	} else if out.VCSModified {
		rev += "-dirty"
	}
	_, _ = fmt.Fprintf(stdout, "%s %s (%s)\n", out.Tool, out.Version, rev)
	if out.ModulePath != "" {
		_, _ = fmt.Fprintf(stdout, "module: %s\n", out.ModulePath)
	}
	if out.BuildTime != "" {
		_, _ = fmt.Fprintf(stdout, "built: %s\n", out.BuildTime)
	}
	return 0
}
