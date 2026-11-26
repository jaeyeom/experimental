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
	Commit      string `json:"commit,omitempty"`
	VCSRevision string `json:"vcsRevision,omitempty"`
	VCSModified bool   `json:"vcsModified,omitempty"`
	BuildTime   string `json:"buildTime,omitempty"`
}

// RunVersion writes version information as human text or JSON.
func (t *Tool) RunVersion(asJSON bool, stdout io.Writer) error {
	bi := t.buildInfo()
	out := VersionOutput{
		Tool:        t.Name,
		ModulePath:  bi.ModulePath,
		Version:     bi.Version,
		Commit:      bi.Commit,
		VCSRevision: bi.VCSRevision,
		VCSModified: bi.VCSModified,
		BuildTime:   bi.BuildTime,
	}

	if asJSON {
		return writeJSON(stdout, out)
	}

	rev := out.VCSRevision
	if rev == "" {
		rev = out.Commit
	}
	if rev == "" {
		rev = "unknown"
	} else if out.VCSModified {
		rev += "-dirty"
	}
	if _, err := fmt.Fprintf(stdout, "%s %s (%s)\n", out.Tool, out.Version, rev); err != nil {
		return fmt.Errorf("write output: %w", err)
	}
	if out.ModulePath != "" {
		if _, err := fmt.Fprintf(stdout, "module: %s\n", out.ModulePath); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	if out.BuildTime != "" {
		if _, err := fmt.Fprintf(stdout, "built: %s\n", out.BuildTime); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
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

	if err := t.RunVersion(*jsonOut, stdout); err != nil {
		return failErr(stderr, err, 2)
	}
	return 0
}
