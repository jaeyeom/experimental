package cli

import (
	"errors"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/mapping"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/spf13/cobra"
)

func newDocsyncTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "docsync",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/docsync/cmd/docsync",
		SkillText:        skillText,
		RequiredCommands: []string{"git"},
		ConfigPath: func() (string, bool) {
			p, err := mapping.Find(".")
			if err != nil {
				return "", false
			}
			return p, true
		},
		CheckConfig: func() error {
			p, err := mapping.Find(".")
			if errors.Is(err, mapping.ErrNotFound) {
				return nil
			}
			if err != nil {
				return fmt.Errorf("find docsync.yml: %w", err)
			}
			if _, err := mapping.Load(p); err != nil {
				return fmt.Errorf("load docsync.yml: %w", err)
			}
			return nil
		},
	}
}

func newToolCLICmd(name, short string, stdout, stderr io.Writer) *cobra.Command {
	return &cobra.Command{
		Use:                name,
		Short:              short,
		DisableFlagParsing: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			code, _ := newDocsyncTool().Handle(cmd.Context(), append([]string{name}, args...), stdout, stderr)
			if code != 0 {
				return &ExitError{Code: code}
			}
			return nil
		},
	}
}

func addToolCLICommands(root *cobra.Command, stdout, stderr io.Writer) {
	root.AddCommand(newToolCLICmd("version", "Print build version and VCS revision", stdout, stderr))
	root.AddCommand(newToolCLICmd("update", "Check for or install the latest docsync binary", stdout, stderr))
	root.AddCommand(newToolCLICmd("doctor", "Run standard diagnostics", stdout, stderr))
	root.AddCommand(newToolCLICmd("skill", "Print or install the embedded docsync skill", stdout, stderr))
}
