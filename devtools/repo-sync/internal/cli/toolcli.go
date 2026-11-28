package cli

import (
	"fmt"
	"os"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/spf13/cobra"
)

func newRepoSyncTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "repo-sync",
		ModulePath:       "github.com/jaeyeom/experimental",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/repo-sync/cmd/repo-sync",
		SkillText:        skillText,
		RequiredCommands: []string{"git"},
	}
}

func newToolCLICmd(name, short string) *cobra.Command {
	return &cobra.Command{
		Use:                name,
		Short:              short,
		DisableFlagParsing: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			code, _ := newRepoSyncTool().Handle(cmd.Context(), append([]string{name}, args...), os.Stdout, os.Stderr)
			if code != 0 {
				return fmt.Errorf("%s exited with code %d", name, code)
			}
			return nil
		},
	}
}

func addToolCLICommands(root *cobra.Command) {
	root.AddCommand(newToolCLICmd("version", "Print build version and VCS revision"))
	root.AddCommand(newToolCLICmd("update", "Check for or install the latest repo-sync binary"))
	root.AddCommand(newToolCLICmd("doctor", "Run standard diagnostics"))
	root.AddCommand(newToolCLICmd("skill", "Print or install the embedded repo-sync skill"))
}
