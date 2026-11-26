package cli

import (
	_ "embed"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

//go:embed skill.md
var skillText string

func newSkillCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:           "skill",
		Short:         "Print or install the agent skill for this CLI",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("skill: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return factoryTool().RunSkill(cmd.OutOrStdout())
		},
	}
	cmd.AddCommand(newSkillInstallCmd())
	cmd.AddCommand(newSkillUninstallCmd())
	cmd.AddCommand(newSkillStatusCmd())
	return cmd
}

func newSkillInstallCmd() *cobra.Command {
	var agents []string
	var scope string
	var dir string
	var dryRun bool
	var force bool
	cmd := &cobra.Command{
		Use:           "install",
		Short:         "Install the factory skill for coding agents",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("skill install: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			err := factoryTool().RunSkillInstall(toolcli.SkillInstallOptions{
				Agents: agents,
				Scope:  toolcli.Scope(scope),
				Dir:    dir,
				DryRun: dryRun,
				Force:  force,
			}, cmd.OutOrStdout())
			if err != nil {
				return exitStatusError{code: ExitUsage, err: err}
			}
			return nil
		},
	}
	cmd.Flags().StringArrayVar(&agents, "agent", nil, "target agent (claude|codex|antigravity|grok|all)")
	cmd.Flags().StringVar(&scope, "scope", string(toolcli.ScopeUser), "installation scope (user|project)")
	cmd.Flags().StringVar(&dir, "dir", "", "override target skill directory")
	cmd.Flags().BoolVar(&dryRun, "dry-run", false, "preview changes without writing")
	cmd.Flags().BoolVar(&force, "force", false, "overwrite existing foreign files")
	return cmd
}

func newSkillUninstallCmd() *cobra.Command {
	var agents []string
	var scope string
	var dir string
	var dryRun bool
	cmd := &cobra.Command{
		Use:           "uninstall",
		Short:         "Remove the installed factory skill",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("skill uninstall: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			err := factoryTool().RunSkillUninstall(toolcli.SkillUninstallOptions{
				Agents: agents,
				Scope:  toolcli.Scope(scope),
				Dir:    dir,
				DryRun: dryRun,
			}, cmd.OutOrStdout())
			if err != nil {
				return exitStatusError{code: ExitUsage, err: err}
			}
			return nil
		},
	}
	cmd.Flags().StringArrayVar(&agents, "agent", nil, "target agent (claude|codex|antigravity|grok|all)")
	cmd.Flags().StringVar(&scope, "scope", string(toolcli.ScopeUser), "installation scope (user|project)")
	cmd.Flags().StringVar(&dir, "dir", "", "override target skill directory")
	cmd.Flags().BoolVar(&dryRun, "dry-run", false, "preview changes without deleting")
	return cmd
}

func newSkillStatusCmd() *cobra.Command {
	var asJSON bool
	cmd := &cobra.Command{
		Use:           "status",
		Short:         "Show installed skill status across coding agents",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("skill status: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			if err := factoryTool().RunSkillStatus(asJSON, cmd.OutOrStdout()); err != nil {
				return exitStatusError{code: ExitUsage, err: err}
			}
			return nil
		},
	}
	cmd.Flags().BoolVar(&asJSON, "json", false, "output JSON")
	return cmd
}
