package cli

import (
	"context"
	"fmt"
	"io"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	"github.com/spf13/cobra"
)

func newPrsyncTool() toolcli.Tool {
	return newPrsyncToolWithConfig("")
}

func newPrsyncToolWithConfig(configPath string) toolcli.Tool {
	return toolcli.Tool{
		Name:              "prsync",
		MainPackage:       "github.com/jaeyeom/experimental/devtools/prsync/cmd/prsync",
		SkillText:         skillText,
		RequiredCommands:  []string{"gh", "git", "herdr"},
		ExtraDoctorChecks: []toolcli.DoctorCheck{func(_ context.Context, _ bool) toolcli.CheckResult { return doctorLoopCheck(configPath) }},
	}
}

func doctorLoopCheck(configPath string) toolcli.CheckResult {
	cfg, err := config.Load(configPath)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "loop",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("unknown (config error: %v)", err),
		}
	}
	rep := loop.Inspect(filepath.Dir(cfg.StateFile), cfg.SourcePath, configPath != "", time.Now(), loop.DefaultStaleAfter, loop.ProcessAlive)
	switch rep.State {
	case loop.StateStale:
		return toolcli.CheckResult{
			Name:   "loop",
			Status: toolcli.StatusWarn,
			Detail: rep.Detail,
			Hint:   "run `prsync loop --ensure` to restart the loop",
		}
	default:
		return toolcli.CheckResult{
			Name:   "loop",
			Status: toolcli.StatusOK,
			Detail: rep.Detail,
		}
	}
}

func runToolCLI(cmd *cobra.Command, name string, args []string, stdout, stderr io.Writer) error {
	forwarded := []string{name}
	for _, boolFlag := range []string{"json", "check", "offline", "dry-run", "force"} {
		if cmd.Flags().Changed(boolFlag) {
			v, _ := cmd.Flags().GetBool(boolFlag)
			if v {
				forwarded = append(forwarded, "--"+boolFlag)
			} else {
				forwarded = append(forwarded, "--"+boolFlag+"=false")
			}
		}
	}
	for _, strFlag := range []string{"ref", "scope", "dir"} {
		if cmd.Flags().Changed(strFlag) {
			v, _ := cmd.Flags().GetString(strFlag)
			forwarded = append(forwarded, "--"+strFlag, v)
		}
	}
	if cmd.Flags().Changed("agent") {
		agents, _ := cmd.Flags().GetStringSlice("agent")
		for _, a := range agents {
			forwarded = append(forwarded, "--agent", a)
		}
	}
	var configPath string
	if cmd.Flags().Changed("config") {
		configPath, _ = cmd.Flags().GetString("config")
	}
	forwarded = append(forwarded, args...)
	code, _ := newPrsyncToolWithConfig(configPath).Handle(cmd.Context(), forwarded, stdout, stderr)
	if code != 0 {
		return &ExitError{Code: code}
	}
	return nil
}

func addToolCLICommands(root *cobra.Command, stdout, stderr io.Writer) {
	versionCmd := &cobra.Command{
		Use:   "version",
		Short: "Print build version and VCS revision",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runToolCLI(cmd, "version", args, stdout, stderr)
		},
	}
	versionCmd.Flags().Bool("json", false, "emit JSON output")

	updateCmd := &cobra.Command{
		Use:   "update",
		Short: "Check for or install the latest prsync binary",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runToolCLI(cmd, "update", args, stdout, stderr)
		},
	}
	updateCmd.Flags().Bool("check", false, "check only without installing")
	updateCmd.Flags().String("ref", "latest", "target branch, tag, or module version")
	updateCmd.Flags().Bool("json", false, "emit JSON output")

	doctorCmd := &cobra.Command{
		Use:   "doctor",
		Short: "Run standard diagnostics",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runToolCLI(cmd, "doctor", args, stdout, stderr)
		},
	}
	doctorCmd.Flags().String("config", "", "config file path")
	doctorCmd.Flags().Bool("json", false, "emit JSON output")
	doctorCmd.Flags().Bool("offline", false, "skip network-dependent checks")

	skillCmd := &cobra.Command{
		Use:   "skill",
		Short: "Print or install the embedded prsync skill",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runToolCLI(cmd, "skill", args, stdout, stderr)
		},
	}
	skillCmd.Flags().StringSlice("agent", nil, "target agent")
	skillCmd.Flags().String("scope", "user", "install scope (user or project)")
	skillCmd.Flags().String("dir", "", "override target directory")
	skillCmd.Flags().Bool("dry-run", false, "report without modifying disk")
	skillCmd.Flags().Bool("force", false, "overwrite foreign skill files")
	skillCmd.Flags().Bool("json", false, "emit JSON output")

	root.AddCommand(versionCmd, updateCmd, doctorCmd, skillCmd)
}
