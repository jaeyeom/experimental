package cli

import (
	"errors"
	"fmt"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
)

func newHoldCmd() *cobra.Command {
	var owner, reason string
	cmd := &cobra.Command{
		Use:           "hold",
		Short:         "Create a hold file that blocks new starts",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("hold: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runHold(cmd, owner, reason)
		},
	}
	cmd.Flags().StringVar(&owner, "owner", "", "name recorded as the hold owner")
	cmd.Flags().StringVar(&reason, "reason", "", "reason recorded in the hold file")
	return cmd
}

func newReleaseHoldCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:           "release-hold",
		Short:         "Remove the hold file so new starts can proceed",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("release-hold: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runReleaseHold(cmd)
		},
	}
	return cmd
}

func runHold(cmd *cobra.Command, owner, reason string) error {
	if cmd.Flags().Changed("owner") && strings.TrimSpace(owner) == "" {
		return usageError{err: errors.New("hold: expected a non-empty owner, found empty")}
	}
	if cmd.Flags().Changed("reason") && strings.TrimSpace(reason) == "" {
		return usageError{err: errors.New("hold: expected a non-empty reason, found empty")}
	}
	owner, reason, err := gate.PrepareHold(owner, reason)
	if err != nil {
		return usageError{err: err}
	}
	path, err := holdCommandPath(cmd, "hold")
	if err != nil {
		return err
	}
	if err := gate.WriteHoldFile(path, owner, reason); err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	if _, err := fmt.Fprintln(cmd.OutOrStdout(), path); err != nil {
		return fmt.Errorf("hold: write path: %w", err)
	}
	return nil
}

func runReleaseHold(cmd *cobra.Command) error {
	path, err := holdCommandPath(cmd, "release-hold")
	if err != nil {
		return err
	}
	if err := gate.RemoveHoldFile(path); err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	if _, err := fmt.Fprintln(cmd.OutOrStdout(), path); err != nil {
		return fmt.Errorf("release-hold: write path: %w", err)
	}
	return nil
}

func holdCommandPath(cmd *cobra.Command, command string) (string, error) {
	cfg, err := loadConfig(cmd)
	if err != nil {
		return "", err
	}
	return oneHoldPath(cfg.Gate, command)
}

func oneHoldPath(g config.Gate, command string) (string, error) {
	switch n := len(g.HoldFiles); n {
	case 0:
		if g.DefaultHoldFile == "" {
			return "", fmt.Errorf("%s: expected the default hold path, found empty", command)
		}
		return g.DefaultHoldFile, nil
	case 1:
		return g.HoldFiles[0], nil
	default:
		return "", fmt.Errorf("%s: expected one path in gate.hold_files, found %d (%s)", command, n, strings.Join(g.HoldFiles, ", "))
	}
}
