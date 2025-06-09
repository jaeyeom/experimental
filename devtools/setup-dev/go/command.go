package main

import (
	"fmt"
	"log/slog"
	"os/exec"
)

func Run(cmd *exec.Cmd) error {
	slog.Info("Running command", "cmd", cmd)
	out, err := cmd.CombinedOutput()
	slog.Info("Output", "output", string(out))
	if err != nil {
		return fmt.Errorf("command execution failed: %w", err)
	}
	return nil
}
