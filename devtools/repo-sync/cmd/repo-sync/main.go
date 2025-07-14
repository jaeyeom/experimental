package main

import (
	"log/slog"
	"os"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/cli"
)

func main() {
	if err := cli.Execute(); err != nil {
		slog.Error("Command execution failed", "error", err)
		os.Exit(1)
	}
}
