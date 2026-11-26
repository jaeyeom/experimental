package cli

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// loadConfig reads the persistent --config flag and loads factory.toml.
// Later commands call it. version and skill do not.
func loadConfig(cmd *cobra.Command) (*config.Config, error) {
	if cmd == nil {
		return nil, fmt.Errorf("loadConfig: expected a command, found nil")
	}
	path, err := cmd.Root().PersistentFlags().GetString("config")
	if err != nil {
		return nil, fmt.Errorf("read --config flag: %w", err)
	}
	return config.Load(path, config.OSEnv()) //nolint:wrapcheck // internal factory error already carries context
}
