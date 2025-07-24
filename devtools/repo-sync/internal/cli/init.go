package cli

import (
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/database"
	"github.com/spf13/cobra"
)

var initCmd = &cobra.Command{
	Use:   "init",
	Short: "Initialize repo-sync configuration and database",
	Long: `Initialize repo-sync by creating the necessary configuration directories,
database schema, and default configuration files.`,
	RunE: runInit,
}

func init() {
	initCmd.Flags().Bool("reset-db", false, "Reset existing database")
}

func runInit(cmd *cobra.Command, _ []string) error {
	slog.Info("Initializing repo-sync...")

	// Create configuration directory
	configDir, err := config.GetConfigDir()
	if err != nil {
		return fmt.Errorf("failed to get config directory: %w", err)
	}

	// Create all necessary directories
	dirs := []string{
		configDir,
		filepath.Join(configDir, "mappings"),
		filepath.Join(configDir, "logs"),
		filepath.Join(configDir, "backups"),
		filepath.Join(configDir, "rollbacks"),
		filepath.Join(configDir, "locks"),
	}

	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return fmt.Errorf("failed to create directory %s: %w", dir, err)
		}
		slog.Debug("Created directory", "directory", dir)
	}

	// Initialize database
	resetDB, _ := cmd.Flags().GetBool("reset-db")
	if err := database.Initialize(resetDB); err != nil {
		return fmt.Errorf("failed to initialize database: %w", err)
	}

	// Create default configuration if it doesn't exist
	configFile := filepath.Join(configDir, "config.yaml")
	if _, err := os.Stat(configFile); errors.Is(err, fs.ErrNotExist) {
		if err := createDefaultConfig(configFile); err != nil {
			return fmt.Errorf("failed to create default config: %w", err)
		}
	}

	slog.Info("Repo-sync initialized successfully")
	fmt.Printf("Configuration directory: %s\n", configDir)
	fmt.Printf("Database location: %s\n", filepath.Join(configDir, "metadata.db"))
	fmt.Printf("\nNext steps:\n")
	fmt.Printf("1. Add a project: repo-sync config add-project <name> --local-dir <path> --remote-repo <url>\n")
	fmt.Printf("2. Sync a project: repo-sync sync <name>\n")

	return nil
}

func createDefaultConfig(configFile string) error {
	defaultConfig := `# Repo-Sync Configuration
# Global settings for repo-sync utility

# Default sync patterns (can be overridden per project)
default_sync_patterns:
  include:
    - "*.yaml"
    - "*.yml"
    - "*.json"
    - "*.toml"
    - "*.ini"
    - "*.conf"
    - "*.config"
    - "*.md"
    - "*.txt"
    - "*.org"
  exclude:
    - "*.tmp"
    - "*.log"
    - "*.cache"
    - ".git/**/*"
    - "node_modules/**/*"
    - "__pycache__/**/*"

# Conflict resolution preferences
conflict_resolution:
  default_strategy: "timestamp_newest"
  backup_conflicts: true

# Performance settings
performance:
  rsync_compression_level: 6
  max_concurrent_operations: 3
  timeout_seconds: 300

# Logging settings
logging:
  level: "info"
  file_retention_days: 30
  max_file_size_mb: 10
`

	if err := os.WriteFile(configFile, []byte(defaultConfig), 0o600); err != nil {
		return fmt.Errorf("failed to write default config file: %w", err)
	}
	return nil
}
