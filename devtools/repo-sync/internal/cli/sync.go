package cli

import (
	"fmt"
	"log/slog"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/sync"
	"github.com/spf13/cobra"
)

var syncCmd = &cobra.Command{
	Use:   "sync <project-name>",
	Short: "Synchronize project files",
	Long: `Perform bidirectional synchronization of project files between local and remote repositories.
This includes rsync operations, Git workflow automation, and conflict resolution.`,
	Args: cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runSync(args[0], cmd)
	},
}

var downloadCmd = &cobra.Command{
	Use:   "download <project-name>",
	Short: "Download files from remote (remote takes precedence)",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runDownload(args[0], cmd)
	},
}

var uploadCmd = &cobra.Command{
	Use:   "upload <project-name>",
	Short: "Upload files to remote (local takes precedence)",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runUpload(args[0], cmd)
	},
}

func init() {
	syncCmd.Flags().Bool("dry-run", false, "Show what would be synced without making changes")
	syncCmd.Flags().Bool("force", false, "Force sync even if there are conflicts")

	downloadCmd.Flags().Bool("dry-run", false, "Show what would be downloaded without making changes")
	uploadCmd.Flags().Bool("dry-run", false, "Show what would be uploaded without making changes")

	// Add subcommands to sync
	syncCmd.AddCommand(downloadCmd)
	syncCmd.AddCommand(uploadCmd)
}

func runSync(projectName string, cmd *cobra.Command) error {
	slog.Info("Starting sync operation", "project", projectName)

	// Load project configuration
	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project configuration: %w", err)
	}

	// Get flags
	dryRun, _ := cmd.Flags().GetBool("dry-run")
	force, _ := cmd.Flags().GetBool("force")

	// Create sync engine
	engine, err := sync.NewEngine(project)
	if err != nil {
		return fmt.Errorf("failed to create sync engine: %w", err)
	}

	// Perform sync
	result, err := engine.Sync(&sync.Options{
		DryRun: dryRun,
		Force:  force,
	})
	if err != nil {
		return fmt.Errorf("sync operation failed: %w", err)
	}

	// Display results
	fmt.Printf("Sync completed successfully:\n")
	fmt.Printf("  Files synced: %d\n", result.FilesSynced)
	fmt.Printf("  Bytes transferred: %d\n", result.BytesTransferred)
	fmt.Printf("  Conflicts resolved: %d\n", result.ConflictsResolved)
	fmt.Printf("  Duration: %v\n", result.Duration)

	return nil
}

func runDownload(projectName string, cmd *cobra.Command) error {
	slog.Info("Starting download operation", "project", projectName)

	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project configuration: %w", err)
	}

	dryRun, _ := cmd.Flags().GetBool("dry-run")

	engine, err := sync.NewEngine(project)
	if err != nil {
		return fmt.Errorf("failed to create sync engine: %w", err)
	}

	result, err := engine.Download(&sync.Options{
		DryRun: dryRun,
	})
	if err != nil {
		return fmt.Errorf("download operation failed: %w", err)
	}

	fmt.Printf("Download completed:\n")
	fmt.Printf("  Files downloaded: %d\n", result.FilesSynced)
	fmt.Printf("  Bytes transferred: %d\n", result.BytesTransferred)

	return nil
}

func runUpload(projectName string, cmd *cobra.Command) error {
	slog.Info("Starting upload operation", "project", projectName)

	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project configuration: %w", err)
	}

	dryRun, _ := cmd.Flags().GetBool("dry-run")

	engine, err := sync.NewEngine(project)
	if err != nil {
		return fmt.Errorf("failed to create sync engine: %w", err)
	}

	result, err := engine.Upload(&sync.Options{
		DryRun: dryRun,
	})
	if err != nil {
		return fmt.Errorf("upload operation failed: %w", err)
	}

	fmt.Printf("Upload completed:\n")
	fmt.Printf("  Files uploaded: %d\n", result.FilesSynced)
	fmt.Printf("  Bytes transferred: %d\n", result.BytesTransferred)

	return nil
}
