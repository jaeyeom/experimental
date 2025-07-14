package cli

import (
	"fmt"
	"log/slog"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/sync"
	"github.com/spf13/cobra"
)

var removeCmd = &cobra.Command{
	Use:   "remove <project-name> <file-path>",
	Short: "Remove a file from both local and remote repositories",
	Long: `Remove a file from both local and remote repositories with deletion tracking.
This will record the deletion in the metadata database for multi-machine coordination.`,
	Args: cobra.ExactArgs(2),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runRemove(args[0], args[1], cmd)
	},
}

func init() {
	removeCmd.Flags().Bool("confirm", false, "Skip confirmation prompt")
}

func runRemove(projectName, filePath string, cmd *cobra.Command) error {
	slog.Info("Starting remove operation", "project", projectName, "file", filePath)

	// Load project configuration
	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project configuration: %w", err)
	}

	// Check for confirmation unless --confirm flag is used
	confirm, _ := cmd.Flags().GetBool("confirm")
	if !confirm {
		fmt.Printf("Are you sure you want to remove '%s' from project '%s'? [y/N]: ", filePath, projectName)
		var response string
		if _, err := fmt.Scanln(&response); err != nil {
			slog.Debug("Failed to read user input, treating as no", "error", err)
			response = "n"
		}
		if response != "y" && response != "Y" && response != "yes" {
			fmt.Println("Operation cancelled")
			return nil
		}
	}

	// Create sync engine
	engine, err := sync.NewEngine(project)
	if err != nil {
		return fmt.Errorf("failed to create sync engine: %w", err)
	}

	// Perform remove operation
	result, err := engine.RemoveFile(filePath)
	if err != nil {
		return fmt.Errorf("remove operation failed: %w", err)
	}

	fmt.Printf("File removed successfully:\n")
	fmt.Printf("  File: %s\n", filePath)
	fmt.Printf("  Git commit: %s\n", result.GitCommitHash)
	fmt.Printf("  Deletion timestamp: %s\n", result.DeletedAt.Format("2006-01-02 15:04:05"))

	return nil
}
