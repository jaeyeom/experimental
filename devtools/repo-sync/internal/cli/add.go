package cli

import (
	"fmt"
	"log/slog"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/sync"
	"github.com/spf13/cobra"
)

var addCmd = &cobra.Command{
	Use:   "add <project-name> <file-path>",
	Short: "Add a new file to synchronization scope",
	Long: `Add a new file to the synchronization scope for the specified project.
This will first perform a sync operation, then copy the file to the remote
work directory and commit it to the remote repository.`,
	Args: cobra.ExactArgs(2),
	RunE: func(_ *cobra.Command, args []string) error {
		return runAdd(args[0], args[1])
	},
}

func runAdd(projectName, filePath string) error {
	slog.Info("Starting add operation", "project", projectName, "file", filePath)

	// Load project configuration
	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project configuration: %w", err)
	}

	// Create sync engine
	engine, err := sync.NewEngine(project)
	if err != nil {
		return fmt.Errorf("failed to create sync engine: %w", err)
	}

	// Perform add operation
	result, err := engine.AddFile(filePath)
	if err != nil {
		return fmt.Errorf("add operation failed: %w", err)
	}

	fmt.Printf("File added successfully:\n")
	fmt.Printf("  File: %s\n", filePath)
	fmt.Printf("  Size: %d bytes\n", result.FileSize)
	fmt.Printf("  Git commit: %s\n", result.GitCommitHash)

	return nil
}
