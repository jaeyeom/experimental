package cli

import (
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/spf13/cobra"
)

var configCmd = &cobra.Command{
	Use:   "config",
	Short: "Manage project configurations",
	Long:  `Manage project configurations including adding, removing, and listing projects.`,
}

var addProjectCmd = &cobra.Command{
	Use:   "add-project <name>",
	Short: "Add a new project configuration",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runAddProject(args[0], cmd)
	},
}

var removeProjectCmd = &cobra.Command{
	Use:   "remove-project <name>",
	Short: "Remove a project configuration",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runRemoveProject(args[0])
	},
}

var listProjectsCmd = &cobra.Command{
	Use:   "list-projects",
	Short: "List all configured projects",
	RunE: func(cmd *cobra.Command, args []string) error {
		return runListProjects()
	},
}

var showProjectCmd = &cobra.Command{
	Use:   "show-project <name>",
	Short: "Show project configuration details",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		return runShowProject(args[0])
	},
}

func init() {
	// Add project flags
	addProjectCmd.Flags().String("local-dir", "", "Local work directory path (required)")
	addProjectCmd.Flags().String("remote-repo", "", "Remote repository URL (required)")
	addProjectCmd.Flags().String("remote-prefix", "", "Remote path prefix within repository (required)")
	addProjectCmd.Flags().StringSlice("include", []string{}, "Include patterns")
	addProjectCmd.Flags().StringSlice("exclude", []string{}, "Exclude patterns")

	if err := addProjectCmd.MarkFlagRequired("local-dir"); err != nil {
		slog.Error("Failed to mark local-dir flag as required", "error", err)
		os.Exit(1)
	}
	if err := addProjectCmd.MarkFlagRequired("remote-repo"); err != nil {
		slog.Error("Failed to mark remote-repo flag as required", "error", err)
		os.Exit(1)
	}
	if err := addProjectCmd.MarkFlagRequired("remote-prefix"); err != nil {
		slog.Error("Failed to mark remote-prefix flag as required", "error", err)
		os.Exit(1)
	}

	// Add subcommands
	configCmd.AddCommand(addProjectCmd)
	configCmd.AddCommand(removeProjectCmd)
	configCmd.AddCommand(listProjectsCmd)
	configCmd.AddCommand(showProjectCmd)
}

func runAddProject(name string, cmd *cobra.Command) error {
	slog.Info("Adding new project configuration", "project", name)

	localDir, _ := cmd.Flags().GetString("local-dir")
	remoteRepo, _ := cmd.Flags().GetString("remote-repo")
	remotePrefix, _ := cmd.Flags().GetString("remote-prefix")
	includePatterns, _ := cmd.Flags().GetStringSlice("include")
	excludePatterns, _ := cmd.Flags().GetStringSlice("exclude")

	// Expand local directory path
	if strings.HasPrefix(localDir, "~/") {
		homeDir, err := os.UserHomeDir()
		if err != nil {
			return fmt.Errorf("failed to get home directory: %w", err)
		}
		localDir = filepath.Join(homeDir, localDir[2:])
	}

	// Convert to absolute path
	absLocalDir, err := filepath.Abs(localDir)
	if err != nil {
		return fmt.Errorf("failed to get absolute path for local directory: %w", err)
	}

	// Validate local directory exists
	if _, err := os.Stat(absLocalDir); errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("local directory does not exist: %s", absLocalDir)
	}

	// Create project configuration
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return fmt.Errorf("failed to get home directory: %w", err)
	}

	remoteWorkDir := filepath.Join(homeDir, ".repo-sync", "remotes", name)

	project := &config.Project{
		Name:             name,
		LocalWorkDir:     absLocalDir,
		RemoteRepo:       remoteRepo,
		RemoteWorkDir:    remoteWorkDir,
		RemotePathPrefix: remotePrefix,
		SyncPatterns: config.SyncPatterns{
			Include: includePatterns,
			Exclude: excludePatterns,
		},
	}

	// Save project configuration
	if err := config.SaveProject(project); err != nil {
		return fmt.Errorf("failed to save project configuration: %w", err)
	}

	fmt.Printf("Project '%s' added successfully:\n", name)
	fmt.Printf("  Local directory: %s\n", absLocalDir)
	fmt.Printf("  Remote repository: %s\n", remoteRepo)
	fmt.Printf("  Remote work directory: %s\n", remoteWorkDir)
	fmt.Printf("  Remote path prefix: %s\n", remotePrefix)

	return nil
}

func runRemoveProject(name string) error {
	slog.Info("Removing project configuration", "project", name)

	// Check if project exists
	if _, err := config.LoadProject(name); err != nil {
		return fmt.Errorf("project not found: %s", name)
	}

	// Confirm removal
	fmt.Printf("Are you sure you want to remove project '%s'? [y/N]: ", name)
	var response string
	if _, err := fmt.Scanln(&response); err != nil {
		slog.Debug("Failed to read user input, treating as no", "error", err)
		response = "n"
	}
	if response != "y" && response != "Y" && response != "yes" {
		fmt.Println("Operation cancelled")
		return nil
	}

	// Remove project configuration
	if err := config.RemoveProject(name); err != nil {
		return fmt.Errorf("failed to remove project configuration: %w", err)
	}

	fmt.Printf("Project '%s' removed successfully\n", name)
	return nil
}

func runListProjects() error {
	projects, err := config.ListProjects()
	if err != nil {
		return fmt.Errorf("failed to list projects: %w", err)
	}

	if len(projects) == 0 {
		fmt.Println("No projects configured")
		fmt.Println("Add a project with: repo-sync config add-project <name> --local-dir <path> --remote-repo <url> --remote-prefix <prefix>")
		return nil
	}

	fmt.Printf("Configured projects (%d):\n\n", len(projects))
	for _, project := range projects {
		fmt.Printf("  %s\n", project.Name)
		fmt.Printf("    Local: %s\n", project.LocalWorkDir)
		fmt.Printf("    Remote: %s\n", project.RemoteRepo)
		fmt.Printf("    Prefix: %s\n", project.RemotePathPrefix)
		fmt.Println()
	}

	return nil
}

func runShowProject(name string) error {
	project, err := config.LoadProject(name)
	if err != nil {
		return fmt.Errorf("failed to load project: %w", err)
	}

	fmt.Printf("Project: %s\n", project.Name)
	fmt.Printf("Local Work Directory: %s\n", project.LocalWorkDir)
	fmt.Printf("Remote Repository: %s\n", project.RemoteRepo)
	fmt.Printf("Remote Work Directory: %s\n", project.RemoteWorkDir)
	fmt.Printf("Remote Path Prefix: %s\n", project.RemotePathPrefix)
	fmt.Printf("\nSync Patterns:\n")
	fmt.Printf("  Include:\n")
	for _, pattern := range project.SyncPatterns.Include {
		fmt.Printf("    - %s\n", pattern)
	}
	fmt.Printf("  Exclude:\n")
	for _, pattern := range project.SyncPatterns.Exclude {
		fmt.Printf("    - %s\n", pattern)
	}

	return nil
}
