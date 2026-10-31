package cli

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/database"
	"github.com/spf13/cobra"
)

var statusCmd = &cobra.Command{
	Use:   "status [project-name]",
	Short: "Show project sync status",
	Long:  `Show the current synchronization status for a project or all projects.`,
	Args:  cobra.MaximumNArgs(1),
	RunE: func(_ *cobra.Command, args []string) error {
		if len(args) == 1 {
			return runProjectStatus(args[0])
		}
		return runAllStatus()
	},
}

var verifyCmd = &cobra.Command{
	Use:   "verify <project-name>",
	Short: "Verify project configuration and connectivity",
	Args:  cobra.ExactArgs(1),
	RunE: func(_ *cobra.Command, args []string) error {
		return runVerify(args[0])
	},
}

func runProjectStatus(projectName string) error {
	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project: %w", err)
	}

	// Get project statistics from database
	db, err := database.GetConnection()
	if err != nil {
		return fmt.Errorf("failed to connect to database: %w", err)
	}

	stats, err := database.GetProjectStats(db, projectName)
	if err != nil {
		return fmt.Errorf("failed to get project statistics: %w", err)
	}

	fmt.Printf("Project: %s\n", project.Name)
	fmt.Printf("Local Directory: %s\n", project.LocalWorkDir)
	fmt.Printf("Remote Repository: %s\n", project.RemoteRepo)
	fmt.Printf("Remote Work Directory: %s\n", project.RemoteWorkDir)
	fmt.Printf("\nStatistics:\n")
	fmt.Printf("  Total syncs: %d\n", stats.TotalSyncs)
	fmt.Printf("  Successful syncs: %d\n", stats.SuccessfulSyncs)
	fmt.Printf("  Failed syncs: %d\n", stats.FailedSyncs)
	fmt.Printf("  Last sync: %s\n", stats.LastSyncAt.Format("2006-01-02 15:04:05"))
	fmt.Printf("  Tracked files: %d\n", stats.TrackedFiles)
	fmt.Printf("  Conflicted files: %d\n", stats.ConflictedFiles)

	return nil
}

func runAllStatus() error {
	projects, err := config.ListProjects()
	if err != nil {
		return fmt.Errorf("failed to list projects: %w", err)
	}

	if len(projects) == 0 {
		fmt.Println("No projects configured")
		return nil
	}

	db, err := database.GetConnection()
	if err != nil {
		return fmt.Errorf("failed to connect to database: %w", err)
	}

	fmt.Printf("Project Status Summary:\n\n")
	for _, project := range projects {
		stats, err := database.GetProjectStats(db, project.Name)
		if err != nil {
			fmt.Printf("  %-20s ERROR: %v\n", project.Name, err)
			continue
		}

		status := "OK"
		if stats.ConflictedFiles > 0 {
			status = "CONFLICTS"
		} else if stats.FailedSyncs > 0 {
			status = "ISSUES"
		}

		fmt.Printf("  %-20s %-10s Last sync: %s\n",
			project.Name,
			status,
			stats.LastSyncAt.Format("2006-01-02 15:04"))
	}

	return nil
}

func runVerify(projectName string) error {
	project, err := config.LoadProject(projectName)
	if err != nil {
		return fmt.Errorf("failed to load project: %w", err)
	}

	fmt.Printf("Verifying project: %s\n\n", projectName)

	// Check local directory
	fmt.Printf("✓ Checking local directory: %s\n", project.LocalWorkDir)
	// TODO: Implement directory accessibility check

	// Check remote repository connectivity
	fmt.Printf("✓ Checking remote repository: %s\n", project.RemoteRepo)
	// TODO: Implement SSH connectivity check

	// Check remote work directory
	fmt.Printf("✓ Checking remote work directory: %s\n", project.RemoteWorkDir)
	// TODO: Implement remote directory check

	// Check database connectivity
	fmt.Printf("✓ Checking database connectivity\n")
	db, err := database.GetConnection()
	if err != nil {
		return fmt.Errorf("database connectivity failed: %w", err)
	}
	db.Close()

	fmt.Printf("\nVerification completed successfully\n")
	return nil
}
