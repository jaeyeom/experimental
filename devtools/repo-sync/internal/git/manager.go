// Package git provides Git operations for repo-sync.
package git

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	executor "github.com/jaeyeom/go-cmdexec"
)

// Manager handles Git operations for repo-sync.
type Manager struct {
	project *config.Project
	logger  *slog.Logger
	exec    executor.Executor
	ctx     context.Context
}

// WorkflowResult represents the result of a Git workflow operation.
type WorkflowResult struct {
	CommitHash        string
	ConflictsResolved int
	HasChanges        bool
}

// NewManager creates a new Git manager instance.
func NewManager(project *config.Project) (*Manager, error) {
	manager := &Manager{
		project: project,
		logger:  slog.With("component", "git", "project", project.Name),
		exec:    executor.NewBasicExecutor(),
		ctx:     context.Background(),
	}

	// Initialize or update the remote repository
	if err := manager.initializeRepository(); err != nil {
		return nil, fmt.Errorf("failed to initialize repository: %w", err)
	}

	return manager, nil
}

// PerformWorkflow executes the complete Git workflow: add, commit, pull, push.
func (m *Manager) PerformWorkflow() (*WorkflowResult, error) {
	return m.PerformWorkflowWithFiles(nil)
}

// PerformWorkflowWithFiles executes the complete Git workflow for specific files.
func (m *Manager) PerformWorkflowWithFiles(filePaths []string) (*WorkflowResult, error) {
	m.logger.Info("Starting Git workflow")

	// Change to remote work directory
	originalDir, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("failed to get current directory: %w", err)
	}
	defer func() {
		if err := os.Chdir(originalDir); err != nil {
			slog.Error("Failed to restore original directory", "error", err)
		}
	}()

	if err := os.Chdir(m.project.RemoteWorkDir); err != nil {
		return nil, fmt.Errorf("failed to change to remote work directory: %w", err)
	}

	result := &WorkflowResult{}

	// Step 1: Check if there are any changes
	hasChanges, err := m.hasUnstagedChanges()
	if err != nil {
		return nil, fmt.Errorf("failed to check for changes: %w", err)
	}

	if !hasChanges {
		m.logger.Info("No changes to commit")
		// Still need to pull to get latest changes
		if _, err := m.pullWithRebase(); err != nil {
			return nil, fmt.Errorf("failed to pull latest changes: %w", err)
		}
		return result, nil
	}

	result.HasChanges = true

	// Step 2: Stage changes (specific files if provided, otherwise all changes)
	if len(filePaths) > 0 {
		if err := m.stageSpecificFiles(filePaths); err != nil {
			return nil, fmt.Errorf("failed to stage specific files: %w", err)
		}
	} else {
		if err := m.stageAllChanges(); err != nil {
			return nil, fmt.Errorf("failed to stage changes: %w", err)
		}
	}

	// Step 3: Commit changes
	commitHash, err := m.commitChanges()
	if err != nil {
		return nil, fmt.Errorf("failed to commit changes: %w", err)
	}
	result.CommitHash = commitHash

	// Step 4: Pull with rebase
	conflictsResolved, err := m.pullWithRebase()
	if err != nil {
		return nil, fmt.Errorf("failed to pull with rebase: %w", err)
	}
	result.ConflictsResolved = conflictsResolved

	// Step 5: Push changes
	if err := m.pushChanges(); err != nil {
		return nil, fmt.Errorf("failed to push changes: %w", err)
	}

	m.logger.Info("Git workflow completed successfully",
		"commit_hash", result.CommitHash,
		"conflicts_resolved", result.ConflictsResolved)

	return result, nil
}

// CommitAndPush commits changes with a custom message and pushes to remote.
func (m *Manager) CommitAndPush(message string) (*WorkflowResult, error) {
	return m.CommitAndPushSpecificFiles(message, nil)
}

// CommitAndPushSpecificFiles commits specific files with a custom message and pushes to remote.
func (m *Manager) CommitAndPushSpecificFiles(message string, filePaths []string) (*WorkflowResult, error) {
	originalDir, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("failed to get current directory: %w", err)
	}
	defer func() {
		if err := os.Chdir(originalDir); err != nil {
			slog.Error("Failed to restore original directory", "error", err)
		}
	}()

	if err := os.Chdir(m.project.RemoteWorkDir); err != nil {
		return nil, fmt.Errorf("failed to change to remote work directory: %w", err)
	}

	// Stage specific files or all changes if no files specified
	if len(filePaths) > 0 {
		if err := m.stageSpecificFiles(filePaths); err != nil {
			return nil, fmt.Errorf("failed to stage specific files: %w", err)
		}
	} else {
		if err := m.stageAllChanges(); err != nil {
			return nil, fmt.Errorf("failed to stage changes: %w", err)
		}
	}

	// Commit with custom message
	commitHash, err := m.commitWithMessage(message)
	if err != nil {
		return nil, fmt.Errorf("failed to commit changes: %w", err)
	}

	// Push changes
	if err := m.pushChanges(); err != nil {
		return nil, fmt.Errorf("failed to push changes: %w", err)
	}

	return &WorkflowResult{
		CommitHash: commitHash,
		HasChanges: true,
	}, nil
}

// PullLatest pulls the latest changes from remote repository.
func (m *Manager) PullLatest() error {
	originalDir, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("failed to get current directory: %w", err)
	}
	defer func() {
		if err := os.Chdir(originalDir); err != nil {
			slog.Error("Failed to restore original directory", "error", err)
		}
	}()

	if err := os.Chdir(m.project.RemoteWorkDir); err != nil {
		return fmt.Errorf("failed to change to remote work directory: %w", err)
	}

	_, err = m.pullWithRebase()
	return err
}

// ValidateConnectivity checks if the remote repository is accessible.
func (m *Manager) ValidateConnectivity() error {
	// Test SSH connectivity to the remote repository
	// #nosec G204 - RemoteRepo is validated during project configuration
	cmd := exec.Command("git", "ls-remote", m.project.RemoteRepo)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to connect to remote repository: %w", err)
	}
	return nil
}

// initializeRepository initializes or updates the local clone of remote repository.
func (m *Manager) initializeRepository() error {
	// Check if remote work directory exists and is a git repository
	gitDir := filepath.Join(m.project.RemoteWorkDir, ".git")
	if _, err := os.Stat(gitDir); errors.Is(err, fs.ErrNotExist) {
		// Clone the repository
		m.logger.Info("Cloning remote repository")
		return m.cloneRepository()
	}

	// Repository exists, ensure it's up to date with correct remote
	m.logger.Debug("Repository exists, validating remote")
	return m.validateAndUpdateRemote()
}

// cloneRepository clones the remote repository.
func (m *Manager) cloneRepository() error {
	// Ensure parent directory exists
	parentDir := filepath.Dir(m.project.RemoteWorkDir)
	if err := os.MkdirAll(parentDir, 0o755); err != nil {
		return fmt.Errorf("failed to create parent directory: %w", err)
	}

	// Clone repository
	// #nosec G204 - RemoteRepo and RemoteWorkDir are validated during project configuration
	cmd := exec.Command("git", "clone", m.project.RemoteRepo, m.project.RemoteWorkDir)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to clone repository: %w\nOutput: %s", err, string(output))
	}

	// Change to repository directory
	if err := os.Chdir(m.project.RemoteWorkDir); err != nil {
		return fmt.Errorf("failed to change to repository directory: %w", err)
	}

	// Set up sparse checkout if we have a path prefix
	if m.project.RemotePathPrefix != "" {
		if err := m.setupSparseCheckout(); err != nil {
			m.logger.Warn("Failed to setup sparse checkout, continuing with full repository", "error", err)
		}
	}

	return nil
}

// validateAndUpdateRemote validates and updates the remote repository configuration.
func (m *Manager) validateAndUpdateRemote() error {
	originalDir, err := os.Getwd()
	if err != nil {
		return fmt.Errorf("failed to get current directory: %w", err)
	}
	defer func() {
		if err := os.Chdir(originalDir); err != nil {
			slog.Error("Failed to restore original directory", "error", err)
		}
	}()

	if err := os.Chdir(m.project.RemoteWorkDir); err != nil {
		return fmt.Errorf("failed to change to remote work directory: %w", err)
	}

	// Get current remote URL
	cmd := exec.Command("git", "remote", "get-url", "origin")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to get remote URL: %w", err)
	}

	currentRemote := strings.TrimSpace(string(output))
	if currentRemote != m.project.RemoteRepo {
		// Update remote URL
		m.logger.Info("Updating remote URL",
			"old_remote", currentRemote,
			"new_remote", m.project.RemoteRepo)

		// #nosec G204 - RemoteRepo is validated during project configuration
		cmd = exec.Command("git", "remote", "set-url", "origin", m.project.RemoteRepo)
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("failed to update remote URL: %w", err)
		}
	}

	// Fetch latest changes
	cmd = exec.Command("git", "fetch", "origin")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to fetch from remote: %w", err)
	}

	return nil
}

// setupSparseCheckout configures sparse checkout for the repository.
func (m *Manager) setupSparseCheckout() error {
	// Enable sparse checkout
	cmd := exec.Command("git", "config", "core.sparseCheckout", "true")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to enable sparse checkout: %w", err)
	}

	// Create sparse checkout file
	sparseCheckoutFile := filepath.Join(m.project.RemoteWorkDir, ".git", "info", "sparse-checkout")
	content := fmt.Sprintf("%s/*\n", m.project.RemotePathPrefix)
	if err := os.WriteFile(sparseCheckoutFile, []byte(content), 0o600); err != nil {
		return fmt.Errorf("failed to create sparse checkout file: %w", err)
	}

	// Apply sparse checkout
	cmd = exec.Command("git", "read-tree", "-m", "-u", "HEAD")
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to apply sparse checkout: %w", err)
	}

	m.logger.Info("Sparse checkout configured", "prefix", m.project.RemotePathPrefix)
	return nil
}

// hasUnstagedChanges checks if there are unstaged changes in the repository.
func (m *Manager) hasUnstagedChanges() (bool, error) {
	cmd := exec.Command("git", "status", "--porcelain")
	output, err := cmd.Output()
	if err != nil {
		return false, fmt.Errorf("failed to check git status: %w", err)
	}

	return len(strings.TrimSpace(string(output))) > 0, nil
}

// stageAllChanges stages all changes in the repository.
func (m *Manager) stageAllChanges() error {
	cmd := exec.Command("git", "add", "-A")
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to stage changes: %w\nOutput: %s", err, string(output))
	}
	return nil
}

// stageSpecificFiles stages only the specified files in the repository.
func (m *Manager) stageSpecificFiles(filePaths []string) error {
	if len(filePaths) == 0 {
		// If no specific files provided, check if there are any changes to stage
		hasChanges, err := m.hasUnstagedChanges()
		if err != nil {
			return fmt.Errorf("failed to check for changes: %w", err)
		}
		if !hasChanges {
			return nil // No changes to stage
		}
		// Fallback to staging all changes if no specific files provided
		return m.stageAllChanges()
	}

	// Stage files in batches to avoid command line length limits
	const maxFilesPerBatch = 100
	for i := 0; i < len(filePaths); i += maxFilesPerBatch {
		end := i + maxFilesPerBatch
		if end > len(filePaths) {
			end = len(filePaths)
		}
		batch := filePaths[i:end]

		// Prepare git add command with specific files
		args := append([]string{"add"}, batch...)
		cmd := exec.Command("git", args...)
		output, err := cmd.CombinedOutput()
		if err != nil {
			return fmt.Errorf("failed to stage files %v: %w\nOutput: %s", batch, err, string(output))
		}
	}

	return nil
}

// commitChanges commits staged changes with an automated message.
func (m *Manager) commitChanges() (string, error) {
	message := fmt.Sprintf("repo-sync: %s - %s", m.project.Name, time.Now().Format("2006-01-02 15:04:05"))
	return m.commitWithMessage(message)
}

// commitWithMessage commits staged changes with a specific message.
func (m *Manager) commitWithMessage(message string) (string, error) {
	cmd := exec.Command("git", "commit", "-m", message)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("failed to commit changes: %w\nOutput: %s", err, string(output))
	}

	// Get the commit hash
	cmd = exec.Command("git", "rev-parse", "HEAD")
	hashOutput, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get commit hash: %w", err)
	}

	commitHash := strings.TrimSpace(string(hashOutput))
	m.logger.Info("Changes committed", "commit_hash", commitHash)
	return commitHash, nil
}

// pullWithRebase pulls changes from remote with rebase and handles conflicts.
func (m *Manager) pullWithRebase() (int, error) {
	m.logger.Info("Pulling changes with rebase")

	cmd := exec.Command("git", "pull", "--rebase", "origin", "HEAD")
	output, err := cmd.CombinedOutput()
	if err != nil {
		outputStr := string(output)
		if strings.Contains(outputStr, "CONFLICT") {
			m.logger.Warn("Merge conflicts detected, attempting automatic resolution")
			return m.resolveConflicts()
		}
		return 0, fmt.Errorf("failed to pull with rebase: %w\nOutput: %s", err, outputStr)
	}

	m.logger.Info("Pull with rebase completed successfully")
	return 0, nil
}

// pushChanges pushes local changes to remote repository.
func (m *Manager) pushChanges() error {
	// Get current branch
	cmd := exec.Command("git", "branch", "--show-current")
	branchOutput, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to get current branch: %w", err)
	}

	currentBranch := strings.TrimSpace(string(branchOutput))

	// Push to remote
	cmd = exec.Command("git", "push", "origin", currentBranch)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("failed to push changes: %w\nOutput: %s", err, string(output))
	}

	m.logger.Info("Changes pushed successfully", "branch", currentBranch)
	return nil
}

// resolveConflicts attempts to automatically resolve merge conflicts.
func (m *Manager) resolveConflicts() (int, error) {
	// Get list of conflicted files
	cmd := exec.Command("git", "diff", "--name-only", "--diff-filter=U")
	output, err := cmd.Output()
	if err != nil {
		return 0, fmt.Errorf("failed to get conflicted files: %w", err)
	}

	conflictedFiles := strings.Split(strings.TrimSpace(string(output)), "\n")
	if len(conflictedFiles) == 0 || conflictedFiles[0] == "" {
		return 0, nil
	}

	m.logger.Info("Resolving conflicts", "files", conflictedFiles)

	resolvedCount := 0
	for _, file := range conflictedFiles {
		if file == "" {
			continue
		}

		if err := m.resolveFileConflict(file); err != nil {
			m.logger.Error("Failed to resolve conflict", "error", err, "file", file)
			continue
		}

		resolvedCount++
	}

	// Stage resolved files
	if err := m.stageAllChanges(); err != nil {
		return resolvedCount, fmt.Errorf("failed to stage resolved files: %w", err)
	}

	// Continue rebase
	cmd = exec.Command("git", "rebase", "--continue")
	if err := cmd.Run(); err != nil {
		return resolvedCount, fmt.Errorf("failed to continue rebase: %w", err)
	}

	return resolvedCount, nil
}

// resolveFileConflict resolves conflicts in a single file using timestamp strategy.
func (m *Manager) resolveFileConflict(filePath string) error {
	// Read file content
	content, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("failed to read conflicted file: %w", err)
	}

	lines := strings.Split(string(content), "\n")
	var resolvedLines []string
	inConflict := false
	var localLines []string
	var remoteLines []string

	for _, line := range lines {
		switch {
		case strings.HasPrefix(line, "<<<<<<<"):
			inConflict = true
			localLines = []string{}
			continue
		case strings.HasPrefix(line, "=======") && inConflict:
			remoteLines = []string{}
			continue
		case strings.HasPrefix(line, ">>>>>>>") && inConflict:
			// Resolve conflict: choose newer content (simple strategy)
			// In a more sophisticated implementation, we'd use file timestamps
			// or content analysis to make better decisions
			if len(remoteLines) > 0 {
				resolvedLines = append(resolvedLines, remoteLines...)
			} else {
				resolvedLines = append(resolvedLines, localLines...)
			}
			inConflict = false
			localLines = nil
			remoteLines = nil
			continue
		}

		if inConflict {
			if remoteLines != nil {
				remoteLines = append(remoteLines, line)
			} else {
				localLines = append(localLines, line)
			}
		} else {
			resolvedLines = append(resolvedLines, line)
		}
	}

	// Write resolved content back to file
	resolvedContent := strings.Join(resolvedLines, "\n")
	if err := os.WriteFile(filePath, []byte(resolvedContent), 0o600); err != nil {
		return fmt.Errorf("failed to write resolved file: %w", err)
	}

	m.logger.Info("Conflict resolved", "file", filePath)
	return nil
}
