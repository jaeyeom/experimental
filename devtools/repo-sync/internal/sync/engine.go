// Package sync provides synchronization engine for repo-sync.
package sync

import (
	"database/sql"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/database"
	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/git"
)

// Engine represents the main synchronization engine.
type Engine struct {
	project    *config.Project
	gitManager *git.Manager
	logger     *slog.Logger
}

// Options represents sync operation options.
type Options struct {
	DryRun bool
	Force  bool
}

// Result represents the result of a sync operation.
type Result struct {
	FilesSynced       int
	BytesTransferred  int64
	ConflictsResolved int
	Duration          time.Duration
	GitCommitHash     string
}

// AddResult represents the result of an add operation.
type AddResult struct {
	FileSize      int64
	GitCommitHash string
}

// RemoveResult represents the result of a remove operation.
type RemoveResult struct {
	GitCommitHash string
	DeletedAt     time.Time
}

// NewEngine creates a new sync engine instance.
func NewEngine(project *config.Project) (*Engine, error) {
	if err := config.ValidateProject(project); err != nil {
		return nil, fmt.Errorf("invalid project configuration: %w", err)
	}

	// Ensure remote work directory exists
	if err := os.MkdirAll(project.RemoteWorkDir, 0o755); err != nil {
		return nil, fmt.Errorf("failed to create remote work directory: %w", err)
	}

	// Initialize git manager
	gitManager, err := git.NewManager(project)
	if err != nil {
		return nil, fmt.Errorf("failed to initialize git manager: %w", err)
	}

	logger := slog.With("project", project.Name)

	return &Engine{
		project:    project,
		gitManager: gitManager,
		logger:     logger,
	}, nil
}

// Sync performs bidirectional synchronization.
func (e *Engine) Sync(opts *Options) (*Result, error) {
	startTime := time.Now()
	e.logger.Info("Starting sync operation")

	// Create database connection
	db, err := database.GetConnection()
	if err != nil {
		return nil, fmt.Errorf("failed to connect to database: %w", err)
	}
	defer db.Close()

	// Get or create project ID
	projectID, err := e.ensureProjectInDB(db)
	if err != nil {
		return nil, fmt.Errorf("failed to ensure project in database: %w", err)
	}

	// Start sync operation tracking
	syncOp := &database.SyncOperation{
		ProjectID:     projectID,
		OperationType: "sync",
		StartedAt:     startTime,
		Status:        "in_progress",
	}

	opID, err := database.CreateSyncOperation(db, syncOp)
	if err != nil {
		return nil, fmt.Errorf("failed to create sync operation: %w", err)
	}

	defer func() {
		// Update operation status on completion
		now := time.Now()
		syncOp.CompletedAt = &now
		if err := database.UpdateSyncOperation(db, opID, syncOp); err != nil {
			e.logger.Error("Failed to update sync operation", "error", err)
		}
	}()

	result := &Result{}

	// Phase 1: Pre-sync validation
	if err := e.validatePreSync(); err != nil {
		syncOp.Status = "failed"
		errorMsg := err.Error()
		syncOp.ErrorMessage = &errorMsg
		return nil, fmt.Errorf("pre-sync validation failed: %w", err)
	}

	// Phase 2: Initial rsync (local to remote work directory)
	e.logger.Info("Performing initial rsync from local to remote work directory")
	rsyncResult, err := e.performRsync(e.project.LocalWorkDir, e.project.RemoteWorkDir, opts.DryRun)
	if err != nil {
		syncOp.Status = "failed"
		errorMsg := err.Error()
		syncOp.ErrorMessage = &errorMsg
		return nil, fmt.Errorf("initial rsync failed: %w", err)
	}

	result.FilesSynced += rsyncResult.FilesSynced
	result.BytesTransferred += rsyncResult.BytesTransferred

	if opts.DryRun {
		e.logger.Info("Dry run completed, no actual changes made")
		syncOp.Status = "completed"
		result.Duration = time.Since(startTime)
		return result, nil
	}

	// Phase 3: Git operations (commit, pull, push)
	e.logger.Info("Performing Git operations")
	gitResult, err := e.gitManager.PerformWorkflowWithFiles(rsyncResult.TransferredFiles)
	if err != nil {
		syncOp.Status = "failed"
		errorMsg := err.Error()
		syncOp.ErrorMessage = &errorMsg
		return nil, fmt.Errorf("git workflow failed: %w", err)
	}

	result.ConflictsResolved = gitResult.ConflictsResolved
	result.GitCommitHash = gitResult.CommitHash

	// Phase 4: Post-sync rsync (remote work directory back to local)
	e.logger.Info("Performing post-sync rsync from remote work directory to local")
	postRsyncResult, err := e.performRsync(e.project.RemoteWorkDir, e.project.LocalWorkDir, false)
	if err != nil {
		syncOp.Status = "failed"
		errorMsg := err.Error()
		syncOp.ErrorMessage = &errorMsg
		return nil, fmt.Errorf("post-sync rsync failed: %w", err)
	}

	result.FilesSynced += postRsyncResult.FilesSynced
	result.BytesTransferred += postRsyncResult.BytesTransferred

	// Update sync operation with results
	syncOp.Status = "completed"
	syncOp.FilesSynced = result.FilesSynced
	syncOp.BytesTransferred = result.BytesTransferred
	syncOp.ConflictsResolved = result.ConflictsResolved
	syncOp.GitCommitHash = &result.GitCommitHash

	result.Duration = time.Since(startTime)
	e.logger.Info("Sync operation completed successfully",
		"files_synced", result.FilesSynced,
		"bytes_transferred", result.BytesTransferred,
		"conflicts_resolved", result.ConflictsResolved,
		"duration", result.Duration)

	return result, nil
}

// Download performs unidirectional sync from remote to local.
func (e *Engine) Download(opts *Options) (*Result, error) {
	startTime := time.Now()
	e.logger.Info("Starting download operation")

	// Ensure we have the latest from remote repository
	if err := e.gitManager.PullLatest(); err != nil {
		return nil, fmt.Errorf("failed to pull latest from remote: %w", err)
	}

	// Sync from remote work directory to local
	rsyncResult, err := e.performRsync(e.project.RemoteWorkDir, e.project.LocalWorkDir, opts.DryRun)
	if err != nil {
		return nil, fmt.Errorf("download rsync failed: %w", err)
	}

	result := &Result{
		FilesSynced:      rsyncResult.FilesSynced,
		BytesTransferred: rsyncResult.BytesTransferred,
		Duration:         time.Since(startTime),
	}

	e.logger.Info("Download operation completed successfully",
		"files_downloaded", result.FilesSynced,
		"bytes_transferred", result.BytesTransferred,
		"duration", result.Duration)

	return result, nil
}

// Upload performs unidirectional sync from local to remote.
func (e *Engine) Upload(opts *Options) (*Result, error) {
	startTime := time.Now()
	e.logger.Info("Starting upload operation")

	// Sync from local to remote work directory
	rsyncResult, err := e.performRsync(e.project.LocalWorkDir, e.project.RemoteWorkDir, opts.DryRun)
	if err != nil {
		return nil, fmt.Errorf("upload rsync failed: %w", err)
	}

	result := &Result{
		FilesSynced:      rsyncResult.FilesSynced,
		BytesTransferred: rsyncResult.BytesTransferred,
	}

	if !opts.DryRun {
		// Commit and push changes, staging only the files that were transferred
		gitResult, err := e.gitManager.CommitAndPushSpecificFiles("Upload from local", rsyncResult.TransferredFiles)
		if err != nil {
			return nil, fmt.Errorf("failed to commit and push: %w", err)
		}
		result.GitCommitHash = gitResult.CommitHash
	}

	result.Duration = time.Since(startTime)

	e.logger.Info("Upload operation completed successfully",
		"files_uploaded", result.FilesSynced,
		"bytes_transferred", result.BytesTransferred,
		"duration", result.Duration)

	return result, nil
}

// AddFile adds a new file to the synchronization scope.
func (e *Engine) AddFile(filePath string) (*AddResult, error) {
	e.logger.Info("Adding file to sync scope", "file", filePath)

	// Ensure file exists in local directory
	localFilePath := filepath.Join(e.project.LocalWorkDir, filePath)
	fileInfo, err := os.Stat(localFilePath)
	if err != nil {
		return nil, fmt.Errorf("file not found: %w", err)
	}

	// First perform a regular sync
	if _, err := e.Sync(&Options{}); err != nil {
		return nil, fmt.Errorf("pre-add sync failed: %w", err)
	}

	// Copy file to remote work directory
	remoteFilePath := filepath.Join(e.project.RemoteWorkDir, e.project.RemotePathPrefix, filePath)
	if err := e.copyFile(localFilePath, remoteFilePath); err != nil {
		return nil, fmt.Errorf("failed to copy file to remote: %w", err)
	}

	// Commit and push the new file
	gitResult, err := e.gitManager.CommitAndPush(fmt.Sprintf("Add file: %s", filePath))
	if err != nil {
		return nil, fmt.Errorf("failed to commit new file: %w", err)
	}

	result := &AddResult{
		FileSize:      fileInfo.Size(),
		GitCommitHash: gitResult.CommitHash,
	}

	e.logger.Info("File added successfully",
		"file", filePath,
		"size", result.FileSize,
		"commit_hash", result.GitCommitHash)

	return result, nil
}

// RemoveFile removes a file from both local and remote repositories.
func (e *Engine) RemoveFile(filePath string) (*RemoveResult, error) {
	e.logger.Info("Removing file from sync scope", "file", filePath)

	deletedAt := time.Now()

	// Remove from local directory
	localFilePath := filepath.Join(e.project.LocalWorkDir, filePath)
	if err := os.Remove(localFilePath); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil, fmt.Errorf("failed to remove local file: %w", err)
	}

	// Remove from remote work directory
	remoteFilePath := filepath.Join(e.project.RemoteWorkDir, e.project.RemotePathPrefix, filePath)
	if err := os.Remove(remoteFilePath); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil, fmt.Errorf("failed to remove remote file: %w", err)
	}

	// Commit and push the deletion
	gitResult, err := e.gitManager.CommitAndPush(fmt.Sprintf("Remove file: %s", filePath))
	if err != nil {
		return nil, fmt.Errorf("failed to commit file removal: %w", err)
	}

	// TODO: Record deletion in database for multi-machine coordination

	result := &RemoveResult{
		GitCommitHash: gitResult.CommitHash,
		DeletedAt:     deletedAt,
	}

	e.logger.Info("File removed successfully",
		"file", filePath,
		"deleted_at", result.DeletedAt,
		"commit_hash", result.GitCommitHash)

	return result, nil
}

// validatePreSync performs validation before sync operation.
func (e *Engine) validatePreSync() error {
	// Check local directory accessibility
	if _, err := os.Stat(e.project.LocalWorkDir); err != nil {
		return fmt.Errorf("local directory not accessible: %w", err)
	}

	// Check remote repository connectivity
	if err := e.gitManager.ValidateConnectivity(); err != nil {
		return fmt.Errorf("remote repository not accessible: %w", err)
	}

	return nil
}

// ensureProjectInDB ensures the project exists in the database.
func (e *Engine) ensureProjectInDB(db *sql.DB) (int, error) {
	projectID, err := database.GetProjectID(db, e.project.Name)
	if err != nil {
		// Project doesn't exist, create it
		if err := database.CreateProject(db, e.project); err != nil {
			return 0, fmt.Errorf("failed to create project in database: %w", err)
		}
		id, err := database.GetProjectID(db, e.project.Name)
		if err != nil {
			return 0, fmt.Errorf("failed to get project ID after creation: %w", err)
		}
		return id, nil
	}
	return projectID, nil
}

// copyFile copies a file from source to destination.
func (e *Engine) copyFile(src, dst string) error {
	// Ensure destination directory exists
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return fmt.Errorf("failed to create destination directory: %w", err)
	}

	// Copy file
	srcFile, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("failed to open source file: %w", err)
	}
	defer srcFile.Close()

	dstFile, err := os.Create(dst)
	if err != nil {
		return fmt.Errorf("failed to create destination file: %w", err)
	}
	defer dstFile.Close()

	if _, err := io.Copy(dstFile, srcFile); err != nil {
		return fmt.Errorf("failed to copy file content: %w", err)
	}

	return nil
}
