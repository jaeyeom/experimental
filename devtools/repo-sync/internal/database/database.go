// Package database provides database operations for repo-sync.
package database

import (
	"database/sql"
	"errors"
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
	_ "github.com/mattn/go-sqlite3" // Import sqlite3 driver for database/sql
)

// ProjectStats represents project statistics.
type ProjectStats struct {
	TotalSyncs      int
	SuccessfulSyncs int
	FailedSyncs     int
	LastSyncAt      time.Time
	TrackedFiles    int
	ConflictedFiles int
}

// SyncOperation represents a sync operation record.
type SyncOperation struct {
	ID                int
	ProjectID         int
	OperationType     string
	StartedAt         time.Time
	CompletedAt       *time.Time
	Status            string
	FilesSynced       int
	BytesTransferred  int64
	ConflictsResolved int
	ErrorMessage      *string
	GitCommitHash     *string
}

// FileTracking represents a file tracking record.
type FileTracking struct {
	ID           int
	ProjectID    int
	FilePath     string
	FileHash     string
	FileSize     int64
	LastModified time.Time
	SyncStatus   string
	CreatedAt    time.Time
	UpdatedAt    time.Time
}

// DeletionTracking represents a deletion tracking record.
type DeletionTracking struct {
	ID                     int
	ProjectID              int
	FilePath               string
	DeletedAt              time.Time
	DeletedBy              string
	GitCommitHash          string
	FileHashBeforeDeletion *string
	FileSizeBeforeDeletion *int64
	DeletionReason         *string
	VerifiedOnMachines     *string
	CreatedAt              time.Time
}

// Initialize creates the database and tables if they don't exist.
func Initialize(reset bool) error {
	configDir, err := config.GetConfigDir()
	if err != nil {
		return fmt.Errorf("failed to get config directory: %w", err)
	}

	dbFile := filepath.Join(configDir, "metadata.db")

	// Remove existing database if reset is requested
	if reset {
		if err := os.Remove(dbFile); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("failed to remove existing database: %w", err)
		}
		slog.Info("Database reset completed")
	}

	// Create database file if it doesn't exist
	db, err := sql.Open("sqlite3", dbFile)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer db.Close()

	// Set database permissions
	if err := os.Chmod(dbFile, 0o600); err != nil {
		slog.Warn("Failed to set database permissions", "error", err)
	}

	// Create tables
	if err := createTables(db); err != nil {
		return fmt.Errorf("failed to create tables: %w", err)
	}

	// Create indexes
	if err := createIndexes(db); err != nil {
		return fmt.Errorf("failed to create indexes: %w", err)
	}

	// Create triggers
	if err := createTriggers(db); err != nil {
		return fmt.Errorf("failed to create triggers: %w", err)
	}

	slog.Info("Database initialized successfully", "database", dbFile)
	return nil
}

// GetConnection returns a database connection.
func GetConnection() (*sql.DB, error) {
	configDir, err := config.GetConfigDir()
	if err != nil {
		return nil, fmt.Errorf("failed to get config directory: %w", err)
	}

	dbFile := filepath.Join(configDir, "metadata.db")
	db, err := sql.Open("sqlite3", dbFile)
	if err != nil {
		return nil, fmt.Errorf("failed to open database: %w", err)
	}

	// Test connection
	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("failed to ping database: %w", err)
	}

	return db, nil
}

// GetProjectStats returns statistics for a project.
func GetProjectStats(db *sql.DB, projectName string) (*ProjectStats, error) {
	query := `
		SELECT
			COALESCE(COUNT(so.id), 0) as total_syncs,
			COALESCE(SUM(CASE WHEN so.status = 'completed' THEN 1 ELSE 0 END), 0) as successful_syncs,
			COALESCE(SUM(CASE WHEN so.status = 'failed' THEN 1 ELSE 0 END), 0) as failed_syncs,
			COALESCE(MAX(so.completed_at), '1970-01-01 00:00:00') as last_sync_at,
			COALESCE(COUNT(DISTINCT ft.id), 0) as tracked_files,
			COALESCE(COUNT(DISTINCT CASE WHEN ft.sync_status = 'conflicted' THEN ft.id END), 0) as conflicted_files
		FROM projects p
		LEFT JOIN sync_operations so ON p.id = so.project_id
		LEFT JOIN file_tracking ft ON p.id = ft.project_id
		WHERE p.name = ?
		GROUP BY p.id
	`

	var stats ProjectStats
	var lastSyncStr string

	err := db.QueryRow(query, projectName).Scan(
		&stats.TotalSyncs,
		&stats.SuccessfulSyncs,
		&stats.FailedSyncs,
		&lastSyncStr,
		&stats.TrackedFiles,
		&stats.ConflictedFiles,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			// Project doesn't exist in database, return empty stats
			return &stats, nil
		}
		return nil, fmt.Errorf("failed to get project stats: %w", err)
	}

	// Parse last sync time
	if lastSyncStr != "" && lastSyncStr != "1970-01-01 00:00:00" {
		stats.LastSyncAt, _ = time.Parse("2006-01-02 15:04:05", lastSyncStr)
	}

	return &stats, nil
}

// CreateProject creates a project record in the database.
func CreateProject(db *sql.DB, project *config.Project) error {
	query := `
		INSERT INTO projects (name, local_work_dir, remote_repo, remote_work_dir, remote_path_prefix)
		VALUES (?, ?, ?, ?, ?)
	`

	_, err := db.Exec(query,
		project.Name,
		project.LocalWorkDir,
		project.RemoteRepo,
		project.RemoteWorkDir,
		project.RemotePathPrefix,
	)
	if err != nil {
		return fmt.Errorf("failed to create project: %w", err)
	}

	return nil
}

// GetProjectID returns the database ID for a project.
func GetProjectID(db *sql.DB, projectName string) (int, error) {
	var id int
	err := db.QueryRow("SELECT id FROM projects WHERE name = ?", projectName).Scan(&id)
	if err != nil {
		if err == sql.ErrNoRows {
			return 0, fmt.Errorf("project not found: %s", projectName)
		}
		return 0, fmt.Errorf("failed to get project ID: %w", err)
	}
	return id, nil
}

// CreateSyncOperation creates a new sync operation record.
func CreateSyncOperation(db *sql.DB, op *SyncOperation) (int64, error) {
	query := `
		INSERT INTO sync_operations (project_id, operation_type, started_at, status)
		VALUES (?, ?, ?, ?)
	`

	result, err := db.Exec(query, op.ProjectID, op.OperationType, op.StartedAt, op.Status)
	if err != nil {
		return 0, fmt.Errorf("failed to create sync operation: %w", err)
	}

	id, err := result.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("failed to get last insert ID: %w", err)
	}
	return id, nil
}

// UpdateSyncOperation updates a sync operation record.
func UpdateSyncOperation(db *sql.DB, id int64, op *SyncOperation) error {
	query := `
		UPDATE sync_operations SET
			completed_at = ?, status = ?, files_synced = ?, bytes_transferred = ?,
			conflicts_resolved = ?, error_message = ?, git_commit_hash = ?
		WHERE id = ?
	`

	_, err := db.Exec(query,
		op.CompletedAt, op.Status, op.FilesSynced, op.BytesTransferred,
		op.ConflictsResolved, op.ErrorMessage, op.GitCommitHash, id,
	)
	if err != nil {
		return fmt.Errorf("failed to update sync operation: %w", err)
	}

	return nil
}

// TrackFile creates or updates a file tracking record.
func TrackFile(db *sql.DB, ft *FileTracking) error {
	query := `
		INSERT OR REPLACE INTO file_tracking
		(project_id, file_path, file_hash, file_size, last_modified, sync_status)
		VALUES (?, ?, ?, ?, ?, ?)
	`

	_, err := db.Exec(query,
		ft.ProjectID, ft.FilePath, ft.FileHash, ft.FileSize,
		ft.LastModified, ft.SyncStatus,
	)
	if err != nil {
		return fmt.Errorf("failed to track file: %w", err)
	}

	return nil
}

// createTables creates all database tables.
func createTables(db *sql.DB) error {
	tables := []string{
		`CREATE TABLE IF NOT EXISTS projects (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT UNIQUE NOT NULL,
			local_work_dir TEXT NOT NULL,
			remote_repo TEXT NOT NULL,
			remote_work_dir TEXT NOT NULL,
			remote_path_prefix TEXT NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			last_sync_at DATETIME,
			sync_count INTEGER DEFAULT 0,
			status TEXT DEFAULT 'active' CHECK (status IN ('active', 'suspended', 'archived'))
		)`,

		`CREATE TABLE IF NOT EXISTS sync_operations (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			project_id INTEGER NOT NULL,
			operation_type TEXT NOT NULL CHECK (operation_type IN ('sync', 'add', 'remove', 'download', 'upload')),
			started_at DATETIME NOT NULL,
			completed_at DATETIME,
			status TEXT NOT NULL CHECK (status IN ('in_progress', 'completed', 'failed', 'cancelled')),
			files_synced INTEGER DEFAULT 0,
			bytes_transferred INTEGER DEFAULT 0,
			conflicts_resolved INTEGER DEFAULT 0,
			error_message TEXT,
			git_commit_hash TEXT,
			FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
		)`,

		`CREATE TABLE IF NOT EXISTS file_tracking (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			project_id INTEGER NOT NULL,
			file_path TEXT NOT NULL,
			file_hash TEXT NOT NULL,
			file_size INTEGER NOT NULL,
			last_modified DATETIME NOT NULL,
			sync_status TEXT NOT NULL CHECK (sync_status IN ('synced', 'pending', 'conflicted', 'deleted')),
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE,
			UNIQUE(project_id, file_path)
		)`,

		`CREATE TABLE IF NOT EXISTS deletion_tracking (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			project_id INTEGER NOT NULL,
			file_path TEXT NOT NULL,
			deleted_at DATETIME NOT NULL,
			deleted_by TEXT NOT NULL,
			git_commit_hash TEXT NOT NULL,
			file_hash_before_deletion TEXT,
			file_size_before_deletion INTEGER,
			deletion_reason TEXT,
			verified_on_machines TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
		)`,

		`CREATE TABLE IF NOT EXISTS machines (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			machine_id TEXT UNIQUE NOT NULL,
			hostname TEXT NOT NULL,
			os_type TEXT NOT NULL,
			first_seen DATETIME DEFAULT CURRENT_TIMESTAMP,
			last_seen DATETIME DEFAULT CURRENT_TIMESTAMP,
			sync_conflicts INTEGER DEFAULT 0,
			sync_successes INTEGER DEFAULT 0
		)`,

		`CREATE TABLE IF NOT EXISTS conflict_resolution (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			project_id INTEGER NOT NULL,
			file_path TEXT NOT NULL,
			conflict_type TEXT NOT NULL CHECK (conflict_type IN ('merge', 'timestamp', 'manual', 'deletion')),
			resolution_strategy TEXT NOT NULL,
			local_hash TEXT,
			remote_hash TEXT,
			resolved_hash TEXT,
			resolved_at DATETIME NOT NULL,
			resolved_by TEXT NOT NULL,
			backup_path TEXT,
			conflict_details TEXT,
			FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE CASCADE
		)`,

		`CREATE TABLE IF NOT EXISTS performance_metrics (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			project_id INTEGER,
			operation TEXT NOT NULL,
			duration REAL NOT NULL,
			result INTEGER NOT NULL,
			timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
			bytes_transferred INTEGER DEFAULT 0,
			files_processed INTEGER DEFAULT 0,
			bandwidth_mbps REAL,
			cpu_usage REAL,
			memory_usage INTEGER,
			FOREIGN KEY (project_id) REFERENCES projects(id) ON DELETE SET NULL
		)`,
	}

	for _, table := range tables {
		if _, err := db.Exec(table); err != nil {
			return fmt.Errorf("failed to create table: %w", err)
		}
	}

	return nil
}

// createIndexes creates database indexes for performance.
func createIndexes(db *sql.DB) error {
	indexes := []string{
		"CREATE INDEX IF NOT EXISTS idx_projects_name ON projects(name)",
		"CREATE INDEX IF NOT EXISTS idx_projects_status ON projects(status)",
		"CREATE INDEX IF NOT EXISTS idx_sync_operations_project ON sync_operations(project_id)",
		"CREATE INDEX IF NOT EXISTS idx_sync_operations_status ON sync_operations(status)",
		"CREATE INDEX IF NOT EXISTS idx_file_tracking_project ON file_tracking(project_id)",
		"CREATE INDEX IF NOT EXISTS idx_file_tracking_status ON file_tracking(sync_status)",
		"CREATE INDEX IF NOT EXISTS idx_deletion_tracking_project ON deletion_tracking(project_id)",
		"CREATE INDEX IF NOT EXISTS idx_performance_metrics_project ON performance_metrics(project_id)",
	}

	for _, index := range indexes {
		if _, err := db.Exec(index); err != nil {
			return fmt.Errorf("failed to create index: %w", err)
		}
	}

	return nil
}

// createTriggers creates database triggers for automation.
func createTriggers(db *sql.DB) error {
	triggers := []string{
		`CREATE TRIGGER IF NOT EXISTS update_projects_timestamp
		AFTER UPDATE ON projects
		FOR EACH ROW
		BEGIN
			UPDATE projects SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
		END`,

		`CREATE TRIGGER IF NOT EXISTS update_file_tracking_timestamp
		AFTER UPDATE ON file_tracking
		FOR EACH ROW
		BEGIN
			UPDATE file_tracking SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
		END`,
	}

	for _, trigger := range triggers {
		if _, err := db.Exec(trigger); err != nil {
			return fmt.Errorf("failed to create trigger: %w", err)
		}
	}

	return nil
}
