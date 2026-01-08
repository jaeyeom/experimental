// Package config provides configuration loading for repo-sync.
package config

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v3"
)

// GlobalConfig represents the main configuration file.
type GlobalConfig struct {
	DefaultSyncPatterns SyncPatterns      `yaml:"default_sync_patterns"`
	ConflictResolution  ConflictConfig    `yaml:"conflict_resolution"`
	Performance         PerformanceConfig `yaml:"performance"`
	Logging             LoggingConfig     `yaml:"logging"`
}

// SyncPatterns defines include/exclude patterns for file synchronization.
type SyncPatterns struct {
	Include []string `yaml:"include"`
	Exclude []string `yaml:"exclude"`
}

// ConflictConfig defines conflict resolution settings.
type ConflictConfig struct {
	DefaultStrategy string `yaml:"default_strategy"`
	BackupConflicts bool   `yaml:"backup_conflicts"`
}

// PerformanceConfig defines performance-related settings.
type PerformanceConfig struct {
	RsyncCompressionLevel   int `yaml:"rsync_compression_level"`
	MaxConcurrentOperations int `yaml:"max_concurrent_operations"`
	TimeoutSeconds          int `yaml:"timeout_seconds"`
}

// LoggingConfig defines logging settings.
type LoggingConfig struct {
	Level             string `yaml:"level"`
	FileRetentionDays int    `yaml:"file_retention_days"`
	MaxFileSizeMB     int    `yaml:"max_file_size_mb"`
}

// Project represents a project configuration.
type Project struct {
	Name             string       `yaml:"project_name"`
	LocalWorkDir     string       `yaml:"local_work_dir"`
	RemoteRepo       string       `yaml:"remote_repo"`
	RemoteWorkDir    string       `yaml:"remote_work_dir"`
	RemotePathPrefix string       `yaml:"remote_path_prefix"`
	SyncPatterns     SyncPatterns `yaml:"sync_patterns"`
}

// GetConfigDir returns the configuration directory path.
func GetConfigDir() (string, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get home directory: %w", err)
	}
	return filepath.Join(homeDir, ".config", "repo-sync"), nil
}

// LoadGlobalConfig loads the global configuration.
func LoadGlobalConfig() (*GlobalConfig, error) {
	configDir, err := GetConfigDir()
	if err != nil {
		return nil, err
	}

	configFile := filepath.Join(configDir, "config.yaml")
	data, err := os.ReadFile(configFile)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			// Return default configuration if file doesn't exist
			return getDefaultGlobalConfig(), nil
		}
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config GlobalConfig
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	return &config, nil
}

// SaveGlobalConfig saves the global configuration.
func SaveGlobalConfig(config *GlobalConfig) error {
	configDir, err := GetConfigDir()
	if err != nil {
		return err
	}

	configFile := filepath.Join(configDir, "config.yaml")
	data, err := yaml.Marshal(config)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}

	if err := os.WriteFile(configFile, data, 0o600); err != nil {
		return fmt.Errorf("failed to write config file: %w", err)
	}
	return nil
}

// LoadProject loads a project configuration.
func LoadProject(name string) (*Project, error) {
	configDir, err := GetConfigDir()
	if err != nil {
		return nil, err
	}

	projectFile := filepath.Join(configDir, "mappings", name+".yaml")
	data, err := os.ReadFile(projectFile)
	if err != nil {
		return nil, fmt.Errorf("failed to read project file: %w", err)
	}

	var project Project
	if err := yaml.Unmarshal(data, &project); err != nil {
		return nil, fmt.Errorf("failed to parse project file: %w", err)
	}

	// Merge with default patterns if project doesn't have custom patterns
	if len(project.SyncPatterns.Include) == 0 && len(project.SyncPatterns.Exclude) == 0 {
		globalConfig, err := LoadGlobalConfig()
		if err == nil {
			project.SyncPatterns = globalConfig.DefaultSyncPatterns
		}
	}

	return &project, nil
}

// SaveProject saves a project configuration.
func SaveProject(project *Project) error {
	configDir, err := GetConfigDir()
	if err != nil {
		return err
	}

	mappingsDir := filepath.Join(configDir, "mappings")
	if err := os.MkdirAll(mappingsDir, 0o755); err != nil {
		return fmt.Errorf("failed to create mappings directory: %w", err)
	}

	projectFile := filepath.Join(mappingsDir, project.Name+".yaml")
	data, err := yaml.Marshal(project)
	if err != nil {
		return fmt.Errorf("failed to marshal project: %w", err)
	}

	if err := os.WriteFile(projectFile, data, 0o600); err != nil {
		return fmt.Errorf("failed to write project file: %w", err)
	}
	return nil
}

// RemoveProject removes a project configuration.
func RemoveProject(name string) error {
	configDir, err := GetConfigDir()
	if err != nil {
		return err
	}

	projectFile := filepath.Join(configDir, "mappings", name+".yaml")
	if err := os.Remove(projectFile); err != nil {
		return fmt.Errorf("failed to remove project file: %w", err)
	}
	return nil
}

// ListProjects returns all configured projects.
func ListProjects() ([]*Project, error) {
	configDir, err := GetConfigDir()
	if err != nil {
		return nil, err
	}

	mappingsDir := filepath.Join(configDir, "mappings")
	entries, err := os.ReadDir(mappingsDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return []*Project{}, nil
		}
		return nil, fmt.Errorf("failed to read mappings directory: %w", err)
	}

	var projects []*Project
	for _, entry := range entries {
		if !entry.IsDir() && filepath.Ext(entry.Name()) == ".yaml" {
			name := entry.Name()[:len(entry.Name())-5] // Remove .yaml extension
			project, err := LoadProject(name)
			if err != nil {
				continue // Skip invalid project files
			}
			projects = append(projects, project)
		}
	}

	return projects, nil
}

// ValidateProject validates a project configuration.
func ValidateProject(project *Project) error {
	if project.Name == "" {
		return fmt.Errorf("project name is required")
	}

	if project.LocalWorkDir == "" {
		return fmt.Errorf("local work directory is required")
	}

	if project.RemoteRepo == "" {
		return fmt.Errorf("remote repository is required")
	}

	if project.RemotePathPrefix == "" {
		return fmt.Errorf("remote path prefix is required")
	}

	// Check if local directory exists
	if _, err := os.Stat(project.LocalWorkDir); errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("local work directory does not exist: %s", project.LocalWorkDir)
	}

	// Validate remote repository URL format
	if !isValidGitURL(project.RemoteRepo) {
		return fmt.Errorf("invalid remote repository URL: %s", project.RemoteRepo)
	}

	return nil
}

// isValidGitURL performs basic validation of Git repository URLs.
func isValidGitURL(url string) bool {
	// Basic validation - should start with git@, https://, or ssh://
	return len(url) > 0 && (
	// SSH format: git@hostname:repo
	(len(url) > 4 && url[:4] == "git@") ||
		// HTTPS format
		(len(url) > 8 && url[:8] == "https://") ||
		// SSH protocol format
		(len(url) > 6 && url[:6] == "ssh://"))
}

// getDefaultGlobalConfig returns the default global configuration.
func getDefaultGlobalConfig() *GlobalConfig {
	return &GlobalConfig{
		DefaultSyncPatterns: SyncPatterns{
			Include: []string{
				"*.yaml", "*.yml", "*.json", "*.toml", "*.ini", "*.conf",
				"*.config", "*.md", "*.txt", "*.org",
			},
			Exclude: []string{
				"*.tmp", "*.log", "*.cache", ".git/**/*",
				"node_modules/**/*", "__pycache__/**/*",
			},
		},
		ConflictResolution: ConflictConfig{
			DefaultStrategy: "timestamp_newest",
			BackupConflicts: true,
		},
		Performance: PerformanceConfig{
			RsyncCompressionLevel:   6,
			MaxConcurrentOperations: 3,
			TimeoutSeconds:          300,
		},
		Logging: LoggingConfig{
			Level:             "info",
			FileRetentionDays: 30,
			MaxFileSizeMB:     10,
		},
	}
}
