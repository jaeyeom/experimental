# Repo-Sync

A specialized utility for synchronizing project-specific files across multiple
development machines using Git-based workflow automation.

## Overview

Repo-Sync addresses the unique challenge of keeping development configurations
and project files in sync without uploading entire repositories to remote
storage. It combines selective file synchronization, automated Git operations,
and intelligent conflict resolution.

## Features

- **Selective Synchronization**: Uses rsync patterns to sync only relevant files
- **Git Integration**: Automated commit-pull-push workflows with conflict resolution
- **Multi-Machine Coordination**: Deletion tracking and conflict resolution across machines
- **SQLite Database**: Metadata tracking, performance metrics, and operation history
- **Robust Error Handling**: Comprehensive error detection and recovery procedures

## Installation

### From Source (Bazel)

This project is part of a Bazel monorepo. To build and run:

```bash
# Build with Bazel
bazel build //devtools/repo-sync/cmd/repo-sync:repo-sync

# Run directly
bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync

# Or use the Makefile shortcuts
cd devtools/repo-sync
make build
make install  # Copies binary to bin/repo-sync
```

## Quick Start

### 1. Initialize Repo-Sync

```bash
# Initialize configuration and database
repo-sync init
```

This creates the configuration directory at `~/.config/repo-sync/` with:
- `config.yaml` - Global configuration
- `mappings/` - Project-specific mapping files
- `metadata.db` - SQLite database for tracking operations
- `logs/`, `backups/`, `rollbacks/`, `locks/` - Working directories

### 2. Add a Project

```bash
# Add a new project mapping
repo-sync config add-project myproject \
  --local-dir /path/to/local/project \
  --remote-repo git@github.com:user/sync-repo.git \
  --remote-prefix projects/myproject \
  --include "*.yaml" --include "*.json" \
  --exclude "*.tmp" --exclude "*.log"
```

### 3. Synchronize Files

```bash
# Perform bidirectional sync
repo-sync sync myproject

# Download only (remote takes precedence)
repo-sync download myproject

# Upload only (local takes precedence)
repo-sync upload myproject

# Dry run to see what would be synced
repo-sync sync myproject --dry-run
```

## Commands

### Core Operations

- `repo-sync init` - Initialize configuration and database
- `repo-sync sync <project>` - Bidirectional synchronization
- `repo-sync add <project> <file>` - Add file to sync scope
- `repo-sync remove <project> <file>` - Remove file from sync scope

### Management

- `repo-sync config add-project <name>` - Add project configuration
- `repo-sync config remove-project <name>` - Remove project configuration
- `repo-sync config list-projects` - List all projects
- `repo-sync config show-project <name>` - Show project details

### Status and Verification

- `repo-sync status [project]` - Show sync status
- `repo-sync verify <project>` - Verify configuration and connectivity

## Configuration

### Global Configuration (`~/.config/repo-sync/config.yaml`)

```yaml
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
```

### Project Configuration (`~/.config/repo-sync/mappings/<project>.yaml`)

```yaml
project_name: "myproject"
local_work_dir: "/home/user/projects/myproject"
remote_repo: "git@github.com:user/dotfiles-sync.git"
remote_work_dir: "/home/user/.repo-sync/remotes/dotfiles-sync"
remote_path_prefix: "projects/myproject"
sync_patterns:
  include:
    - "*.yaml"
    - "*.json"
    - "config/**/*"
  exclude:
    - "*.tmp"
    - "*.log"
    - ".git/**/*"
```

## Architecture

### Core Components

1. **Configuration Management** - YAML-based project configurations and user settings
2. **Database Layer** - SQLite for metadata and operation tracking
3. **Synchronization Engine** - Rsync integration for efficient file transfer
4. **Git Workflow Manager** - Automated commit, pull, and push operations
5. **Conflict Resolution System** - Multiple resolution strategies with backup capability

### Workflow

1. **Pre-sync validation** - Ensures remote repository is accessible
2. **Initial rsync** - Syncs files from local to remote work directory using selective patterns
3. **Git operations** - Commits, pulls with rebase, resolves conflicts, and pushes
4. **Post-sync rsync** - Syncs any changes back to local work directory

## Development

### Building

```bash
# Build binary with Bazel
make build
# or: bazel build //devtools/repo-sync/cmd/repo-sync:repo-sync

# Run tests with Bazel
make test
# or: bazel test //devtools/repo-sync/...

# Format code (from repo root)
make format

# Run the application
make run
# or: bazel run //devtools/repo-sync/cmd/repo-sync:repo-sync
```

### Project Structure

```
devtools/repo-sync/
├── cmd/repo-sync/           # Main application entry point
├── internal/
│   ├── cli/                 # CLI command implementations
│   ├── config/              # Configuration management
│   ├── database/            # SQLite operations
│   ├── git/                 # Git workflow manager
│   └── sync/                # Synchronization engine
├── Makefile                 # Build automation
├── go.mod                   # Go module file
└── README.md               # This file
```

## Dependencies

- **Go 1.21+** - Programming language
- **rsync** - File synchronization tool
- **git** - Version control system
- **sqlite3** - Database engine

### Go Dependencies

- `github.com/spf13/cobra` - CLI framework
- `github.com/spf13/viper` - Configuration management
- `github.com/mattn/go-sqlite3` - SQLite driver
- `github.com/sirupsen/logrus` - Structured logging
- `gopkg.in/yaml.v3` - YAML parsing

## Troubleshooting

### Common Issues

1. **SSH Authentication Failed**
   ```bash
   # Generate SSH key
   ssh-keygen -t ed25519 -C "your_email@example.com"

   # Add to SSH agent
   ssh-add ~/.ssh/id_ed25519

   # Add public key to remote Git service
   ```

2. **Rsync Not Found**
   ```bash
   # Install rsync
   sudo apt install rsync  # Ubuntu/Debian
   brew install rsync      # macOS
   ```

3. **Database Locked**
   ```bash
   # Check for running processes
   lsof ~/.config/repo-sync/metadata.db

   # Reset database if necessary
   repo-sync init --reset-db
   ```

### Debug Mode

```bash
# Enable verbose logging
repo-sync --verbose sync myproject

# Enable debug tracing
repo-sync --debug sync myproject
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Run `make format` and `make test`
6. Submit a pull request

## License

This project is part of the experimental repository and follows the same
licensing terms.
