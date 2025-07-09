# Unified Storage System Design

## Overview

A unified storage system for all gh-nudge tools that provides:
- Hierarchical data organization
- Atomic operations with file locking
- Multiple data format support
- Centralized path management
- Backup and versioning

## Storage Structure

```
~/.config/gh-nudge/
├── config.yaml                    # Main configuration
├── notifications.json             # Legacy notification tracking
├── storage/                       # New unified storage
│   ├── metadata.json              # Storage metadata and versioning
│   ├── repos/                     # GitHub-structured data
│   │   └── owner/
│   │       └── repo/
│   │           ├── metadata.json  # Repo-level metadata
│   │           └── pull/
│   │               └── 123/       # PR number
│   │                   ├── diff-hunks.json
│   │                   ├── comments.json
│   │                   ├── notifications.json
│   │                   └── metadata.json
│   └── cache/                    # Cached data (GitHub API responses, etc.)
│       ├── github/
│       │   ├── repos/
│       │   └── users/
│       └── temp/                 # Temporary files
├── logs/                         # Application logs
└── backups/                      # Automatic backups
    ├── 2025-07-09_config.yaml.bak
    └── 2025-07-09_notifications.json.bak
```

## Storage Interface

### Core Storage Operations

```go
// Storage interface for all data operations
type Storage interface {
    // Basic operations
    Get(key string, dest interface{}) error
    Set(key string, data interface{}) error
    Delete(key string) error
    Exists(key string) bool

    // Atomic operations
    WithLock(key string, fn func() error) error
    WithTransaction(fn func(tx Transaction) error) error

    // Hierarchical operations
    List(prefix string) ([]string, error)
    GetChildren(path string) ([]string, error)

    // Metadata operations
    GetMetadata(key string) (*Metadata, error)
    SetMetadata(key string, metadata *Metadata) error

    // Backup operations
    Backup(key string) error
    Restore(key string, timestamp time.Time) error

    // Cleanup operations
    Cleanup() error
    Vacuum() error
}

// Transaction interface for atomic operations
type Transaction interface {
    Get(key string, dest interface{}) error
    Set(key string, data interface{}) error
    Delete(key string) error
    Rollback() error
    Commit() error
}
```

### Specialized Storage Types

```go
// GitHub-structured storage (repos, PRs, issues, etc.)
type GitHubStorage interface {
    Storage
    // Repository operations
    SetRepoMetadata(owner, repo string, metadata map[string]interface{}) error
    GetRepoMetadata(owner, repo string) (map[string]interface{}, error)
    // Pull request operations
    CaptureDiffHunks(owner, repo string, prNumber int, diffHunks []DiffHunk) error
    GetDiffHunks(owner, repo string, prNumber int) ([]DiffHunk, error)
    AddComment(owner, repo string, prNumber int, comment Comment) error
    GetComments(owner, repo string, prNumber int) ([]Comment, error)
    DeleteComment(owner, repo string, prNumber int, file string, line int, side string) error
    DeleteCommentsInRange(owner, repo string, prNumber int, file string, startLine, endLine int, side string) error
    DeleteAllCommentsOnLine(owner, repo string, prNumber int, file string, line int, side string) error
    ClearComments(owner, repo string, prNumber int) error
    // Notification operations
    RecordNotification(owner, repo string, prNumber int, reviewerLogin string, timestamp time.Time) error
    GetLastNotification(owner, repo string, prNumber int, reviewerLogin string) (*time.Time, error)
    CleanupOldNotifications(olderThan time.Duration) error
    // Pull request metadata
    SetPRMetadata(owner, repo string, prNumber int, metadata map[string]interface{}) error
    GetPRMetadata(owner, repo string, prNumber int) (map[string]interface{}, error)
}

// Cache storage
type CacheStorage interface {
    Storage
    SetWithTTL(key string, data interface{}, ttl time.Duration) error
    GetWithTTL(key string, dest interface{}) (bool, error)
    InvalidatePrefix(prefix string) error
}
```

## Storage Manager CLI

### Command Structure

```bash
gh-storage <command> [options]

Commands:
  init                    Initialize storage system
  migrate                 Migrate legacy data to unified storage
  backup                  Create backup of storage data
  restore                 Restore from backup
  clean                   Clean up old/temporary data
  vacuum                  Optimize storage (defragment, compress)
  verify                  Verify storage integrity
  info                    Show storage statistics
  ls                      List storage contents
  get                     Get value from storage
  set                     Set value in storage
  delete                  Delete value from storage
  lock                    Manage file locks
  export                  Export storage data
  import                  Import storage data
```

### Command Examples

```bash
# Initialize storage system
gh-storage init

# Migrate legacy notifications.json to new format
gh-storage migrate --from notifications.json --to repos/

# Create backup
gh-storage backup --all
gh-storage backup --path repos/owner/repo/pull/123

# Clean up old data
gh-storage clean --older-than 30d
gh-storage clean --type cache

# Storage information
gh-storage info
gh-storage info --path repos/

# List contents
gh-storage ls
gh-storage ls repos/owner/repo/
gh-storage ls --recursive repos/

# Get/Set operations
gh-storage get repos/owner/repo/pull/123/notifications.json
gh-storage set cache/github/users/user1 --file user1.json
gh-storage set repos/owner/repo/pull/123/metadata --json '{"status":"draft"}'

# Lock management
gh-storage lock --list
gh-storage lock --release repos/owner/repo/pull/123
gh-storage lock --status repos/owner/repo/pull/123

# Export/Import
gh-storage export --path repos/owner/repo/pull/123 --format json > pr-123.json
gh-storage import --path repos/owner/repo/pull/124 --format json < pr-123.json
```

## Implementation Strategy

### Phase 1: Core Storage Interface
1. Implement basic storage interface with file locking
2. Add hierarchical path management
3. Create storage factory and configuration

### Phase 2: Specialized Storage Types
1. Implement GitHubStorage with GitHub-structured paths
3. Add CacheStorage for performance

### Phase 3: Storage Manager CLI
1. Create basic CLI commands (init, info, ls, get, set)
2. Add migration tools for legacy data
3. Add backup/restore functionality

### Phase 4: Advanced Features
1. Add transaction support
2. Implement vacuum and optimization
3. Add data validation and integrity checks

## Migration Strategy

### Backward Compatibility
- Keep existing config.yaml and notifications.json working
- Provide automatic migration on first use
- Maintain API compatibility for existing tools

### Migration Process
1. **Detection**: Check if legacy files exist
2. **Backup**: Create backup of legacy files
3. **Transform**: Convert data to new format
4. **Validate**: Verify migrated data integrity
5. **Switch**: Update tools to use new storage
6. **Cleanup**: Optionally remove legacy files

## Benefits

### For Users
- Unified storage location and format
- Better performance with caching
- Automatic backups and recovery
- Storage management tools

### For Developers
- Consistent storage interface
- Atomic operations with proper locking
- Hierarchical data organization
- Easy testing with mock storage

### For Scripts
- Shell-friendly storage operations
- Batch operations support
- Export/import capabilities
- Lock management tools
