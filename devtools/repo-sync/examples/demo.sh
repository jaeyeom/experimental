#!/bin/bash

# Demo script for repo-sync utility
# This script demonstrates basic usage of repo-sync

set -e

echo "=== Repo-Sync Demo ==="
echo

# Check if repo-sync is built
if [ ! -f "../bin/repo-sync" ]; then
    echo "Building repo-sync..."
    cd .. && make build && cd examples
    echo
fi

REPO_SYNC="../bin/repo-sync"

echo "1. Initialize repo-sync"
echo "$ repo-sync init"
$REPO_SYNC init
echo

echo "2. Show help"
echo "$ repo-sync --help"
$REPO_SYNC --help | head -10
echo "... (output truncated)"
echo

echo "3. List projects (should be empty initially)"
echo "$ repo-sync config list-projects"
$REPO_SYNC config list-projects
echo

echo "4. Example: Add a project configuration"
echo "You would run:"
echo "$ repo-sync config add-project myproject \\"
echo "    --local-dir /path/to/local/project \\"
echo "    --remote-repo git@github.com:user/sync-repo.git \\"
echo "    --remote-prefix projects/myproject \\"
echo "    --include '*.yaml' --include '*.json'"
echo

echo "5. Example: Sync a project"
echo "You would run:"
echo "$ repo-sync sync myproject"
echo

echo "6. Example: Check status"
echo "You would run:"
echo "$ repo-sync status myproject"
echo

echo "=== Demo Complete ==="
echo
echo "Configuration directory: ~/.config/repo-sync/"
echo "Database location: ~/.config/repo-sync/metadata.db"
echo
echo "Next steps:"
echo "1. Add a real project with 'repo-sync config add-project'"
echo "2. Sync files with 'repo-sync sync <project-name>'"
echo "3. Check status with 'repo-sync status'"