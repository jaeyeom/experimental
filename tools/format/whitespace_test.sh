#!/usr/bin/env bash
# Workspace test verifying no tracked/untracked *.md or *.org file has trailing
# whitespace.
set -euo pipefail

whitespace_bin="$1"
workspace_file="$2"

exec "$whitespace_bin" -check -workspace "$workspace_file"
