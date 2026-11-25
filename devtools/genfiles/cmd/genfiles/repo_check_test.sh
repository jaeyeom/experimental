#!/usr/bin/env bash
# Repository check verifying generated_files.yaml matches tracked files and
# .gitattributes.
set -euo pipefail

genfiles_bin="$1"
workspace_file="$2"

# Under Bazel, the data file is a symlink into the real checkout.
repo_root="$(dirname "$(realpath "$workspace_file")")"
exec "$genfiles_bin" check --root "$repo_root"
