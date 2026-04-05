#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LOCAL_DIR="$SCRIPT_DIR/private/local"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
TARGET_DIR="$CONFIG_DIR/private/local"

# Replace Spacemacs's placeholder private/local directory with a symlink.
mkdir -p "$CONFIG_DIR/private"
if [ -L "$TARGET_DIR" ]; then
  rm "$TARGET_DIR"
elif [ -d "$TARGET_DIR" ]; then
  ENTRY_COUNT="$(find "$TARGET_DIR" -mindepth 1 -maxdepth 1 | wc -l | tr -d '[:space:]')"
  if [ "$ENTRY_COUNT" = "1" ] && [ -f "$TARGET_DIR/README.md" ]; then
    rm -rf "$TARGET_DIR"
  else
    echo "Refusing to replace existing directory at $TARGET_DIR; move its contents first." >&2
    exit 1
  fi
elif [ -e "$TARGET_DIR" ]; then
  echo "Refusing to replace non-directory path at $TARGET_DIR." >&2
  exit 1
fi

ln -s "$LOCAL_DIR" "$TARGET_DIR"
echo "Linked $TARGET_DIR -> $LOCAL_DIR"

# Hide the README.md deletion from git status in the Spacemacs clone.
if ! git -C "$CONFIG_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Expected $CONFIG_DIR to be a Git checkout before setting skip-worktree." >&2
  exit 1
fi
git -C "$CONFIG_DIR" update-index --skip-worktree private/local/README.md
echo "Set skip-worktree for private/local/README.md"
