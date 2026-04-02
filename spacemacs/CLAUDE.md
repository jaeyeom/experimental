# Spacemacs Configuration

This directory contains the upstream Spacemacs configuration to be merged with `~/.spacemacs`.

## Sync Workflow

The `.spacemacs` file here serves as an **upstream reference**. It is manually merged with the user's actual `~/.spacemacs` using ediff:

```
~/.spacemacs  ←──ediff──→  spacemacs/.spacemacs
```

## Navigation

The `.spacemacs` file is large. Use the **Table of Contents** at the top (lines 5-39) to locate sections. Use Grep to jump to a section header:

```
Grep pattern=";;; Claude Code" path="spacemacs/.spacemacs"
```

When adding or renaming a `;;; Section` header in `dotspacemacs/user-config`, update the TOC at the top of the file to match.

## Platform Detection

The configuration uses these variables for platform-specific behavior:

- `my/crostini-p` - Non-nil if running on Chrome OS Crostini
- `my/termux-p` - Non-nil if running in Termux on Android
- `my/macos-p` - Non-nil if running on macOS

## Conventions

### Naming

- All user-defined functions and variables use the `my/` prefix (e.g., `my/claude-code-display-buffer-right`)
- Declare functions with `declare-function` near the top of the file to silence byte-compiler warnings
- Declare external variables with `defvar` to suppress warnings

### Keybindings

- `SPC o` prefix is reserved for user-defined bindings
- `SPC $` prefix is used for AI/LLM tools (e.g., `SPC $ C` for claude-code, `SPC $ a m` for aider)
- Use `spacemacs/set-leader-keys` for global bindings

### Elisp Style

- Use `setopt` for `defcustom` variables (not `setq`)
- Use `setq-default` for buffer-local defaults
- Use `with-eval-after-load` for package-specific config (not `eval-after-load`)
- Use `(require 'package nil 'noerror)` for optional dependencies
- Prefer named `defun` + `add-hook` over inline lambdas for non-trivial hooks

### Adding Packages

Additional packages go in `dotspacemacs-additional-packages` inside `dotspacemacs/layers`.

### Custom Elisp Files

Standalone elisp packages (e.g., `my-attention.el`) live at `~/.config/emacs/private/local/`. To use them from `.spacemacs`:

```elisp
(add-to-list 'load-path "~/.config/emacs/private/local/")
(require 'my-package nil 'noerror)
```

### Claude Code Integration

The config uses `claude-code.el` with event hooks dispatched through `my/claude-hook-listener`, which integrates `alert.el` and `my-attention.el`. Hook event types include `notification` (Claude waiting) and `stop` (Claude finished). To add a new handler, extend the `cond` in `my/claude-hook-listener`.

## Local Validation

Run `make` to byte-compile and check for warnings:

```bash
make check
```

Resolve any byte-compile warnings before committing changes.
