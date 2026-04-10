---
name: spacemacs
description: Edit the Spacemacs .spacemacs configuration, add packages/layers, write elisp hooks, and manage custom elisp in private/local. Use when working with spacemacs/.spacemacs, Emacs Lisp for the user's config, Spacemacs layers or keybindings, or elisp packages in ~/.config/emacs/private/local/. Also triggers for Claude Code Emacs integration (claude-code.el hooks, alert.el, my-attention.el).
---

# Spacemacs Configuration Skill

Read `spacemacs/CLAUDE.md` for all conventions, naming rules, keybinding prefixes, elisp style, and validation steps before making changes.

## Quick Reference

- **Config file**: `spacemacs/.spacemacs` (upstream reference, merged to `~/.spacemacs` via ediff)
- **Custom elisp**: `~/.config/emacs/private/local/` (e.g., `my-attention.el`)
- **Validate**: `cd spacemacs && make check` (byte-compile, fix all warnings)
- **TOC**: Lines 5-39 of `.spacemacs` — update when adding/renaming section headers
- **User bindings**: `SPC o` prefix. AI tools: `SPC $` prefix
- **Naming**: `my/` prefix for all user-defined functions and variables
