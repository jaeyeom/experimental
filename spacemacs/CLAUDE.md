# Spacemacs Configuration

This directory contains the upstream Spacemacs configuration to be merged with `~/.spacemacs`.

## Sync Workflow

The `.spacemacs` file here serves as an **upstream reference**. It is manually merged with the user's actual `~/.spacemacs` using ediff:

```
~/.spacemacs  ←──ediff──→  spacemacs/.spacemacs
```

## Platform Detection

The configuration uses these variables for platform-specific behavior:

- `my/crostini-p` - Non-nil if running on Chrome OS Crostini
- `my/termux-p` - Non-nil if running in Termux on Android
- `my/macos-p` - Non-nil if running on macOS

## Local Validation

Run `make` to byte-compile and check for warnings:

```bash
make check
```

Resolve any byte-compile warnings before committing changes.
