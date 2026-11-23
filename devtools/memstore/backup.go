package memstore

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// BackupOptions controls BackupVault.
type BackupOptions struct {
	// EncryptCmd is an optional command template run to encrypt the snapshot.
	// It is split on whitespace (no shell is involved), and the tokens {{in}}
	// and {{out}} are replaced by the plaintext snapshot path and the final
	// output path. Both tokens are required, for example
	// "age -r RECIPIENT -o {{out}} {{in}}".
	EncryptCmd string
	// RequireEncrypted refuses to write an unencrypted snapshot.
	RequireEncrypted bool
}

// BackupReport describes a finished backup.
type BackupReport struct {
	Path      string `json:"path"`
	Bytes     int64  `json:"bytes"`
	Encrypted bool   `json:"encrypted"`
}

// BackupVault writes a consistent snapshot of vault to outPath using SQLite
// VACUUM INTO, which is safe under WAL. The output file is created with mode
// 0600 and an existing outPath is never overwritten. The plaintext snapshot is
// staged in a private temporary directory beside outPath and removed before
// BackupVault returns, whether or not encryption succeeds.
func BackupVault(ctx context.Context, vault VaultConfig, outPath string, opts BackupOptions) (BackupReport, error) {
	var rep BackupReport
	if opts.EncryptCmd == "" && opts.RequireEncrypted {
		return rep, errors.New("backup: unencrypted snapshots are not allowed; set an encrypt command")
	}
	var argv []string
	if opts.EncryptCmd != "" {
		var err error
		if argv, err = parseEncryptCmd(opts.EncryptCmd); err != nil {
			return rep, err
		}
	}
	if err := RejectGitWorkTree(outPath); err != nil {
		return rep, err
	}
	if _, err := os.Lstat(outPath); err == nil {
		return rep, fmt.Errorf("backup: %s already exists", outPath)
	}
	if err := EnsureSecureDir(filepath.Dir(outPath)); err != nil {
		return rep, err
	}
	stage, err := os.MkdirTemp(filepath.Dir(outPath), ".memstore-backup-*")
	if err != nil {
		return rep, fmt.Errorf("backup: stage dir: %w", err)
	}
	defer os.RemoveAll(stage)
	plain := filepath.Join(stage, "snapshot.db")
	if err := vacuumInto(ctx, vault, plain); err != nil {
		return rep, err
	}
	if argv == nil {
		err = os.Rename(plain, outPath)
	} else {
		err = runEncrypt(ctx, argv, plain, outPath)
	}
	if err != nil {
		os.Remove(outPath)
		return rep, err
	}
	if err := os.Chmod(outPath, 0o600); err != nil {
		os.Remove(outPath)
		return rep, fmt.Errorf("backup: chmod: %w", err)
	}
	fi, err := os.Stat(outPath)
	if err != nil {
		return rep, fmt.Errorf("backup: stat: %w", err)
	}
	return BackupReport{Path: outPath, Bytes: fi.Size(), Encrypted: argv != nil}, nil
}

func parseEncryptCmd(cmd string) ([]string, error) {
	argv := strings.Fields(cmd)
	if !strings.Contains(cmd, "{{in}}") || !strings.Contains(cmd, "{{out}}") {
		return nil, errors.New("backup: encrypt command must contain {{in}} and {{out}}")
	}
	return argv, nil
}

func vacuumInto(ctx context.Context, vault VaultConfig, dest string) error {
	v, err := OpenVault(vault)
	if err != nil {
		return err
	}
	defer v.Close()
	if _, err := v.db.ExecContext(ctx, `VACUUM INTO ?`, dest); err != nil {
		return fmt.Errorf("backup: vacuum into: %w", err)
	}
	if err := os.Chmod(dest, 0o600); err != nil {
		return fmt.Errorf("backup: chmod snapshot: %w", err)
	}
	return nil
}

func runEncrypt(ctx context.Context, argv []string, in, out string) error {
	args := make([]string, len(argv))
	for i, a := range argv {
		args[i] = strings.NewReplacer("{{in}}", in, "{{out}}", out).Replace(a)
	}
	// The command is the operator's own --encrypt-cmd, run without a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, args[0], args[1:]...)
	var stderr strings.Builder
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("backup: encrypt command failed: %w: %s", err, strings.TrimSpace(stderr.String()))
	}
	if _, err := os.Stat(out); err != nil {
		return fmt.Errorf("backup: encrypt command did not create %s: %w", out, err)
	}
	return nil
}
