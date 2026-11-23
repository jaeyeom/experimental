package memstore

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

func backupFixture(t *testing.T) (VaultConfig, string) {
	t.Helper()
	v := newAgeVault(t)
	seedEntity(t, v.db)
	return v.config, filepath.Join(t.TempDir(), "snap", "out.db")
}

func assertSnapshotReadable(t *testing.T, path string) {
	t.Helper()
	db, err := sqlitedb.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer db.Close()
	if n := count(t, db, `SELECT COUNT(*) FROM entities`); n != 1 {
		t.Errorf("snapshot entities = %d, want 1", n)
	}
}

func assertOnlyOut(t *testing.T, out string) {
	t.Helper()
	entries, err := os.ReadDir(filepath.Dir(out))
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		if e.Name() != filepath.Base(out) {
			t.Errorf("stray file %q left next to backup", e.Name())
		}
	}
}

func TestBackupVaultPlain(t *testing.T) {
	vc, out := backupFixture(t)
	rep, err := BackupVault(context.Background(), vc, out, BackupOptions{})
	if err != nil {
		t.Fatal(err)
	}
	if rep.Encrypted || rep.Path != out || rep.Bytes <= 0 {
		t.Errorf("report = %+v", rep)
	}
	fi, err := os.Stat(out)
	if err != nil {
		t.Fatal(err)
	}
	if fi.Mode().Perm() != 0o600 {
		t.Errorf("mode = %v, want 0600", fi.Mode().Perm())
	}
	assertSnapshotReadable(t, out)
	assertOnlyOut(t, out)
	if _, err := BackupVault(context.Background(), vc, out, BackupOptions{}); err == nil {
		t.Error("overwriting an existing backup must fail")
	}
}

func TestBackupVaultEncryptHook(t *testing.T) {
	vc, out := backupFixture(t)
	rep, err := BackupVault(context.Background(), vc, out, BackupOptions{
		EncryptCmd: "cp {{in}} {{out}}", RequireEncrypted: true,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !rep.Encrypted {
		t.Error("report not encrypted")
	}
	fi, err := os.Stat(out)
	if err != nil || fi.Mode().Perm() != 0o600 {
		t.Fatalf("out stat = %v, %v", fi, err)
	}
	assertSnapshotReadable(t, out) // cp stands in for the cipher
	assertOnlyOut(t, out)          // plaintext temp is gone
}

func TestBackupVaultEncryptHookFailureCleansUp(t *testing.T) {
	vc, out := backupFixture(t)
	_, err := BackupVault(context.Background(), vc, out, BackupOptions{EncryptCmd: "false {{in}} {{out}}"})
	if err == nil {
		t.Fatal("want error")
	}
	if _, serr := os.Stat(out); serr == nil {
		t.Error("partial output left behind")
	}
	assertOnlyOut(t, out)
}

func TestBackupVaultRequireEncrypted(t *testing.T) {
	vc, out := backupFixture(t)
	if _, err := BackupVault(context.Background(), vc, out, BackupOptions{RequireEncrypted: true}); err == nil {
		t.Fatal("want error without encrypt cmd")
	}
	if _, err := os.Stat(out); err == nil {
		t.Error("unencrypted snapshot written despite policy")
	}
}

func TestBackupVaultEncryptCmdNeedsPlaceholders(t *testing.T) {
	vc, out := backupFixture(t)
	for _, c := range []string{"cp {{in}} x", "cp x {{out}}", ""} {
		opts := BackupOptions{EncryptCmd: c, RequireEncrypted: c == ""}
		if _, err := BackupVault(context.Background(), vc, out, opts); err == nil || (c != "" && !strings.Contains(err.Error(), "{{")) {
			t.Errorf("cmd %q: err = %v", c, err)
		}
	}
}
