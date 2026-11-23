package main

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

// adminVault resolves vaultName for a maintenance command. Both age and
// backup touch every compartment, so the profile must hold the admin
// capability on that vault.
func (a *app) adminVault(opts commonOpts, vaultName string) (memstore.Config, memstore.VaultConfig, int) {
	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return cfg, memstore.VaultConfig{}, exitError
	}
	profile, _ := a.resolveActor(opts)
	policy, err := memstore.ResolveProfile(cfg, profile)
	if err != nil {
		a.errorf("%v", err)
		return cfg, memstore.VaultConfig{}, exitUsage
	}
	vc, known := cfg.Vaults[vaultName]
	if !known || !slices.Contains(policy.AllowedVaults(), vaultName) || !policy.HasCapability(memstore.CapabilityAdmin) {
		a.errorf("profile %q lacks admin capability on vault %q", profile, vaultName)
		return cfg, memstore.VaultConfig{}, exitUsage
	}
	return cfg, vc, exitOK
}

// dispatchMaintenance runs the maintenance and audit commands.
func (a *app) dispatchMaintenance(name string, args []string) (int, bool) {
	switch name {
	case "age":
		return a.cmdAge(args), true
	case "backup":
		return a.cmdBackup(args), true
	case "log":
		return a.cmdLog(args), true
	default:
		return 0, false
	}
}

func (a *app) cmdAge(args []string) int {
	fs := a.flags("age", "memstore age --vault V [--retention-days N] [--dry-run] [--drop] [--json]")
	var vaultName string
	var ao memstore.AgeOptions
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault to age")
	fs.IntVar(&ao.RetentionDays, "retention-days", 0, "retention in days (default from config)")
	fs.BoolVar(&ao.DryRun, "dry-run", false, "report what would change without changing it")
	fs.BoolVar(&ao.Drop, "drop", false, "delete old events instead of compacting them")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" {
		fs.Usage()
		return exitUsage
	}
	cfg, vc, code := a.adminVault(opts, vaultName)
	if code != exitOK {
		return code
	}
	if ao.RetentionDays == 0 {
		ao.RetentionDays = cfg.RetentionDays
	}
	v, err := memstore.OpenVault(vc)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	defer v.Close()
	v.SetClock(a.now)

	res, err := v.Age(context.Background(), ao)
	if errors.Is(err, memstore.ErrAgeLocked) {
		if opts.jsonOut {
			return a.writeJSON(map[string]any{"locked": true})
		}
		fmt.Fprintln(a.stdout, "locked: another age run is in progress")
		return exitOK
	}
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(res)
	}
	fmt.Fprintf(a.stdout, "expired-facts=%d compacted-events=%d summaries=%d dropped-events=%d deleted-revisions=%d deleted-access-rows=%d dry-run=%t\n",
		res.ExpiredFacts, res.CompactedEvents, res.Summaries, res.DroppedEvents, res.DeletedRevisions, res.DeletedAccessRows, ao.DryRun)
	return exitOK
}

func (a *app) cmdBackup(args []string) int {
	fs := a.flags("backup", "memstore backup --vault V --out <path> [--encrypt-cmd CMD] [--require-encrypted] [--json]")
	var vaultName, out string
	var bo memstore.BackupOptions
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault to back up")
	fs.StringVar(&out, "out", "", "output snapshot path")
	fs.StringVar(&bo.EncryptCmd, "encrypt-cmd", "", "encrypt command template using {{in}} and {{out}}, e.g. \"age -r RECIPIENT -o {{out}} {{in}}\"")
	fs.BoolVar(&bo.RequireEncrypted, "require-encrypted", false, "refuse to write an unencrypted snapshot")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" || out == "" {
		fs.Usage()
		return exitUsage
	}
	_, vc, code := a.adminVault(opts, vaultName)
	if code != exitOK {
		return code
	}
	rep, err := memstore.BackupVault(context.Background(), vc, out, bo)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(rep)
	}
	fmt.Fprintf(a.stdout, "wrote %s (%d bytes, encrypted=%t)\n", rep.Path, rep.Bytes, rep.Encrypted)
	return exitOK
}
