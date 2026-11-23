package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdTriage(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore triage list|approve|reject ...")
		return exitUsage
	}
	switch args[0] {
	case "list":
		return a.cmdTriageList(args[1:])
	case "approve":
		return a.cmdTriageApprove(args[1:])
	case "reject":
		return a.cmdTriageReject(args[1:])
	default:
		a.errorf("unknown triage subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdTriageList(args []string) int {
	fs := a.flags("triage list", "memstore triage list [--status pending] [--vault V] [--json]")
	var status, vaultName string
	var opts commonOpts
	fs.StringVar(&status, "status", memstore.TriageStatusPending, "filter by status (pending, approved, rejected, or empty for all)")
	fs.StringVar(&vaultName, "vault", "", "vault filter")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if status == "all" {
		status = ""
	}

	svc, _, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	items, err := svc.ListTriage(context.Background(), memstore.TriageFilter{
		Vault:  vaultName,
		Status: status,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(items)
	}
	for _, it := range items {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\t[%s/%s]\t%s\n",
			it.ID, it.Vault, it.Status, it.Source, it.ProposedCompartment, it.ProposedSensitivity, it.TargetEntityID)
	}
	return exitOK
}

func (a *app) cmdTriageApprove(args []string) int {
	fs := a.flags("triage approve", "memstore triage approve <id> [--vault V] [--compartment C] [--sensitivity S] [--json]")
	var vaultName, compartment, sensitivity string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name (optional if unique)")
	fs.StringVar(&compartment, "compartment", "", "override compartment")
	fs.StringVar(&sensitivity, "sensitivity", "", "override sensitivity")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	ctx := context.Background()
	id := pos[0]
	if vaultName == "" {
		resolved, err := findTriageVault(ctx, svc, id)
		if err != nil {
			a.errorf("%v", err)
			return mapServiceErr(err)
		}
		vaultName = resolved
	}

	if err := svc.ApproveTriage(ctx, vaultName, id, compartment, memstore.Sensitivity(sensitivity), actor); err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]string{"id": id, "vault": vaultName, "status": memstore.TriageStatusApproved})
	}
	fmt.Fprintf(a.stdout, "approved %s (%s)\n", id, vaultName)
	return exitOK
}

func (a *app) cmdTriageReject(args []string) int {
	fs := a.flags("triage reject", "memstore triage reject <id> [--vault V] [--json]")
	var vaultName string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name (optional if unique)")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	ctx := context.Background()
	id := pos[0]
	if vaultName == "" {
		resolved, err := findTriageVault(ctx, svc, id)
		if err != nil {
			a.errorf("%v", err)
			return mapServiceErr(err)
		}
		vaultName = resolved
	}

	if err := svc.RejectTriage(ctx, vaultName, id, actor); err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]string{"id": id, "vault": vaultName, "status": memstore.TriageStatusRejected})
	}
	fmt.Fprintf(a.stdout, "rejected %s (%s)\n", id, vaultName)
	return exitOK
}

func findTriageVault(ctx context.Context, svc *memstore.Service, id string) (string, error) {
	items, err := svc.ListTriage(ctx, memstore.TriageFilter{})
	if err != nil {
		return "", fmt.Errorf("list triage for %q: %w", id, err)
	}
	for _, it := range items {
		if it.ID == id {
			return it.Vault, nil
		}
	}
	return "", fmt.Errorf("%w: triage item %q", memstore.ErrNotFound, id)
}
