package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdEntity(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore entity put|get ...")
		return exitUsage
	}
	switch args[0] {
	case "put":
		return a.cmdEntityPut(args[1:])
	case "get":
		return a.cmdEntityGet(args[1:])
	default:
		a.errorf("unknown entity subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdEntityPut(args []string) int {
	fs := a.flags("entity put", "memstore entity put <kind> <slug> --vault V --title T [--alias A]... [--compartment C] [--sensitivity S] [--contacts-ref ID] [--summary S] [--pin-l1] [--json]")
	var vaultName, title, compartment, sensitivity, contactsRef, summary string
	var aliases stringSliceFlag
	var pinL1 bool
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "target vault")
	fs.StringVar(&title, "title", "", "entity title")
	fs.Var(&aliases, "alias", "entity alias (repeatable)")
	fs.StringVar(&compartment, "compartment", "", "default compartment")
	fs.StringVar(&sensitivity, "sensitivity", "", "sensitivity level")
	fs.StringVar(&contactsRef, "contacts-ref", "", "contacts reference ID")
	fs.StringVar(&summary, "summary", "", "entity summary")
	fs.BoolVar(&pinL1, "pin-l1", false, "pin entity to L1")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 2, 2)
	if !ok {
		return exitUsage
	}
	if vaultName == "" || title == "" {
		fs.Usage()
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	var aliasSlice []string
	if len(aliases) > 0 {
		aliasSlice = aliases
	}
	ent, err := svc.PutEntity(context.Background(), vaultName, memstore.EntityInput{
		Kind:               pos[0],
		Slug:               pos[1],
		Title:              title,
		ContactsRef:        contactsRef,
		DefaultCompartment: compartment,
		Sensitivity:        memstore.Sensitivity(sensitivity),
		Summary:            summary,
		PinnedL1:           pinL1,
		Aliases:            aliasSlice,
	}, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(ent)
	}
	fmt.Fprintf(a.stdout, "%s\t%s\n", ent.ID, ent.Title)
	return exitOK
}

func (a *app) cmdEntityGet(args []string) int {
	fs := a.flags("entity get", "memstore entity get <ref> [--vault V] [--json]")
	var vaultName string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name (optional)")
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
	entityID := pos[0]
	if vaultName == "" {
		matches, err := svc.ResolveEntity(ctx, pos[0], actor)
		if err != nil {
			a.errorf("%v", err)
			return mapServiceErr(err)
		}
		vaultName = matches[0].Vault
		entityID = matches[0].Entity.ID
	}

	view, err := svc.GetEntity(ctx, vaultName, entityID, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(view)
	}
	fmt.Fprintf(a.stdout, "%s (%s): %s [%s/%s]\n",
		view.Entity.ID, view.Vault, view.Entity.Title, view.Entity.DefaultCompartment, view.Entity.Sensitivity)
	if len(view.Aliases) > 0 {
		fmt.Fprintf(a.stdout, "Aliases: %s\n", strings.Join(view.Aliases, ", "))
	}
	if view.Entity.Summary != "" {
		fmt.Fprintf(a.stdout, "Summary: %s\n", view.Entity.Summary)
	}
	return exitOK
}
