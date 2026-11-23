package main

import (
	"context"
	"fmt"
	"os"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdNote(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore note set|list ...")
		return exitUsage
	}
	switch args[0] {
	case "set":
		return a.cmdNoteSet(args[1:])
	case "list":
		return a.cmdNoteList(args[1:])
	default:
		a.errorf("unknown note subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdNoteSet(args []string) int {
	fs := a.flags("note set", "memstore note set <entity> <section-key> --vault V --heading H --body B|--file PATH [--order N] [--compartment C] [--sensitivity S] [--json]")
	var vaultName, heading, body, filePath, compartment, sensitivity string
	var order int
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "target vault")
	fs.StringVar(&heading, "heading", "", "section heading")
	fs.StringVar(&body, "body", "", "markdown body")
	fs.StringVar(&filePath, "file", "", "read markdown body from file")
	fs.IntVar(&order, "order", 100, "sort order")
	fs.StringVar(&compartment, "compartment", "", "compartment")
	fs.StringVar(&sensitivity, "sensitivity", "", "sensitivity level")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 2, 2)
	if !ok {
		return exitUsage
	}
	if vaultName == "" || heading == "" {
		fs.Usage()
		return exitUsage
	}
	if (body == "" && filePath == "") || (body != "" && filePath != "") {
		a.errorf("exactly one of --body or --file is required")
		return exitUsage
	}
	if filePath != "" {
		data, err := os.ReadFile(filePath)
		if err != nil {
			a.errorf("read note file: %v", err)
			return exitError
		}
		body = string(data)
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	nb, err := svc.SetNoteBlock(context.Background(), vaultName, memstore.NoteBlockInput{
		EntityID:    pos[0],
		SectionKey:  pos[1],
		Heading:     heading,
		SortOrder:   order,
		BodyMD:      body,
		Compartment: compartment,
		Sensitivity: memstore.Sensitivity(sensitivity),
	}, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(nb)
	}
	fmt.Fprintf(a.stdout, "%s\t%s\t%s\n", nb.ID, nb.EntityID, nb.SectionKey)
	return exitOK
}

func (a *app) cmdNoteList(args []string) int {
	fs := a.flags("note list", "memstore note list <entity> [--vault V] [--json]")
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

	blocks, err := svc.ListNoteBlocks(ctx, vaultName, entityID, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(blocks)
	}
	for _, b := range blocks {
		fmt.Fprintf(a.stdout, "## %s (%s) [%s/%s]\n%s\n", b.Heading, b.SectionKey, b.Compartment, b.Sensitivity, b.BodyMD)
	}
	return exitOK
}
