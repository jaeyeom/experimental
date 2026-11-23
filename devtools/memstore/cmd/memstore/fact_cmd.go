package main

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdFact(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore fact set|list ...")
		return exitUsage
	}
	switch args[0] {
	case "set":
		return a.cmdFactSet(args[1:])
	case "list":
		return a.cmdFactList(args[1:])
	default:
		a.errorf("unknown fact subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdFactSet(args []string) int {
	fs := a.flags("fact set", "memstore fact set <entity> <key> <value> --vault V [--type T] [--compartment C] [--sensitivity S] [--valid-until YYYY-MM-DD] [--source-ref R] [--json]")
	var vaultName, valType, compartment, sensitivity, validUntilStr, sourceRef, sourceType string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "target vault")
	fs.StringVar(&valType, "type", "string", "value type")
	fs.StringVar(&compartment, "compartment", "", "compartment")
	fs.StringVar(&sensitivity, "sensitivity", "", "sensitivity level")
	fs.StringVar(&validUntilStr, "valid-until", "", "expiration date (YYYY-MM-DD or RFC3339)")
	fs.StringVar(&sourceRef, "source-ref", "", "source reference")
	fs.StringVar(&sourceType, "source-type", "manual", "source type")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 3, 3)
	if !ok {
		return exitUsage
	}
	if vaultName == "" {
		fs.Usage()
		return exitUsage
	}

	validUntil, err := parseDateOrTime(validUntilStr)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}

	valJSON := pos[2]
	if !json.Valid([]byte(valJSON)) {
		b, err := json.Marshal(valJSON)
		if err != nil {
			a.errorf("encode value: %v", err)
			return exitError
		}
		valJSON = string(b)
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	in := memstore.FactInput{
		EntityID:    pos[0],
		Key:         pos[1],
		ValueJSON:   valJSON,
		ValueType:   valType,
		Compartment: compartment,
		Sensitivity: memstore.Sensitivity(sensitivity),
		SourceType:  sourceType,
		SourceRef:   sourceRef,
	}
	if !validUntil.IsZero() {
		in.ValidUntil = &validUntil
	}

	fact, err := svc.SetFact(context.Background(), vaultName, in, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(fact)
	}
	fmt.Fprintf(a.stdout, "%s\t%s\t%s=%s\n", fact.ID, fact.EntityID, fact.Key, fact.ValueJSON)
	return exitOK
}

func (a *app) cmdFactList(args []string) int {
	fs := a.flags("fact list", "memstore fact list <entity> [--vault V] [--key K] [--include-expired] [--include-superseded] [--json]")
	var vaultName, key string
	var includeExpired, includeSuperseded bool
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault filter")
	fs.StringVar(&key, "key", "", "fact key filter")
	fs.BoolVar(&includeExpired, "include-expired", false, "include expired facts")
	fs.BoolVar(&includeSuperseded, "include-superseded", false, "include superseded facts")
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

	facts, err := svc.ListFacts(context.Background(), memstore.FactFilter{
		Vault:             vaultName,
		EntityID:          pos[0],
		Key:               key,
		IncludeExpired:    includeExpired,
		IncludeSuperseded: includeSuperseded,
	}, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(facts)
	}
	for _, f := range facts {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s=%s\t[%s/%s]\n", f.Vault, f.EntityID, f.Key, f.ValueJSON, f.Compartment, f.Sensitivity)
	}
	return exitOK
}
