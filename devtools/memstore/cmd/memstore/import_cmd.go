package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdImportAside(args []string) int {
	fs := a.flags("import-aside", "memstore import-aside --src <dir> --vault <default-vault> [--rules <file>] [--dry-run] [--json]")
	var srcDir, vaultName, rulesPath string
	var dryRun bool
	var opts commonOpts
	fs.StringVar(&srcDir, "src", "", "source Aside Markdown directory")
	fs.StringVar(&vaultName, "vault", "", "default target vault")
	fs.StringVar(&rulesPath, "rules", "", "optional TOML classification rules file")
	fs.BoolVar(&dryRun, "dry-run", false, "report planned counts without modifying any DB")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if srcDir == "" || vaultName == "" {
		fs.Usage()
		return exitUsage
	}

	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	profile, actor := a.resolveActor(opts)

	report, err := memstore.ImportAside(context.Background(), cfg, srcDir, memstore.ImportOptions{
		Vault:     vaultName,
		RulesPath: rulesPath,
		DryRun:    dryRun,
		Profile:   profile,
		Actor:     actor,
		Now:       a.now,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(report)
	}
	fmt.Fprintf(a.stdout, "files=%d entities=%d facts=%d notes=%d events=%d direct=%d triage=%d dryRun=%v\n",
		report.FilesScanned, report.Entities, report.Facts, report.NoteBlocks, report.EpisodicEvents, report.DirectWrites, report.TriageStaged, report.DryRun)
	return exitOK
}
