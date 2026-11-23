package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdExport(args []string) int {
	fs := a.flags("export", "memstore export --profile P --out <dir> [--allow-git] [--by C] [--reason R] [--json]")
	var outDir string
	var allowGit bool
	var opts commonOpts
	fs.StringVar(&outDir, "out", "", "output directory for projected Markdown files")
	fs.BoolVar(&allowGit, "allow-git", false, "allow exporting into a directory inside a git work tree")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if outDir == "" {
		fs.Usage()
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	report, err := memstore.Export(context.Background(), svc, outDir, memstore.ExportOptions{
		AllowGit: allowGit,
		Actor:    actor,
		Getenv:   a.getenv,
		Now:      a.now,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(report)
	}
	fmt.Fprintf(a.stdout, "written=%d unchanged=%d removed=%d\n", report.Written, report.Unchanged, report.Removed)
	return exitOK
}
