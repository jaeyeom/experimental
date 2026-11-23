package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdSyncCheck(args []string) int {
	fs := a.flags("sync-check", "memstore sync-check --profile P --dir <dir> [--stage] [--by C] [--reason R] [--json]")
	var dir string
	var stage bool
	var opts commonOpts
	fs.StringVar(&dir, "dir", "", "exported Markdown directory to check for external edits")
	fs.BoolVar(&stage, "stage", false, "stage detected diff proposals into triage_inbox")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if dir == "" {
		fs.Usage()
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	report, err := memstore.SyncCheck(context.Background(), svc, dir, memstore.SyncCheckOptions{
		Stage: stage,
		Actor: actor,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(report)
	}
	fmt.Fprintf(a.stdout, "unchanged=%d modified=%d added=%d deleted=%d staged=%d skipped=%d\n",
		report.Unchanged, len(report.Modified), len(report.Added), len(report.Deleted), report.Staged, report.Skipped)
	for _, chg := range report.Changes {
		fmt.Fprintf(a.stdout, "  %s\t%s\n", chg.Status, chg.RelPath)
	}
	return exitOK
}
