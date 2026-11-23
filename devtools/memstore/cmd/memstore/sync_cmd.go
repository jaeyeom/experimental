package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdSync(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore sync pull|push ...")
		return exitUsage
	}
	switch args[0] {
	case "pull":
		return a.cmdSyncPull(args[1:])
	case "push":
		return a.cmdSyncPush(args[1:])
	default:
		a.errorf("unknown sync subcommand %q (want pull or push)", args[0])
		return exitUsage
	}
}

func (a *app) cmdSyncPull(args []string) int {
	fs := a.flags("sync pull", "memstore sync pull --inbox-dir <dir>|--inbox-url <url> --vault V --profile P [--limit N] [--dry-run] [--by C] [--json]")
	var inboxDir, inboxURL, vaultName string
	var limit int
	var dryRun bool
	var opts commonOpts
	fs.StringVar(&inboxDir, "inbox-dir", "", "local or GCS-mounted inbox directory")
	fs.StringVar(&inboxURL, "inbox-url", "", "remote memstore HTTP server base URL")
	fs.StringVar(&vaultName, "vault", "", "target local vault name")
	fs.IntVar(&limit, "limit", 0, "maximum number of envelopes to pull")
	fs.BoolVar(&dryRun, "dry-run", false, "validate without writing or acknowledging")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" || (inboxDir == "") == (inboxURL == "") {
		fs.Usage()
		return exitUsage
	}

	transport := a.buildInboxTransport(inboxDir, inboxURL)
	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	report, err := memstore.PullInbox(context.Background(), svc, vaultName, transport, memstore.PullOptions{
		Limit:  limit,
		DryRun: dryRun,
		Actor:  actor,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(report)
	}
	fmt.Fprintf(a.stdout, "listed=%d ingested=%d skipped=%d refused=%d acked=%d\n",
		report.Listed, report.Ingested, report.Skipped, report.Refused, report.Acked)
	return exitOK
}

func (a *app) cmdSyncPush(args []string) int {
	fs := a.flags("sync push", "memstore sync push --inbox-dir <dir>|--inbox-url <url> --summary S [--compartment C] [--sensitivity S] [--source S] [--entity E] [--channel C] [--source-ref R] [--details D] [--id ID] [--json]")
	var inboxDir, inboxURL string
	var env memstore.InboxEnvelope
	var sensStr string
	var opts commonOpts
	fs.StringVar(&inboxDir, "inbox-dir", "", "local or GCS-mounted inbox directory")
	fs.StringVar(&inboxURL, "inbox-url", "", "remote memstore HTTP server base URL")
	fs.StringVar(&env.ID, "id", "", "envelope ID (generated if omitted)")
	fs.StringVar(&env.Source, "source", memstore.DefaultInboxSource, "envelope source")
	fs.StringVar(&env.EntityRef, "entity", "", "target entity ref")
	fs.StringVar(&env.Compartment, "compartment", "general", "compartment")
	fs.StringVar(&sensStr, "sensitivity", string(memstore.SensitivityNormal), "sensitivity")
	fs.StringVar(&env.Channel, "channel", "", "channel")
	fs.StringVar(&env.SourceRef, "source-ref", "", "source ref")
	fs.StringVar(&env.Summary, "summary", "", "event summary")
	fs.StringVar(&env.DetailsMD, "details", "", "event details markdown")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if env.Summary == "" || (inboxDir == "") == (inboxURL == "") {
		fs.Usage()
		return exitUsage
	}
	env.Sensitivity = memstore.Sensitivity(sensStr)

	transport := a.buildInboxTransport(inboxDir, inboxURL)
	if err := transport.Push(context.Background(), env); err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]any{"pushed": true, "id": env.ID})
	}
	fmt.Fprintf(a.stdout, "pushed %s\n", env.ID)
	return exitOK
}

func (a *app) buildInboxTransport(inboxDir, inboxURL string) memstore.InboxTransport {
	if inboxDir != "" {
		return memstore.NewDirInboxTransport(inboxDir)
	}
	return memstore.NewHTTPInboxTransport(inboxURL, a.getenv("MEMSTORE_INBOX_TOKEN"))
}
