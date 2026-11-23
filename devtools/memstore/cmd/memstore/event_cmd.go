package main

import (
	"context"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func (a *app) cmdEvent(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore event add|list ...")
		return exitUsage
	}
	switch args[0] {
	case "add":
		return a.cmdEventAdd(args[1:])
	case "list":
		return a.cmdEventList(args[1:])
	default:
		a.errorf("unknown event subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdEventAdd(args []string) int {
	fs := a.flags("event add", "memstore event add --vault V --channel CH --summary S [--entity E] [--date YYYY-MM-DD] [--source-ref R] [--compartment C] [--sensitivity S] [--details D] [--json]")
	var vaultName, channel, summary, entityID, dateStr, sourceRef, compartment, sensitivity, details string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "target vault")
	fs.StringVar(&channel, "channel", "", "source channel")
	fs.StringVar(&summary, "summary", "", "event summary")
	fs.StringVar(&entityID, "entity", "", "associated entity ID")
	fs.StringVar(&dateStr, "date", "", "event date/time (YYYY-MM-DD or RFC3339)")
	fs.StringVar(&sourceRef, "source-ref", "", "source reference")
	fs.StringVar(&compartment, "compartment", "", "compartment")
	fs.StringVar(&sensitivity, "sensitivity", "", "sensitivity level")
	fs.StringVar(&details, "details", "", "markdown details")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" || channel == "" || summary == "" {
		fs.Usage()
		return exitUsage
	}
	occurredAt, err := parseDateOrTime(dateStr)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	ev, err := svc.AddEvent(context.Background(), vaultName, memstore.EventInput{
		OccurredAt:  occurredAt,
		EntityID:    entityID,
		Compartment: compartment,
		Sensitivity: memstore.Sensitivity(sensitivity),
		Channel:     channel,
		SourceRef:   sourceRef,
		Summary:     summary,
		DetailsMD:   details,
		Reporter:    actor.Caller,
	}, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(ev)
	}
	fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\n", ev.ID, ev.EventDate, ev.Channel, ev.Summary)
	return exitOK
}

func (a *app) cmdEventList(args []string) int {
	fs := a.flags("event list", "memstore event list [--entity E] [--since D] [--until D] [--limit N] [--vault V] [--channel CH] [--json]")
	var entityID, sinceStr, untilStr, vaultName, channel string
	var limit int
	var opts commonOpts
	fs.StringVar(&entityID, "entity", "", "entity ID filter")
	fs.StringVar(&sinceStr, "since", "", "start date/time (YYYY-MM-DD or RFC3339)")
	fs.StringVar(&untilStr, "until", "", "end date/time (YYYY-MM-DD or RFC3339)")
	fs.IntVar(&limit, "limit", 0, "maximum number of events")
	fs.StringVar(&vaultName, "vault", "", "vault filter")
	fs.StringVar(&channel, "channel", "", "channel filter")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	since, err := parseDateOrTime(sinceStr)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}
	until, err := parseDateOrTime(untilStr)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}

	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	events, err := svc.ListEvents(context.Background(), memstore.EventFilter{
		Vault:    vaultName,
		Since:    since,
		Until:    until,
		EntityID: entityID,
		Channel:  channel,
		Limit:    limit,
	}, actor)
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(events)
	}
	for _, ev := range events {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\t%s\n", ev.Vault, ev.EventDate, ev.Channel, ev.EntityID, ev.Summary)
	}
	return exitOK
}
