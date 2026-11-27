package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func isAskSubcommand(s string) bool {
	switch s {
	case "ls", "show", "done", "drop", "add":
		return true
	default:
		return false
	}
}

func cmdAskSubcommand(sub string, args []string, stdout, stderr io.Writer) int {
	switch sub {
	case "ls":
		return cmdAskList(args, stdout, stderr)
	case "show":
		return cmdAskShow(args, stdout, stderr)
	case "done":
		return cmdAskClose(decide.AskStatusDone, args, stdout, stderr)
	case "drop":
		return cmdAskClose(decide.AskStatusDropped, args, stdout, stderr)
	case "add":
		return cmdAskAdd(args, stdout, stderr)
	default:
		return fail(stderr, fmt.Errorf("decide ask: unknown subcommand %q", sub), 2)
	}
}

func cmdAskList(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("ask ls", flag.ContinueOnError)
	fs.SetOutput(stderr)
	openOnly := fs.Bool("open", false, "list only open asks")
	jsonOut := fs.Bool("json", false, "print asks as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide ask ls takes no positional arguments"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	asks, err := led.ListAsks()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *openOnly {
		filtered := make([]decide.AskRecord, 0, len(asks))
		for _, a := range asks {
			if a.Status == decide.AskStatusOpen {
				filtered = append(filtered, a)
			}
		}
		asks = filtered
	}
	if asks == nil {
		asks = []decide.AskRecord{}
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, asks)
	}
	for i := range asks {
		a := &asks[i]
		line := fmt.Sprintf("%s\t%s\t%s\t%s\t%s: %s",
			a.ID, a.Status, a.ItemID, a.CreatedAt.Local().Format("2006-01-02 15:04"), a.Person, a.What)
		if a.Owner != "" {
			line += "\towner:" + a.Owner
		}
		if len(a.Links) > 0 {
			line += "\tlinks:" + strings.Join(a.Links, ",")
		}
		fmt.Fprintln(stdout, line)
	}
	return 0
}

func cmdAskShow(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, nil)
	fs := flag.NewFlagSet("ask show", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print ask as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, err := oneArg(fs.Args(), "ask id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	ask, err := led.GetAsk(id)
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, ask)
	}
	writeAskRecord(stdout, &ask)
	return 0
}

func writeAskRecord(w io.Writer, a *decide.AskRecord) {
	fmt.Fprintf(w, "id: %s\n", a.ID)
	fmt.Fprintf(w, "status: %s\n", a.Status)
	fmt.Fprintf(w, "item: %s\n", a.ItemID)
	writeField(w, "event", a.EventID)
	writeField(w, "choice", a.ChoiceKey)
	fmt.Fprintf(w, "person: %s\n", a.Person)
	fmt.Fprintf(w, "what: %s\n", a.What)
	writeField(w, "owner", a.Owner)
	writeList(w, "link", a.Links)
	writeTime(w, "createdAt", a.CreatedAt)
	if a.ClosedAt != nil {
		writeTime(w, "closedAt", *a.ClosedAt)
	}
	writeField(w, "closedBy", a.ClosedBy)
	writeField(w, "reason", a.CloseReason)
}

func cmdAskClose(status string, args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{"reason": true, "by": true, "via": true})
	fs := flag.NewFlagSet("ask "+status, flag.ContinueOnError)
	fs.SetOutput(stderr)
	reason := fs.String("reason", "", "reason for closing the ask")
	by := fs.String("by", "", "actor closing the ask (owning agent or human)")
	humanFlag := fs.Bool("human", false, "close as human")
	via := fs.String("via", decide.ViaCLI, "where the close came from: cli or ui")
	jsonOut := fs.Bool("json", false, "print updated ask as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	rest := fs.Args()
	if len(rest) == 0 || strings.TrimSpace(rest[0]) == "" {
		return fail(stderr, fmt.Errorf("usage: decide ask %s --reason TEXT <ask-id>", status), 2)
	}
	askID := rest[0]
	reasonText := strings.TrimSpace(*reason)
	if reasonText == "" && len(rest) > 1 {
		reasonText = strings.TrimSpace(strings.Join(rest[1:], " "))
	} else if len(rest) > 1 {
		return fail(stderr, fmt.Errorf("decide ask %s received extra positional arguments", status), 2)
	}
	if reasonText == "" {
		return fail(stderr, fmt.Errorf("decide ask %s requires --reason TEXT", status), 2)
	}
	actor := strings.TrimSpace(*by)
	switch {
	case *humanFlag || *via == decide.ViaUI:
		actor = decide.Human
	case actor == "":
		actor = senderOf(context.Background())
		if actor == "" {
			actor = decide.Human
		}
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	closed, err := led.CloseAsk(askID, status, reasonText, actor, *via)
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, closed)
	}
	fmt.Fprintf(stdout, "%s %s (%s)\n", closed.ID, closed.Status, closed.ItemID)
	return 0
}

func cmdAskAdd(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{"event": true, "link": true, "via": true})
	fs := flag.NewFlagSet("ask add", flag.ContinueOnError)
	fs.SetOutput(stderr)
	eventID := fs.String("event", "", "choice event id to link (defaults to latest choice event on item)")
	var links stringList
	fs.Var(&links, "link", "optional chat thread, PR, or tracker key link (repeatable)")
	via := fs.String("via", decide.ViaCLI, "where the add came from: cli or ui")
	jsonOut := fs.Bool("json", false, "print new ask as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	itemID, spec, err := twoArgs(fs.Args(), "item id", `"<person>: <what>"`)
	if err != nil {
		return fail(stderr, err, 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	rec, err := led.AddAsk(itemID, spec, []string(links), *eventID, *via)
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, rec)
	}
	fmt.Fprintln(stdout, rec.ID)
	return 0
}

func attachOptionAskSpecs(opts []decide.Option, idxByKey map[string]int, askSpecs []string) (askCount, singleAskIdx int, err error) {
	singleAskIdx = -1
	for _, raw := range askSpecs {
		key, spec, ok := strings.Cut(raw, "=")
		key = strings.TrimSpace(key)
		spec = strings.TrimSpace(spec)
		if !ok || key == "" || spec == "" {
			return 0, -1, fmt.Errorf("decide --ask %q must be key=\"<person>: <what>\"", raw)
		}
		idx, exists := idxByKey[key]
		if !exists {
			return 0, -1, fmt.Errorf("decide --ask references unknown option key %q", key)
		}
		if opts[idx].Ask != "" {
			return 0, -1, fmt.Errorf("decide --ask option %q specified more than once", key)
		}
		if _, _, err := decide.ParseAskSpec(spec); err != nil {
			return 0, -1, err //nolint:wrapcheck // ParseAskSpec names the spec
		}
		opts[idx].Ask = spec
		askCount++
		singleAskIdx = idx
	}
	return askCount, singleAskIdx, nil
}

func attachOptionAskLink(opts []decide.Option, idxByKey map[string]int, raw string, askCount, singleAskIdx int) error {
	trimmed := strings.TrimSpace(raw)
	if trimmed == "" {
		return fmt.Errorf("decide --ask-link is empty")
	}
	k, v, hasEq := strings.Cut(trimmed, "=")
	kTrim := strings.TrimSpace(k)
	vTrim := strings.TrimSpace(v)
	switch {
	case hasEq && idxByKeyExists(idxByKey, kTrim):
		idx := idxByKey[kTrim]
		if opts[idx].Ask == "" {
			return fmt.Errorf("decide --ask-link for option %q requires --ask %s=\"<person>: <what>\"", kTrim, kTrim)
		}
		if vTrim == "" {
			return fmt.Errorf("decide --ask-link for option %q is empty", kTrim)
		}
		opts[idx].AskLinks = append(opts[idx].AskLinks, vTrim)
		return nil
	case hasEq && !strings.ContainsAny(kTrim, ":/?"):
		return fmt.Errorf("decide --ask-link references unknown option key %q", kTrim)
	case askCount == 1:
		opts[singleAskIdx].AskLinks = append(opts[singleAskIdx].AskLinks, trimmed)
		return nil
	default:
		return fmt.Errorf("decide --ask-link %q must specify <option-key>=<link>", raw)
	}
}

// applyOptionAsks attaches --ask key="<person>: <what>" and --ask-link [key=]link
// annotations onto opts.
func applyOptionAsks(opts []decide.Option, askSpecs, askLinks []string) ([]decide.Option, error) {
	if len(askSpecs) == 0 && len(askLinks) == 0 {
		return opts, nil
	}
	if len(opts) == 0 {
		return nil, fmt.Errorf("decide --ask requires options")
	}
	idxByKey := make(map[string]int, len(opts))
	for i := range opts {
		idxByKey[opts[i].Key] = i
	}
	askCount, singleAskIdx, err := attachOptionAskSpecs(opts, idxByKey, askSpecs)
	if err != nil {
		return nil, err
	}
	for _, raw := range askLinks {
		if err := attachOptionAskLink(opts, idxByKey, raw, askCount, singleAskIdx); err != nil {
			return nil, err
		}
	}
	_ = time.Second
	return opts, nil
}

func idxByKeyExists(m map[string]int, k string) bool {
	_, ok := m[k]
	return ok
}
