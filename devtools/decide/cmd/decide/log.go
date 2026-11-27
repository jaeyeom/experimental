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

// cmdLog runs decide log subcommands:
//   - decide log search ... (decision log full-text search)
//   - decide log cause ... (append cause annotation onto a choice/misfit event)
//   - decide log reasons   (list configured flag/cause reason IDs)
//   - decide log [--misfit] [--flags] [--agent A] [--cause R] [--kind K] [--group-by-agent] [--json]
func cmdLog(args []string, stdout, stderr io.Writer) int {
	if len(args) > 0 {
		switch args[0] {
		case "search":
			return cmdLogSearch(args[1:], stdout, stderr)
		case "cause":
			return cmdLogCause(args[1:], stdout, stderr)
		case "reasons":
			return cmdLogReasons(args[1:], stdout, stderr)
		default:
			if !strings.HasPrefix(args[0], "-") {
				return fail(stderr, fmt.Errorf("decide log: unknown subcommand %q (expected search, cause, reasons, or flags)", args[0]), 2)
			}
		}
	}
	return cmdLogEvents(args, stdout, stderr)
}

func cmdLogSearch(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("log search", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print entries as JSON")
	all := fs.Bool("all", false, "include superseded items")
	classes := fs.String("class", "", "comma-separated classes to search (default decision,fact)")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	opts := decide.SearchOptions{Words: fs.Args(), All: *all}
	if *classes != "" {
		opts.Classes = strings.Split(*classes, ",")
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	found, err := decide.SearchLog(items, opts)
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *jsonOut {
		if found == nil {
			found = []decide.Item{}
		}
		return writeJSON(stdout, stderr, found)
	}
	for i := range found {
		writeLogEntry(stdout, &found[i])
	}
	return 0
}

func cmdLogReasons(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("log reasons", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print reasons as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide log reasons takes no arguments"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	reasons := cfg.EffectiveReasons()
	if *jsonOut {
		return writeJSON(stdout, stderr, reasons)
	}
	for _, r := range reasons {
		suffix := ""
		if r.RequireNote {
			suffix = " [note required]"
		}
		fmt.Fprintf(stdout, "%s\t%s\t%s%s\n", r.ID, r.Category, r.Label, suffix)
	}
	return 0
}

func cmdLogCause(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{"note": true, "who": true})
	fs := flag.NewFlagSet("log cause", flag.ContinueOnError)
	fs.SetOutput(stderr)
	note := fs.String("note", "", "optional free-text note (required for other)")
	who := fs.String("who", "", "who added the cause (default: caller or human)")
	jsonOut := fs.Bool("json", false, "print the annotation event as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	rest := fs.Args()
	if len(rest) < 2 || strings.TrimSpace(rest[0]) == "" {
		return fail(stderr, fmt.Errorf("usage: decide log cause [--note TEXT] [--json] <event-id> <reason> [reason]"), 2)
	}
	eventID := rest[0]
	reasons := rest[1:]
	actor := strings.TrimSpace(*who)
	if actor == "" {
		actor = senderOf(context.Background())
	}
	if actor == "" {
		actor = decide.Human
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	ev, err := led.AddCause(eventID, reasons, *note, actor, decide.ViaCLI)
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, ev)
	}
	fmt.Fprintf(stdout, "%s annotated %s: %s\n", ev.ID, ev.TargetEventID, strings.Join(ev.Reasons, ", "))
	return 0
}

func cmdLogReasonsInline(args []string, jsonOut bool, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		return fail(stderr, fmt.Errorf("decide log --reasons takes no arguments"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	reasons := cfg.EffectiveReasons()
	if jsonOut {
		return writeJSON(stdout, stderr, reasons)
	}
	for _, r := range reasons {
		fmt.Fprintf(stdout, "%s\t%s\t%s\n", r.ID, r.Category, r.Label)
	}
	return 0
}

func renderLogEventsOutput(events []decide.Event, groupByAgent, flagsOnly, jsonOut bool, stdout, stderr io.Writer) int {
	if jsonOut {
		if groupByAgent {
			return writeJSON(stdout, stderr, decide.GroupEventsByAgent(events))
		}
		if events == nil {
			events = []decide.Event{}
		}
		return writeJSON(stdout, stderr, events)
	}
	if groupByAgent || flagsOnly {
		groups := decide.GroupEventsByAgent(events)
		for i, g := range groups {
			if i > 0 {
				fmt.Fprintln(stdout)
			}
			fmt.Fprintf(stdout, "agent: %s (%d)\n", g.Agent, len(g.Events))
			for j := range g.Events {
				writeEventEntry(stdout, &g.Events[j], "  ")
			}
		}
		return 0
	}
	for i := range events {
		writeEventEntry(stdout, &events[i], "")
	}
	return 0
}

func cmdLogEvents(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("log", flag.ContinueOnError)
	fs.SetOutput(stderr)
	misfit := fs.Bool("misfit", false, "list free-text replies where options did not fit")
	flagsOnly := fs.Bool("flags", false, "list inbox flags grouped by sending agent")
	agent := fs.String("agent", "", "filter by asking or sending agent")
	cause := fs.String("cause", "", "filter by reason ID")
	kind := fs.String("kind", "", "filter by event kind (choice, cause, flag, ask_open, ask_done, ask_drop)")
	groupByAgent := fs.Bool("group-by-agent", false, "group events by agent")
	reasonsFlag := fs.Bool("reasons", false, "list configured flag and misfit reasons")
	jsonOut := fs.Bool("json", false, "print JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if *reasonsFlag {
		return cmdLogReasonsInline(fs.Args(), *jsonOut, stdout, stderr)
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("usage: decide log [--misfit] [--flags] [--agent A] [--cause R] [--group-by-agent] [--json] | search | cause"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	events, err := led.QueryEvents(decide.EventFilter{
		Misfit: *misfit,
		Flags:  *flagsOnly,
		Agent:  *agent,
		Cause:  *cause,
		Kind:   strings.TrimSpace(*kind),
	})
	if err != nil {
		return fail(stderr, err, 1)
	}
	return renderLogEventsOutput(events, *groupByAgent, *flagsOnly, *jsonOut, stdout, stderr)
}

func writeChoiceEventEntry(w io.Writer, ev *decide.Event, ts, indent string) {
	flagMark := ""
	if ev.Misfit {
		flagMark = "\t[" + ev.MisfitFlag + "]"
	}
	fmt.Fprintf(w, "%s%s\t%s\t%s\t%s\tvia:%s%s\n", indent, ev.ID, ts, ev.ItemID, ev.AgentName(), ev.Via, flagMark)
	switch {
	case ev.ChoiceKey != "":
		fmt.Fprintf(w, "%s  choice: %s: %s\n", indent, ev.ChoiceKey, ev.ChoiceLabel)
	case ev.ReplyText != "":
		fmt.Fprintf(w, "%s  reply: %s\n", indent, ev.ReplyText)
	}
	if len(ev.Options) > 0 {
		parts := make([]string, len(ev.Options))
		for i, o := range ev.Options {
			parts[i] = o.Key + ": " + o.Label
		}
		fmt.Fprintf(w, "%s  options: %s\n", indent, strings.Join(parts, " | "))
	}
	for _, c := range ev.Causes {
		line := fmt.Sprintf("%s  cause (%s): %s", indent, c.EventID, strings.Join(c.Reasons, ", "))
		if c.Note != "" {
			line += " - " + c.Note
		}
		fmt.Fprintln(w, line)
	}
}

func writeFlagEventEntry(w io.Writer, ev *decide.Event, ts, indent string) {
	msgHint := ""
	if ev.MessageIndex != nil {
		msgHint = fmt.Sprintf("\tmsg#%d", *ev.MessageIndex)
	}
	fmt.Fprintf(w, "%s%s\t%s\t%s\treasons:%s%s\n", indent, ev.ID, ts, ev.ItemID, strings.Join(ev.Reasons, ","), msgHint)
	if ev.MessageText != "" {
		firstLine, _, _ := strings.Cut(ev.MessageText, "\n")
		fmt.Fprintf(w, "%s  message: %s\n", indent, firstLine)
	}
	if ev.Note != "" {
		fmt.Fprintf(w, "%s  note: %s\n", indent, ev.Note)
	}
}

func writeEventEntry(w io.Writer, ev *decide.Event, indent string) {
	ts := ev.Time.Local().Format("2006-01-02 15:04")
	switch ev.Kind {
	case decide.EventChoice:
		writeChoiceEventEntry(w, ev, ts, indent)
	case decide.EventFlag:
		writeFlagEventEntry(w, ev, ts, indent)
	case decide.EventCause:
		line := fmt.Sprintf("%s%s\t%s\tcause\t%s -> %s\t%s", indent, ev.ID, ts, ev.ItemID, ev.TargetEventID, strings.Join(ev.Reasons, ","))
		if ev.Note != "" {
			line += "\t" + ev.Note
		}
		fmt.Fprintln(w, line)
	default:
		fmt.Fprintf(w, "%s%s\t%s\t%s\t%s\t%s\n", indent, ev.ID, ts, ev.Kind, ev.ItemID, strings.TrimSpace(ev.AskID+" "+ev.AskPerson+": "+ev.AskWhat+" "+ev.Note))
	}
}

func writeLogEntry(w io.Writer, it *decide.Item) {
	fmt.Fprintf(w, "%s\t%s\t%s\n", it.ID, decide.LogTime(it).Local().Format(time.DateOnly), it.Question)
	if a := decide.AnswerText(it); a != "" {
		fmt.Fprintf(w, "\tanswer: %s\n", a)
	}
	if it.Outcome != "" {
		fmt.Fprintf(w, "\toutcome: %s\n", it.Outcome)
	}
	if it.Supersedes != "" {
		fmt.Fprintf(w, "\tsupersedes %s\n", it.Supersedes)
	}
	if it.SupersededBy != "" {
		fmt.Fprintf(w, "\tsuperseded by %s\n", it.SupersededBy)
	}
}

// cmdSupersede runs decide supersede <old> --by <new>.
func cmdSupersede(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("supersede", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print both items as JSON")
	by := fs.String("by", "", "id of the newer item that replaces <old>")
	// Accept the id before or after the flags.
	var id string
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		id, args = args[0], args[1:]
	}
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if id == "" && fs.NArg() == 1 {
		id = fs.Arg(0)
	} else if fs.NArg() != 0 {
		return fail(stderr, fmt.Errorf("decide supersede takes one <old> id"), 2)
	}
	if strings.TrimSpace(id) == "" || strings.TrimSpace(*by) == "" {
		return fail(stderr, fmt.Errorf("usage: decide supersede <old> --by <new>"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	old, newer, err := led.Supersede(id, *by, senderOf(context.Background()))
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, []decide.Item{old, newer})
	}
	fmt.Fprintf(stdout, "%s supersedes %s\n", newer.ID, old.ID)
	return 0
}
