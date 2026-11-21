// Command decide is the decision inbox CLI.
//
// It stores items in the decide ledger. After a human answers a relay item,
// it runs the notify template from config.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	args, remote, err := remoteFlag(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	if len(args) > 0 && args[0] == "config" {
		return cmdConfig(args[1:], remote, stdout, stderr)
	}
	if code, done := maybeRunOnHub(args, remote, stdout, stderr); done {
		return code
	}
	return runLocal(args, stdout, stderr)
}

// itemCommands act on one existing item.
var itemCommands = map[string]func([]string, io.Writer, io.Writer) int{
	"show":     cmdShow,
	"withdraw": cmdWithdraw,
	"done":     cmdDone,
	"answer":   cmdAnswer,
	"snooze":   cmdSnooze,
	"unsnooze": cmdUnsnooze,
	"reassign": cmdReassign,
	"assign":   cmdAssign,
}

// ledgerCommands run against this machine's ledger until stopped: the
// hub side of a hub-mode inbox and the stale-answer runner.
var ledgerCommands = map[string]func([]string, io.Writer, io.Writer) int{
	"serve":  cmdServe,
	"follow": cmdFollow,
	"keys":   cmdKeys,
}

func runLocal(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		usage(stderr)
		return 2
	}
	if cmd, ok := itemCommands[args[0]]; ok {
		return cmd(args[1:], stdout, stderr)
	}
	if cmd, ok := ledgerCommands[args[0]]; ok {
		return cmd(args[1:], stdout, stderr)
	}
	if code, handled := newDecideTool().Handle(context.Background(), args, stdout, stderr); handled {
		return code
	}
	switch args[0] {
	case "ask":
		return cmdAsk(args[1:], stdout, stderr)
	case "list":
		return cmdList(args[1:], stdout, stderr)
	case "inbox":
		return cmdInbox(args[1:], stdout, stderr)
	case "status":
		return cmdStatus(args[1:], stdout, stderr)
	case "targets":
		return cmdTargets(args[1:], stdout, stderr)
	case "send", "reply":
		return cmdSendOrReply(args, stdout, stderr)
	case "avatars":
		return cmdAvatars(args[1:], stdout, stderr)
	case "contacts":
		return cmdContacts(args[1:], stdout, stderr)
	case "help", "-h", "--help":
		usage(stderr)
		return 0
	default:
		fmt.Fprintf(stderr, "decide: unknown command %q\n", args[0])
		usage(stderr)
		return 2
	}
}

func usage(w io.Writer) {
	fmt.Fprintf(w, `Usage:
  decide ask [--owner <label>] [--owner-replies-only] --kind relay|type-in \
    --option key:label --rec key [--free-text] [--options-only] [--deadline RFC3339] \
    [--link url] [--notify-pane pane] [--type-text text] [--where place] \
    [--verbatim-file path] [--json] <question>
  decide list [--state open|answered|done|withdrawn] [--snoozed] [--off-line] [--undelivered] [--json]
  decide show [--json] <id>
  decide withdraw [--json] <id> <reason>
  decide done [--json] <id> <outcome>
  decide answer [--json] [--via cli|ui] <id> <option|text>
  decide snooze [--json] <id> <2h30m|HH:MM|YYYY-MM-DD HH:MM|mon 9:00>
  decide unsnooze [--json] <id>
  decide [--remote <user@host>] inbox [--snoozed] [--utc] [--print-keys]
  decide keys [--json]
  decide follow [--every 1m] [--once]
  decide status set --agent <name> [--state working|idle|paused|blocked] <text>
  decide status set --resource <name> free|busy [<note>]
  decide status list [--json]
  decide status clear --agent <name>
  decide status clear --resource <name>
  decide targets [--json]
  decide send --to <name> [--json] [--via cli|ui] <text>
  decide send --id <id> [--to owner|worker|both] [--json] [--via cli|ui] <text>
  decide reply <id> [--option key:label ...] [--rec key] [--free-text] [--verbatim-file path] [--json] <text>
  decide reassign <id> [--owner <label>] [--notify-pane <pane>] [--json]
  decide assign <id> <agent> [--notify-pane <pane>] [--json]
  decide assign <id> --clear [--json]
  decide skill
  decide skill install --agent <claude|codex|antigravity|grok|all> [--scope user|project] [--dir D] [--dry-run] [--force]
  decide skill uninstall --agent <claude|codex|antigravity|grok|all> [--scope user|project] [--dir D] [--dry-run]
  decide skill status [--json]
  decide version [--json]
  decide update [--check] [--ref <ref>] [--json]
  decide doctor [--json] [--offline]
  decide avatars list [--name <name>] [--ids]
  decide avatars set [--local] <name> <id|default|none>
  decide contacts hide <name>
  decide contacts unhide <name>
  decide contacts alias <name> <agent>
  decide contacts prune --dead [--release] [--yes]
  decide [--remote <user@host>] config show [--effective]

Text for ask, reply, send, done, withdraw, and status set may be a positional argument, --text-file <path>, or - for stdin.
Author text must follow inbox shape rules: verdict on the first line (max 100 chars, followed by a blank line or bullet), no paragraph over 3 lines, max 160 chars per line, and max 1200 chars total.
Pass --verbatim-file <path> on ask or reply for an exact post draft (shown as a quote block without shape checks).
Use reassign when the owning tab is retired, or for a handoff to another agent.
Use assign when another agent works on an item while its owner stays; the
worker is notified at agentbus:<agent> unless --notify-pane says otherwise.
send --id on an item with a worker warns on stderr and notifies the owner;
--to worker or --to both reaches the worker.
`)
}

func cmdSendOrReply(args []string, stdout, stderr io.Writer) int {
	if args[0] == "reply" {
		return cmdReply(args[1:], stdout, stderr)
	}
	return cmdSend(args[1:], stdout, stderr)
}

func cmdSend(args []string, stdout, stderr io.Writer) int {
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fs := flag.NewFlagSet("send", flag.ContinueOnError)
	fs.SetOutput(stderr)
	to := fs.String("to", "", "recipient name; with --id, owner, worker, or both")
	id := fs.String("id", "", "thread to append a human message")
	jsonOut := fs.Bool("json", false, "print JSON")
	via := fs.String("via", decide.ViaCLI, "where the message came from: cli, or ui for the inbox front ends only")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if err := decide.CheckVia(*via); err != nil {
		return fail(stderr, err, 2)
	}
	hasTo := strings.TrimSpace(*to) != ""
	hasID := strings.TrimSpace(*id) != ""
	if !hasTo && !hasID {
		return fail(stderr, fmt.Errorf("decide send requires --to or --id"), 2)
	}
	route, err := composeRoute(*to, hasID)
	if err != nil {
		return fail(stderr, err, 2)
	}
	text, err := bodyFrom(fs.Args(), file, "text")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if hasID {
		return cmdCompose(strings.TrimSpace(*id), text, *via, senderOf(context.Background()), route, *jsonOut, stdout, stderr)
	}
	return sendNewThread(*to, text, *via, *jsonOut, stdout, stderr)
}

func sendNewThread(to, text, via string, jsonOut bool, stdout, stderr io.Writer) int {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	rec, code, err := resolveSendTo(cfg, to)
	if err != nil {
		return fail(stderr, err, code)
	}
	offLine, code := enforceOrWarnOwner(cfg, rec.Name, via, stderr)
	if code != 0 {
		return code
	}
	dir, err := decide.DefaultDir()
	if err != nil {
		return fail(stderr, fmt.Errorf("decide directory: %w", err), 1)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	item, err := deliverNewThread(led, dir, cfg.SendTemplate, rec, text, via)
	if err != nil {
		return fail(stderr, err, 1)
	}
	if offLine {
		if item, err = led.AddTag(item.ID, decide.TagOffLine); err != nil {
			return fail(stderr, err, 1)
		}
	}
	return printSent(stdout, stderr, rec.Name, text, item.ID, jsonOut)
}

// resolveSendTo finds name on the same recipient list the inbox uses.
// A list or parse failure is an error with exit 1. An unknown name is
// exit 2.
func resolveSendTo(cfg decide.Config, name string) (decide.SendRecipient, int, error) {
	bus, err := sendBusNames(cfg)
	if err != nil {
		return decide.SendRecipient{}, 1, err
	}
	rec, ok := decide.ResolveSend(cfg.Targets, bus, cfg.Recipients.Agentbus, name)
	if !ok {
		return decide.SendRecipient{}, 2, fmt.Errorf("decide send recipient %q is unknown", name)
	}
	return rec, 0, nil
}

// sendBusNames is the agentbus half of the recipient list. The gate off
// returns no names and does not run agentbus.
func sendBusNames(cfg decide.Config) ([]decide.BusRecipient, error) {
	if !cfg.Recipients.Agentbus {
		return nil, nil
	}
	ctx, cancel := context.WithTimeout(context.Background(), cfg.BusListTimeout())
	defer cancel()
	out, err := agentbusRun(ctx, []string{"list", "--json"})
	if err != nil {
		return nil, err //nolint:wrapcheck // agentbus names the failure; it is not an unknown recipient
	}
	names, err := tui.ParseBusList(out)
	if err != nil {
		return nil, err //nolint:wrapcheck // ParseBusList names the parse failure
	}
	bus := make([]decide.BusRecipient, len(names))
	for i, tg := range names {
		bus[i] = decide.BusRecipient{Name: tg.Name, Handle: tg.Pane}
	}
	return bus, nil
}

// deliverNewThread starts a thread and delivers it. A bus name, including
// a configured agentbus: pane, goes through agentbus. A herdr pane uses
// the send template. A failed template send deletes the new item.
func deliverNewThread(led *decide.Ledger, dir, template string, rec decide.SendRecipient, text, via string) (decide.Item, error) {
	if rec.Bus {
		return decide.OpenBusThread(led, dir, "", rec.Name, text, via, agentbusRun) //nolint:wrapcheck // OpenBusThread names the failure
	}
	item, err := led.StartThread(rec.Name, rec.Pane, text, via)
	if err != nil {
		return decide.Item{}, err //nolint:wrapcheck // StartThread names the refusal
	}
	note := decide.DeliveryNote(item.ID, "human", text)
	if err := decide.RunSend(decide.SendArgv(template, rec.Pane, note)); err != nil {
		if derr := led.Delete(item.ID); derr != nil {
			return decide.Item{}, fmt.Errorf("%w; %w", err, derr)
		}
		return decide.Item{}, err //nolint:wrapcheck // RunSend names the send
	}
	item, err = led.RecordCompose(item.ID, item.Messages[0].Time, decide.Delivery{Target: rec.Pane, Outcome: decide.OutcomeDelivered})
	if err != nil {
		return decide.Item{}, err //nolint:wrapcheck // RecordCompose names the ledger failure
	}
	if err := decide.AppendSend(dir, rec.Name, rec.Pane, text); err != nil {
		return decide.Item{}, err //nolint:wrapcheck // AppendSend names the history
	}
	return item, nil
}

func printSent(stdout, stderr io.Writer, name, text, id string, jsonOut bool) int {
	sent := struct {
		Name string `json:"name"`
		Text string `json:"text"`
		ID   string `json:"id"`
	}{Name: name, Text: text, ID: id}
	if jsonOut {
		return writeJSON(stdout, stderr, sent)
	}
	fmt.Fprintf(stdout, "sent to %s as %s\n", name, id)
	return 0
}

// composeRoute is the --to of send --id: owner, worker, both, or empty.
// Without --id, --to is a recipient name and the route is empty.
func composeRoute(to string, hasID bool) (string, error) {
	route := strings.TrimSpace(to)
	if !hasID || route == "" {
		return "", nil
	}
	if err := decide.CheckRoute(route); err != nil {
		return "", fmt.Errorf("decide send --id takes --to owner, worker, or both: %w", err)
	}
	return route, nil
}

// cmdCompose appends a human message to id and notifies the agents route
// names. An empty route notifies the owner and warns on stderr when
// another agent is working on the item.
func cmdCompose(id, text, via, by, route string, jsonOut bool, stdout, stderr io.Writer) int {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	var item decide.Item
	code := withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		cur, err := led.Get(id)
		if err != nil {
			return decide.Item{}, fmt.Errorf("compose decide item: %w", err)
		}
		if cur.State != decide.StateWithdrawn {
			if err := cur.RejectForeignSender(by); err != nil {
				return decide.Item{}, err //nolint:wrapcheck // the refusal sentence is printed unchanged
			}
		}
		if (route == decide.RouteWorker || route == decide.RouteBoth) && !cur.HasOtherWorker() {
			return decide.Item{}, fmt.Errorf("decide item %s has no worker other than its owner", id)
		}
		got, err := led.Compose(id, text, via, by)
		if err != nil {
			return decide.Item{}, fmt.Errorf("compose decide item: %w", err)
		}
		item = got
		return got, nil
	})
	if code != 0 {
		return code
	}
	if err := notifyCompose(&item, cfg, route, text, stderr); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

// notifyCompose sends text to the agents route names and records each
// delivery. An empty route warns on stderr when another agent is working
// on item. The first failure is returned after every send was tried.
func notifyCompose(item *decide.Item, cfg decide.Config, route, text string, stderr io.Writer) error {
	if route == "" {
		if warning := decide.RouteWarning(item); warning != "" {
			fmt.Fprintf(stderr, "decide: %s Use --to worker or --to both to reach %s.\n", warning, item.Worker)
		}
	}
	var first error
	for _, tg := range decide.ReplyTargets(item, cfg, route) {
		d, err := decide.NotifyOwner(tg.Pane, tg.Machine, item.ID, text)
		recordCompose(item, d, err)
		if err != nil && first == nil {
			first = err
		}
	}
	return first
}

// recordCompose stores the delivery of item's newest message, the one
// just composed. A send that failed before it had a target stores only
// notifyError. A ledger error leaves the send result unchanged.
func recordCompose(item *decide.Item, d decide.Delivery, err error) {
	if d.Outcome == "" || len(item.Messages) == 0 {
		recordNotifyError(item.ID, err)
		return
	}
	led, oerr := openLedger()
	if oerr != nil {
		return
	}
	_, _ = led.RecordCompose(item.ID, item.Messages[len(item.Messages)-1].Time, d)
}

// cmdAssign sets or clears the agent working on an item. The owner stays.
func cmdAssign(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{"notify-pane": true})
	fs := flag.NewFlagSet("assign", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the item as JSON")
	clearWorker := fs.Bool("clear", false, "remove the worker")
	pane := fs.String("notify-pane", "", "worker notify pane (default: agentbus:<agent>)")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	rest := fs.Args()
	var id, worker string
	var err error
	switch {
	case *clearWorker && strings.TrimSpace(*pane) != "":
		return fail(stderr, fmt.Errorf("decide assign --clear does not take --notify-pane"), 2)
	case *clearWorker:
		id, err = oneArg(rest, "id")
	default:
		id, worker, err = twoArgs(rest, "id", "agent")
	}
	if err != nil {
		return fail(stderr, err, 2)
	}
	machine, err := machineForNotifyPane(*pane)
	if err != nil {
		return fail(stderr, err, 1)
	}
	by := senderOf(context.Background())
	return withItem(stdout, stderr, *jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Assign(id, decide.Assign{
			Worker:  strings.TrimSpace(worker),
			Pane:    *pane,
			Machine: machine,
			Clear:   *clearWorker,
			By:      by,
		})
	})
}

// cmdReassign moves an item to a new owner, notify pane, or both.
func cmdReassign(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{"owner": true, "notify-pane": true})
	fs := flag.NewFlagSet("reassign", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the item as JSON")
	var owner setString
	var pane setString
	fs.Var(&owner, "owner", "new owner label")
	fs.Var(&pane, "notify-pane", "new notify pane, or empty to clear it")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, err := oneArg(fs.Args(), "id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if !owner.set && !pane.set {
		return fail(stderr, fmt.Errorf("decide reassign requires --owner or --notify-pane"), 2)
	}
	if owner.set && strings.TrimSpace(owner.val) == "" {
		return fail(stderr, fmt.Errorf("decide owner is empty"), 2)
	}
	machine := ""
	if pane.set {
		machine, err = machineForNotifyPane(pane.val)
		if err != nil {
			return fail(stderr, err, 1)
		}
	}
	by := senderOf(context.Background())
	return withItem(stdout, stderr, *jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Reassign(id, decide.Reassign{
			Owner:    strings.TrimSpace(owner.val),
			SetOwner: owner.set,
			Pane:     pane.val,
			SetPane:  pane.set,
			Machine:  machine,
			By:       by,
		})
	})
}

func cmdReply(args []string, stdout, stderr io.Writer) int {
	if err := rejectLongLinesFlag(args); err != nil {
		return fail(stderr, err, 2)
	}
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	args = hoistFlags(args, map[string]bool{"option": true, "rec": true, "verbatim": true, "verbatim-file": true})
	fs := flag.NewFlagSet("reply", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the thread as JSON")
	rec := fs.String("rec", "", "recommended option key")
	freeText := fs.Bool("free-text", false, "allow an answer that is not an option")
	verbatim := fs.String("verbatim", "", "exact post draft shown as a quote block without shape checks")
	verbatimFile := fs.String("verbatim-file", "", "file containing exact post draft shown as a quote block without shape checks")
	var options stringList
	fs.Var(&options, "option", "option as key:label (repeatable)")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, text, err := pairBody(fs.Args(), "id", "text", file)
	if err != nil {
		return fail(stderr, err, 2)
	}
	verb, err := resolveVerbatim(*verbatim, *verbatimFile)
	if err != nil {
		return fail(stderr, err, 2)
	}
	opts, err := parseOptions([]string(options))
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *rec != "" {
		opts, err = applyRec(opts, *rec)
		if err != nil {
			return fail(stderr, err, 2)
		}
	}
	allowFree := len(opts) > 0 && *freeText
	by := senderOf(context.Background())
	return withItem(stdout, stderr, *jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.ReplyVerbatim(id, by, text, verb, opts, allowFree)
	})
}

func cmdTargets(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("targets", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print recipients as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide targets does not take arguments"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	targets := cfg.Targets
	if targets == nil {
		targets = []decide.ConfigTarget{}
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, targets)
	}
	for _, tg := range targets {
		fmt.Fprintln(stdout, tg.Name)
	}
	return 0
}

func cmdStatus(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		return fail(stderr, fmt.Errorf("decide status requires set, list, or clear"), 2)
	}
	switch args[0] {
	case "set":
		return cmdStatusSet(args[1:], stdout, stderr)
	case "list":
		return cmdStatusList(args[1:], stdout, stderr)
	case "clear":
		return cmdStatusClear(args[1:], stdout, stderr)
	default:
		return fail(stderr, fmt.Errorf("decide status command %q is unknown", args[0]), 2)
	}
}

func cmdStatusSet(args []string, stdout, stderr io.Writer) int {
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fs := flag.NewFlagSet("status set", flag.ContinueOnError)
	fs.SetOutput(stderr)
	agent := fs.String("agent", "", "agent name")
	resource := fs.String("resource", "", "shared resource name")
	state := fs.String("state", "", "working, idle, paused, or blocked")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	hasAgent := strings.TrimSpace(*agent) != ""
	hasResource := strings.TrimSpace(*resource) != ""
	if hasAgent == hasResource {
		return fail(stderr, fmt.Errorf("decide status set requires --agent or --resource"), 2)
	}
	board, err := openStatus()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if hasResource {
		return setResource(stdout, stderr, board, *resource, file, fs.Args())
	}
	text, err := bodyFrom(fs.Args(), file, "text")
	if err != nil {
		return fail(stderr, err, 2)
	}
	row, err := board.SetAgent(*agent, *state, text)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fmt.Fprintf(stdout, "%s %s %s\n", row.Name, row.State, row.Text)
	return 0
}

func setResource(stdout, stderr io.Writer, board *decide.Status, name, file string, args []string) int {
	availability, note, err := resourceArgs(args, file)
	if err != nil {
		return fail(stderr, err, 2)
	}
	row, err := board.SetResource(name, availability, note)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fmt.Fprintln(stdout, decide.ResourceLine(row))
	return 0
}

// agentbusRun runs agentbus for the status board. Tests replace it.
var agentbusRun decide.AgentbusRunner = decide.ExecAgentbus

func cmdStatusList(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("status list", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the status board as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide status list does not take arguments"), 2)
	}
	board, err := openStatus()
	if err != nil {
		return fail(stderr, err, 1)
	}
	agents, err := board.Agents()
	if err != nil {
		return fail(stderr, err, 1)
	}
	resources, err := board.Resources()
	if err != nil {
		return fail(stderr, err, 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	now := time.Now()
	resources = decide.ResourceView(context.Background(), resources, cfg.Checkouts, agentbusRun, now)
	if *jsonOut {
		return writeJSON(stdout, stderr, struct {
			Agents    []decide.AgentStatus    `json:"agents"`
			Resources []decide.ResourceStatus `json:"resources"`
		}{Agents: agents, Resources: resources})
	}
	for _, row := range agents {
		fmt.Fprintf(stdout, "%s %s %s %s\n", row.Name, row.State, statusAge(now.Sub(row.UpdatedAt)), row.Text)
	}
	for _, row := range resources {
		fmt.Fprintln(stdout, decide.ResourceLine(row))
	}
	return 0
}

func cmdStatusClear(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("status clear", flag.ContinueOnError)
	fs.SetOutput(stderr)
	agent := fs.String("agent", "", "agent name")
	resource := fs.String("resource", "", "shared resource name")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	hasAgent := strings.TrimSpace(*agent) != ""
	hasResource := strings.TrimSpace(*resource) != ""
	if hasAgent == hasResource {
		return fail(stderr, fmt.Errorf("decide status clear requires --agent or --resource"), 2)
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide status clear does not take arguments"), 2)
	}
	board, err := openStatus()
	if err != nil {
		return fail(stderr, err, 1)
	}
	name := strings.TrimSpace(*agent)
	clearErr := board.ClearAgent(name)
	if hasResource {
		name = strings.TrimSpace(*resource)
		clearErr = board.ClearResource(name)
	}
	if clearErr != nil {
		return fail(stderr, clearErr, 1)
	}
	fmt.Fprintf(stdout, "cleared %s\n", name)
	return 0
}

func openStatus() (*decide.Status, error) {
	dir, err := decide.DefaultDir()
	if err != nil {
		return nil, fmt.Errorf("decide directory: %w", err)
	}
	board, err := decide.OpenStatus(dir)
	if err != nil {
		return nil, fmt.Errorf("open decide status: %w", err)
	}
	return board, nil
}

func statusAge(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	switch {
	case d < time.Minute:
		return "now"
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d/time.Hour))
	default:
		return fmt.Sprintf("%dd", int(d/(24*time.Hour)))
	}
}

// runInbox runs the inbox TUI. Tests replace it.
var runInbox = tui.Run

func cmdInbox(args []string, stdout, stderr io.Writer) int {
	snoozed, utc, printKeys, code, ok := inboxFlags(args, stderr)
	if !ok {
		return code
	}
	if printKeys {
		cfg, err := decide.LoadConfig()
		if err != nil {
			return fail(stderr, err, 1)
		}
		return printKeyMap(cfg, false, stdout, stderr)
	}
	dir, err := decide.DefaultDir()
	if err != nil {
		return fail(stderr, fmt.Errorf("decide directory: %w", err), 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	opts := inboxOptions(cfg, stdout, snoozed)
	opts.UTC = utc || cfg.UI.UTC
	opts.Dir = dir
	if err := runInbox(opts); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

func cmdKeys(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("keys", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the active keymap as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide keys does not take arguments"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	return printKeyMap(cfg, *jsonOut, stdout, stderr)
}

func printKeyMap(cfg decide.Config, jsonOut bool, stdout, stderr io.Writer) int {
	km, err := decide.ResolveKeyMap(cfg.UI.Keys)
	if err != nil {
		return fail(stderr, err, 1)
	}
	if jsonOut {
		data, err := decide.FormatKeyMapJSON(km)
		if err != nil {
			return fail(stderr, err, 1)
		}
		_, _ = stdout.Write(data)
		return 0
	}
	_, _ = io.WriteString(stdout, decide.FormatKeyMapText(km))
	return 0
}

// inboxFlags parses the inbox flags and reports whether to go on.
func inboxFlags(args []string, stderr io.Writer) (snoozed, utc, printKeys bool, code int, ok bool) {
	fs := flag.NewFlagSet("inbox", flag.ContinueOnError)
	fs.SetOutput(stderr)
	snoozedFlag := fs.Bool("snoozed", false, "open on the Snoozed folder")
	utcFlag := fs.Bool("utc", false, "show times in UTC")
	printKeysFlag := fs.Bool("print-keys", false, "print the active keymap and exit")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return false, false, false, code, false
	}
	if len(fs.Args()) != 0 {
		return false, false, false, fail(stderr, fmt.Errorf("decide inbox does not take arguments"), 2), false
	}
	return *snoozedFlag, *utcFlag, *printKeysFlag, 0, true
}

// inboxOptions are the inbox settings from this machine's config.
func inboxOptions(cfg decide.Config, stdout io.Writer, snoozed bool) tui.Options {
	return tui.Options{
		In:                os.Stdin,
		Out:               stdout,
		Targets:           inboxTargets(cfg),
		SendTemplate:      cfg.SendTemplate,
		LateDeliveryAfter: cfg.LateDeliveryAfter(),
		OutboxStaleAfter:  cfg.OutboxStaleAfter(),
		Agentbus:          cfg.Recipients.Agentbus,
		RecipientsTimeout: cfg.BusListTimeout(),
		Snoozed:           snoozed,
		Avatars:           cfg.Avatars,
		AvatarDefault:     cfg.AvatarDefault,
		AvatarMono:        cfg.AvatarMono,
		MessageAvatars:    cfg.MessageAvatarsOn(),
		Checkouts:         cfg.Checkouts,
		Machine:           decide.LocalMachine(cfg),
		OpenCommand:       cfg.OpenCommand,
		OrgTree:           cfg.UI.OrgTree,
		OrgSpec:           cfg.UI.OrgSpec,
		NoHyperlinks:      !cfg.HyperlinksOn(),
		Clipboard:         cfg.UI.Clipboard,
		FillColumn:        cfg.UI.FillColumn,
		ArchiveGrace:      cfg.ArchiveGrace(),
		BusHealthInterval: cfg.BusHealthInterval(),
		HiddenContacts:    cfg.HiddenContacts,
		FavoriteContacts:  cfg.FavoriteContacts,
		ContactAliases:    cfg.ContactAliases,
		Keys:              cfg.UI.Keys,
		ASCIIKeys:         cfg.UI.ASCIIKeys,
		AnswerAction:      cfg.UI.AnswerAction,
	}
}

func inboxTargets(cfg decide.Config) []tui.Target {
	if len(cfg.Targets) == 0 {
		return nil
	}
	targets := make([]tui.Target, len(cfg.Targets))
	for i, tg := range cfg.Targets {
		targets[i] = tui.Target{Name: tg.Name, Pane: tg.Pane}
	}
	return targets
}

func cmdAsk(args []string, stdout, stderr io.Writer) int {
	if err := rejectLongLinesFlag(args); err != nil {
		return fail(stderr, err, 2)
	}
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fs := flag.NewFlagSet("ask", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the new item as JSON")
	owner := fs.String("owner", "", "owner label (default: this herdr tab's label)")
	kind := fs.String("kind", "", "relay or type-in")
	rec := fs.String("rec", "", "recommended option key")
	freeText := fs.Bool("free-text", false, "allow an answer that is not an option")
	optionsOnly := fs.Bool("options-only", false, "reject an answer that is not an option")
	ownerRepliesOnly := fs.Bool("owner-replies-only", false, "only the owner may reply or send on this item")
	verbatim := fs.String("verbatim", "", "exact post draft shown as a quote block without shape checks")
	verbatimFile := fs.String("verbatim-file", "", "file containing exact post draft shown as a quote block without shape checks")
	deadline := fs.String("deadline", "", "deadline in RFC3339")
	notifyPane := fs.String("notify-pane", "", "pane stored for a later notify command")
	typeText := fs.String("type-text", "", "exact text to type")
	where := fs.String("where", "", "where to type the text")
	var options stringList
	var links stringList
	fs.Var(&options, "option", "option as key:label (repeatable)")
	fs.Var(&links, "link", "link (repeatable)")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	question, err := bodyFrom(fs.Args(), file, "question")
	if err != nil {
		return fail(stderr, err, 2)
	}
	verb, err := resolveVerbatim(*verbatim, *verbatimFile)
	if err != nil {
		return fail(stderr, err, 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	allowFree := relayFreeText(*kind, *freeText, *optionsOnly, cfg.OptionsOnly)
	who := askOwner(*owner, askerOf(context.Background()), stderr)
	item, err := buildAsk(who, *kind, *rec, allowFree, *ownerRepliesOnly, *deadline, *notifyPane, *typeText, *where, []string(options), []string(links), question, verb)
	if err != nil {
		return fail(stderr, err, 2)
	}
	offLine, code := enforceOrWarnOwner(cfg, item.Owner, decide.ViaCLI, stderr)
	if code != 0 {
		return code
	}
	if offLine {
		item.Tags = append(item.Tags, decide.TagOffLine)
	}
	if err := notifyHubCaller(&item); err != nil {
		return fail(stderr, err, 1)
	}
	if strings.TrimSpace(item.NotifyPane) == "" {
		item.NotifyPane = strings.TrimSpace(os.Getenv("HERDR_PANE_ID"))
	}
	return withItem(stdout, stderr, *jsonOut, true, func(led *decide.Ledger) (decide.Item, error) {
		return led.Create(&item)
	})
}

func cmdList(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("list", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print items as JSON")
	state := fs.String("state", decide.StateOpen, "state to list")
	snoozed := fs.Bool("snoozed", false, "list only snoozed items, soonest wake first")
	offLine := fs.Bool("off-line", false, "list only items tagged off-line")
	undelivered := fs.Bool("undelivered", false, "list only items with delivery problems")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide list does not take arguments"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if _, err := led.MarkLateDeliveries(cfg.LateDeliveryAfter()); err != nil {
		return fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	stateSet := false
	fs.Visit(func(f *flag.Flag) {
		if f.Name == "state" {
			stateSet = true
		}
	})
	selected, err := listSelection(items, *state, stateSet, *snoozed, *offLine, *undelivered)
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, selected)
	}
	for _, it := range selected {
		deadline := "-"
		if it.Deadline != nil {
			deadline = it.Deadline.Format(time.RFC3339)
		}
		line := fmt.Sprintf("%s\t%s\t%s\t%s", it.ID, deadline, it.Owner, it.Question)
		if it.SnoozedUntil != nil {
			line += "\tsnoozed until " + it.SnoozedUntil.Local().Format(time.RFC3339)
		}
		fmt.Fprintln(stdout, line)
	}
	return 0
}

func listSelection(items []decide.Item, state string, stateSet, snoozed, offLine, undelivered bool) ([]decide.Item, error) {
	var selected []decide.Item
	if undelivered && !stateSet {
		selected = append([]decide.Item(nil), items...)
	} else {
		var err error
		selected, err = decide.Select(items, state)
		if err != nil {
			return nil, fmt.Errorf("select decide list: %w", err)
		}
	}
	if snoozed {
		selected = decide.SelectSnoozed(items)
	}
	if offLine {
		selected = filterByTag(selected, decide.TagOffLine)
	}
	if undelivered {
		selected = filterByTag(selected, decide.TagUndelivered)
	}
	return selected, nil
}

func filterByTag(items []decide.Item, tag string) []decide.Item {
	out := make([]decide.Item, 0)
	for _, it := range items {
		if slices.Contains(it.Tags, tag) {
			out = append(out, it)
		}
	}
	return out
}

// cmdSnooze hides an item until a time. The time may be split across
// arguments, so `decide snooze D-1 mon 9:00` needs no quotes.
func cmdSnooze(args []string, stdout, stderr io.Writer) int {
	fs, jsonOut, code := itemCmd("snooze", args, stderr)
	if fs == nil {
		return code
	}
	rest := fs.Args()
	if len(rest) < 2 || strings.TrimSpace(rest[0]) == "" {
		return fail(stderr, fmt.Errorf("decide id and time are required"), 2)
	}
	until, err := decide.ParseSnooze(strings.Join(rest[1:], " "), time.Now())
	if err != nil {
		return fail(stderr, err, 2)
	}
	return withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Snooze(rest[0], until)
	})
}

func cmdUnsnooze(args []string, stdout, stderr io.Writer) int {
	fs, jsonOut, code := itemCmd("unsnooze", args, stderr)
	if fs == nil {
		return code
	}
	id, err := oneArg(fs.Args(), "id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	return withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Unsnooze(id)
	})
}

func cmdShow(args []string, stdout, stderr io.Writer) int {
	fs, jsonOut, code := itemCmd("show", args, stderr)
	if fs == nil {
		return code
	}
	id, err := oneArg(fs.Args(), "id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if jsonOut {
		return writeShowJSON(id, stdout, stderr)
	}
	return withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Get(id)
	})
}

// writeShowJSON prints item id with ownerLocation, where its owner pane lives
// in herdr now.
func writeShowJSON(id string, stdout, stderr io.Writer) int {
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	item, err := led.Get(id)
	if err != nil {
		return fail(stderr, err, 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	locs := decide.LocationResolver{}.Resolve(context.Background(), []decide.OwnerLocation{decide.OwnerPane(&item, decide.LocalMachine(cfg))})
	itemJSON, err := json.Marshal(item)
	if err != nil {
		return fail(stderr, fmt.Errorf("encode json: %w", err), 1)
	}
	locJSON, err := json.Marshal(locs[0])
	if err != nil {
		return fail(stderr, fmt.Errorf("encode json: %w", err), 1)
	}
	// Item has its own MarshalJSON, so embedding it in a struct would
	// drop ownerLocation. Append the field to the item object instead.
	itemJSON = append(itemJSON[:len(itemJSON)-1], `,"ownerLocation":`...)
	itemJSON = append(append(itemJSON, locJSON...), '}')
	return writeJSON(stdout, stderr, json.RawMessage(itemJSON))
}

func cmdWithdraw(args []string, stdout, stderr io.Writer) int {
	return cmdText(args, stdout, stderr, "withdraw", "reason", func(led *decide.Ledger, id, text string) (decide.Item, error) {
		return led.Withdraw(id, text)
	})
}

// cmdDone closes an item. The default, an agent's done, keeps an item
// with an unread message in the user's inbox as done-unread. --via ui is
// the user's own done from an inbox front end and closes it at once.
func cmdDone(args []string, stdout, stderr io.Writer) int {
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fs := flag.NewFlagSet("done", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print JSON")
	via := fs.String("via", decide.ViaCLI, "who closes it: cli for an agent, or ui for the inbox front ends only")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if err := decide.CheckVia(*via); err != nil {
		return fail(stderr, err, 2)
	}
	id, text, err := pairBody(fs.Args(), "id", "outcome", file)
	if err != nil {
		return fail(stderr, err, 2)
	}
	return withItem(stdout, stderr, *jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		item, err := led.DoneVia(id, text, *via)
		if err == nil && item.State == decide.StateDoneUnread {
			fmt.Fprintf(stderr, "decide: %s has a message the user has not read; it stays in the inbox until the user reads it\n", id)
		}
		return item, err //nolint:wrapcheck // DoneVia names the item in its errors
	})
}

func cmdAnswer(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("answer", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print JSON")
	via := fs.String("via", decide.ViaCLI, "where the answer came from: cli, or ui for the inbox front ends only")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if err := decide.CheckVia(*via); err != nil {
		return fail(stderr, err, 2)
	}
	id, text, err := twoArgs(fs.Args(), "id", "option or text")
	if err != nil {
		return fail(stderr, err, 2)
	}
	var answered decide.Item
	code := withItem(stdout, stderr, *jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		item, err := led.Answer(id, text, *via)
		if err != nil {
			return decide.Item{}, fmt.Errorf("answer decide item: %w", err)
		}
		answered = item
		return item, nil
	})
	if code != 0 {
		return code
	}
	if answered.Kind != decide.KindRelay {
		return 0
	}
	if err := deliverAnswer(&answered, stderr); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

// deliverAnswer notifies item and stores the attempt on the ledger.
func deliverAnswer(item *decide.Item, out io.Writer) error {
	led, err := openLedger()
	if err != nil {
		return err
	}
	_, err = led.DeliverAnswer(item, out)
	return err //nolint:wrapcheck // DeliverAnswer names the failure
}

func cmdText(args []string, stdout, stderr io.Writer, name, textName string, fn func(*decide.Ledger, string, string) (decide.Item, error)) int {
	file, args, err := pullTextFile(args)
	if err != nil {
		return fail(stderr, err, 2)
	}
	fs, jsonOut, code := itemCmd(name, args, stderr)
	if fs == nil {
		return code
	}
	id, text, err := pairBody(fs.Args(), "id", textName, file)
	if err != nil {
		return fail(stderr, err, 2)
	}
	return withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return fn(led, id, text)
	})
}

func itemCmd(name string, args []string, stderr io.Writer) (*flag.FlagSet, bool, int) {
	fs := flag.NewFlagSet(name, flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return nil, false, code
	}
	return fs, *jsonOut, 0
}

func withItem(stdout, stderr io.Writer, jsonOut, idOnly bool, fn func(*decide.Ledger) (decide.Item, error)) int {
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	item, err := fn(led)
	if err != nil {
		if errors.Is(err, decide.ErrShape) {
			return fail(stderr, err, 2)
		}
		return fail(stderr, err, 1)
	}
	if jsonOut {
		return writeJSON(stdout, stderr, item)
	}
	if idOnly {
		fmt.Fprintln(stdout, item.ID)
		return 0
	}
	writeShow(stdout, &item)
	return 0
}

// recordNotifyError stores err on id so decide show can surface it.
// A nil err clears a previous failure. A ledger error leaves the
// notification result unchanged.
func recordNotifyError(id string, err error) {
	led, oerr := openLedger()
	if oerr != nil {
		return
	}
	message := ""
	if err != nil {
		message = err.Error()
	}
	_, _ = led.SetNotifyError(id, message)
}

func openLedger() (*decide.Ledger, error) {
	dir, err := decide.DefaultDir()
	if err != nil {
		return nil, fmt.Errorf("decide directory: %w", err)
	}
	led, err := decide.Open(dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
	return led, nil
}

// relayFreeText reports whether a relay ask stores free-text answers.
// --free-text wins over --options-only and over the config default.
func relayFreeText(kind string, freeText, optionsOnly, configOptionsOnly bool) bool {
	if kind != decide.KindRelay {
		return freeText
	}
	if freeText {
		return true
	}
	if optionsOnly || configOptionsOnly {
		return false
	}
	return true
}

func buildAsk(owner, kind, rec string, freeText, ownerRepliesOnly bool, deadline, notifyPane, typeText, where string, options, links []string, question, verbatim string) (decide.Item, error) {
	if strings.TrimSpace(owner) == "" {
		return decide.Item{}, fmt.Errorf("decide owner is required")
	}
	opts, err := parseOptions(options)
	if err != nil {
		return decide.Item{}, err
	}
	switch kind {
	case decide.KindRelay:
		if len(opts) < 2 {
			return decide.Item{}, fmt.Errorf("decide relay requires at least two options")
		}
		opts, err = applyRec(opts, rec)
		if err != nil {
			return decide.Item{}, err
		}
	case decide.KindTypeIn:
		if strings.TrimSpace(typeText) == "" || strings.TrimSpace(where) == "" {
			return decide.Item{}, fmt.Errorf("decide type-in requires --type-text and --where")
		}
		if rec != "" {
			opts, err = applyRec(opts, rec)
			if err != nil {
				return decide.Item{}, err
			}
		}
	default:
		return decide.Item{}, fmt.Errorf("decide kind %q is invalid", kind)
	}
	when, err := parseDeadline(deadline)
	if err != nil {
		return decide.Item{}, err
	}
	var storedLinks []string
	if len(links) > 0 {
		storedLinks = append([]string(nil), links...)
	}
	return decide.Item{
		Question:         question,
		Verbatim:         verbatim,
		Options:          opts,
		FreeText:         freeText,
		Owner:            owner,
		Links:            storedLinks,
		Deadline:         when,
		Kind:             kind,
		NotifyPane:       notifyPane,
		TypeText:         typeText,
		Where:            where,
		OwnerRepliesOnly: ownerRepliesOnly,
	}, nil
}

func parseOptions(raw []string) ([]decide.Option, error) {
	if len(raw) == 0 {
		return nil, nil
	}
	out := make([]decide.Option, 0, len(raw))
	seen := map[string]bool{}
	for _, r := range raw {
		key, label, ok := strings.Cut(r, ":")
		if !ok || strings.TrimSpace(key) == "" {
			return nil, fmt.Errorf("decide option %q must be key:label", r)
		}
		if seen[key] {
			return nil, fmt.Errorf("decide option %q is duplicated", key)
		}
		seen[key] = true
		out = append(out, decide.Option{Key: key, Label: label})
	}
	return out, nil
}

func applyRec(opts []decide.Option, rec string) ([]decide.Option, error) {
	if rec == "" {
		return nil, fmt.Errorf("decide relay requires --rec")
	}
	found := false
	for i := range opts {
		if opts[i].Key != rec {
			continue
		}
		opts[i].Recommended = true
		found = true
	}
	if !found {
		return nil, fmt.Errorf("decide recommended option %q is missing", rec)
	}
	return opts, nil
}

func parseDeadline(raw string) (*time.Time, error) {
	if raw == "" {
		return nil, nil
	}
	t, err := time.Parse(time.RFC3339, raw)
	if err != nil {
		return nil, fmt.Errorf("decide deadline %q is not RFC3339", raw)
	}
	return &t, nil
}

const flagHelp = -1

// hoistFlags moves flags in front of positionals so they parse after an id.
// valued flags consume the following argument unless the flag contains '='.
func hoistFlags(args []string, valued map[string]bool) []string {
	flags, pos := splitFlags(args, valued)
	return append(flags, pos...)
}

func parseFlags(fs *flag.FlagSet, args []string) int {
	err := fs.Parse(args)
	if err == nil {
		return 0
	}
	if errors.Is(err, flag.ErrHelp) {
		return flagHelp
	}
	return 2
}

func stopped(code int) (int, bool) {
	if code == flagHelp {
		return 0, true
	}
	if code != 0 {
		return code, true
	}
	return 0, false
}

func oneArg(args []string, name string) (string, error) {
	if len(args) != 1 || strings.TrimSpace(args[0]) == "" {
		return "", fmt.Errorf("decide %s is required", name)
	}
	return args[0], nil
}

func twoArgs(args []string, first, second string) (string, string, error) {
	if len(args) != 2 || strings.TrimSpace(args[0]) == "" || strings.TrimSpace(args[1]) == "" {
		return "", "", fmt.Errorf("decide %s and %s are required", first, second)
	}
	return args[0], args[1], nil
}

func writeJSON(stdout, stderr io.Writer, v any) int {
	data, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		return fail(stderr, fmt.Errorf("encode json: %w", err), 1)
	}
	data = append(data, '\n')
	if _, err := stdout.Write(data); err != nil {
		return fail(stderr, fmt.Errorf("write json: %w", err), 1)
	}
	return 0
}

func writeShow(w io.Writer, it *decide.Item) {
	fmt.Fprintf(w, "id: %s\n", it.ID)
	fmt.Fprintf(w, "question: %s\n", it.Question)
	writeField(w, "verbatim", it.Verbatim)
	fmt.Fprintf(w, "owner: %s\n", it.Owner)
	writeField(w, "working on it", it.Worker)
	if it.ForwardedFrom != "" {
		fmt.Fprintf(w, "forwarded from %s\n", it.ForwardedFrom)
	}
	fmt.Fprintf(w, "kind: %s\n", it.Kind)
	fmt.Fprintf(w, "state: %s\n", it.State)
	fmt.Fprintf(w, "freeText: %t\n", it.FreeText)
	if it.OwnerRepliesOnly {
		fmt.Fprintf(w, "ownerRepliesOnly: true\n")
	}
	fmt.Fprintf(w, "unread: %t\n", it.Unread)
	for _, opt := range it.Options {
		mark := ""
		if opt.Recommended {
			mark = " recommended"
		}
		fmt.Fprintf(w, "option: %s: %s%s\n", opt.Key, opt.Label, mark)
	}
	writeList(w, "link", it.Links)
	writeList(w, "tag", it.Tags)
	if it.Deadline != nil {
		fmt.Fprintf(w, "deadline: %s\n", it.Deadline.Format(time.RFC3339))
	}
	writeTime(w, "createdAt", it.CreatedAt)
	writeTime(w, "answeredAt", it.AnsweredAt)
	writeTime(w, "doneAt", it.DoneAt)
	writeTime(w, "withdrawnAt", it.WithdrawnAt)
	if it.SnoozedUntil != nil {
		fmt.Fprintf(w, "snoozed until: %s\n", it.SnoozedUntil.Local().Format(time.RFC3339))
	}
	if it.Answer != nil {
		fmt.Fprintf(w, "answer: %s\n", decide.AnswerText(it))
		fmt.Fprintf(w, "answeredBy: %s\n", it.Answer.AnsweredBy)
		writeField(w, "via", it.Answer.Via)
	}
	writeField(w, "notifyPane", it.NotifyPane)
	writeField(w, "notifyMachine", it.NotifyMachine)
	writeField(w, "workerNotifyPane", it.WorkerNotifyPane)
	writeField(w, "workerNotifyMachine", it.WorkerNotifyMachine)
	writeField(w, "notifyError", it.NotifyError)
	writeField(w, "follow-up", it.FollowUp)
	for _, d := range it.Delivery {
		line := d.State()
		if d.BusID != "" {
			line += " " + d.BusID
		}
		if d.Reason != "" {
			line += " (" + d.Reason + ")"
		}
		if d.Error != "" {
			line += ": " + d.Error
		}
		fmt.Fprintf(w, "delivery: %s %s %s\n", d.At.UTC().Format(time.RFC3339Nano), d.Target, line)
	}
	writeField(w, "typeText", it.TypeText)
	writeField(w, "where", it.Where)
	writeField(w, "reason", it.Reason)
	writeField(w, "outcome", it.Outcome)
	for _, msg := range it.Messages {
		when := ""
		if !msg.Time.IsZero() {
			when = msg.Time.Format(time.RFC3339Nano)
		}
		fmt.Fprintf(w, "message: %s %s %s\n", when, msg.Sender(), msg.Text)
		if msg.Verbatim != "" {
			fmt.Fprintf(w, "verbatim: %s\n", msg.Verbatim)
		}
	}
}

func writeTime(w io.Writer, name string, t time.Time) {
	if t.IsZero() {
		return
	}
	fmt.Fprintf(w, "%s: %s\n", name, t.Format(time.RFC3339Nano))
}

func writeField(w io.Writer, name, value string) {
	if value == "" {
		return
	}
	fmt.Fprintf(w, "%s: %s\n", name, value)
}

func writeList(w io.Writer, name string, values []string) {
	for _, v := range values {
		fmt.Fprintf(w, "%s: %s\n", name, v)
	}
}

func fail(stderr io.Writer, err error, code int) int {
	fmt.Fprintf(stderr, "decide: %v\n", err)
	return code
}

// enforceOrWarnOwner checks whether owner may open a new item when via is
// not ViaUI. In enforce mode it prints the error on stderr and returns exit 3.
// In warn mode it prints the warning on stderr and returns offLine=true.
func enforceOrWarnOwner(cfg decide.Config, owner, via string, stderr io.Writer) (offLine bool, code int) {
	if via == decide.ViaUI {
		return false, 0
	}
	msg, enforce := decide.CheckOwner(cfg.Owners, owner)
	if msg == "" {
		return false, 0
	}
	if enforce {
		return false, fail(stderr, errors.New(msg), 3)
	}
	fmt.Fprintf(stderr, "decide: %s\n", msg)
	return true, 0
}

// setString is a flag that records whether it was passed, including empty.
type setString struct {
	set bool
	val string
}

func (s *setString) String() string { return s.val }

func (s *setString) Set(v string) error {
	s.set = true
	s.val = v
	return nil
}

type stringList []string

func (s *stringList) String() string {
	return strings.Join(*s, ",")
}

func (s *stringList) Set(v string) error {
	*s = append(*s, v)
	return nil
}
