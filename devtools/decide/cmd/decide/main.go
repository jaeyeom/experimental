// Command decide is the decision inbox CLI.
//
// It stores items in the decide ledger. After a human answers a relay item,
// it runs the notify template from config.
package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		usage(stderr)
		return 2
	}
	switch args[0] {
	case "ask":
		return cmdAsk(args[1:], stdout, stderr)
	case "list":
		return cmdList(args[1:], stdout, stderr)
	case "show":
		return cmdShow(args[1:], stdout, stderr)
	case "withdraw":
		return cmdWithdraw(args[1:], stdout, stderr)
	case "done":
		return cmdDone(args[1:], stdout, stderr)
	case "answer":
		return cmdAnswer(args[1:], stdout, stderr)
	case "inbox":
		return cmdInbox(args[1:], stdout, stderr)
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
  decide ask --owner <label> --kind relay|type-in \
    --option key:label --rec key [--free-text] [--options-only] [--deadline RFC3339] \
    [--link url] [--notify-pane pane] [--type-text text] [--where place] \
    [--json] <question>
  decide list [--state open|answered|done|withdrawn] [--json]
  decide show [--json] <id>
  decide withdraw [--json] <id> <reason>
  decide done [--json] <id> <outcome>
  decide answer [--json] <id> <option|text>
  decide inbox
`)
}

func cmdInbox(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("inbox", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide inbox does not take arguments"), 2)
	}
	dir, err := decide.DefaultDir()
	if err != nil {
		return fail(stderr, fmt.Errorf("decide directory: %w", err), 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if err := tui.Run(tui.Options{
		Dir:          dir,
		In:           os.Stdin,
		Out:          stdout,
		Targets:      inboxTargets(cfg),
		SendTemplate: cfg.SendTemplate,
	}); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
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
	fs := flag.NewFlagSet("ask", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print the new item as JSON")
	owner := fs.String("owner", "", "owner label")
	kind := fs.String("kind", "", "relay or type-in")
	rec := fs.String("rec", "", "recommended option key")
	freeText := fs.Bool("free-text", false, "allow an answer that is not an option")
	optionsOnly := fs.Bool("options-only", false, "reject an answer that is not an option")
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
	question, err := oneArg(fs.Args(), "question")
	if err != nil {
		return fail(stderr, err, 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	allowFree := relayFreeText(*kind, *freeText, *optionsOnly, cfg.OptionsOnly)
	item, err := buildAsk(*owner, *kind, *rec, allowFree, *deadline, *notifyPane, *typeText, *where, []string(options), []string(links), question)
	if err != nil {
		return fail(stderr, err, 2)
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
	items, err := led.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	selected, err := decide.Select(items, *state)
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
		fmt.Fprintf(stdout, "%s\t%s\t%s\t%s\n", it.ID, deadline, it.Owner, it.Question)
	}
	return 0
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
	return withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		return led.Get(id)
	})
}

func cmdWithdraw(args []string, stdout, stderr io.Writer) int {
	return cmdText(args, stdout, stderr, "withdraw", "reason", func(led *decide.Ledger, id, text string) (decide.Item, error) {
		return led.Withdraw(id, text)
	})
}

func cmdDone(args []string, stdout, stderr io.Writer) int {
	return cmdText(args, stdout, stderr, "done", "outcome", func(led *decide.Ledger, id, text string) (decide.Item, error) {
		return led.Done(id, text)
	})
}

func cmdAnswer(args []string, stdout, stderr io.Writer) int {
	fs, jsonOut, code := itemCmd("answer", args, stderr)
	if fs == nil {
		return code
	}
	id, text, err := twoArgs(fs.Args(), "id", "option or text")
	if err != nil {
		return fail(stderr, err, 2)
	}
	var answered decide.Item
	code = withItem(stdout, stderr, jsonOut, false, func(led *decide.Ledger) (decide.Item, error) {
		item, err := led.Answer(id, text)
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
	if err := decide.Notify(&answered, stderr); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}

func cmdText(args []string, stdout, stderr io.Writer, name, textName string, fn func(*decide.Ledger, string, string) (decide.Item, error)) int {
	fs, jsonOut, code := itemCmd(name, args, stderr)
	if fs == nil {
		return code
	}
	id, text, err := twoArgs(fs.Args(), "id", textName)
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

func buildAsk(owner, kind, rec string, freeText bool, deadline, notifyPane, typeText, where string, options, links []string, question string) (decide.Item, error) {
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
		Question:   question,
		Options:    opts,
		FreeText:   freeText,
		Owner:      owner,
		Links:      storedLinks,
		Deadline:   when,
		Kind:       kind,
		NotifyPane: notifyPane,
		TypeText:   typeText,
		Where:      where,
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
	fmt.Fprintf(w, "owner: %s\n", it.Owner)
	fmt.Fprintf(w, "kind: %s\n", it.Kind)
	fmt.Fprintf(w, "state: %s\n", it.State)
	fmt.Fprintf(w, "freeText: %t\n", it.FreeText)
	fmt.Fprintf(w, "unread: %t\n", it.Unread)
	for _, opt := range it.Options {
		mark := ""
		if opt.Recommended {
			mark = " recommended"
		}
		fmt.Fprintf(w, "option: %s: %s%s\n", opt.Key, opt.Label, mark)
	}
	for _, link := range it.Links {
		fmt.Fprintf(w, "link: %s\n", link)
	}
	if it.Deadline != nil {
		fmt.Fprintf(w, "deadline: %s\n", it.Deadline.Format(time.RFC3339))
	}
	writeTime(w, "createdAt", it.CreatedAt)
	writeTime(w, "answeredAt", it.AnsweredAt)
	writeTime(w, "doneAt", it.DoneAt)
	writeTime(w, "withdrawnAt", it.WithdrawnAt)
	if it.Answer != nil {
		fmt.Fprintf(w, "answer: %s\n", it.Answer.Value)
		fmt.Fprintf(w, "answeredBy: %s\n", it.Answer.AnsweredBy)
	}
	writeField(w, "notifyPane", it.NotifyPane)
	writeField(w, "typeText", it.TypeText)
	writeField(w, "where", it.Where)
	writeField(w, "reason", it.Reason)
	writeField(w, "outcome", it.Outcome)
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

func fail(stderr io.Writer, err error, code int) int {
	fmt.Fprintf(stderr, "decide: %v\n", err)
	return code
}

type stringList []string

func (s *stringList) String() string {
	return strings.Join(*s, ",")
}

func (s *stringList) Set(v string) error {
	*s = append(*s, v)
	return nil
}
