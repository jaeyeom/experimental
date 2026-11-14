package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func cmdContacts(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		return fail(stderr, fmt.Errorf("decide contacts requires hide, unhide, alias, or prune"), 2)
	}
	switch args[0] {
	case "hide":
		return cmdContactsHide(args[1:], true, stdout, stderr)
	case "unhide":
		return cmdContactsHide(args[1:], false, stdout, stderr)
	case "prune":
		return cmdContactsPrune(args[1:], stdout, stderr)
	case "alias":
		return cmdContactsAlias(args[1:], stdout, stderr)
	default:
		return fail(stderr, fmt.Errorf("decide contacts requires hide, unhide, alias, or prune"), 2)
	}
}

func cmdContactsHide(args []string, hide bool, stdout, stderr io.Writer) int {
	verb := "hide"
	if !hide {
		verb = "unhide"
	}
	fs := flag.NewFlagSet("contacts "+verb, flag.ContinueOnError)
	fs.SetOutput(stderr)
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 1 {
		return fail(stderr, fmt.Errorf("decide contacts %s requires <name>", verb), 2)
	}
	name := strings.TrimSpace(fs.Arg(0))
	if name == "" {
		return fail(stderr, fmt.Errorf("decide contacts name is empty"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	names := append([]string(nil), cfg.HiddenContacts...)
	if hide {
		if !containsName(names, name) {
			names = append(names, name)
		}
	} else {
		kept := make([]string, 0, len(names))
		for _, n := range names {
			if n != name {
				kept = append(kept, n)
			}
		}
		names = kept
	}
	if _, err := decide.SetHiddenContacts(names); err != nil {
		return fail(stderr, err, 1)
	}
	if hide {
		fmt.Fprintf(stdout, "hid %s\n", name)
		return 0
	}
	fmt.Fprintf(stdout, "unhid %s\n", name)
	return 0
}

func cmdContactsPrune(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("contacts prune", flag.ContinueOnError)
	fs.SetOutput(stderr)
	dead := fs.Bool("dead", false, "hide dead contacts")
	release := fs.Bool("release", false, "also release dead bus names")
	yes := fs.Bool("yes", false, "do it; without this, print the plan")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide contacts prune does not take arguments"), 2)
	}
	if !*dead {
		return fail(stderr, fmt.Errorf("decide contacts prune requires --dead"), 2)
	}
	cfg, hide, rel, code := loadDeadContacts(stderr)
	if code != 0 {
		return code
	}
	if len(hide) == 0 {
		fmt.Fprintln(stdout, "nothing to hide")
		return 0
	}
	if !*yes {
		printPrunePlan(stdout, hide, rel, *release)
		return 0
	}
	return applyContactPrune(stdout, stderr, cfg, hide, rel, *release)
}

// loadDeadContacts lists the ledger and the bus. A failed bus list hides nothing.
func loadDeadContacts(stderr io.Writer) (decide.Config, []string, []string, int) {
	led, err := openLedger()
	if err != nil {
		return decide.Config{}, nil, nil, fail(stderr, err, 1)
	}
	items, err := led.List()
	if err != nil {
		return decide.Config{}, nil, nil, fail(stderr, err, 1)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return decide.Config{}, nil, nil, fail(stderr, err, 1)
	}
	ctx, cancel := context.WithTimeout(context.Background(), cfg.BusListTimeout())
	defer cancel()
	out, err := agentbusRun(ctx, []string{"list", "--json"})
	if err != nil {
		return decide.Config{}, nil, nil, fail(stderr, fmt.Errorf("agentbus list: %w", err), 1)
	}
	bus, err := tui.ParseBusList(out)
	if err != nil {
		return decide.Config{}, nil, nil, fail(stderr, err, 1)
	}
	hide, rel := tui.DeadContactNames(items, bus, cfg.ContactAliases)
	return cfg, hide, rel, 0
}

func printPrunePlan(stdout io.Writer, hide, rel []string, release bool) {
	for _, name := range hide {
		fmt.Fprintf(stdout, "would hide %s\n", name)
	}
	if !release {
		return
	}
	for _, name := range rel {
		fmt.Fprintf(stdout, "would release %s\n", name)
	}
}

func applyContactPrune(stdout, stderr io.Writer, cfg decide.Config, hide, rel []string, release bool) int {
	names := append([]string(nil), cfg.HiddenContacts...)
	have := map[string]bool{}
	for _, name := range names {
		have[name] = true
	}
	for _, name := range hide {
		if !have[name] {
			names = append(names, name)
			have[name] = true
		}
		fmt.Fprintf(stdout, "hid %s\n", name)
	}
	if _, err := decide.SetHiddenContacts(names); err != nil {
		return fail(stderr, err, 1)
	}
	if !release {
		return 0
	}
	return releaseDeadNames(stdout, cfg, rel)
}

func releaseDeadNames(stdout io.Writer, cfg decide.Config, rel []string) int {
	for _, name := range rel {
		relCtx, relCancel := context.WithTimeout(context.Background(), cfg.BusListTimeout())
		relOut, err := agentbusRun(relCtx, []string{"release", name})
		relCancel()
		if err != nil {
			fmt.Fprintf(stdout, "release %s: %v\n", name, err)
			return 1
		}
		text := strings.TrimSpace(string(relOut))
		if text == "" {
			text = "ok"
		}
		fmt.Fprintf(stdout, "released %s: %s\n", name, text)
	}
	return 0
}

func cmdContactsAlias(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("contacts alias", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 2 {
		return fail(stderr, fmt.Errorf("decide contacts alias requires <name> <agent>"), 2)
	}
	name := strings.TrimSpace(fs.Arg(0))
	agent := strings.TrimSpace(fs.Arg(1))
	if name == "" || agent == "" {
		return fail(stderr, fmt.Errorf("decide contacts alias requires <name> <agent>"), 2)
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return fail(stderr, err, 1)
	}
	aliases := map[string]string{}
	for k, v := range cfg.ContactAliases {
		aliases[k] = v
	}
	aliases[name] = agent
	if _, err := decide.SetContactAliases(aliases); err != nil {
		return fail(stderr, err, 1)
	}
	fmt.Fprintf(stdout, "aliased %s to %s\n", name, agent)
	return 0
}

func containsName(names []string, name string) bool {
	for _, n := range names {
		if n == name {
			return true
		}
	}
	return false
}
