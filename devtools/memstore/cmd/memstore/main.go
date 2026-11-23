// Command memstore is a compartmentalized personal memory store CLI for
// coding agents.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

// Exit codes shared by every command.
const (
	exitOK        = 0
	exitError     = 1
	exitUsage     = 2 // also policy-denied or not-found
	exitAmbiguous = 3
)

func main() {
	a := &app{stdout: os.Stdout, stderr: os.Stderr, getenv: os.Getenv, now: time.Now}
	os.Exit(a.run(os.Args[1:]))
}

type app struct {
	stdout, stderr io.Writer
	getenv         func(string) string
	now            func() time.Time
	configFlag     string
	profileFlag    string
	byFlag         string
	reasonFlag     string
}

type commonOpts struct {
	profile string
	by      string
	reason  string
	jsonOut bool
}

type stringSliceFlag []string

func (s *stringSliceFlag) String() string {
	return strings.Join(*s, ",")
}

func (s *stringSliceFlag) Set(val string) error {
	*s = append(*s, val)
	return nil
}

func usages() []struct{ name, line string } {
	return []struct{ name, line string }{
		{"init", "memstore init --vault <name> [--json]"},
		{"profile", "memstore profile check <name> [--json]"},
		{"entity", "memstore entity put|get ..."},
		{"resolve", "memstore resolve <query> [--json]"},
		{"fact", "memstore fact set|list ..."},
		{"note", "memstore note set|list ..."},
		{"event", "memstore event add|list ..."},
		{"search", "memstore search <query> [--kind K]... [--limit N] [--since D] [--until D] [--vault V] [--entity E] [--json]"},
		{"export", "memstore export --profile P --out <dir> [--allow-git] [--by C] [--reason R] [--json]"},
		{"sync-check", "memstore sync-check --profile P --dir <dir> [--stage] [--by C] [--reason R] [--json]"},
		{"triage", "memstore triage list|approve|reject ..."},
		{"import-aside", "memstore import-aside --src <dir> --vault <default-vault> [--rules <file>] [--dry-run] [--json]"},
		{"age", "memstore age --vault V [--retention-days N] [--dry-run] [--drop] [--json]"},
		{"backup", "memstore backup --vault V --out <path> [--encrypt-cmd CMD] [--require-encrypted] [--json]"},
		{"log", "memstore log --vault V [--caller C] [--entity E] [--since D] [--limit N] [--json]"},
	}
}

func (a *app) run(args []string) int {
	fs := flag.NewFlagSet("memstore", flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	fs.Usage = func() { a.usage() }
	fs.StringVar(&a.configFlag, "config", "", "config file (default $MEMSTORE_CONFIG or $XDG_CONFIG_HOME/memstore/config.toml)")
	fs.StringVar(&a.profileFlag, "profile", "", "caller profile (default $MEMSTORE_PROFILE)")
	fs.StringVar(&a.byFlag, "by", "", "caller identifier (default $MEMSTORE_CALLER)")
	fs.StringVar(&a.reasonFlag, "reason", "", "audit reason")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	args = fs.Args()
	if len(args) == 0 {
		a.usage()
		return exitUsage
	}
	if args[0] == "help" {
		a.usage()
		return exitOK
	}
	if code, ok := newMemstoreTool(a.configFlag, a.getenv).Handle(context.Background(), args, a.stdout, a.stderr); ok {
		return code
	}
	return a.dispatch(args[0], args[1:])
}

func (a *app) dispatch(name string, args []string) int {
	switch name {
	case "init":
		return a.cmdInit(args)
	case "profile":
		return a.cmdProfile(args)
	case "entity":
		return a.cmdEntity(args)
	case "resolve":
		return a.cmdResolve(args)
	case "fact":
		return a.cmdFact(args)
	case "note":
		return a.cmdNote(args)
	case "event":
		return a.cmdEvent(args)
	case "search":
		return a.cmdSearch(args)
	case "export":
		return a.cmdExport(args)
	case "sync-check":
		return a.cmdSyncCheck(args)
	case "triage":
		return a.cmdTriage(args)
	case "import-aside":
		return a.cmdImportAside(args)
	default:
		if code, ok := a.dispatchMaintenance(name, args); ok {
			return code
		}
		a.errorf("unknown command %q", name)
		a.usage()
		return exitUsage
	}
}

func (a *app) usage() {
	fmt.Fprintln(a.stderr, "Usage: memstore [--config path] [--profile name] <command> [args]")
	fmt.Fprintln(a.stderr, "\nCommands:")
	for _, u := range usages() {
		fmt.Fprintf(a.stderr, "  %s\n", u.line)
	}
}

func (a *app) errorf(format string, args ...any) {
	fmt.Fprintf(a.stderr, "memstore: "+format+"\n", args...)
}

func (a *app) flags(name, usageLine string) *flag.FlagSet {
	fs := flag.NewFlagSet("memstore "+name, flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	fs.Usage = func() { fmt.Fprintf(a.stderr, "Usage: %s\n", usageLine) }
	return fs
}

func (a *app) addCommonFlags(fs *flag.FlagSet, opts *commonOpts) {
	fs.StringVar(&opts.profile, "profile", a.profileFlag, "caller profile (default $MEMSTORE_PROFILE)")
	fs.StringVar(&opts.by, "by", a.byFlag, "caller identifier (default $MEMSTORE_CALLER)")
	fs.StringVar(&opts.reason, "reason", a.reasonFlag, "audit reason")
	fs.BoolVar(&opts.jsonOut, "json", false, "output JSON")
}

func (a *app) resolveActor(opts commonOpts) (string, memstore.Actor) {
	profile := opts.profile
	if profile == "" {
		profile = a.getenv("MEMSTORE_PROFILE")
	}
	caller := opts.by
	if caller == "" {
		caller = a.getenv("MEMSTORE_CALLER")
	}
	if caller == "" {
		caller = "cli"
	}
	return profile, memstore.Actor{Caller: caller, Reason: opts.reason}
}

func (a *app) parse(fs *flag.FlagSet, args []string, minPos, maxPos int) ([]string, bool) {
	var pos []string
	for {
		if err := fs.Parse(args); err != nil {
			return nil, false
		}
		args = fs.Args()
		if len(args) == 0 {
			break
		}
		pos = append(pos, args[0])
		args = args[1:]
	}
	if len(pos) < minPos || (maxPos >= 0 && len(pos) > maxPos) {
		fs.Usage()
		return nil, false
	}
	return pos, true
}

func (a *app) loadConfig() (memstore.Config, error) {
	path, err := memstore.ConfigPath(a.configFlag, a.getenv)
	if err != nil {
		return memstore.Config{}, fmt.Errorf("config path: %w", err)
	}
	cfg, err := memstore.LoadConfig(path, a.getenv)
	if err != nil {
		return cfg, fmt.Errorf("load config: %w", err)
	}
	return cfg, nil
}

func (a *app) openService(opts commonOpts) (*memstore.Service, memstore.Actor, int) {
	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return nil, memstore.Actor{}, exitError
	}
	profile, actor := a.resolveActor(opts)
	svc, err := memstore.OpenService(cfg, profile)
	if err != nil {
		a.errorf("%v", err)
		return nil, memstore.Actor{}, mapServiceErr(err)
	}
	return svc, actor, exitOK
}

func mapServiceErr(err error) int {
	switch {
	case errors.Is(err, memstore.ErrAmbiguous):
		return exitAmbiguous
	case errors.Is(err, memstore.ErrForbidden), errors.Is(err, memstore.ErrNotFound):
		return exitUsage
	default:
		return exitError
	}
}

func (a *app) writeJSON(v any) int {
	enc := json.NewEncoder(a.stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		a.errorf("encode json: %v", err)
		return exitError
	}
	return exitOK
}

func parseDateOrTime(raw string) (time.Time, error) {
	if raw == "" {
		return time.Time{}, nil
	}
	if t, err := time.Parse(time.RFC3339, raw); err == nil {
		return t.UTC(), nil
	}
	if t, err := time.Parse("2006-01-02", raw); err == nil {
		return t.UTC(), nil
	}
	return time.Time{}, fmt.Errorf("invalid date/time %q (want YYYY-MM-DD or RFC3339)", raw)
}

func (a *app) cmdInit(args []string) int {
	fs := a.flags("init", "memstore init --vault <name> [--json]")
	var vaultName string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name to initialize")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" {
		fs.Usage()
		return exitUsage
	}
	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	vc, ok := cfg.Vaults[vaultName]
	if !ok {
		a.errorf("unknown vault %q", vaultName)
		return exitUsage
	}
	v, err := memstore.InitVault(vc)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if err := v.Close(); err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]string{"vault": vaultName, "path": vc.Path})
	}
	fmt.Fprintf(a.stdout, "initialized %s (%s)\n", vaultName, vc.Path)
	return exitOK
}

func (a *app) cmdProfile(args []string) int {
	if len(args) == 0 || args[0] != "check" {
		a.errorf("usage: memstore profile check <name> [--json]")
		return exitUsage
	}
	fs := a.flags("profile check", "memstore profile check <name> [--json]")
	var opts commonOpts
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args[1:], 1, 1)
	if !ok {
		return exitUsage
	}
	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	policy, err := memstore.ResolveProfile(cfg, pos[0])
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}
	pc := cfg.Profiles[pos[0]]
	out := struct {
		Name           string                `json:"name"`
		Vaults         []string              `json:"vaults"`
		Compartments   []string              `json:"compartments"`
		MaxSensitivity memstore.Sensitivity  `json:"maxSensitivity"`
		Capabilities   []memstore.Capability `json:"capabilities"`
	}{
		Name:           pos[0],
		Vaults:         policy.AllowedVaults(),
		Compartments:   policy.AllowedCompartments(),
		MaxSensitivity: policy.MaxSensitivity(),
		Capabilities:   pc.Capabilities,
	}
	if opts.jsonOut {
		return a.writeJSON(out)
	}
	fmt.Fprintf(a.stdout, "profile %s: vaults=%v compartments=%v maxSensitivity=%s capabilities=%v\n",
		out.Name, out.Vaults, out.Compartments, out.MaxSensitivity, out.Capabilities)
	return exitOK
}

func (a *app) cmdResolve(args []string) int {
	fs := a.flags("resolve", "memstore resolve <query> [--json]")
	var opts commonOpts
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	svc, actor, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	matches, err := svc.ResolveEntity(context.Background(), pos[0], actor)
	if err != nil {
		var amb *memstore.AmbiguousError
		if errors.As(err, &amb) {
			if opts.jsonOut {
				_ = a.writeJSON(amb.Candidates)
			} else {
				for _, c := range amb.Candidates {
					fmt.Fprintf(a.stdout, "%s\t%s\t%s\t(%s)\n", c.Vault, c.Entity.ID, c.Entity.Title, c.Match)
				}
			}
			a.errorf("%v", err)
			return exitAmbiguous
		}
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(matches[0])
	}
	m := matches[0]
	fmt.Fprintf(a.stdout, "%s\t%s\t%s\t(%s)\n", m.Vault, m.Entity.ID, m.Entity.Title, m.Match)
	return exitOK
}

func (a *app) cmdSearch(args []string) int {
	fs := a.flags("search", "memstore search <query> [--kind K]... [--limit N] [--since D] [--until D] [--vault V] [--entity E] [--json]")
	var kinds stringSliceFlag
	var limit int
	var sinceStr, untilStr, vaultName, entityID string
	var opts commonOpts
	fs.Var(&kinds, "kind", "document kind filter (entity, fact, note, event)")
	fs.IntVar(&limit, "limit", memstore.DefaultSearchLimit, "maximum number of results")
	fs.StringVar(&sinceStr, "since", "", "start date/time (YYYY-MM-DD or RFC3339)")
	fs.StringVar(&untilStr, "until", "", "end date/time (YYYY-MM-DD or RFC3339)")
	fs.StringVar(&vaultName, "vault", "", "restrict search to one vault")
	fs.StringVar(&entityID, "entity", "", "restrict search to one entity ID")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
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

	hits, err := memstore.NewSQLiteRetriever(svc).Search(context.Background(), memstore.SearchQuery{
		Query:    pos[0],
		Vault:    vaultName,
		Kinds:    kinds,
		EntityID: entityID,
		Since:    since,
		Until:    until,
		Limit:    limit,
		Actor:    actor,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(hits)
	}
	for _, h := range hits {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\t%.4f\t%s\n", h.Vault, h.DocKind, h.DocID, h.Title, h.Score, h.Snippet)
	}
	return exitOK
}

func (a *app) cmdLog(args []string) int {
	fs := a.flags("log", "memstore log --vault V [--caller C] [--entity E] [--since D] [--limit N] [--json]")
	var vaultName, caller, entityID, sinceStr string
	var limit int
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name")
	fs.StringVar(&caller, "caller", "", "filter by caller")
	fs.StringVar(&entityID, "entity", "", "filter by entity ID")
	fs.StringVar(&sinceStr, "since", "", "start date/time (YYYY-MM-DD or RFC3339)")
	fs.IntVar(&limit, "limit", 0, "maximum number of entries")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if vaultName == "" {
		fs.Usage()
		return exitUsage
	}
	since, err := parseDateOrTime(sinceStr)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}

	svc, _, code := a.openService(opts)
	if code != exitOK {
		return code
	}
	defer svc.Close()

	entries, err := svc.QueryAccessLog(context.Background(), vaultName, memstore.AccessLogFilter{
		Caller:   caller,
		EntityID: entityID,
		Since:    since,
		Limit:    limit,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(entries)
	}
	for _, e := range entries {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\t%s\t%s\n",
			e.Time.Format(time.RFC3339), e.Caller, e.Profile, e.Command, e.EntityID, e.Reason)
	}
	return exitOK
}
