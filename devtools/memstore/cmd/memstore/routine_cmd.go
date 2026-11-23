package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

type routineStatusEntry struct {
	ID         string     `json:"id"`
	Name       string     `json:"name"`
	Vault      string     `json:"vault"`
	Profile    string     `json:"profile"`
	Residency  string     `json:"residency"`
	Schedule   string     `json:"schedule"`
	Enabled    bool       `json:"enabled"`
	LastStatus string     `json:"lastStatus,omitempty"`
	LastRun    *time.Time `json:"lastRun,omitempty"`
	Due        bool       `json:"due"`
	NextRun    *time.Time `json:"nextRun,omitempty"`
}

func (a *app) cmdRoutine(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore routine list|status|run|tick|runs|state|launchd-plist ...")
		return exitUsage
	}
	switch args[0] {
	case "list":
		return a.cmdRoutineList(args[1:])
	case "status":
		return a.cmdRoutineStatus(args[1:])
	case "run":
		return a.cmdRoutineRun(args[1:])
	case "tick":
		return a.cmdRoutineTick(args[1:])
	case "runs":
		return a.cmdRoutineRuns(args[1:])
	case "state":
		return a.cmdRoutineState(args[1:])
	case "launchd-plist":
		return a.cmdRoutineLaunchdPlist(args[1:])
	default:
		a.errorf("unknown routine subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) resolveRoutinesDir(flagDir string) (string, error) {
	if flagDir != "" {
		return flagDir, nil
	}
	if envDir := a.getenv("MEMSTORE_ROUTINES_DIR"); envDir != "" {
		return envDir, nil
	}
	cfgPath, err := memstore.ConfigPath(a.configFlag, a.getenv)
	if err != nil {
		return "", fmt.Errorf("config path: %w", err)
	}
	return filepath.Join(filepath.Dir(cfgPath), "routines"), nil
}

func (a *app) loadManifests(flagDir string) ([]memstore.RoutineManifest, error) {
	dir, err := a.resolveRoutinesDir(flagDir)
	if err != nil {
		return nil, err
	}
	manifests, err := memstore.LoadRoutineManifests(dir)
	if err != nil {
		return nil, fmt.Errorf("load routine manifests: %w", err)
	}
	return manifests, nil
}

func findManifestByID(manifests []memstore.RoutineManifest, id string) (memstore.RoutineManifest, bool) {
	for _, m := range manifests {
		if m.ID == id {
			return m, true
		}
	}
	return memstore.RoutineManifest{}, false
}

func (a *app) cmdRoutineList(args []string) int {
	fs := a.flags("routine list", "memstore routine list [--routines-dir DIR] [--json]")
	var routinesDir string
	var opts commonOpts
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}

	manifests, err := a.loadManifests(routinesDir)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(manifests)
	}
	for _, m := range manifests {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\tenabled=%t\t%s\n", m.ID, m.Residency, m.Vault, m.Enabled, m.Schedule)
	}
	return exitOK
}

func (a *app) cmdRoutineStatus(args []string) int {
	fs := a.flags("routine status", "memstore routine status [--routines-dir DIR] [--json]")
	var routinesDir string
	var opts commonOpts
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}

	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	manifests, err := a.loadManifests(routinesDir)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}

	now := a.now().UTC()
	ctx := context.Background()
	entries := make([]routineStatusEntry, 0, len(manifests))
	for _, m := range manifests {
		entry, err := buildRoutineStatusEntry(ctx, cfg, m, now)
		if err != nil {
			a.errorf("%v", err)
			return exitError
		}
		entries = append(entries, entry)
	}

	if opts.jsonOut {
		return a.writeJSON(entries)
	}
	for _, e := range entries {
		last := "never"
		if e.LastRun != nil {
			last = e.LastRun.Format(time.RFC3339)
		}
		fmt.Fprintf(a.stdout, "%s\tenabled=%t\tdue=%t\tlast=%s\tstatus=%s\n", e.ID, e.Enabled, e.Due, last, e.LastStatus)
	}
	return exitOK
}

func buildRoutineStatusEntry(ctx context.Context, cfg memstore.Config, m memstore.RoutineManifest, now time.Time) (routineStatusEntry, error) {
	entry := routineStatusEntry{
		ID:        m.ID,
		Name:      m.Name,
		Vault:     m.Vault,
		Profile:   m.Profile,
		Residency: m.Residency,
		Schedule:  m.Schedule,
		Enabled:   m.Enabled,
	}
	vc, ok := cfg.Vaults[m.Vault]
	if !ok {
		return entry, nil
	}
	v, err := memstore.OpenVault(vc)
	if err != nil {
		return entry, nil
	}
	defer v.Close()

	runs, err := v.ListRoutineRuns(ctx, memstore.RoutineRunFilter{RoutineID: m.ID, Limit: 1})
	if err != nil {
		return entry, fmt.Errorf("list runs for %s: %w", m.ID, err)
	}
	var lastOK *time.Time
	if len(runs) > 0 {
		entry.LastStatus = runs[0].Status
		t := runs[0].StartedAt
		entry.LastRun = &t
		if runs[0].Status == memstore.RoutineRunOK {
			lastOK = &t
		}
	}
	if lastOK == nil {
		okRuns, err := v.ListRoutineRuns(ctx, memstore.RoutineRunFilter{RoutineID: m.ID, Status: memstore.RoutineRunOK, Limit: 1})
		if err == nil && len(okRuns) > 0 {
			t := okRuns[0].StartedAt
			lastOK = &t
		}
	}
	due, next, err := memstore.IsDue(m.Schedule, m.Timezone, lastOK, now)
	if err != nil {
		return entry, fmt.Errorf("evaluate schedule for %s: %w", m.ID, err)
	}
	entry.Due = due
	if !next.IsZero() {
		entry.NextRun = &next
	}
	return entry, nil
}

func (a *app) cmdRoutineRun(args []string) int {
	fs := a.flags("routine run", "memstore routine run <id> [--routines-dir DIR] [--dry-run] [--idempotency-key K] [--json]")
	var routinesDir, idemKey string
	var dryRun bool
	var opts commonOpts
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	fs.BoolVar(&dryRun, "dry-run", false, "plan without executing or committing DB writes")
	fs.StringVar(&idemKey, "idempotency-key", "", "override idempotency key")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}

	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	manifests, err := a.loadManifests(routinesDir)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	m, found := findManifestByID(manifests, pos[0])
	if !found {
		a.errorf("routine %q not found", pos[0])
		return exitUsage
	}

	_, actor := a.resolveActor(opts)
	res, err := memstore.RunRoutine(context.Background(), cfg, m, memstore.RoutineRunOptions{
		IdempotencyKey: idemKey,
		Trigger:        "manual",
		DryRun:         dryRun,
		Now:            a.now,
		Actor:          actor,
	})
	if err != nil {
		a.errorf("%v", err)
		return mapServiceErr(err)
	}
	if opts.jsonOut {
		return a.writeJSON(res)
	}
	fmt.Fprintf(a.stdout, "%s\t%s\t%s\n", res.RoutineID, res.Status, res.IdempotencyKey)
	return exitOK
}

func (a *app) cmdRoutineTick(args []string) int {
	fs := a.flags("routine tick", "memstore routine tick [--routines-dir DIR] [--dry-run] [--json]")
	var routinesDir string
	var dryRun bool
	var opts commonOpts
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	fs.BoolVar(&dryRun, "dry-run", false, "identify due routines without executing or mutating state")
	a.addCommonFlags(fs, &opts)
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}

	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	manifests, err := a.loadManifests(routinesDir)
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}

	_, actor := a.resolveActor(opts)
	results, err := memstore.Tick(context.Background(), cfg, manifests, a.now(), memstore.RoutineRunOptions{
		Trigger: "tick",
		DryRun:  dryRun,
		Now:     a.now,
		Actor:   actor,
	})
	if err != nil {
		a.errorf("%v", err)
		if opts.jsonOut {
			_ = a.writeJSON(results)
		}
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(results)
	}
	for _, r := range results {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\n", r.RoutineID, r.Status, r.IdempotencyKey)
	}
	return exitOK
}

func (a *app) openRoutineVault(routineID, vaultFlag, routinesDir string) (*memstore.Vault, int) {
	cfg, err := a.loadConfig()
	if err != nil {
		a.errorf("%v", err)
		return nil, exitError
	}
	vaultName := vaultFlag
	if vaultName == "" && routineID != "" {
		if manifests, loadErr := a.loadManifests(routinesDir); loadErr == nil {
			if m, ok := findManifestByID(manifests, routineID); ok {
				vaultName = m.Vault
			}
		}
	}
	if vaultName == "" && len(cfg.Vaults) == 1 {
		for k := range cfg.Vaults {
			vaultName = k
		}
	}
	if vaultName == "" {
		a.errorf("--vault is required when routine manifest is not found")
		return nil, exitUsage
	}
	vc, ok := cfg.Vaults[vaultName]
	if !ok {
		a.errorf("vault %q not found in config", vaultName)
		return nil, exitUsage
	}
	v, err := memstore.OpenVault(vc)
	if err != nil {
		a.errorf("%v", err)
		return nil, exitError
	}
	return v, exitOK
}

func (a *app) cmdRoutineRuns(args []string) int {
	fs := a.flags("routine runs", "memstore routine runs <id> [--vault V] [--routines-dir DIR] [--limit N] [--json]")
	var vaultName, routinesDir string
	var limit int
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name")
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	fs.IntVar(&limit, "limit", 20, "maximum runs to return")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}

	v, code := a.openRoutineVault(pos[0], vaultName, routinesDir)
	if code != exitOK {
		return code
	}
	defer v.Close()

	runs, err := v.ListRoutineRuns(context.Background(), memstore.RoutineRunFilter{
		RoutineID: pos[0],
		Limit:     limit,
	})
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(runs)
	}
	for _, r := range runs {
		fmt.Fprintf(a.stdout, "%s\t%s\t%s\t%s\n", r.ID, r.Status, r.IdempotencyKey, r.StartedAt.Format(time.RFC3339))
	}
	return exitOK
}

func (a *app) cmdRoutineState(args []string) int {
	if len(args) == 0 {
		a.errorf("usage: memstore routine state get|set <id> <key> [value]")
		return exitUsage
	}
	switch args[0] {
	case "get":
		return a.cmdRoutineStateGet(args[1:])
	case "set":
		return a.cmdRoutineStateSet(args[1:])
	default:
		a.errorf("unknown routine state subcommand %q", args[0])
		return exitUsage
	}
}

func (a *app) cmdRoutineStateGet(args []string) int {
	fs := a.flags("routine state get", "memstore routine state get <id> <key> [--vault V] [--routines-dir DIR] [--json]")
	var vaultName, routinesDir string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name")
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 2, 2)
	if !ok {
		return exitUsage
	}

	v, code := a.openRoutineVault(pos[0], vaultName, routinesDir)
	if code != exitOK {
		return code
	}
	defer v.Close()

	val, found, err := v.GetRoutineState(context.Background(), pos[0], pos[1])
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if !found {
		a.errorf("%v: routine state %s/%s", memstore.ErrNotFound, pos[0], pos[1])
		return exitUsage
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]any{
			"routineId": pos[0],
			"key":       pos[1],
			"value":     json.RawMessage(val),
		})
	}
	fmt.Fprintln(a.stdout, val)
	return exitOK
}

func (a *app) cmdRoutineStateSet(args []string) int {
	fs := a.flags("routine state set", "memstore routine state set <id> <key> <value> [--vault V] [--routines-dir DIR] [--json]")
	var vaultName, routinesDir string
	var opts commonOpts
	fs.StringVar(&vaultName, "vault", "", "vault name")
	fs.StringVar(&routinesDir, "routines-dir", "", "directory containing routine TOML manifests")
	a.addCommonFlags(fs, &opts)
	pos, ok := a.parse(fs, args, 3, 3)
	if !ok {
		return exitUsage
	}

	valJSON := pos[2]
	if !json.Valid([]byte(valJSON)) {
		b, err := json.Marshal(valJSON)
		if err != nil {
			a.errorf("encode value: %v", err)
			return exitError
		}
		valJSON = string(b)
	}

	v, code := a.openRoutineVault(pos[0], vaultName, routinesDir)
	if code != exitOK {
		return code
	}
	defer v.Close()

	if err := v.SetRoutineState(context.Background(), pos[0], pos[1], valJSON); err != nil {
		a.errorf("%v", err)
		return exitError
	}
	if opts.jsonOut {
		return a.writeJSON(map[string]any{
			"routineId": pos[0],
			"key":       pos[1],
			"value":     json.RawMessage(valJSON),
		})
	}
	fmt.Fprintf(a.stdout, "%s/%s = %s\n", pos[0], pos[1], valJSON)
	return exitOK
}

func (a *app) cmdRoutineLaunchdPlist(args []string) int {
	fs := a.flags("routine launchd-plist", "memstore routine launchd-plist --interval-seconds 300 [--label com.jaeyeom.memstore.tick]")
	var intervalSeconds int
	var label, routinesDir string
	fs.IntVar(&intervalSeconds, "interval-seconds", 300, "tick interval in seconds")
	fs.StringVar(&label, "label", "com.jaeyeom.memstore.tick", "launchd job label")
	fs.StringVar(&routinesDir, "routines-dir", "", "optional routines directory to pass to tick")
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	if intervalSeconds <= 0 {
		a.errorf("--interval-seconds must be positive")
		return exitUsage
	}

	bin, err := os.Executable()
	if err != nil || bin == "" {
		bin = "memstore"
	}
	plist, err := memstore.RenderLaunchdPlist(memstore.LaunchdPlistOptions{
		Label:           label,
		IntervalSeconds: intervalSeconds,
		BinaryPath:      bin,
		ConfigPath:      a.configFlag,
		RoutinesDir:     routinesDir,
	})
	if err != nil {
		a.errorf("%v", err)
		return exitError
	}
	_, _ = a.stdout.Write(plist)
	return exitOK
}
