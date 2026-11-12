package main

import (
	"bytes"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/leasecache"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// leaseHookTimeout bounds the hook's daemon call; past it the hook
// allows the tool call. Tests shorten it.
var leaseHookTimeout = 3 * time.Second

const (
	leaseHookScriptName = "agentbus-lease.sh"
	// leaseHookMatcher is the PreToolUse matcher of the lease hook.
	leaseHookMatcher = "Edit|Write|MultiEdit|NotebookEdit|Bash"
)

// leaseHookScript is the lease PreToolUse command. Like the other hook
// scripts it lets the call through when agentbus is missing or fails.
const leaseHookScript = `#!/bin/sh
# Refuse Claude Code edits and state-changing git commands in a guarded
# checkout unless this pane holds the agentbus lease. Fail open.
set -eu
if ! command -v agentbus >/dev/null 2>&1; then
  exit 0
fi
agentbus lease hook || exit 0
exit 0
`

// leaseHook is the Claude Code PreToolUse hook. It always exits 0. When
// the tool call would change a guarded checkout this pane does not hold,
// it prints a deny decision with the reason and the claim hint. A path
// outside every guarded checkout is decided from the config alone. In
// hub mode the daemon is the hub's, over the tunnel or ssh. When the
// daemon cannot tell within leaseHookTimeout, it allows the call with a
// warning on stderr. A "you hold it" answer is cached for leasecache.TTL.
func leaseHook(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	warn := func(format string, a ...any) {
		fmt.Fprintf(stderr, "agentbus lease hook: "+format+"; allowing\n", a...)
	}
	if len(args) != 0 {
		warn("unexpected arguments")
		return 0
	}
	raw, err := io.ReadAll(io.LimitReader(stdin, 1<<20))
	if err != nil || len(bytes.TrimSpace(raw)) == 0 {
		return 0
	}
	tool, targets := hookTargets(raw, env)
	if len(targets) == 0 {
		return 0
	}
	cfg, err := hubSettings(env)
	if err != nil {
		warn("%s", err)
		return 0
	}
	roots := guardedRoots(targets, cfg.LeaseCheckouts, env, warn)
	if len(roots) == 0 {
		return 0
	}
	machine, err := leaseMachine(cfg, env)
	if err != nil {
		warn("%s", err)
		return 0
	}
	checks := make([]lease.Args, len(roots))
	for i, root := range roots {
		checks[i] = lease.Args{Checkout: machine + ":" + root.root, Hook: true, Tool: tool, Target: root.target}
	}
	// In hub mode the whole decision, reaching the hub included, is
	// bounded by leaseHookTimeout.
	route := func(ctx context.Context) (context.Context, error) { return ctx, nil }
	if cfg.Hub != "" {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, leaseHookTimeout)
		defer cancel()
		route = func(ctx context.Context) (context.Context, error) {
			return leaseHubRoute(ctx, cfg, env, true, io.Discard)
		}
	}
	if reason, denied := hookDecide(ctx, env, checks, route, warn); denied {
		printDeny(stdout, reason)
	}
	return 0
}

// hookTarget is a place a tool call would change: a file, or where a
// git command runs and its subcommand.
type hookTarget struct {
	path string
	// git is the git command, such as "git switch"; empty for a file.
	git string
}

// guardedRoot is a guarded checkout's top-level and the short target of
// the tool call in it: the file path relative to root, or the git
// command.
type guardedRoot struct {
	root, target string
}

// guardedRoots returns the guarded checkouts that targets are in, from
// the local config and git alone, each with the first target in it. A
// target it cannot place is warned about and skipped.
func guardedRoots(targets []hookTarget, guarded, env []string, warn func(string, ...any)) []guardedRoot {
	var roots []guardedRoot
	for _, target := range targets {
		root, err := checkout.Resolve(target.path, guarded, env)
		if err != nil {
			warn("%s", err)
			continue
		}
		if root == "" || slices.ContainsFunc(roots, func(g guardedRoot) bool { return g.root == root }) {
			continue
		}
		short := target.git
		if short == "" {
			short = relTarget(root, target.path)
		}
		roots = append(roots, guardedRoot{root: root, target: short})
	}
	return roots
}

// relTarget is path relative to root, or path itself when it is not
// under root as written.
func relTarget(root, path string) string {
	rel, err := filepath.Rel(root, path)
	if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return path
	}
	return rel
}

// hookDecide checks each checkout, from the cache or the daemon that
// route leads to, and returns the reason for the first one the caller
// may not change. route runs once, at the first checkout the cache does
// not answer. A checkout the daemon cannot decide is warned about and
// allowed.
func hookDecide(ctx context.Context, env []string, checks []lease.Args, route func(context.Context) (context.Context, error), warn func(string, ...any)) (string, bool) {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		warn("%s", err)
		return "", false
	}
	cache := leasecache.Cache{Dir: dirs.LeaseCacheDir()}
	pane := cachePane(env)
	var routed context.Context
	for _, args := range checks {
		key := args.Checkout
		if cache.Allowed(key, pane) {
			continue
		}
		if routed == nil {
			if routed, err = route(ctx); err != nil {
				warn("%s", err)
				return "", false
			}
		}
		started := time.Now()
		r, err := hookCheck(routed, env, args)
		if err != nil {
			warn("%s", err)
			continue
		}
		if r.Result == lease.ResultAllowed {
			// A cache that cannot be written only costs the next call a round trip.
			_ = cache.Put(key, pane, started)
			continue
		}
		return denyReason(*r.Lease, r.Caller), true
	}
	return "", false
}

// hookCheck asks the daemon whether the caller may change the checkout,
// within leaseHookTimeout. args marks the check as the hook's, so the
// daemon logs a denial. It never starts the daemon.
func hookCheck(ctx context.Context, env []string, args lease.Args) (lease.Reply, error) {
	callCtx, cancel := context.WithTimeout(ctx, leaseHookTimeout)
	defer cancel()
	resp, err := dialBus(callCtx, env, daemon.Request{Op: "lease-check", Env: env, Lease: &args})
	if err != nil {
		return lease.Reply{}, fmt.Errorf("agentbusd did not answer: %w", err)
	}
	if !resp.OK || resp.Lease == nil || resp.Lease.Lease == nil {
		return lease.Reply{}, fmt.Errorf("lease check failed: %s", resp.Error)
	}
	return *resp.Lease, nil
}

// cachePane names the caller for the cache: its hub-mode handle, or its
// herdr pane. Empty means the caller is not cached.
func cachePane(env []string) string {
	if handle := envGet(env, herdr.AsHandleEnv); handle != "" {
		return handle
	}
	pane := envGet(env, "HERDR_PANE_ID")
	if pane == "" {
		return ""
	}
	return envGet(env, "HERDR_SOCKET_PATH") + "#" + pane
}

func printDeny(stdout io.Writer, reason string) {
	out, err := json.Marshal(map[string]any{
		"hookSpecificOutput": map[string]string{
			"hookEventName":            "PreToolUse",
			"permissionDecision":       "deny",
			"permissionDecisionReason": "agentbus lease: " + reason,
		},
	})
	if err != nil {
		return
	}
	fmt.Fprintf(stdout, "%s\n", out)
}

// hookTargets reads a PreToolUse input and returns the paths the call
// would change: the file of Edit, Write, MultiEdit and NotebookEdit, and
// where Bash runs a state-changing git command. Relative paths are taken
// from the input's cwd. It also returns the tool name.
func hookTargets(raw []byte, env []string) (string, []hookTarget) {
	var doc map[string]any
	if err := json.Unmarshal(raw, &doc); err != nil {
		return "", nil
	}
	tool, _ := doc["tool_name"].(string)
	input, _ := doc["tool_input"].(map[string]any)
	cwd, _ := doc["cwd"].(string)
	if cwd == "" {
		wd, err := os.Getwd()
		if err != nil {
			return tool, nil
		}
		cwd = wd
	}
	switch tool {
	case "Edit", "Write", "MultiEdit":
		return tool, toolPath(cwd, input, "file_path")
	case "NotebookEdit":
		return tool, toolPath(cwd, input, "notebook_path")
	case "Bash":
		command, _ := input["command"].(string)
		var targets []hookTarget
		for _, c := range gitChangeList(command, cwd, envGet(env, "HOME")) {
			targets = append(targets, hookTarget{path: c.dir, git: "git " + c.sub})
		}
		return tool, targets
	}
	return tool, nil
}

func toolPath(cwd string, input map[string]any, key string) []hookTarget {
	path, _ := input[key].(string)
	if strings.TrimSpace(path) == "" {
		return nil
	}
	return []hookTarget{{path: joinDir(cwd, path)}}
}

// leaseInstallHook prints the lease hook script and settings entry, and
// with --write installs them in the user's Claude Code settings.
func leaseInstallHook(args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("lease install-hook", flag.ContinueOnError)
	fs.SetOutput(stderr)
	write := fs.Bool("write", false, "write the script and add it to the user's Claude Code settings")
	if err := fs.Parse(args); err != nil {
		if err == flag.ErrHelp {
			return 0
		}
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "agentbus lease install-hook: unexpected arguments")
		return 2
	}
	home, err := homeFrom(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease install-hook: %s\n", err)
		return 1
	}
	scriptPath := filepath.Join(home, ".claude", "hooks", leaseHookScriptName)
	settingsPath := filepath.Join(home, ".claude", "settings.json")
	if !*write {
		entry, err := json.MarshalIndent(leaseHookEntry(scriptPath), "      ", "  ")
		if err != nil {
			fmt.Fprintf(stderr, "agentbus lease install-hook: %s\n", err)
			return 1
		}
		fmt.Fprintf(stdout, "Nothing was written. To install the lease hook, save this as %s (mode 0755):\n\n%s\n", scriptPath, indent(strings.TrimSuffix(leaseHookScript, "\n")))
		fmt.Fprintf(stdout, "\nand add this entry to hooks.PreToolUse in %s:\n\n      %s\n", settingsPath, entry)
		fmt.Fprintln(stdout, "\nOr run 'agentbus lease install-hook --write' to do both.")
		return 0
	}
	if _, err := writeHookScript(home, leaseHookScriptName, leaseHookScript); err != nil {
		fmt.Fprintf(stderr, "agentbus lease install-hook: %s\n", err)
		return 1
	}
	if err := mergeLeaseHook(settingsPath, scriptPath); err != nil {
		fmt.Fprintf(stderr, "agentbus lease install-hook: %s\n", err)
		return 1
	}
	fmt.Fprintf(stdout, "installed hook %s in %s\n", scriptPath, settingsPath)
	return 0
}

func leaseHookEntry(command string) map[string]any {
	return map[string]any{
		"matcher": leaseHookMatcher,
		"hooks": []any{
			map[string]any{"type": "command", "command": command},
		},
	}
}

// mergeLeaseHook adds the lease hook to the settings at path once. An
// entry that already runs the script gets the current matcher and
// command; other entries are left alone.
func mergeLeaseHook(path, command string) error {
	doc, err := readSettings(path)
	if err != nil {
		return err
	}
	hooks, err := ensureObject(doc, "hooks")
	if err != nil {
		return err
	}
	pre, err := ensureList(hooks, "PreToolUse")
	if err != nil {
		return err
	}
	found := false
	for _, item := range pre {
		if entry, ok := item.(map[string]any); ok && updateCommand(entry, command, leaseHookScriptName) {
			entry["matcher"] = leaseHookMatcher
			found = true
			break
		}
	}
	if !found {
		hooks["PreToolUse"] = append(pre, leaseHookEntry(command))
	}
	data, err := encodeSettings(doc)
	if err != nil {
		return err
	}
	return writeAtomic(path, data, 0o600)
}
