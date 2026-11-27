package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/web"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newDecideTool() toolcli.Tool {
	return toolcli.Tool{
		Name:          "decide",
		MainPackage:   "github.com/jaeyeom/experimental/devtools/decide/cmd/decide",
		DaemonService: "decide follow",
		SkillText:     skillText,
		ConfigPath:    decide.ConfigPath,
		CheckConfig: func() error {
			if _, err := decide.LoadConfig(); err != nil {
				return fmt.Errorf("load decide config: %w", err)
			}
			return nil
		},
		ExtraDoctorChecks: []toolcli.DoctorCheck{
			checkDecideKeys,
			checkDecideHub,
			checkDecideNotifyTarget,
			checkDecideAgentbus,
			checkDecidePanels,
			checkDecideDuplicateAgents,
		},
		ExtraDoctorCheckGroups: []toolcli.DoctorCheckGroup{
			checkDecideWeb,
		},
	}
}

func checkDecideWeb(ctx context.Context, offline bool) []toolcli.CheckResult {
	return web.DoctorWebChecks(ctx, web.DoctorEnv{}, offline)
}

func checkDecideKeys(_ context.Context, _ bool) toolcli.CheckResult {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "keys",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix ui.keys in decide config.json",
		}
	}
	if warnings := decide.RemovedKeyWarnings(cfg.UI.Keys); len(warnings) > 0 {
		return toolcli.CheckResult{
			Name:   "keys",
			Status: toolcli.StatusWarn,
			Detail: strings.Join(warnings, "; "),
			Hint:   "remove the removed action(s) from ui.keys in config.json",
		}
	}
	return toolcli.CheckResult{
		Name:   "keys",
		Status: toolcli.StatusOK,
		Detail: "ok",
	}
}

func checkDecideHub(ctx context.Context, offline bool) toolcli.CheckResult {
	settings, err := hubConfig("")
	if err != nil {
		return toolcli.CheckResult{
			Name:   "hub",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix hub or hubTimeout in decide config.json",
		}
	}
	if settings.hub.Target == "" {
		return toolcli.CheckResult{
			Name:   "hub",
			Status: toolcli.StatusOK,
			Detail: "not configured (local mode)",
		}
	}
	if offline {
		return toolcli.CheckResult{
			Name:   "hub",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("skipped reachability check for %s (--offline)", settings.hub.Target),
			Hint:   "run without --offline to verify hub connectivity",
		}
	}
	var outBuf, errBuf bytes.Buffer
	code, err := settings.hub.Run(ctx, hubcall.Call{
		Args:   []string{"version"},
		Stdout: &outBuf,
		Stderr: &errBuf,
	})
	if err != nil || code != 0 {
		detail := fmt.Sprintf("%s unreachable", settings.hub.Target)
		if err != nil {
			detail = fmt.Sprintf("%s: %v", settings.hub.Target, err)
		} else if msg := strings.TrimSpace(errBuf.String()); msg != "" {
			detail = fmt.Sprintf("%s: %s", settings.hub.Target, msg)
		}
		return toolcli.CheckResult{
			Name:   "hub",
			Status: toolcli.StatusFail,
			Detail: detail,
			Hint:   "verify ssh connectivity and that decide is installed on the hub",
		}
	}
	return toolcli.CheckResult{
		Name:   "hub",
		Status: toolcli.StatusOK,
		Detail: settings.hub.Target + " reachable",
	}
}

func checkDecideNotifyTarget(_ context.Context, _ bool) toolcli.CheckResult {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "notify-target",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix decide config.json",
		}
	}
	hasTemplate := strings.TrimSpace(cfg.NotifyTemplate) != ""
	if !hasTemplate && len(cfg.Targets) == 0 {
		return toolcli.CheckResult{
			Name:   "notify-target",
			Status: toolcli.StatusWarn,
			Detail: "no notifyTemplate or targets configured",
			Hint:   "set notifyTemplate or targets in config.json to deliver answer notifications",
		}
	}
	var parts []string
	if hasTemplate {
		parts = append(parts, "notifyTemplate configured")
	}
	if len(cfg.Targets) > 0 {
		parts = append(parts, fmt.Sprintf("%d target(s)", len(cfg.Targets)))
	}
	return toolcli.CheckResult{
		Name:   "notify-target",
		Status: toolcli.StatusOK,
		Detail: strings.Join(parts, ", "),
	}
}

func checkDecideAgentbus(_ context.Context, _ bool) toolcli.CheckResult {
	if path, err := exec.LookPath("agentbus"); err == nil {
		return toolcli.CheckResult{
			Name:   "agentbus",
			Status: toolcli.StatusOK,
			Detail: path,
		}
	}
	cfg, err := decide.LoadConfig()
	if err == nil {
		if reasons := agentbusConfigReasons(cfg); len(reasons) > 0 {
			return toolcli.CheckResult{
				Name:   "agentbus",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("agentbus not found on PATH (required by %s)", strings.Join(reasons, ", ")),
				Hint:   "install agentbus and ensure it is on PATH",
			}
		}
	}
	return toolcli.CheckResult{
		Name:   "agentbus",
		Status: toolcli.StatusWarn,
		Detail: "agentbus not found on PATH (optional)",
		Hint:   "install agentbus to enable bus messaging and lease checks",
	}
}

func agentbusConfigReasons(cfg decide.Config) []string {
	var reasons []string
	if cfg.Recipients.Agentbus {
		reasons = append(reasons, "recipients.agentbus")
	}
	if len(cfg.Checkouts) > 0 {
		reasons = append(reasons, "checkouts")
	}
	if hasAgentbusTarget(cfg) {
		reasons = append(reasons, "agentbus: target")
	}
	return reasons
}

func hasAgentbusTarget(cfg decide.Config) bool {
	if strings.HasPrefix(strings.TrimSpace(cfg.NotifyPane), decide.BusPanePrefix) {
		return true
	}
	for _, tg := range cfg.Targets {
		if strings.HasPrefix(strings.TrimSpace(tg.Pane), decide.BusPanePrefix) {
			return true
		}
	}
	return false
}

func checkDecidePanels(ctx context.Context, offline bool) toolcli.CheckResult {
	settings, err := hubConfig("")
	if err != nil {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix hub config in decide config.json",
		}
	}
	if settings.hub.Target != "" {
		return checkRemotePanels(ctx, settings.hub, offline)
	}
	dir, err := decide.DefaultDir()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusWarn,
			Detail: err.Error(),
		}
	}
	store, err := decide.OpenPanels(dir)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
		}
	}
	entries, err := store.List()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
		}
	}
	return summarizePanelsDoctor(entries)
}

func checkRemotePanels(ctx context.Context, hub hubcall.Hub, offline bool) toolcli.CheckResult {
	if offline {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("skipped remote panel check on %s (--offline)", hub.Target),
			Hint:   "run without --offline to check panels on the hub",
		}
	}
	var outBuf, errBuf bytes.Buffer
	code, err := hub.Run(ctx, hubcall.Call{
		Args:   []string{"panel", "ls", "--json"},
		Stdout: &outBuf,
		Stderr: &errBuf,
	})
	if err != nil || code != 0 {
		detail := fmt.Sprintf("check panels on %s failed", hub.Target)
		if err != nil {
			detail = fmt.Sprintf("check panels on %s: %v", hub.Target, err)
		} else if msg := strings.TrimSpace(errBuf.String()); msg != "" {
			detail = fmt.Sprintf("check panels on %s: %s", hub.Target, msg)
		}
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: detail,
			Hint:   "verify decide is updated on the hub",
		}
	}
	var entries []decide.PanelEntry
	if err := json.Unmarshal(outBuf.Bytes(), &entries); err != nil {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: fmt.Sprintf("decode remote panels: %v", err),
		}
	}
	return summarizePanelsDoctor(entries)
}

func summarizePanelsDoctor(entries []decide.PanelEntry) toolcli.CheckResult {
	var bad []string
	var stale []string
	for _, e := range entries {
		if e.Unreadable {
			bad = append(bad, panelBadDetail(e))
			continue
		}
		if e.Stale {
			stale = append(stale, e.ID)
		}
	}
	if len(bad) > 0 {
		detail := fmt.Sprintf("bad panel(s): %s", strings.Join(bad, "; "))
		if len(stale) > 0 {
			detail += fmt.Sprintf("; stale: %s", strings.Join(stale, ", "))
		}
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusFail,
			Detail: detail,
			Hint:   "fix or remove invalid panel files in panels/",
		}
	}
	if len(stale) > 0 {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("stale panel(s): %s", strings.Join(stale, ", ")),
			Hint:   "refresh stale panels with decide panel set",
		}
	}
	if len(entries) == 0 {
		return toolcli.CheckResult{
			Name:   "panels",
			Status: toolcli.StatusOK,
			Detail: "no panels",
		}
	}
	return toolcli.CheckResult{
		Name:   "panels",
		Status: toolcli.StatusOK,
		Detail: fmt.Sprintf("%d panel(s) ok", len(entries)),
	}
}

func checkDecideDuplicateAgents(ctx context.Context, offline bool) toolcli.CheckResult {
	settings, err := hubConfig("")
	if err != nil {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix decide config.json",
		}
	}
	if settings.hub.Target != "" {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusOK,
			Detail: fmt.Sprintf("skipped (ledger is on hub %s)", settings.hub.Target),
		}
	}
	dir, err := decide.DefaultDir()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusWarn,
			Detail: err.Error(),
		}
	}
	if _, err := os.Stat(filepath.Join(dir, "ledger.json")); errors.Is(err, fs.ErrNotExist) {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusOK,
			Detail: "no ledger",
		}
	}
	led, err := decide.Open(dir)
	if err != nil {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusWarn,
			Detail: err.Error(),
		}
	}
	items, err := led.List()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusWarn,
			Detail: err.Error(),
		}
	}
	cfg, err := decide.LoadConfig()
	if err != nil {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusFail,
			Detail: err.Error(),
			Hint:   "fix decide config.json",
		}
	}
	var bus []decide.BusRecipient
	if !offline {
		bus = statsBusRecipients(ctx, cfg)
	}
	res := decide.NewNameResolver(cfg, bus)
	dups := decide.FindUnaliasedDuplicates(items, res, decide.DefaultDuplicateAgentWindow)
	if len(dups) == 0 {
		return toolcli.CheckResult{
			Name:   "agent-aliases",
			Status: toolcli.StatusOK,
			Detail: "no unaliased duplicate agents",
		}
	}
	parts := make([]string, 0, len(dups))
	for _, d := range dups {
		loc := "pane " + d.Pane
		if d.Machine != "" {
			loc = fmt.Sprintf("pane %s@%s", d.Pane, d.Machine)
		}
		parts = append(parts, fmt.Sprintf("%s on %s", strings.Join(d.Names, " and "), loc))
	}
	return toolcli.CheckResult{
		Name:   "agent-aliases",
		Status: toolcli.StatusWarn,
		Detail: fmt.Sprintf("likely unaliased duplicate agents: %s", strings.Join(parts, "; ")),
		Hint:   "merge duplicates with `decide contacts alias <alias> <canonical>` or contactAliases in config.json",
	}
}
