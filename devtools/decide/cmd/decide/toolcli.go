package main

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
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
			checkDecideHub,
			checkDecideNotifyTarget,
			checkDecideAgentbus,
		},
		ExtraDoctorCheckGroups: []toolcli.DoctorCheckGroup{
			checkDecideWeb,
		},
	}
}

func checkDecideWeb(ctx context.Context, offline bool) []toolcli.CheckResult {
	return web.DoctorWebChecks(ctx, web.DoctorEnv{}, offline)
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
