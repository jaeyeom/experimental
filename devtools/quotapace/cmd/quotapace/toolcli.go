package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func newQuotapaceTool() toolcli.Tool {
	return toolcli.Tool{
		Name:                   "quotapace",
		ModulePath:             "github.com/jaeyeom/experimental",
		MainPackage:            "github.com/jaeyeom/experimental/devtools/quotapace/cmd/quotapace",
		SkillText:              skillText,
		ExtraDoctorCheckGroups: []toolcli.DoctorCheckGroup{checkScheduleResets},
	}
}

func checkScheduleResets(_ context.Context, _ bool) []toolcli.CheckResult {
	dir, err := quotapace.DataDir(os.Getenv)
	if err != nil {
		return nil
	}
	store := quotapace.Store{Dir: dir}
	profiles, err := store.Profiles()
	if err != nil || len(profiles) == 0 {
		return nil
	}
	snap, err := store.Status(profiles, time.Now(), quotapace.StatusOptions{})
	if err != nil {
		return []toolcli.CheckResult{{
			Name:   "schedule-reset",
			Status: toolcli.StatusWarn,
			Detail: fmt.Sprintf("cannot inspect profiles: %v", err),
		}}
	}
	var checks []toolcli.CheckResult
	for _, ws := range snap.Windows() {
		for _, w := range ws.Warnings {
			checks = append(checks, toolcli.CheckResult{
				Name:   fmt.Sprintf("schedule-reset-%s-%s", ws.Profile, ws.Window),
				Status: toolcli.StatusWarn,
				Detail: fmt.Sprintf("%s %s: %s", ws.Profile, ws.Window, w),
				Hint:   fmt.Sprintf("verify --reset/--tz with `quotapace plan set --profile %s --window %s ...`", ws.Profile, ws.Window),
			})
		}
	}
	return checks
}
