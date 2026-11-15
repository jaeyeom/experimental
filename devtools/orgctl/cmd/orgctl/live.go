package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/orgctl"
	"github.com/jaeyeom/experimental/devtools/orgctl/adapter/agentbus"
	"github.com/jaeyeom/experimental/devtools/orgctl/adapter/crons"
	"github.com/jaeyeom/experimental/devtools/orgctl/adapter/herdr"
)

// loadLive reads herdr, agentbus, cron files, and briefs.
// A source that cannot be read is recorded on the snapshot.
// Nothing is written.
func loadLive(spec orgctl.Spec) (orgctl.Live, error) {
	host, err := os.Hostname()
	if err != nil {
		return orgctl.Live{}, fmt.Errorf("hostname: %w", err)
	}
	live := orgctl.Live{
		Local: localMachine(spec, host),
		Now:   time.Now().UTC(),
	}
	if err := readHerdr(&live); err != nil {
		return orgctl.Live{}, err
	}
	if err := readNames(&live); err != nil {
		return orgctl.Live{}, err
	}
	readRoleFiles(spec, &live)
	return live, nil
}

func localMachine(spec orgctl.Spec, host string) string {
	names := make([]string, 0, len(spec.Machines))
	for name := range spec.Machines {
		names = append(names, name)
	}
	slices.Sort(names)
	for _, name := range names {
		machine := spec.Machines[name]
		if name == host || machine.Host == host {
			return name
		}
	}
	return ""
}

func readHerdr(live *orgctl.Live) error {
	snap, err := herdr.Read(context.Background(), nil)
	if err != nil {
		var unread *herdr.NotReadError
		if errors.As(err, &unread) {
			live.HerdrNotRead = true
			return nil
		}
		return fmt.Errorf("read herdr: %w", err)
	}
	labels := make(map[string]string, len(snap.Workspaces))
	for _, workspace := range snap.Workspaces {
		labels[workspace.ID] = workspace.Label
	}
	live.Tabs = make([]orgctl.LiveTab, 0, len(snap.Tabs))
	for _, tab := range snap.Tabs {
		workspace := labels[tab.Workspace]
		if workspace == "" {
			workspace = tab.Workspace
		}
		live.Tabs = append(live.Tabs, orgctl.LiveTab{Workspace: workspace, Label: tab.Label})
	}
	live.Agents = make([]orgctl.LiveAgent, 0, len(snap.Agents))
	for _, agent := range snap.Agents {
		live.Agents = append(live.Agents, orgctl.LiveAgent{
			Name: agent.Name, Cwd: agent.Cwd, SessionID: agent.SessionID,
		})
	}
	return nil
}

func readNames(live *orgctl.Live) error {
	names, _, err := agentbus.List(context.Background(), nil)
	if err != nil {
		var unread *agentbus.NotReadError
		if errors.As(err, &unread) {
			live.NamesNotRead = true
			return nil
		}
		return fmt.Errorf("read agentbus: %w", err)
	}
	live.Names = make([]orgctl.LiveName, 0, len(names))
	for _, name := range names {
		live.Names = append(live.Names, orgctl.LiveName{Name: name.Name, When: name.When})
	}
	return nil
}

func readRoleFiles(spec orgctl.Spec, live *orgctl.Live) {
	names := make([]string, 0, len(spec.Roles))
	for name := range spec.Roles {
		names = append(names, name)
	}
	slices.Sort(names)
	for _, name := range names {
		role := spec.Roles[name]
		if role.Machine != live.Local {
			continue
		}
		readCrons(role, live)
		readBrief(name, role, spec, live)
	}
}

func readCrons(role orgctl.Role, live *orgctl.Live) {
	schedules := make([]crons.Schedule, len(role.Schedules))
	for i, schedule := range role.Schedules {
		schedules[i] = crons.Schedule{ID: schedule.ID, Durable: schedule.Durable}
	}
	jobs, _, warnings := crons.Read(role.Cwd, schedules, crons.Config{})
	if cronUnreadable(warnings) {
		return
	}
	live.CronRead = append(live.CronRead, role.Cwd)
	for _, job := range jobs {
		live.Crons = append(live.Crons, orgctl.LiveCron{
			Cwd: role.Cwd, ID: job.ID, Cron: job.Cron,
			ExpiresAt: job.ExpiresAt, CreatedBySessionID: job.CreatedBySessionID,
		})
	}
}

func cronUnreadable(warnings []string) bool {
	for _, warning := range warnings {
		if strings.Contains(warning, "malformed file") || strings.Contains(warning, "not read") {
			return true
		}
	}
	return false
}

func readBrief(name string, role orgctl.Role, spec orgctl.Spec, live *orgctl.Live) {
	status, err := orgctl.Compare(role, spec, role.Cwd)
	if err != nil {
		return
	}
	if live.Briefs == nil {
		live.Briefs = map[string]string{}
	}
	live.Briefs[name] = status
}
