package main

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
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
	attachServers(&live)
	readRoleFiles(spec, &live)
	return live, nil
}

// loadImportLive reads herdr, agentbus and each agent cwd's cron file.
// There is no spec yet. Nothing is written except by import's --out.
func loadImportLive() (orgctl.Live, error) {
	live := orgctl.Live{Now: time.Now().UTC()}
	if err := readHerdr(&live); err != nil {
		return orgctl.Live{}, err
	}
	if err := readNames(&live); err != nil {
		return orgctl.Live{}, err
	}
	attachServers(&live)
	readImportCrons(&live)
	return live, nil
}

func attachServers(live *orgctl.Live) {
	servers := make(map[string]string, len(live.Names))
	for _, name := range live.Names {
		servers[name.Name] = name.Server
	}
	for i, agent := range live.Agents {
		if agent.Machine == "" {
			live.Agents[i].Machine = servers[agent.Name]
		}
	}
}

func readImportCrons(live *orgctl.Live) {
	seen := map[string]bool{}
	for _, agent := range live.Agents {
		if agent.Cwd == "" || seen[agent.Cwd] {
			continue
		}
		seen[agent.Cwd] = true
		jobs, _, warnings := crons.Read(agent.Cwd, nil, crons.Config{})
		if cronUnreadable(warnings) {
			continue
		}
		for _, job := range jobs {
			live.Crons = append(live.Crons, orgctl.LiveCron{
				Cwd: agent.Cwd, ID: job.ID, Cron: job.Cron, Prompt: job.Prompt,
				ExpiresAt: job.ExpiresAt, CreatedBySessionID: job.CreatedBySessionID,
			})
		}
	}
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
	paneCwd := make(map[string]string, len(snap.Panes))
	for _, pane := range snap.Panes {
		paneCwd[pane.ID] = pane.Cwd
	}
	tabByID := make(map[string]herdr.Tab, len(snap.Tabs))
	for _, tab := range snap.Tabs {
		tabByID[tab.ID] = tab
	}
	live.Agents = make([]orgctl.LiveAgent, 0, len(snap.Agents))
	for _, agent := range snap.Agents {
		cwd := paneCwd[agent.Pane]
		if cwd == "" {
			cwd = agent.Cwd
		}
		tab := tabByID[agent.Tab]
		workspace := labels[tab.Workspace]
		if workspace == "" {
			workspace = tab.Workspace
		}
		live.Agents = append(live.Agents, orgctl.LiveAgent{
			Name: agent.Name, Cwd: cwd, SessionID: agent.SessionID,
			Kind: agent.Kind, Workspace: workspace, Tab: tab.Label,
			Pane: agent.Pane,
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
		live.Names = append(live.Names, orgctl.LiveName{
			Name: name.Name, When: name.When, Server: name.Server,
			SessionID: name.SessionID, Liveness: name.Liveness,
			Pane: name.Pane,
		})
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
	cwd, err := orgctl.ExpandPath(role.Cwd)
	if err != nil || pathSeen(live.CronRead, cwd) || pathSeen(live.MissingCwd, cwd) {
		return
	}
	info, err := os.Stat(cwd)
	if err != nil || !info.IsDir() {
		if err == nil || errors.Is(err, fs.ErrNotExist) {
			live.MissingCwd = append(live.MissingCwd, cwd)
		}
		return
	}
	schedules := make([]crons.Schedule, len(role.Schedules))
	for i, schedule := range role.Schedules {
		schedules[i] = crons.Schedule{ID: schedule.ID, Durable: schedule.Durable}
	}
	jobs, _, warnings := crons.Read(cwd, schedules, crons.Config{})
	if cronUnreadable(warnings) {
		return
	}
	live.CronRead = append(live.CronRead, cwd)
	for _, job := range jobs {
		live.Crons = append(live.Crons, orgctl.LiveCron{
			Cwd: cwd, ID: job.ID, Cron: job.Cron, Prompt: job.Prompt,
			ExpiresAt: job.ExpiresAt, CreatedBySessionID: job.CreatedBySessionID,
		})
	}
}

func pathSeen(paths []string, cwd string) bool {
	for _, item := range paths {
		if item == cwd {
			return true
		}
	}
	return false
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
