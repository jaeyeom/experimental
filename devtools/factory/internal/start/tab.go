package start

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// validateTabRef rejects shapes that TabRef would panic on.
// TabRef returns no error, so the check has to happen before it is called.
func validateTabRef(it source.Item, cfg config.Source, kind string) error {
	if kind != "github" {
		return nil
	}
	if _, _, err := source.ParseGitHubID(it.ID); err != nil {
		return fmt.Errorf("start: %w", err)
	}
	if cfg.Workspace != "" {
		return nil
	}
	owner, name, ok := strings.Cut(cfg.Repo, "/")
	if !ok || owner == "" || name == "" || strings.Contains(name, "/") ||
		strings.ContainsAny(owner, " \t") || strings.ContainsAny(name, " \t") {
		return fmt.Errorf("start: expected owner/repo, found %q", cfg.Repo)
	}
	return nil
}

func (s *Starter) existingTab(ctx context.Context, it source.Item) error {
	wsLabel, tabLabel := ready.TabRef(it, s.Cfg, s.Kind)
	tabs, err := s.Herdr.Tabs(ctx)
	if err != nil {
		return fmt.Errorf("list tabs: %w", err)
	}
	switch s.Kind {
	case "jira":
		return tabTaken(tabs, "", tabLabel, "")
	case "github":
		workspaces, err := s.Herdr.Workspaces(ctx)
		if err != nil {
			return fmt.Errorf("list workspaces: %w", err)
		}
		return githubTabTaken(workspaces, tabs, wsLabel, tabLabel)
	default:
		return fmt.Errorf("start: expected kind jira or github, found %q", s.Kind)
	}
}

func githubTabTaken(workspaces []herdr.Workspace, tabs []herdr.Tab, wsLabel, tabLabel string) error {
	var ids []string
	for _, ws := range workspaces {
		if ws.Label == wsLabel {
			ids = append(ids, ws.ID)
		}
	}
	for _, id := range ids {
		if err := tabTaken(tabs, id, tabLabel, wsLabel); err != nil {
			return err
		}
	}
	return nil
}

func tabTaken(tabs []herdr.Tab, workspaceID, label, wsLabel string) error {
	_, ok, err := herdr.FindTab(tabs, workspaceID, label)
	if err != nil {
		return fmt.Errorf("tab %q: %w", label, err)
	}
	if !ok {
		return nil
	}
	if wsLabel == "" {
		return fmt.Errorf("tab %q already exists", label)
	}
	return fmt.Errorf("tab %q already exists in workspace %q", label, wsLabel)
}
