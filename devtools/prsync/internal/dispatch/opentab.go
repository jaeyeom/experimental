package dispatch

import (
	"context"
	"fmt"
	"path"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// agentStartMaxTimeout is the largest `herdr agent start --timeout`.
const agentStartMaxTimeout = 300 * time.Second

// agentNameMax is herdr's limit on an agent name.
const agentNameMax = 32

// TabOpener is the herdr client of the machine a new tab is opened on
// (open_missing_tabs).
type TabOpener interface {
	TabList(ctx context.Context) ([]herdr.Tab, error)
	WorkspaceList(ctx context.Context) ([]herdr.Workspace, error)
	TabCreate(ctx context.Context, workspaceID, cwd, label string) (herdr.NewTab, error)
	AgentStart(ctx context.Context, name, kind, pane string, args []string, timeout time.Duration) error
	Wait(ctx context.Context, paneID string, until []string, timeout time.Duration) herdr.PromptOutcome
	TabClose(ctx context.Context, tabID string) error
}

// tabBudget is how many more tabs this run may open.
type tabBudget struct{ left int }

func newTabBudget(cfg config.Config) *tabBudget {
	return &tabBudget{left: cfg.OpenTabsMaxPerRound}
}

// openPlan is where the tab for one untabbed PR goes. machine is empty
// for the local server.
type openPlan struct {
	machine   string
	workspace string
	cwd       string
	label     string
}

func (p openPlan) String() string {
	s := "label " + p.label
	if p.workspace != "" {
		s += ", workspace " + p.workspace
	}
	return s + ", cwd " + p.cwd
}

// planOpen decides whether the untabbed candidate c, whose result is
// item (skipped_no_tab), gets a tab. Without one it returns the item to
// report. A PR the run would not dispatch to anyway (deduped, for
// example) keeps that result, and no tab is opened for it.
func planOpen(c Candidate, cfg config.Config, st State, req Request, now time.Time, item Item, budget *tabBudget) (openPlan, Item, bool) {
	if !cfg.OpenMissingTabs || item.Action != ActionSkippedNoTab || !cfg.IncludeUntabbed() {
		return openPlan{}, item, false
	}
	if probe := Evaluate(withTab(c, scan.Tab{AgentStatus: "idle"}), cfg, st, req.Rebase, req.CIFix, req.Force, now); probe.Action != "" {
		return openPlan{}, probe, false
	}
	if c.PR.Identifier == nil {
		item.Detail = "no identifier for a tab label; not opening a tab"
		return openPlan{}, item, false
	}
	plan := openPlan{
		machine:   cfg.GateMachine(),
		label:     scan.TabLabel(cfg.TabLabelTemplate, *c.PR.Identifier),
		workspace: cfg.OpenTabWorkspace(cfg.GateMachine()),
		cwd:       cfg.OpenTabCwd(cfg.GateMachine()),
	}
	if plan.cwd == "" && plan.machine == "" {
		plan.cwd = gateConfig(cfg, c.Repo).Checkout
	}
	if plan.cwd == "" {
		return openPlan{}, openFailed(c, plan.machine, "", "no open_tabs_cwd or machine_checkouts entry for "+machineName(plan.machine)), false
	}
	if budget.left <= 0 {
		return openPlan{}, Item{
			Repo: c.Repo, Number: c.Number, Action: ActionSkippedOpenTabCap,
			Detail: fmt.Sprintf("open_tabs_max_per_round=%d reached", cfg.OpenTabsMaxPerRound),
		}, false
	}
	return plan, Item{}, true
}

func machineName(machine string) string {
	if machine == "" {
		return config.LocalMachine
	}
	return machine
}

// openIfUntabbed opens a tab for a candidate whose result item is
// skipped_no_tab, when open_missing_tabs allows it. On success it
// returns c with the new tab, an empty item, and the new tab's id.
// Otherwise it returns c unchanged and the item to report.
func openIfUntabbed(ctx context.Context, cfg config.Config, st State, req Request, now time.Time, c Candidate, item Item, budget *tabBudget) (Candidate, Item, string) {
	if item.Action != ActionSkippedNoTab {
		return c, item, ""
	}
	plan, alt, ok := planOpen(c, cfg, st, req, now, item, budget)
	if !ok {
		return c, alt, ""
	}
	tab, failed := openTab(ctx, cfg, req, c, plan, budget)
	if failed.Action != "" {
		return c, failed, ""
	}
	return withTab(c, tab), Item{}, tab.TabID
}

// openTab creates the tab on plan.machine, starts the agent in its root
// pane, and waits until herdr reports it idle. A failure after the tab
// was created closes it.
func openTab(ctx context.Context, cfg config.Config, req Request, c Candidate, plan openPlan, budget *tabBudget) (scan.Tab, Item) {
	if req.Opener == nil {
		return scan.Tab{}, openFailed(c, plan.machine, "", "no herdr client to open a tab")
	}
	o := req.Opener(plan.machine)
	tabs, err := o.TabList(ctx)
	if err != nil {
		return scan.Tab{}, openFailed(c, plan.machine, "", "list tabs: "+err.Error())
	}
	for _, t := range tabs {
		if t.Label == plan.label {
			return scan.Tab{}, Item{
				Repo: c.Repo, Number: c.Number, Action: ActionSkippedNoTab, Machine: plan.machine,
				Detail: fmt.Sprintf("tab %s is already labeled %s; not replacing it", t.TabID, plan.label),
			}
		}
	}
	workspace, err := resolveWorkspace(ctx, o, plan.workspace)
	if err != nil {
		return scan.Tab{}, openFailed(c, plan.machine, "", err.Error())
	}
	budget.left--
	created, err := o.TabCreate(ctx, workspace, plan.cwd, plan.label)
	if err != nil {
		return scan.Tab{}, openFailed(c, plan.machine, "", "create tab: "+err.Error())
	}
	runlog.FromContext(ctx).Info("open_tab", "repo", c.Repo, "number", c.Number, "machine", plan.machine,
		"tab_id", created.TabID, "pane_id", created.PaneID, "label", plan.label, "cwd", plan.cwd)
	if err := startAgent(ctx, cfg, o, plan.label, created.PaneID); err != nil {
		detail := err.Error()
		if closeErr := o.TabClose(ctx, created.TabID); closeErr != nil {
			detail += "; close tab: " + closeErr.Error()
		}
		return scan.Tab{}, openFailed(c, plan.machine, created.TabID, detail)
	}
	pane := created.PaneID
	return scan.Tab{
		TabID:       created.TabID,
		PaneID:      &pane,
		WorkspaceID: created.WorkspaceID,
		Label:       plan.label,
		AgentStatus: "idle",
		Machine:     plan.machine,
	}, Item{}
}

// startAgent starts open_tabs_agent in pane and waits for it to be idle,
// both within open_tabs_ready_timeout.
func startAgent(ctx context.Context, cfg config.Config, o TabOpener, label, pane string) error {
	timeout := cfg.OpenTabsReadyTimeout
	deadline := time.Now().Add(timeout)
	kind := path.Base(cfg.OpenTabsAgent[0])
	if err := o.AgentStart(ctx, agentName(label), kind, pane, cfg.OpenTabsAgent[1:], min(timeout, agentStartMaxTimeout)); err != nil {
		return fmt.Errorf("start %s: %w", kind, err)
	}
	remaining := time.Until(deadline)
	if remaining > 0 {
		out := o.Wait(ctx, pane, []string{"idle"}, remaining)
		if out.Status == herdr.PromptMatched {
			return nil
		}
		if out.Err != nil {
			return fmt.Errorf("%s not idle within %s: %w", kind, timeout, out.Err)
		}
	}
	return fmt.Errorf("%s not idle within %s", kind, timeout)
}

// resolveWorkspace returns the id of the workspace whose id or label is
// want. Empty want leaves the workspace to herdr.
func resolveWorkspace(ctx context.Context, o TabOpener, want string) (string, error) {
	if want == "" {
		return "", nil
	}
	list, err := o.WorkspaceList(ctx)
	if err != nil {
		return "", fmt.Errorf("list workspaces: %w", err)
	}
	var ids []string
	for _, w := range list {
		if w.WorkspaceID == want {
			return want, nil
		}
		if w.Label == want {
			ids = append(ids, w.WorkspaceID)
		}
	}
	switch len(ids) {
	case 1:
		return ids[0], nil
	case 0:
		return "", fmt.Errorf("no herdr workspace %q", want)
	default:
		return "", fmt.Errorf("herdr workspace label %q matches %s", want, strings.Join(ids, ", "))
	}
}

// agentName is a herdr agent name for the tab labeled label:
// "pr-" and the label, lowercased, with every character herdr does not
// allow replaced by "-".
func agentName(label string) string {
	mapped := strings.Map(func(r rune) rune {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '_', r == '-':
			return r
		default:
			return '-'
		}
	}, strings.ToLower(label))
	name := "pr-" + strings.Trim(mapped, "-")
	if len(name) > agentNameMax {
		name = strings.TrimRight(name[:agentNameMax], "-")
	}
	return name
}

func openFailed(c Candidate, machine, tabID, reason string) Item {
	return Item{Repo: c.Repo, Number: c.Number, Action: ActionOpenTabFailed, Machine: machine, TabID: tabID, Detail: reason}
}

// withTab is c with its PR's tab set to tab.
func withTab(c Candidate, tab scan.Tab) Candidate {
	pr := *c.PR
	if tab.PaneID == nil {
		pane := ""
		tab.PaneID = &pane
	}
	pr.Tab = &tab
	c.PR = &pr
	return c
}

// exemptNewPane keeps the tab prsync just opened (openedTab non-empty)
// from holding the gate before its prompt.
func exemptNewPane(cfg config.Config, tab *scan.Tab, openedTab string) config.Config {
	if openedTab == "" || tab == nil || tab.PaneID == nil {
		return cfg
	}
	cfg.GateExemptPanes = append(slices.Clone(cfg.GateExemptPanes), *tab.PaneID)
	return cfg
}

// markOpenedTab sets the tab prsync opened, if any, on a send result;
// a plain dispatch becomes opened_tab_and_dispatched.
func markOpenedTab(item Item, tabID string) Item {
	if tabID == "" {
		return item
	}
	item.TabID = tabID
	if item.Action == ActionDispatched {
		item.Action = ActionOpenedTabAndDispatched
	}
	return item
}

// wouldOpenTab is the dry-run result for an untabbed candidate.
func wouldOpenTab(c Candidate, cfg config.Config, st State, req Request, now time.Time, item Item, budget *tabBudget) Item {
	plan, alt, ok := planOpen(c, cfg, st, req, now, item, budget)
	if !ok {
		return alt
	}
	budget.left--
	return Item{
		Repo:           c.Repo,
		Number:         c.Number,
		Action:         ActionWouldOpenTab,
		Machine:        plan.machine,
		RenderedPrompt: renderPrompt(cfg, req, *c.PR),
		Detail:         plan.String(),
	}
}
