package decide

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// Owner location states.
const (
	// LocationLive is a pane herdr lists now.
	LocationLive = "live"
	// LocationClosed is a pane herdr no longer lists.
	LocationClosed = "closed"
	// LocationUnknown is an item without a pane, a pane not looked up
	// yet, or a pane herdr could not be asked about.
	LocationUnknown = "unknown"
	// LocationUnreachable is a pane on a machine that did not answer.
	LocationUnreachable = "unreachable"
	// LocationPull is a pull-delivered item without a herdr pane.
	LocationPull = "pull"
	// LocationTarget is a configured target (target:<name>) without a herdr pane.
	LocationTarget = "target"
)

// OwnerLocation is where an item's owner pane lives in herdr or a custom
// transport plugin. Machine is empty for this machine. Workspace and Tab are
// labels, and WorkspaceID and TabID the herdr ids, set only for a live herdr
// pane. Label is an optional human-readable label returned by a transport plugin.
type OwnerLocation struct {
	Machine     string `json:"machine"`
	Workspace   string `json:"workspace"`
	Tab         string `json:"tab"`
	Pane        string `json:"pane"`
	State       string `json:"state"`
	Label       string `json:"label,omitempty"`
	WorkspaceID string `json:"workspaceId,omitempty"`
	TabID       string `json:"tabId,omitempty"`
}

// LocalMachine is this machine's herdr label: DECIDE_MACHINE, else the
// config machine.
func LocalMachine(cfg Config) string {
	if env := strings.TrimSpace(os.Getenv("DECIDE_MACHINE")); env != "" {
		return env
	}
	return strings.TrimSpace(cfg.Machine)
}

// OwnerPane is the not yet resolved location of item's stored pane. A
// machine equal to local, this machine's label, is this machine.
func OwnerPane(item *Item, local string) OwnerLocation {
	target := ParseTarget(item.NotifyPane, item.NotifyMachine)
	switch target.Scheme {
	case SchemePull:
		return OwnerLocation{Pane: target.String(), State: LocationPull}
	case SchemeTarget:
		return OwnerLocation{Pane: target.String(), State: LocationTarget}
	case SchemeAgentbus:
		return OwnerLocation{Pane: target.String(), State: LocationUnknown}
	case SchemeHerdr:
		machine := target.Machine
		if machine == local {
			machine = ""
		}
		return OwnerLocation{Machine: machine, Pane: target.Address, State: LocationUnknown}
	case "":
		return OwnerLocation{State: LocationUnknown}
	default:
		return OwnerLocation{Machine: target.Machine, Pane: target.String(), State: LocationUnknown}
	}
}

// Text is the location as the item view shows it, such as
// "mac: experimental / review (w3:p7)".
func (l OwnerLocation) Text() string {
	if l.State == LocationPull {
		return "waiting (pull)"
	}
	if l.State == LocationTarget {
		return l.Pane
	}
	if l.Pane == "" {
		return "unknown"
	}
	prefix := ""
	if l.Machine != "" {
		prefix = l.Machine + ": "
	}
	id := "(" + l.Pane + ")"
	switch l.State {
	case LocationLive:
		if l.Workspace != "" || l.Tab != "" {
			return prefix + l.Workspace + " / " + l.Tab + " " + id
		}
		if l.Label != "" {
			return prefix + l.Label + " " + id
		}
		return prefix + id
	case LocationClosed:
		if l.Label != "" {
			return prefix + l.Label + " " + id + " — closed"
		}
		return prefix + id + " — tab closed"
	case LocationUnreachable:
		return prefix + id + " (unreachable)"
	default:
		if l.Label != "" {
			return prefix + l.Label + " " + id
		}
		return prefix + id
	}
}

// OwnerAt is the item view's owner line value: the owner and where it lives.
func OwnerAt(owner string, loc OwnerLocation) string {
	return owner + " at " + loc.Text()
}

// HerdrCall runs `herdr args...` on machine, empty for this machine, and
// returns stdout. A machine that cannot be reached fails with a
// *remotecall.UnreachableError in the error chain.
type HerdrCall func(ctx context.Context, machine string, args []string) (string, error)

// LocationResolver looks up owner panes in herdr or configured transport
// plugins. A nil Run calls herdr. A nil Transports loads transports from
// config.json on demand when a custom scheme is encountered.
type LocationResolver struct {
	Run        HerdrCall
	Transports map[string][]string
}

// Resolve fills in each location from one `herdr pane list`, `tab list`,
// and `workspace list` per machine, or by querying a configured transport
// plugin's `status` op for custom schemes. Locations without a herdr pane or
// registered plugin stay as they are. A machine herdr cannot be asked about
// leaves its panes unknown, or unreachable when the machine does not answer.
func (r LocationResolver) Resolve(ctx context.Context, locs []OwnerLocation) []OwnerLocation {
	out := make([]OwnerLocation, len(locs))
	copy(out, locs)
	snapshots := map[string]*herdrSnapshot{}
	failures := map[string]string{}
	transports := r.Transports
	loadedTransports := transports != nil
	for i, loc := range out {
		target := ParseTarget(loc.Pane, loc.Machine)
		if handled := r.resolveSpecial(ctx, target, &out[i], &transports, &loadedTransports); handled {
			continue
		}
		if target.Address == "" {
			continue
		}
		loc.Pane = target.Address
		out[i].Pane = target.Address
		snap, state, failed := r.machineSnapshot(ctx, loc.Machine, snapshots, failures)
		if failed {
			out[i].State = state
			continue
		}
		out[i] = snap.locate(loc)
	}
	return out
}

func (r LocationResolver) resolveSpecial(ctx context.Context, target Target, loc *OwnerLocation, transports *map[string][]string, loaded *bool) bool {
	switch target.Scheme {
	case SchemePull:
		loc.State = LocationPull
		loc.Machine = ""
		return true
	case SchemeTarget:
		loc.State = LocationTarget
		loc.Machine = ""
		return true
	case SchemeAgentbus, "":
		return true
	case SchemeHerdr:
		return false
	default:
		r.resolvePlugin(ctx, target, loc, transports, loaded)
		return true
	}
}

func (r LocationResolver) resolvePlugin(ctx context.Context, target Target, loc *OwnerLocation, transports *map[string][]string, loaded *bool) {
	if !*loaded {
		if cfg, err := LoadConfig(); err == nil {
			*transports = cfg.Transports
		}
		*loaded = true
	}
	argv, ok := (*transports)[target.Scheme]
	if !ok || len(argv) == 0 {
		loc.State = LocationUnknown
		return
	}
	resp, err := pluginStatus(ctx, argv, target)
	if err != nil {
		loc.State = LocationUnknown
		return
	}
	loc.State = resp.State
	loc.Label = resp.Label
}

func (r LocationResolver) machineSnapshot(ctx context.Context, machine string, snapshots map[string]*herdrSnapshot, failures map[string]string) (*herdrSnapshot, string, bool) {
	if snap, seen := snapshots[machine]; seen {
		return snap, "", false
	}
	if state, failed := failures[machine]; failed {
		return nil, state, true
	}
	snap, err := r.snapshot(ctx, machine)
	if err != nil {
		state := LocationUnknown
		if remotecall.AsUnreachable(err) != nil {
			state = LocationUnreachable
		}
		failures[machine] = state
		return nil, state, true
	}
	snapshots[machine] = snap
	return snap, "", false
}

// Focus brings loc's workspace and then its tab to the front in herdr,
// through `herdr --machine` for another machine, and returns the status
// line text. herdr cannot focus a pane by id, so the tab is the target.
// A location that is not live runs nothing and says why.
func (r LocationResolver) Focus(ctx context.Context, loc OwnerLocation) string {
	switch {
	case loc.State == LocationPull:
		return "owner uses pull delivery; nothing to focus"
	case loc.State == LocationTarget:
		return "owner uses target delivery; nothing to focus"
	case loc.State == LocationClosed:
		return "owner tab closed; nothing to focus"
	case loc.State == LocationUnreachable:
		return "owner is on " + loc.Machine + ", which did not answer; nothing to focus"
	case loc.State != LocationLive || loc.WorkspaceID == "" || loc.TabID == "":
		return "owner location unknown; nothing to focus"
	}
	run := r.Run
	if run == nil {
		run = ExecHerdr
	}
	for _, args := range [][]string{{"workspace", "focus", loc.WorkspaceID}, {"tab", "focus", loc.TabID}} {
		if _, err := run(ctx, loc.Machine, args); err != nil {
			if loc.Machine != "" {
				return "owner is on " + loc.Machine + "; not focusable from here"
			}
			return "focus owner: " + err.Error()
		}
	}
	prefix := ""
	if loc.Machine != "" {
		prefix = loc.Machine + ": "
	}
	return "focused " + prefix + loc.Workspace + " / " + loc.Tab
}

type herdrSnapshot struct {
	panes      map[string]herdrPane
	tabs       map[string]string
	workspaces map[string]string
}

type herdrPane struct {
	tab       string
	workspace string
}

func (s *herdrSnapshot) locate(loc OwnerLocation) OwnerLocation {
	pane, ok := s.panes[loc.Pane]
	if !ok {
		loc.State = LocationClosed
		return loc
	}
	loc.State = LocationLive
	loc.TabID = pane.tab
	loc.WorkspaceID = pane.workspace
	loc.Tab = labelOr(s.tabs[pane.tab], pane.tab)
	loc.Workspace = labelOr(s.workspaces[pane.workspace], pane.workspace)
	return loc
}

func labelOr(label, id string) string {
	if label == "" {
		return id
	}
	return label
}

//nolint:tagliatelle // herdr wire format
type herdrLists struct {
	Result struct {
		Panes []struct {
			PaneID      string `json:"pane_id"`
			TabID       string `json:"tab_id"`
			WorkspaceID string `json:"workspace_id"`
		} `json:"panes"`
		Tabs []struct {
			TabID string `json:"tab_id"`
			Label string `json:"label"`
		} `json:"tabs"`
		Workspaces []struct {
			WorkspaceID string `json:"workspace_id"`
			Label       string `json:"label"`
		} `json:"workspaces"`
	} `json:"result"`
}

func (r LocationResolver) snapshot(ctx context.Context, machine string) (*herdrSnapshot, error) {
	run := r.Run
	if run == nil {
		run = ExecHerdr
	}
	var lists [3]herdrLists
	for i, kind := range []string{"pane", "tab", "workspace"} {
		stdout, err := run(ctx, machine, []string{kind, "list"})
		if err != nil {
			return nil, err
		}
		if err := json.Unmarshal([]byte(stdout), &lists[i]); err != nil {
			return nil, fmt.Errorf("decode herdr %s list: %w", kind, err)
		}
	}
	snap := &herdrSnapshot{panes: map[string]herdrPane{}, tabs: map[string]string{}, workspaces: map[string]string{}}
	for _, p := range lists[0].Result.Panes {
		snap.panes[p.PaneID] = herdrPane{tab: p.TabID, workspace: p.WorkspaceID}
	}
	for _, t := range lists[1].Result.Tabs {
		snap.tabs[t.TabID] = t.Label
	}
	for _, w := range lists[2].Result.Workspaces {
		snap.workspaces[w.WorkspaceID] = w.Label
	}
	return snap, nil
}

// ExecHerdr is the HerdrCall that runs the herdr on PATH under
// MachineTimeout.
func ExecHerdr(ctx context.Context, machine string, args []string) (string, error) {
	if machine != "" {
		stdout, stderr, code, err := runOnMachine(ctx, machine, "herdr", args, nil)
		if err != nil {
			return "", fmt.Errorf("herdr on %s: %w", machine, err)
		}
		if code != 0 {
			return "", fmt.Errorf("herdr on %s: exit %d: %s", machine, code, strings.TrimSpace(stderr))
		}
		return stdout, nil
	}
	callCtx, cancel := context.WithTimeout(ctx, MachineTimeout)
	defer cancel()
	res, err := remotecall.Run(callCtx, "herdr", args...)
	if err != nil {
		return "", fmt.Errorf("herdr: %w", err)
	}
	if res.Code != 0 || res.TimedOut {
		return "", fmt.Errorf("herdr: exit %d: %s", res.Code, strings.TrimSpace(res.Stderr))
	}
	return res.Stdout, nil
}
