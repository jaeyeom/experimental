package gate

import (
	"context"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Lane is one [[lane]] as CheckLane sees it.
type Lane struct {
	Name       string // Lane name.
	Machine    string // herdr saved machine. Empty is the local server.
	Checkout   string // Directory the lane's workers run in, on Machine.
	MaxWorkers int    // Cap on running workers.
	MaxActive  int    // Cap on every active claim on the lane. 0 means no cap.
}

// LaneResult is one lane's checks.
// A skipped lane was not checked. OK is false and Reasons is nil.
type LaneResult struct {
	Name       string
	OK         bool
	Reasons    []string // Every failing lane check. Nil when OK or skipped.
	Workers    int      // Running workers, the count max_workers caps.
	MaxWorkers int
	Active     int // Every active claim on the lane, handed off or not.
	MaxActive  int // 0 means no max_active.
	// Skipped is why the lane was not checked, for example an unreachable
	// machine. Empty means the lane was checked.
	Skipped string
	// Note is printed after an OK lane, for example a checkout that was not read.
	Note string
}

// CheckGlobal evaluates the checks that do not depend on a lane: hold
// files, hold tabs, max_in_flight, and min_start_interval. in.Tabs are the
// tabs the hold-tab check reads. The working-agent and checkout checks are
// per lane, in CheckLane.
func CheckGlobal(in Inputs, d config.Defaults) Result {
	reasons := holdFileReasons(in)
	reasons = append(reasons, holdTabReasons(in)...)
	reasons, flight, counted := applyFlight(reasons, in, d)
	if elapsed := in.Now.Sub(in.LastStartedAt); elapsed < d.MinStartInterval {
		reasons = append(reasons, fmt.Sprintf("last start %s ago (min %s)", elapsed, d.MinStartInterval))
	}
	return finishFlight(Result{
		OK:      len(reasons) == 0,
		Reasons: reasons,
	}, flight, counted, d.MaxInFlight)
}

// CheckLane evaluates one lane: the working agents in in.Tabs and in.Panes
// (the lane machine's), the lane checkout, max_workers against the lane's
// running workers, and max_active against its active claims (both from
// in.ActiveClaims). in.Checkouts is replaced by the lane checkout.
// A remote lane's checkout and agentbus lease are not read, because git
// and agentbus run locally and cannot see that machine's checkout. The
// result says so in Note instead of failing, under every lease gate.
func CheckLane(ctx context.Context, in Inputs, lane Lane) LaneResult {
	in.Checkouts = []string{lane.Checkout}
	in.Lease = onceLease(in.Lease)
	reasons := workingReasons(in, lane.Checkout)
	held, leaseNote := laneLeaseHolder(ctx, in, lane)
	running, active := CountLane(in.ActiveClaims, lane.Name, in.Tabs, true, held)
	note := ""
	if lane.Machine == "" {
		reasons = append(reasons, checkoutReasons(ctx, in, lane.Checkout)...)
		var leaseReason string
		leaseReason, note = leaseCheck(ctx, in, lane.Checkout)
		if leaseReason != "" {
			reasons = append(reasons, leaseReason)
		}
	} else {
		note = "checkout not checked on remote machine " + lane.Machine
		if in.LeaseGate == config.LeaseGateAuto || in.LeaseGate == config.LeaseGateRequired {
			note = "checkout and lease not checked on remote machine " + lane.Machine
		}
	}
	if leaseNote != "" {
		note = joinNote(note, leaseNote)
	}
	reasons = append(reasons, capReasons(lane, running, active)...)
	return LaneResult{
		Name:       lane.Name,
		OK:         len(reasons) == 0,
		Reasons:    reasons,
		Workers:    running,
		MaxWorkers: lane.MaxWorkers,
		Active:     active,
		MaxActive:  lane.MaxActive,
		Note:       note,
	}
}

// SkippedLane is a lane that was not checked, with why. Its tabs were not
// read, so every claim still in state claimed or started counts as running.
func SkippedLane(lane Lane, claims []store.Claim, why string) LaneResult {
	running, active := CountLane(claims, lane.Name, nil, false, nil)
	return LaneResult{
		Name:       lane.Name,
		Workers:    running,
		MaxWorkers: lane.MaxWorkers,
		Active:     active,
		MaxActive:  lane.MaxActive,
		Skipped:    why,
	}
}

// CountLane counts the active claims recorded for lane, and the running
// workers among them. A claim on the lane is a running worker when:
//   - it is still claimed, so its start is under way;
//   - its tab is open on the lane machine, whatever the agent status
//     (tabsRead and the claim's tab id is in tabs);
//   - held reports that it holds the lane checkout's lease; or
//   - it is started and the lane's tabs could not be read, or it has no tab id.
//
// A handed-off claim whose tab is closed and that holds no lease is not
// running: its pull request is waiting for review. held may be nil.
// A claim recorded before lanes were configured has no lane and counts for none.
func CountLane(claims []store.Claim, lane string, tabs []herdr.Tab, tabsRead bool, held func(store.Claim) bool) (running, active int) {
	open := make(map[string]struct{}, len(tabs))
	for _, tab := range tabs {
		open[tab.ID] = struct{}{}
	}
	for _, claim := range claims {
		if claim.Lane != lane {
			continue
		}
		active++
		_, tabOpen := open[claim.TabID]
		switch {
		case claim.State == store.Claimed:
		case tabsRead && claim.TabID != "" && tabOpen:
		case held != nil && held(claim):
		case claim.State == store.Started && (!tabsRead || claim.TabID == ""):
		default:
			continue
		}
		running++
	}
	return running, active
}

func capReasons(lane Lane, running, active int) []string {
	var out []string
	switch {
	case lane.MaxWorkers < 1:
		out = append(out, fmt.Sprintf("lane %s max_workers: expected a positive integer, found %d", lane.Name, lane.MaxWorkers))
	case running >= lane.MaxWorkers:
		out = append(out, fmt.Sprintf("%d running workers on lane %s (max_workers %d)", running, lane.Name, lane.MaxWorkers))
	}
	switch {
	case lane.MaxActive < 0:
		out = append(out, fmt.Sprintf("lane %s max_active: expected a positive integer or unset, found %d", lane.Name, lane.MaxActive))
	case lane.MaxActive > 0 && active >= lane.MaxActive:
		out = append(out, fmt.Sprintf("%d active claims on lane %s (max_active %d)", active, lane.Name, lane.MaxActive))
	}
	return out
}

func joinNote(a, b string) string {
	if a == "" {
		return b
	}
	return a + "; " + b
}

// Line is the one-line summary of a lane, for example `linux: OK 2/4`,
// `mac: unreachable (skipped): host unreachable: ...`, or
// `linux: blocked 4/4: 4 workers on lane linux (max 4)`.
func (r LaneResult) Line() string {
	switch {
	case r.Skipped != "":
		return fmt.Sprintf("%s: unreachable (skipped): %s", r.Name, r.Skipped)
	case r.OK:
		line := fmt.Sprintf("%s: OK %s", r.Name, r.Counts())
		if r.Note != "" {
			line += " (" + r.Note + ")"
		}
		return line
	default:
		return fmt.Sprintf("%s: blocked %s: %s", r.Name, r.Counts(), strings.Join(r.Reasons, "; "))
	}
}

// Counts is "workers R/MAX (running), active A", with "/MAX_ACTIVE" after
// A when max_active is set.
func (r LaneResult) Counts() string {
	active := fmt.Sprintf("%d", r.Active)
	if r.MaxActive > 0 {
		active = fmt.Sprintf("%d/%d", r.Active, r.MaxActive)
	}
	return fmt.Sprintf("workers %d/%d (running), active %s", r.Workers, r.MaxWorkers, active)
}
