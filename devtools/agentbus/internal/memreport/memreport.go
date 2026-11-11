// Package memreport builds the per-agent memory report of `agentbus
// mem` from what herdr, the process table, and the daemon report.
package memreport

import (
	"cmp"
	"fmt"
	"io"
	"maps"
	"slices"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// Lease roles.
const (
	RoleHolder = "holder"
	RoleQueued = "queued"
)

// unknown is the idle-since of every row: neither herdr's agent list
// nor the event log records when an agent's state last changed.
const unknown = "unknown"

// Bus is what the daemon knows about the agents.
type Bus struct {
	// Names are the registered names by handle.
	Names map[string][]string
	// Leases is the daemon's lease table.
	Leases []lease.Status
}

// Input is everything one report reads.
type Input struct {
	// Agents are the local herdr panes; those without an agent are
	// skipped.
	Agents []adapter.Agent
	// Foreground are each agent pane's foreground pids, and InfoErrors
	// why a pane's could not be read.
	Foreground map[string][]int
	InfoErrors map[string]error
	// Procs is the process table, read once.
	Procs []procinfo.Process
	// Bus is nil when the daemon could not be asked.
	Bus *Bus
	// Available is nil when it could not be read.
	Available *int64
	Machine   string
	Notes     []string
	// Policy is the hibernation policy for the footer, or nil to leave
	// it out.
	Policy *Policy
}

// Policy is the memory-pressure hibernation policy of this machine.
type Policy struct {
	// Mode is off, dry-run, or on. The rest is left out when off.
	Mode           string `json:"mode"`
	BelowBytes     int64  `json:"below_bytes,omitempty"`      //nolint:tagliatelle // mem JSON shape
	TargetBytes    int64  `json:"target_bytes,omitempty"`     //nolint:tagliatelle // mem JSON shape
	MinIdleSeconds int64  `json:"min_idle_seconds,omitempty"` //nolint:tagliatelle // mem JSON shape
}

// Job is one long-lived child of an agent. AgeSeconds is -1 when
// unknown.
type Job struct {
	PID        int    `json:"pid"`
	AgeSeconds int64  `json:"age_seconds"` //nolint:tagliatelle // mem JSON shape
	Command    string `json:"command"`
}

// Lease is a checkout an agent holds or is queued for.
type Lease struct {
	Checkout string `json:"checkout"`
	Role     string `json:"role"`
}

// Row is one agent pane. Names and Leases are nil when the daemon could
// not be asked, and empty when it has none for the pane.
type Row struct {
	Pane      string   `json:"pane"`
	Tab       string   `json:"tab"`
	Handle    string   `json:"handle"`
	Names     []string `json:"names,omitzero"`
	State     string   `json:"state"`
	IdleSince string   `json:"idle_since"` //nolint:tagliatelle // mem JSON shape
	RSSBytes  int64    `json:"rss_bytes"`  //nolint:tagliatelle // mem JSON shape
	Jobs      []Job    `json:"jobs"`
	Leases    []Lease  `json:"leases,omitzero"`
	Session   string   `json:"session"`
	// Error is why the pane's processes could not be read.
	Error string `json:"error,omitempty"`
}

// Report is the `agentbus mem --json` document.
type Report struct {
	Agents        []Row `json:"agents"`
	TotalRSSBytes int64 `json:"total_rss_bytes"` //nolint:tagliatelle // mem JSON shape
	// AvailableBytes is left out when it could not be read.
	AvailableBytes *int64         `json:"available_bytes,omitempty"` //nolint:tagliatelle // mem JSON shape
	Machine        string         `json:"machine"`
	States         map[string]int `json:"states"`
	Notes          []string       `json:"notes,omitempty"`
	Policy         *Policy        `json:"hibernate_policy,omitempty"` //nolint:tagliatelle // mem JSON shape
	bus            bool
}

// Build makes the report: one row per agent pane, largest RSS first.
func Build(in Input) Report {
	r := Report{
		Agents:         []Row{},
		AvailableBytes: in.Available,
		Machine:        in.Machine,
		States:         map[string]int{},
		Notes:          in.Notes,
		Policy:         in.Policy,
		bus:            in.Bus != nil,
	}
	for _, a := range in.Agents {
		if a.Kind == "" {
			continue
		}
		row := Row{
			Pane: a.PaneID, Tab: a.TabLabel, Handle: a.Handle, Session: a.SessionID,
			State: state(a.Status), IdleSince: unknown, Jobs: []Job{},
		}
		if err := in.InfoErrors[a.PaneID]; err != nil {
			row.Error = err.Error()
		}
		roots := in.Foreground[a.PaneID]
		row.RSSBytes = procinfo.SumRSS(procinfo.Tree(in.Procs, roots))
		for _, j := range procinfo.Jobs(in.Procs, roots, procinfo.DefaultJobMinAge, procinfo.DefaultHelpers) {
			age := int64(-1)
			if j.Age >= 0 {
				age = int64(j.Age / time.Second)
			}
			row.Jobs = append(row.Jobs, Job{PID: j.PID, AgeSeconds: age, Command: procinfo.CommandLine(j)})
		}
		if in.Bus != nil {
			row.Names = append([]string{}, in.Bus.Names[a.Handle]...)
			row.Leases = LeasesOf(in.Bus.Leases, a.Handle)
		}
		r.Agents = append(r.Agents, row)
		r.TotalRSSBytes += row.RSSBytes
		r.States[row.State]++
	}
	slices.SortStableFunc(r.Agents, func(a, b Row) int {
		return cmp.Or(cmp.Compare(b.RSSBytes, a.RSSBytes), cmp.Compare(a.Pane, b.Pane))
	})
	return r
}

func state(status string) string {
	switch status {
	case "idle", "done", "working", "blocked":
		return status
	}
	return unknown
}

// LeasesOf are the checkouts in table that handle holds or is queued
// for, in table order.
func LeasesOf(table []lease.Status, handle string) []Lease {
	out := []Lease{}
	for _, s := range table {
		if s.Holder != nil && s.Holder.Handle == handle {
			out = append(out, Lease{Checkout: s.Checkout, Role: RoleHolder})
		}
		for _, w := range s.Queue {
			if w.Handle == handle {
				out = append(out, Lease{Checkout: s.Checkout, Role: RoleQueued})
			}
		}
	}
	return out
}

// WriteText prints the report as a table with a footer. Without the
// daemon the names and lease columns are left out.
func (r Report) WriteText(w io.Writer) error {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	header := []string{"PANE", "TAB"}
	if r.bus {
		header = append(header, "NAMES")
	}
	header = append(header, "STATE", "SINCE", "RSS", "JOBS")
	if r.bus {
		header = append(header, "LEASE")
	}
	fmt.Fprintln(tw, strings.Join(header, "\t"))
	var notes []string
	for _, row := range r.Agents {
		cols := []string{row.Pane, dash(row.Tab)}
		if r.bus {
			cols = append(cols, dash(strings.Join(row.Names, ",")))
		}
		cols = append(cols, row.State, row.IdleSince, HumanBytes(row.RSSBytes), fmt.Sprint(len(row.Jobs)))
		if r.bus {
			leases := make([]string, len(row.Leases))
			for i, l := range row.Leases {
				verb := "holds"
				if l.Role == RoleQueued {
					verb = "queued"
				}
				leases[i] = verb + " " + l.Checkout
			}
			cols = append(cols, dash(strings.Join(leases, ", ")))
		}
		fmt.Fprintln(tw, strings.Join(cols, "\t"))
		if row.Error != "" {
			notes = append(notes, row.Pane+": "+row.Error)
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	if _, err := fmt.Fprintln(w, r.footer()); err != nil {
		return fmt.Errorf("write report: %w", err)
	}
	for _, note := range append(notes, r.Notes...) {
		if _, err := fmt.Fprintln(w, "note: "+note); err != nil {
			return fmt.Errorf("write report: %w", err)
		}
	}
	return nil
}

// footer is the total, the count by state, the available memory, and
// the hibernation policy.
func (r Report) footer() string {
	states := make([]string, 0, len(r.States))
	for _, s := range slices.Sorted(maps.Keys(r.States)) {
		states = append(states, fmt.Sprintf("%s %d", s, r.States[s]))
	}
	agents := "agents"
	if len(r.Agents) == 1 {
		agents = "agent"
	}
	line := fmt.Sprintf("total %s in %d %s", HumanBytes(r.TotalRSSBytes), len(r.Agents), agents)
	if len(states) > 0 {
		line += " (" + strings.Join(states, ", ") + ")"
	}
	if r.AvailableBytes == nil {
		line += "; available unknown"
	} else {
		line += "; " + HumanBytes(*r.AvailableBytes) + " available"
	}
	return line + r.Policy.footer()
}

func (p *Policy) footer() string {
	switch {
	case p == nil:
		return ""
	case p.Mode == "off":
		return "; hibernate policy off"
	}
	return fmt.Sprintf("; hibernate policy %s (below %s, target %s, min idle %s)",
		p.Mode, HumanBytes(p.BelowBytes), HumanBytes(p.TargetBytes), time.Duration(p.MinIdleSeconds)*time.Second)
}

func dash(s string) string {
	if s == "" {
		return "-"
	}
	return s
}

// HumanBytes is n in binary units with one decimal, such as 471.0 MiB.
func HumanBytes(n int64) string {
	const unit = 1024
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	div, exp := int64(unit), 0
	for m := n / unit; m >= unit && exp < 4; m /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %ciB", float64(n)/float64(div), "KMGTP"[exp])
}
