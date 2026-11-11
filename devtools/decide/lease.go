package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

const (
	// SourceLease marks a resource row read from the agentbus lease.
	SourceLease = "lease"
	// SourceStored marks a resource row from `decide status set`.
	SourceStored = "stored"

	// leaseTimeout bounds one `agentbus lease status` call.
	leaseTimeout = 2 * time.Second
)

// ConfigCheckout maps a status resource name to a checkout path. A
// checkout guarded by an agentbus lease shows the lease instead of the
// stored row. Machine names the agentbus machine that holds the
// checkout in hub mode; empty means this machine.
type ConfigCheckout struct {
	Name    string `json:"name"`
	Machine string `json:"machine,omitempty"`
	Path    string `json:"path"`
}

// AgentbusRunner runs `agentbus` with args and returns its stdout. An
// error carries agentbus's stderr when it printed one.
type AgentbusRunner func(ctx context.Context, args []string) ([]byte, error)

// ExecAgentbus runs the agentbus on PATH. A missing binary fails at once.
func ExecAgentbus(ctx context.Context, args []string) ([]byte, error) {
	path, err := exec.LookPath("agentbus")
	if err != nil {
		return nil, fmt.Errorf("agentbus: %w", err)
	}
	cmd := exec.CommandContext(ctx, path, args...)
	cmd.WaitDelay = 100 * time.Millisecond
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		if msg := strings.TrimSpace(stderr.String()); msg != "" {
			return out, errors.New(msg)
		}
		return out, fmt.Errorf("agentbus %s: %w", args[0], err)
	}
	return out, nil
}

// leaseStatus is the part of `agentbus lease status --json` decide shows.
type leaseStatus struct {
	Checkout string `json:"checkout"`
	State    string `json:"state"`
	Holder   *struct {
		Handle   string `json:"handle"`
		Name     string `json:"name"`
		Deadline string `json:"deadline"`
	} `json:"holder"`
	Queue     []json.RawMessage `json:"queue"`
	BackupRef string            `json:"backup_ref"` //nolint:tagliatelle // agentbus status shape
}

// ResourceView returns the rows to display: stored rows, with each
// mapped checkout's lease in place of its stored row. An unguarded
// checkout keeps its stored row. When the lease cannot be read, the
// stored row stays and is marked Guess. A checkout with a Machine is
// looked up as <machine>:<path> in the daemon's lease list, and no
// lease under that key also marks the stored row Guess.
func ResourceView(ctx context.Context, stored []ResourceStatus, checkouts []ConfigCheckout, run AgentbusRunner, now time.Time) []ResourceStatus {
	rows := make([]ResourceStatus, len(stored))
	for i, row := range stored {
		row.Source = SourceStored
		rows[i] = row
	}
	list := &leaseList{run: run}
	for _, c := range checkouts {
		if strings.TrimSpace(c.Name) == "" || strings.TrimSpace(c.Path) == "" {
			continue
		}
		st, guarded, err := list.read(ctx, c)
		idx := resourceIndex(rows, c.Name)
		switch {
		case err != nil && idx >= 0:
			rows[idx].Guess = true
		case err != nil || !guarded:
			continue
		case idx >= 0:
			rows[idx] = leaseRow(c.Name, st, now)
		default:
			rows = append(rows, leaseRow(c.Name, st, now))
			sortResources(rows)
		}
	}
	return rows
}

// readLease asks --guarded first, which makes no daemon call, and reads
// the lease only for a guarded path.
func readLease(ctx context.Context, run AgentbusRunner, path string) (leaseStatus, bool, error) {
	var g struct {
		Guarded bool `json:"guarded"`
	}
	if err := runJSON(ctx, run, &g, "lease", "status", "--guarded", "--json", "--path", path); err != nil || !g.Guarded {
		return leaseStatus{}, false, err
	}
	var st leaseStatus
	if err := runJSON(ctx, run, &st, "lease", "status", "--json", "--path", path); err != nil {
		return leaseStatus{}, true, err
	}
	return st, true, nil
}

// leaseList reads `agentbus lease status --all --json` once per view.
// The --guarded precheck reads this machine's config, so it cannot
// answer for another machine's checkout.
type leaseList struct {
	run    AgentbusRunner
	loaded bool
	leases []leaseStatus
	err    error
}

// read returns c's lease and whether c is guarded: for a checkout on
// another machine, from the lease list; otherwise as readLease does.
func (a *leaseList) read(ctx context.Context, c ConfigCheckout) (leaseStatus, bool, error) {
	if c.Machine == "" {
		return readLease(ctx, a.run, c.Path)
	}
	st, err := a.find(ctx, c.Machine+":"+c.Path)
	return st, err == nil, err
}

// find returns the lease for key, or an error when none is known.
func (a *leaseList) find(ctx context.Context, key string) (leaseStatus, error) {
	if !a.loaded {
		a.loaded = true
		a.err = runJSON(ctx, a.run, &a.leases, "lease", "status", "--all", "--json")
	}
	if a.err != nil {
		return leaseStatus{}, a.err
	}
	for _, st := range a.leases {
		if st.Checkout == key {
			return st, nil
		}
	}
	return leaseStatus{}, fmt.Errorf("no lease for %s", key)
}

func runJSON(ctx context.Context, run AgentbusRunner, out any, args ...string) error {
	ctx, cancel := context.WithTimeout(ctx, leaseTimeout)
	defer cancel()
	raw, err := run(ctx, args)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(raw, out); err != nil {
		return fmt.Errorf("decode agentbus lease status: %w", err)
	}
	return nil
}

func leaseRow(name string, st leaseStatus, now time.Time) ResourceStatus {
	row := ResourceStatus{Name: name, Availability: ResourceBusy, UpdatedAt: now.UTC(), Source: SourceLease}
	var parts []string
	if st.State == "free" {
		row.Availability = ResourceFree
	} else {
		parts = append(parts, leaseDetail(st))
	}
	if n := len(st.Queue); n > 0 {
		parts = append(parts, fmt.Sprintf("%d queued", n))
	}
	row.Note = strings.Join(parts, ", ")
	return row
}

// leaseDetail is "held by X until HH:MM", "paused by user", or
// "needs adopt (backup <ref>)".
func leaseDetail(st leaseStatus) string {
	switch st.State {
	case "paused":
		return "paused by user"
	case "needs-adopt":
		if st.BackupRef != "" {
			return "needs adopt (backup " + st.BackupRef + ")"
		}
		return "needs adopt"
	}
	h := st.Holder
	if h == nil {
		return st.State
	}
	who := h.Name
	if who == "" {
		who = h.Handle
	}
	out := st.State + " by " + who
	if t, err := time.Parse(time.RFC3339, h.Deadline); err == nil {
		out += " until " + t.Local().Format("15:04")
	}
	return out
}
