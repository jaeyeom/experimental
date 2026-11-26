package herdr

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
)

// MachineStatusError is the status herdr machine status reports for a
// machine it could not reach.
const MachineStatusError = "error"

// MachineStatus is one entry from `herdr machine status --json`.
type MachineStatus struct {
	ID     string `json:"id"`     // Saved machine id.
	Label  string `json:"label"`  // Saved machine label.
	Status string `json:"status"` // MachineStatusError when the machine did not answer.
	Error  string `json:"error"`  // Why, when Status is MachineStatusError.
}

// MachineStatus runs `herdr machine status --json MACHINE` on the local
// server. It does not prompt for authentication. The result is the entry
// whose label or id is machine.
func (c *Client) MachineStatus(ctx context.Context, machine string) (MachineStatus, error) {
	if machine == "" {
		return MachineStatus{}, errors.New("herdr machine status: expected a machine, found empty")
	}
	local := c.OnMachine("")
	raw, err := local.output(ctx, machineStatusTimeout, "machine", "status", "--json", machine)
	if err != nil {
		return MachineStatus{}, fmt.Errorf("herdr machine status: %w", err)
	}
	return decodeMachineStatus(raw, machine)
}

func decodeMachineStatus(raw []byte, machine string) (MachineStatus, error) {
	var list []MachineStatus
	if err := json.Unmarshal(raw, &list); err != nil {
		return MachineStatus{}, fmt.Errorf("herdr machine status: expected a JSON array, found %q", strings.TrimSpace(string(raw)))
	}
	var found []MachineStatus
	for _, entry := range list {
		if entry.Label == machine || entry.ID == machine {
			found = append(found, entry)
		}
	}
	if len(found) != 1 {
		return MachineStatus{}, fmt.Errorf("herdr machine status: expected one entry for %q, found %d", machine, len(found))
	}
	if found[0].Status == "" {
		return MachineStatus{}, fmt.Errorf("herdr machine status: expected a status for %q, found empty", machine)
	}
	return found[0], nil
}

// Unreachable reports whether err means herdr did not answer: the run
// failed or timed out, or herdr exited non-zero. A decode error, a
// missing binary, or a nil error is not unreachable.
func Unreachable(err error) bool {
	if err == nil || errors.Is(err, ErrNotInstalled) {
		return false
	}
	var run *RunError
	if errors.As(err, &run) {
		return true
	}
	var proc *ProcError
	return errors.As(err, &proc)
}

// WhyUnreachable explains why a call on this client's machine failed.
// It probes `herdr machine status`, so a host that does not answer is
// told apart from a host that answers while the call hangs or fails,
// which is usually a locked SSH or gpg agent.
func (c *Client) WhyUnreachable(ctx context.Context, callErr error) string {
	call := oneLine(callErr.Error())
	if c.machine == "" {
		return call
	}
	status, err := c.MachineStatus(ctx, c.machine)
	switch {
	case err != nil:
		return fmt.Sprintf("host unreachable: %s; %s", call, oneLine(err.Error()))
	case status.Status == MachineStatusError:
		detail := oneLine(status.Error)
		if detail == "" {
			detail = call
		}
		return "host unreachable: " + detail
	default:
		return fmt.Sprintf("authentication locked (unlock your SSH/gpg agent): machine status %s; %s", status.Status, call)
	}
}

func oneLine(text string) string {
	return strings.Join(strings.Fields(text), " ")
}
