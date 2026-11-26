// Package agentbus reads the agentbus workdir lease of a checkout and
// wakes a hibernated agent before a prompt.
// Commands run through an injected go-cmdexec Executor.
//
// The lease shape is the stable `agentbus lease status --json` output:
// a checkout key, a state, an optional holder, and a queue. Decoding is
// strict: an unknown state or a field of the wrong type is an error.
// Extra fields that agentbus adds later are ignored.
package agentbus

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

// callTimeout bounds one agentbus call. A hub reached over ssh can hang
// instead of failing, and the gate must not wait on it for long.
const callTimeout = 10 * time.Second

// wakeTimeout bounds one `agentbus wake`. agentbus waits up to its own
// wake_timeout (60s by default) for the relaunched agent, and may reach
// a hub over ssh first.
const wakeTimeout = 3 * time.Minute

// LocalMachine is the --machine value for the local herdr server.
const LocalMachine = "local"

// Lease states, from the agentbus status shape.
const (
	StateFree       = "free"
	StateHeld       = "held"
	StatePaused     = "paused"
	StateReleasing  = "releasing"
	StateNeedsAdopt = "needs-adopt"
)

var errEmptyPath = errors.New("agentbus lease status: expected a path, found empty")

// ErrMissing means the agentbus binary is not installed.
var ErrMissing = errors.New("agentbus not installed")

// Holder is the lease holder. Since and Deadline are RFC 3339.
// Deadline is empty for the user's pause.
type Holder struct {
	Handle   string
	Name     string
	Since    string
	Deadline string
	Why      string
}

// Waiter is one queue entry.
type Waiter struct {
	Handle string
	Name   string
	Urgent bool
	For    string
	Since  string
}

// Status is one checkout's lease.
type Status struct {
	Checkout string // Checkout key, <machine>:<top-level>.
	State    string // One of the State constants.
	Holder   *Holder
	Queue    []Waiter
	// BackupRef is set when a takeover saved leftovers. Empty otherwise.
	BackupRef string
}

// Reading is what Read found for a path. Status is set only when Guarded.
type Reading struct {
	// Guarded is false when the path is outside every agentbus
	// lease_checkouts entry. Such a checkout has no lease.
	Guarded bool
	Status  Status
}

// Owner names the agent factory is about to start in a checkout.
// The zero Owner matches no holder.
type Owner struct {
	// ItemID matches a holder whose --why starts with this id, the way the
	// start prompt's lease rule writes it.
	ItemID string
	// Agent matches a holder whose registered name is the herdr agent name.
	Agent string
}

// Client runs agentbus through an injected executor.
type Client struct {
	exec executor.Executor
	bin  string
}

// NewClient returns a client that runs bin through exec.
func NewClient(exec executor.Executor, bin string) *Client {
	return &Client{exec: exec, bin: bin}
}

// Read asks whether path is guarded and, when it is, reads its lease.
// The guarded question is answered from the local agentbus config and
// does not contact the daemon or hub.
func (c *Client) Read(ctx context.Context, path string) (Reading, error) {
	guarded, err := c.Guarded(ctx, path)
	if err != nil {
		return Reading{}, err
	}
	if !guarded {
		return Reading{}, nil
	}
	st, err := c.Status(ctx, path)
	if err != nil {
		return Reading{}, err
	}
	return Reading{Guarded: true, Status: st}, nil
}

// Guarded runs `agentbus lease status --guarded --json --path PATH`.
func (c *Client) Guarded(ctx context.Context, path string) (bool, error) {
	if path == "" {
		return false, errEmptyPath
	}
	out, err := c.run(ctx, "agentbus lease status", callTimeout, "lease", "status", "--guarded", "--json", "--path", path)
	if err != nil {
		return false, err
	}
	return DecodeGuarded(out)
}

// Status runs `agentbus lease status --path PATH --json`.
func (c *Client) Status(ctx context.Context, path string) (Status, error) {
	if path == "" {
		return Status{}, errEmptyPath
	}
	out, err := c.run(ctx, "agentbus lease status", callTimeout, "lease", "status", "--path", path, "--json")
	if err != nil {
		return Status{}, err
	}
	return DecodeStatus(out)
}

// All runs `agentbus lease status --all --json`: every checkout the
// daemon has seen, including other machines', keyed <machine>:<path>.
func (c *Client) All(ctx context.Context) ([]Status, error) {
	out, err := c.run(ctx, "agentbus lease status --all", callTimeout, "lease", "status", "--all", "--json")
	if err != nil {
		return nil, err
	}
	return DecodeStatusList(out)
}

// DecodeStatusList decodes a JSON array of lease status objects.
func DecodeStatusList(data []byte) ([]Status, error) {
	const label = "agentbus lease status --all"
	var raw []json.RawMessage
	if err := json.Unmarshal(bytes.TrimSpace(data), &raw); err != nil || raw == nil {
		return nil, fmt.Errorf("%s: expected a JSON array, found %s", label, compact(data))
	}
	out := make([]Status, 0, len(raw))
	for i, item := range raw {
		st, err := DecodeStatus(item)
		if err != nil {
			return nil, fmt.Errorf("%s: entry %d: %w", label, i, err)
		}
		out = append(out, st)
	}
	return out, nil
}

// Whoami runs `agentbus whoami` in the caller's environment: the first
// name this pane holds, or its raw handle herdr:<machine>/<session>/<pane>
// when it holds none. A missing agentbus is ErrMissing.
func (c *Client) Whoami(ctx context.Context) (string, error) {
	out, err := c.run(ctx, "agentbus whoami", callTimeout, "whoami")
	if err != nil {
		return "", err
	}
	for _, line := range strings.Split(string(out), "\n") {
		if name := strings.TrimSpace(line); name != "" {
			if strings.ContainsAny(name, " \t\"'") {
				return "", fmt.Errorf("agentbus whoami: expected a name or handle without spaces or quotes, found %q", name)
			}
			return name, nil
		}
	}
	return "", errors.New("agentbus whoami: expected a name or handle, found empty output")
}

// Wake runs `agentbus wake --if-hibernated --wait --machine MACHINE PANE`.
// An empty machine is LocalMachine. agentbus prints nothing when the agent
// was not hibernated and a line when it relaunched one, so woke reports
// non-empty output. A missing agentbus is ErrMissing.
func (c *Client) Wake(ctx context.Context, machine, pane string) (bool, error) {
	if pane == "" {
		return false, errors.New("agentbus wake: expected a pane, found empty")
	}
	if machine == "" {
		machine = LocalMachine
	}
	out, err := c.run(ctx, "agentbus wake", wakeTimeout, "wake", "--if-hibernated", "--wait", "--machine", machine, pane)
	if err != nil {
		return false, err
	}
	return len(bytes.TrimSpace(out)) > 0, nil
}

func (c *Client) run(ctx context.Context, label string, timeout time.Duration, args ...string) ([]byte, error) {
	if c == nil || c.exec == nil {
		return nil, errors.New("agentbus: expected an executor, found nil")
	}
	if c.bin == "" {
		return nil, errors.New("agentbus: expected a binary name, found empty")
	}
	result, err := c.exec.Execute(ctx, executor.ToolConfig{
		Command: c.bin,
		Args:    args,
		Timeout: timeout,
	})
	if err != nil {
		var notFound *executor.ExecutableNotFoundError
		if errors.As(err, &notFound) {
			return nil, ErrMissing
		}
		return nil, fmt.Errorf("%s: %w", label, err)
	}
	if result == nil {
		return nil, fmt.Errorf("%s: expected an execution result, found nil", label)
	}
	if result.ExitCode != 0 {
		detail := strings.TrimSpace(result.Stderr)
		if detail == "" {
			detail = strings.TrimSpace(result.Output)
		}
		if detail == "" {
			return nil, fmt.Errorf("%s: expected success, found exit %d", label, result.ExitCode)
		}
		return nil, fmt.Errorf("%s: exit %d: %s", label, result.ExitCode, detail)
	}
	return []byte(result.Output), nil
}

// DecodeGuarded decodes `lease status --guarded --json`:
// {"guarded": true, "checkout": "<key>"} or {"guarded": false}.
func DecodeGuarded(data []byte) (bool, error) {
	fields, err := object(data, "agentbus lease status --guarded")
	if err != nil {
		return false, err
	}
	raw, ok := fields["guarded"]
	if !ok {
		return false, errors.New(`agentbus lease status --guarded: expected a "guarded" field, found none`)
	}
	var guarded bool
	if err := json.Unmarshal(raw, &guarded); err != nil {
		return false, fmt.Errorf("agentbus lease status --guarded: expected guarded to be a boolean, found %s", raw)
	}
	return guarded, nil
}

type wireHolder struct {
	Handle   string `json:"handle"`
	Name     string `json:"name"`
	Since    string `json:"since"`
	Deadline string `json:"deadline"`
	Why      string `json:"why"`
}

type wireWaiter struct {
	Handle string `json:"handle"`
	Name   string `json:"name"`
	Urgent bool   `json:"urgent"`
	For    string `json:"for"`
	Since  string `json:"since"`
}

type wireStatus struct {
	Checkout  string       `json:"checkout"`
	State     string       `json:"state"`
	Holder    *wireHolder  `json:"holder"`
	Queue     []wireWaiter `json:"queue"`
	BackupRef string       `json:"backup_ref"` //nolint:tagliatelle // agentbus JSON wire format uses snake_case
}

// DecodeStatus decodes one `lease status --json` object.
// checkout, state, and queue are required. queue may be null.
// A holder is required for held, paused, and releasing, and refused for free.
func DecodeStatus(data []byte) (Status, error) {
	const label = "agentbus lease status"
	fields, err := object(data, label)
	if err != nil {
		return Status{}, err
	}
	for _, key := range []string{"checkout", "state", "queue"} {
		if _, ok := fields[key]; !ok {
			return Status{}, fmt.Errorf("%s: expected a %q field, found none", label, key)
		}
	}
	var wire wireStatus
	if err := json.Unmarshal(data, &wire); err != nil {
		return Status{}, fmt.Errorf("%s: expected the lease status shape, found %s: %w", label, compact(data), err)
	}
	st := Status{Checkout: wire.Checkout, State: wire.State, BackupRef: wire.BackupRef}
	if wire.Holder != nil {
		h := Holder(*wire.Holder)
		st.Holder = &h
	}
	for _, w := range wire.Queue {
		st.Queue = append(st.Queue, Waiter(w))
	}
	if err := st.validate(); err != nil {
		return Status{}, fmt.Errorf("%s: %w", label, err)
	}
	return st, nil
}

func (s Status) validate() error {
	if s.Checkout == "" {
		return errors.New("expected a non-empty checkout, found empty")
	}
	switch s.State {
	case StateFree:
		if s.Holder != nil {
			return fmt.Errorf("expected no holder for state free, found %q", s.Holder.Handle)
		}
	case StateHeld, StatePaused, StateReleasing:
		if s.Holder == nil {
			return fmt.Errorf("expected a holder for state %s, found none", s.State)
		}
	case StateNeedsAdopt:
	default:
		return fmt.Errorf("expected state free, held, paused, releasing, or needs-adopt, found %q", s.State)
	}
	if s.Holder != nil {
		if err := s.Holder.validate(); err != nil {
			return err
		}
	}
	for i, w := range s.Queue {
		if w.Handle == "" {
			return fmt.Errorf("queue %d: expected a non-empty handle, found empty", i+1)
		}
		if err := stamp(fmt.Sprintf("queue %d since", i+1), w.Since); err != nil {
			return err
		}
	}
	return nil
}

func (h Holder) validate() error {
	if h.Handle == "" {
		return errors.New("holder: expected a non-empty handle, found empty")
	}
	if err := stamp("holder since", h.Since); err != nil {
		return err
	}
	return stamp("holder deadline", h.Deadline)
}

// stamp accepts an empty value or an RFC 3339 time.
func stamp(field, value string) error {
	if value == "" {
		return nil
	}
	if _, err := time.Parse(time.RFC3339, value); err != nil {
		return fmt.Errorf("%s: expected an RFC 3339 time, found %q", field, value)
	}
	return nil
}

func object(data []byte, label string) (map[string]json.RawMessage, error) {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 {
		return nil, fmt.Errorf("%s: expected a JSON object, found empty output", label)
	}
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(trimmed, &fields); err != nil || fields == nil {
		return nil, fmt.Errorf("%s: expected a JSON object, found %s", label, compact(trimmed))
	}
	return fields, nil
}

func compact(data []byte) string {
	text := strings.TrimSpace(string(data))
	const limit = 200
	if len(text) > limit {
		text = text[:limit] + "…"
	}
	return fmt.Sprintf("%q", text)
}

// Who is the holder's registered name, or its handle when it has none.
// Empty when there is no holder.
func (s Status) Who() string {
	if s.Holder == nil {
		return ""
	}
	if s.Holder.Name != "" {
		return s.Holder.Name
	}
	return s.Holder.Handle
}

// HeldBy reports whether the lease is held by owner: the state is held
// and the holder's registered name is owner.Agent, or the first word of
// its --why is owner.ItemID.
func (s Status) HeldBy(owner Owner) bool {
	if s.State != StateHeld || s.Holder == nil {
		return false
	}
	if owner.Agent != "" && s.Holder.Name == owner.Agent {
		return true
	}
	if owner.ItemID == "" {
		return false
	}
	first, _, _ := strings.Cut(strings.TrimSpace(s.Holder.Why), " ")
	return first == owner.ItemID
}

// Summary is one line: "free", "held by NAME until 14:20 (1 queued)",
// "paused by user (0 queued)", or "needs-adopt (no holder, 0 queued)".
// The deadline is shown as HH:MM in loc.
func (s Status) Summary(loc *time.Location) string {
	queued := fmt.Sprintf("%d queued", len(s.Queue))
	if s.State == StateFree {
		if len(s.Queue) == 0 {
			return StateFree
		}
		return StateFree + " (" + queued + ")"
	}
	if s.Holder == nil {
		return s.State + " (no holder, " + queued + ")"
	}
	out := s.State + " by " + s.Who()
	if s.Holder.Deadline != "" {
		out += " until " + clock(s.Holder.Deadline, loc)
	}
	return out + " (" + queued + ")"
}

func clock(value string, loc *time.Location) string {
	t, err := time.Parse(time.RFC3339, value)
	if err != nil {
		return value
	}
	if loc == nil {
		loc = time.Local
	}
	return t.In(loc).Format("15:04")
}
