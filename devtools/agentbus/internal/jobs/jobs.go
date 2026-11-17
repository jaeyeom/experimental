// Package jobs keeps the agentbus job queue: work that producers put on
// one queue and per-machine runners claim, run, and report. The daemon
// owns one Store. It keeps state, persists it, and logs every change;
// running a job is the runner's work.
package jobs

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"time"
)

// State is a job's place in the state machine.
type State string

// Job states. Done, failed, and dead are final. Unknown is an outward
// job whose runner was lost: it may have acted, so it is never run
// again; a reconcile or the user settles it.
const (
	StateQueued  State = "queued"
	StateGated   State = "gated"
	StateReady   State = "ready"
	StateRunning State = "running"
	StateDone    State = "done"
	StateFailed  State = "failed"
	StateDead    State = "dead"
	StateUnknown State = "unknown"
	StateParked  State = "parked"
)

// AllStates lists every state in state machine order.
var AllStates = []State{
	StateQueued, StateGated, StateReady, StateRunning,
	StateDone, StateFailed, StateDead, StateUnknown, StateParked,
}

// transitions are the allowed moves. Running goes back to ready only
// for a retry or a lost idempotent job, and to gated when the runner
// finds it cannot go on without the user. Unknown never goes back to
// ready.
var transitions = map[State][]State{
	StateQueued:  {StateGated, StateReady, StateDead},
	StateGated:   {StateReady, StateParked, StateDead},
	StateReady:   {StateRunning, StateParked, StateDead},
	StateRunning: {StateDone, StateFailed, StateDead, StateUnknown, StateReady, StateGated, StateParked},
	StateUnknown: {StateDone, StateFailed, StateDead, StateParked},
	StateParked:  {StateReady, StateDone, StateFailed, StateDead},
}

// CanTransition reports whether a job may move from one state to another.
func CanTransition(from, to State) bool {
	return slices.Contains(transitions[from], to)
}

// Final reports whether s is done, failed, or dead.
func (s State) Final() bool {
	return s == StateDone || s == StateFailed || s == StateDead
}

func parseState(s string) (State, error) {
	if slices.Contains(AllStates, State(s)) {
		return State(s), nil
	}
	return "", fmt.Errorf("unknown state %q", s)
}

// Needs is what a job needs from the runner that claims it.
type Needs struct {
	// Machine is an exact machine, or "any".
	Machine string `json:"machine"`
	// Affinity is tab, lane, or owner; empty when the producer gave none.
	Affinity string `json:"affinity,omitempty"`
	// Checkout is "none", or the absolute path of a checkout on the
	// claiming runner's machine, taken through the lease with the claim.
	Checkout string `json:"checkout"`
	// Executor is cli, tab, or new-tab.
	Executor string `json:"executor"`
}

func (n *Needs) normalize() error {
	if n.Machine == "" {
		n.Machine = "any"
	}
	if n.Checkout == "" {
		n.Checkout = "none"
	}
	if n.Executor == "" {
		n.Executor = "cli"
	}
	if n.Affinity != "" && !slices.Contains([]string{"tab", "lane", "owner"}, n.Affinity) {
		return fmt.Errorf("affinity %q: want tab, lane, or owner", n.Affinity)
	}
	if !slices.Contains([]string{"cli", "tab", "new-tab"}, n.Executor) {
		return fmt.Errorf("executor %q: want cli, tab, or new-tab", n.Executor)
	}
	if n.Checkout != "none" && (!filepath.IsAbs(n.Checkout) || filepath.Clean(n.Checkout) != n.Checkout) {
		return fmt.Errorf("checkout %q: want none or a clean absolute path", n.Checkout)
	}
	return nil
}

// Entry is one state change in a job's history.
type Entry struct {
	At    time.Time `json:"at"`
	From  State     `json:"from,omitempty"`
	To    State     `json:"to"`
	Actor string    `json:"actor"`
	Note  string    `json:"note,omitempty"`
}

// Job is one unit of work. It is also the stable `jobs show --json`
// shape.
type Job struct {
	ID      string `json:"id"`
	Kind    string `json:"kind"`
	Subject string `json:"subject"`
	Head    string `json:"head,omitempty"`
	// Dedupe is kind, subject, and head. Only one live job has it.
	Dedupe   string `json:"dedupe"`
	Needs    Needs  `json:"needs"`
	Priority int    `json:"priority"`
	State    State  `json:"state"`
	// ParkedFrom is the state a parked job would otherwise be in.
	ParkedFrom  State     `json:"parkedFrom,omitempty"`
	Attempts    int       `json:"attempts"`
	MaxAttempts int       `json:"maxAttempts"`
	NotBefore   time.Time `json:"notBefore,omitzero"`
	Holder      string    `json:"holder,omitempty"`
	Heartbeat   time.Time `json:"heartbeat,omitzero"`
	// Lease is the checkout lease key the job holds, <machine>:<path>,
	// while it runs, and while it is unknown until it is settled.
	Lease string `json:"lease,omitempty"`
	// Exit is the exit code the last fail reported.
	Exit *int `json:"exit,omitempty"`
	// Note is the last note given with a change.
	Note string `json:"note,omitempty"`
	// Asked is set once a decide item was raised for an unsettled job.
	Asked   bool      `json:"asked,omitempty"`
	Created time.Time `json:"created"`
	Updated time.Time `json:"updated"`
	History []Entry   `json:"history"`
}

// unsettled reports whether j is an outward job whose runner was lost
// and that nobody has settled yet, parked or not.
func (j *Job) unsettled() bool {
	return j.State == StateUnknown || (j.State == StateParked && j.ParkedFrom == StateUnknown)
}

// OnBlocked values: what a blocked job of a kind does to its subject.
const (
	// OnBlockedBlock holds the subject's other jobs until it is settled.
	OnBlockedBlock = "block"
	// OnBlockedPark parks the job and lets the subject's other jobs run.
	OnBlockedPark = "park"
)

// Kind is the per-kind behavior from the jobs config.
type Kind struct {
	// Idempotent kinds go back to ready when their runner is lost.
	// Others are outward and go to unknown.
	Idempotent bool
	// MaxAttempts bounds the claims of one job.
	MaxAttempts int
	// RetryExits are the exit codes a fail retries.
	RetryExits []int
	// Backoff is the wait before the first retry. It doubles after each.
	Backoff time.Duration
	// OnBlocked is OnBlockedBlock or OnBlockedPark.
	OnBlocked string
	// Reconcile is the argv that checks a lost job's real state.
	Reconcile []string
}

// Defaults for a kind and for the heartbeat.
const (
	DefaultBackoff          = time.Minute
	DefaultHeartbeatTimeout = 2 * time.Minute
	maxBackoff              = time.Hour
)

// Config is the jobs config: per-kind behavior and the heartbeat
// timeout. Gates are not here; they come only from the user's policy.
type Config struct {
	Kinds            map[string]Kind
	HeartbeatTimeout time.Duration
}

// Kind returns the behavior of name. An undeclared kind is outward,
// tried once, and blocks its subject.
func (c Config) Kind(name string) Kind {
	k := c.Kinds[name]
	if k.MaxAttempts == 0 {
		k.MaxAttempts = 1
	}
	if k.Backoff == 0 {
		k.Backoff = DefaultBackoff
	}
	if k.OnBlocked == "" {
		k.OnBlocked = OnBlockedBlock
	}
	return k
}

// Timeout is how long a running job may go without a heartbeat.
func (c Config) Timeout() time.Duration {
	if c.HeartbeatTimeout <= 0 {
		return DefaultHeartbeatTimeout
	}
	return c.HeartbeatTimeout
}

type kindFile struct {
	Idempotent  bool     `json:"idempotent"`
	MaxAttempts int      `json:"maxAttempts"`
	RetryExits  []int    `json:"retryExits"`
	Backoff     string   `json:"backoff"`
	OnBlocked   string   `json:"onBlocked"`
	Reconcile   []string `json:"reconcile"`
}

type configFile struct {
	HeartbeatTimeout string              `json:"heartbeatTimeout"`
	Kinds            map[string]kindFile `json:"kinds"`
}

var kindPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9._-]*$`)

func checkKind(name string) error {
	if !kindPattern.MatchString(name) {
		return fmt.Errorf("kind %q: want lowercase letters, digits, '.', '_' or '-'", name)
	}
	return nil
}

// ParseConfig reads the jobs config JSON. Unknown keys are errors, so a
// gate written here is refused rather than ignored.
func ParseConfig(data []byte) (Config, error) {
	var f configFile
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&f); err != nil {
		return Config{}, fmt.Errorf("decode jobs config: %w", err)
	}
	var cfg Config
	if f.HeartbeatTimeout != "" {
		d, err := time.ParseDuration(f.HeartbeatTimeout)
		if err != nil || d <= 0 {
			return Config{}, fmt.Errorf("heartbeatTimeout %q: want a positive duration such as \"2m\"", f.HeartbeatTimeout)
		}
		cfg.HeartbeatTimeout = d
	}
	cfg.Kinds = map[string]Kind{}
	for name, kf := range f.Kinds {
		k, err := kindFromFile(name, kf)
		if err != nil {
			return Config{}, err
		}
		cfg.Kinds[name] = k
	}
	return cfg, nil
}

func kindFromFile(name string, kf kindFile) (Kind, error) {
	if err := checkKind(name); err != nil {
		return Kind{}, err
	}
	k := Kind{Idempotent: kf.Idempotent, MaxAttempts: kf.MaxAttempts, RetryExits: kf.RetryExits, OnBlocked: kf.OnBlocked, Reconcile: kf.Reconcile}
	if kf.MaxAttempts < 0 {
		return Kind{}, fmt.Errorf("kind %s: maxAttempts must be at least 1", name)
	}
	if kf.Backoff != "" {
		d, err := time.ParseDuration(kf.Backoff)
		if err != nil || d <= 0 {
			return Kind{}, fmt.Errorf("kind %s: backoff %q: want a positive duration such as \"1m\"", name, kf.Backoff)
		}
		k.Backoff = d
	}
	if k.OnBlocked != "" && k.OnBlocked != OnBlockedBlock && k.OnBlocked != OnBlockedPark {
		return Kind{}, fmt.Errorf("kind %s: onBlocked %q: want block or park", name, k.OnBlocked)
	}
	return k, nil
}

// LoadConfig reads the jobs config at path. A missing file is the
// default config.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read jobs config: %w", err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	return cfg, nil
}
