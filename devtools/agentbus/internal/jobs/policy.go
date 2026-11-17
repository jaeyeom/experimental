package jobs

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"

	"golang.org/x/sys/unix"
)

// Gate is how a job of a kind passes from queued: auto goes to ready,
// ask waits in gated for the user's answer in decide, and off is
// refused at enqueue.
type Gate string

// Gates.
const (
	GateAuto Gate = "auto"
	GateAsk  Gate = "ask"
	GateOff  Gate = "off"
)

func parseGate(s string) (Gate, error) {
	switch g := Gate(s); g {
	case GateAuto, GateAsk, GateOff:
		return g, nil
	}
	return "", fmt.Errorf("gate %q: want auto, ask, or off", s)
}

// Policy is the user's gate policy. Only the user writes its file; the
// queue, producers, and runners only read it. The zero Policy asks for
// every kind.
type Policy struct {
	// Default is the gate of a kind not in Kinds. Empty is ask.
	Default Gate
	// Kinds are per-kind gates.
	Kinds map[string]Gate
	// Floor are the kinds that are never auto: an auto gate is ask.
	Floor []string
}

// Gate is the gate of kind.
func (p Policy) Gate(kind string) Gate {
	g, ok := p.Kinds[kind]
	if !ok {
		g = p.Default
	}
	if g == "" {
		g = GateAsk
	}
	if g == GateAuto && slices.Contains(p.Floor, kind) {
		return GateAsk
	}
	return g
}

type policyFile struct {
	Default string            `json:"default"`
	Kinds   map[string]string `json:"kinds"`
	Floor   []string          `json:"floor"`
}

// ParsePolicy reads the policy JSON. Unknown keys are errors.
func ParsePolicy(data []byte) (Policy, error) {
	var f policyFile
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&f); err != nil {
		return Policy{}, fmt.Errorf("decode jobs policy: %w", err)
	}
	var p Policy
	if f.Default != "" {
		g, err := parseGate(f.Default)
		if err != nil {
			return Policy{}, fmt.Errorf("default: %w", err)
		}
		p.Default = g
	}
	p.Kinds = map[string]Gate{}
	for kind, s := range f.Kinds {
		if err := checkKind(kind); err != nil {
			return Policy{}, err
		}
		g, err := parseGate(s)
		if err != nil {
			return Policy{}, fmt.Errorf("kind %s: %w", kind, err)
		}
		p.Kinds[kind] = g
	}
	for _, kind := range f.Floor {
		if err := checkKind(kind); err != nil {
			return Policy{}, fmt.Errorf("floor: %w", err)
		}
	}
	p.Floor = f.Floor
	return p, nil
}

// ErrNoPolicy is a policy file that does not exist.
var ErrNoPolicy = errors.New("no jobs policy file")

// LoadPolicy reads the policy file at path. It must be a regular file
// owned by this user that no group or other user can write. On any
// error, ErrNoPolicy included, it returns the zero Policy, which asks
// for every kind.
func LoadPolicy(path string) (Policy, error) {
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return Policy{}, fmt.Errorf("%w: %s", ErrNoPolicy, path)
	}
	if err != nil {
		return Policy{}, fmt.Errorf("open jobs policy: %w", err)
	}
	defer f.Close()
	// Check the file that was opened, so it cannot be swapped between
	// the check and the read.
	var st unix.Stat_t
	if err := unix.Fstat(int(f.Fd()), &st); err != nil {
		return Policy{}, fmt.Errorf("stat jobs policy %s: %w", path, err)
	}
	switch {
	case st.Mode&unix.S_IFMT != unix.S_IFREG:
		return Policy{}, fmt.Errorf("jobs policy %s is not a regular file", path)
	case int(st.Uid) != os.Getuid():
		return Policy{}, fmt.Errorf("jobs policy %s is owned by uid %d, not this user (uid %d)", path, st.Uid, os.Getuid())
	case st.Mode&0o022 != 0:
		return Policy{}, fmt.Errorf("jobs policy %s is group- or world-writable (mode %o); only its owner may write it", path, st.Mode&0o777)
	}
	data, err := io.ReadAll(f)
	if err != nil {
		return Policy{}, fmt.Errorf("read jobs policy: %w", err)
	}
	p, err := ParsePolicy(data)
	if err != nil {
		return Policy{}, fmt.Errorf("%s: %w", path, err)
	}
	return p, nil
}
