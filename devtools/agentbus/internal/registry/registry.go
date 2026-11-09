// Package registry is the agentbus name registry.
// Names do not expire by time. A holder who is unreachable is still the holder.
package registry

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

const registryFile = "registry.json"

// Binding is one name held by a caller.
type Binding struct {
	Name   string
	Handle string
	When   string
}

// Registry persists name ownership in the daemon state directory.
type Registry struct {
	dir           string
	names         map[string]record
	adapters      []adapter.Adapter
	log           *eventlog.Log
	broken        map[string]bool
	tested        bool
	snaps         map[string][]adapter.Agent
	invBroken     map[string]bool
	haveInventory bool
	mu            sync.Mutex
}

// record is one name binding. There is no deadline field.
type record struct {
	Handle string `json:"handle"`
	When   string `json:"when"`
}

type file struct {
	Names map[string]record `json:"names"`
}

// Open loads the registry. A missing file is an empty registry.
func Open(dir string, adapters []adapter.Adapter, log *eventlog.Log) (*Registry, error) {
	names, err := load(dir)
	if err != nil {
		return nil, err
	}
	return &Registry{
		dir:      dir,
		names:    names,
		adapters: adapters,
		log:      log,
		broken:   map[string]bool{},
	}, nil
}

// Register claims name for the caller stamped from env.
// The same holder may register again. A live or unreachable holder is refused.
func (r *Registry) Register(ctx context.Context, env []string, name, when string) (string, error) {
	if err := validateName(name); err != nil {
		return "", err
	}
	if strings.TrimSpace(when) == "" {
		return "", errors.New("--when is required")
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	handle, err := r.identify(ctx, env)
	if err != nil {
		return "", err
	}
	if err := r.claim(ctx, handle, name, when); err != nil {
		return "", err
	}
	return handle, nil
}

// Describe updates --when for every name the caller holds.
func (r *Registry) Describe(ctx context.Context, env []string, when string) ([]string, error) {
	if strings.TrimSpace(when) == "" {
		return nil, errors.New("--when is required")
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	handle, err := r.identify(ctx, env)
	if err != nil {
		return nil, err
	}
	names := r.heldBy(handle)
	if len(names) == 0 {
		return nil, errors.New("caller is not registered")
	}
	for _, name := range names {
		rec := r.names[name]
		rec.When = when
		r.names[name] = rec
	}
	if err := r.save(); err != nil {
		return nil, err
	}
	for _, name := range names {
		if err := r.logEvent(eventlog.Event{
			Event:  eventlog.EventDescribe,
			Actor:  handle,
			Name:   name,
			Detail: when,
		}); err != nil {
			return nil, err
		}
	}
	return names, nil
}

// WhoAmI returns the stamped handle and the names that handle holds.
func (r *Registry) WhoAmI(ctx context.Context, env []string) (string, []Binding, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	handle, err := r.identify(ctx, env)
	if err != nil {
		return "", nil, err
	}
	names := r.heldBy(handle)
	out := make([]Binding, 0, len(names))
	for _, name := range names {
		rec := r.names[name]
		out = append(out, Binding{Name: name, Handle: handle, When: rec.When})
	}
	return handle, out, nil
}

// Release drops name, or hands it to claimer when to is set.
// to is a registered name or a raw handle. The name is never left unowned
// during a handoff.
func (r *Registry) Release(ctx context.Context, env []string, name, to string) (string, error) {
	if err := validateName(name); err != nil {
		return "", err
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	handle, err := r.identify(ctx, env)
	if err != nil {
		return "", err
	}
	rec, ok := r.names[name]
	if !ok {
		return "", fmt.Errorf("%s is not registered", name)
	}
	if rec.Handle != handle {
		return "", fmt.Errorf("only the holder can release %s", name)
	}
	if to == "" {
		delete(r.names, name)
		if err := r.save(); err != nil {
			return "", err
		}
		if err := r.logEvent(eventlog.Event{
			Event:  eventlog.EventRelease,
			Actor:  handle,
			Name:   name,
			Detail: "released",
		}); err != nil {
			return "", err
		}
		return "", nil
	}
	claimer, err := r.resolveClaimer(to)
	if err != nil {
		return "", err
	}
	rec.Handle = claimer
	r.names[name] = rec
	if err := r.save(); err != nil {
		return "", err
	}
	if err := r.logEvent(eventlog.Event{
		Event:  eventlog.EventHandoff,
		Actor:  handle,
		Name:   name,
		Detail: "to " + claimer,
	}); err != nil {
		return "", err
	}
	return claimer, nil
}

func (r *Registry) claim(ctx context.Context, handle, name, when string) error {
	rec, ok := r.names[name]
	if ok && rec.Handle != handle {
		live := r.liveness(ctx, rec.Handle)
		if live != "gone" {
			msg := fmt.Sprintf("%s is held by %s (%s); message them with: agentbus send %s", name, rec.Handle, live, name)
			if err := r.logEvent(eventlog.Event{
				Event:  eventlog.EventClaimRefused,
				Actor:  handle,
				Name:   name,
				Detail: rec.Handle,
				Reason: msg,
			}); err != nil {
				return err
			}
			return errors.New(msg)
		}
	}
	r.names[name] = record{Handle: handle, When: when}
	if err := r.save(); err != nil {
		return err
	}
	return r.logEvent(eventlog.Event{
		Event:  eventlog.EventRegister,
		Actor:  handle,
		Name:   name,
		Detail: when,
	})
}

func (r *Registry) heldBy(handle string) []string {
	names := make([]string, 0, 1)
	for name, rec := range r.names {
		if rec.Handle == handle {
			names = append(names, name)
		}
	}
	sort.Strings(names)
	return names
}

func (r *Registry) resolveClaimer(to string) (string, error) {
	if rec, ok := r.names[to]; ok && rec.Handle != "" {
		return rec.Handle, nil
	}
	if strings.Contains(to, ":") && strings.Contains(to, "/") {
		return to, nil
	}
	return "", fmt.Errorf("unknown claimer %s", to)
}

// identify stamps the caller. The caller holds r.mu.
func (r *Registry) identify(ctx context.Context, env []string) (string, error) {
	ads, err := r.usable(ctx)
	if err != nil {
		return "", err
	}
	outside := errors.New("a caller outside herdr cannot register")
	if len(ads) == 0 {
		return "", outside
	}
	for _, ad := range ads {
		handle, idErr := ad.Identify(ctx, env)
		if idErr != nil || handle == "" {
			continue
		}
		if !strings.HasPrefix(handle, "herdr:") {
			return "", outside
		}
		return handle, nil
	}
	return "", outside
}

// liveness reports alive, gone, or unreachable. The caller holds r.mu.
// Unreachable counts as alive for claims: the name stays put.
func (r *Registry) liveness(ctx context.Context, handle string) string {
	ads, err := r.usable(ctx)
	if err != nil || len(ads) == 0 {
		return "unreachable"
	}
	sawGone := false
	sawUnreachable := false
	for _, ad := range ads {
		switch judge(ctx, ad, handle) {
		case liveAlive:
			return "alive"
		case liveGone:
			sawGone = true
		default:
			sawUnreachable = true
		}
	}
	if sawUnreachable || !sawGone {
		return "unreachable"
	}
	return "gone"
}

type live int

const (
	liveUnreachable live = iota
	liveAlive
	liveGone
)

func judge(ctx context.Context, ad adapter.Adapter, handle string) live {
	agents, invErr := ad.Inventory(ctx)
	if invErr == nil && sessionReplaced(handle, agents) {
		return liveGone
	}
	state, err := ad.State(ctx, handle)
	if err != nil {
		return liveUnreachable
	}
	switch state {
	case adapter.StateIdle, adapter.StateWorking, adapter.StateBlocked, adapter.StateDone:
		return liveAlive
	case adapter.StateNoAgent, adapter.StateGone:
		return liveGone
	default:
		return liveUnreachable
	}
}

// sessionReplaced is true when the pane is occupied by a different session.
func sessionReplaced(handle string, agents []adapter.Agent) bool {
	scheme, session, pane, ok := splitHandle(handle)
	if !ok {
		return false
	}
	for _, agent := range agents {
		if agent.Handle == handle {
			return false
		}
	}
	prefix := scheme + ":"
	for _, agent := range agents {
		if agent.PaneID == "" || agent.PaneID != pane || agent.SessionID == "" || agent.SessionID == session {
			continue
		}
		if agent.Handle != "" && !strings.HasPrefix(agent.Handle, prefix) {
			continue
		}
		return true
	}
	return false
}

func splitHandle(handle string) (scheme, session, pane string, ok bool) {
	scheme, rest, found := strings.Cut(handle, ":")
	if !found || scheme == "" || rest == "" {
		return "", "", "", false
	}
	parts := strings.SplitN(rest, "/", 3)
	if len(parts) != 3 || parts[0] == "" || parts[1] == "" || parts[2] == "" {
		return "", "", "", false
	}
	return scheme, parts[1], parts[2], true
}

func (r *Registry) usable(ctx context.Context) ([]adapter.Adapter, error) {
	if !r.tested {
		r.tested = true
		for _, ad := range r.adapters {
			err := ad.SelfTest(ctx)
			if err == nil {
				continue
			}
			r.broken[ad.Name()] = true
			if logErr := r.logEvent(eventlog.Event{
				Event:  eventlog.EventAdapterError,
				Actor:  "agentbus",
				Name:   ad.Name(),
				Detail: "self-test failed",
				Reason: err.Error(),
			}); logErr != nil {
				return nil, logErr
			}
		}
	}
	out := make([]adapter.Adapter, 0, len(r.adapters))
	for _, ad := range r.adapters {
		if !r.broken[ad.Name()] {
			out = append(out, ad)
		}
	}
	return out, nil
}

func (r *Registry) logEvent(e eventlog.Event) error {
	if r.log == nil {
		return nil
	}
	if err := r.log.Append(e); err != nil {
		return fmt.Errorf("log %s: %w", e.Event, err)
	}
	return nil
}

func validateName(name string) error {
	if name == "human" || strings.HasPrefix(name, "human:") || strings.HasPrefix(name, "human/") {
		return errors.New("human:* is reserved")
	}
	if name == "" || strings.TrimSpace(name) != name || strings.Contains(name, ":") {
		return errors.New("name must be a logical name such as ops/inbox")
	}
	if strings.HasPrefix(name, "/") || strings.HasSuffix(name, "/") || strings.Contains(name, "//") {
		return errors.New("name must be a logical name such as ops/inbox")
	}
	return nil
}

func load(dir string) (map[string]record, error) {
	data, err := os.ReadFile(filepath.Join(dir, registryFile))
	if errors.Is(err, os.ErrNotExist) {
		return map[string]record{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read registry: %w", err)
	}
	var doc file
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("decode registry: %w", err)
	}
	if doc.Names == nil {
		doc.Names = map[string]record{}
	}
	return doc.Names, nil
}

func (r *Registry) save() error {
	doc := file{Names: r.names}
	if doc.Names == nil {
		doc.Names = map[string]record{}
	}
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode registry: %w", err)
	}
	data = append(data, '\n')
	tmp := filepath.Join(r.dir, ".registry.json.tmp")
	if err := os.WriteFile(tmp, data, 0o600); err != nil {
		return fmt.Errorf("write registry: %w", err)
	}
	if err := os.Rename(tmp, filepath.Join(r.dir, registryFile)); err != nil {
		return fmt.Errorf("replace registry: %w", err)
	}
	return nil
}
