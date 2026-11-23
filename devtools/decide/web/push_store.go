package web

import (
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// DefaultPushClasses are the classes a new subscription is pushed for.
// Notes are never pushed.
var DefaultPushClasses = []string{
	decide.ClassApproval, decide.ClassDecision, decide.ClassFact,
	decide.ClassTask, decide.ClassRequest, decide.ClassInbound,
}

// SubscriptionKeys are a browser PushSubscription's keys.
type SubscriptionKeys struct {
	P256dh string `json:"p256dh"`
	Auth   string `json:"auth"`
}

// Subscription is one browser's push subscription and its preferences.
type Subscription struct {
	ID         string           `json:"id"`
	Endpoint   string           `json:"endpoint"`
	Keys       SubscriptionKeys `json:"keys"`
	Enabled    bool             `json:"enabled"`
	Classes    []string         `json:"classes"`
	QuietStart string           `json:"quietStart,omitempty"`
	QuietEnd   string           `json:"quietEnd,omitempty"`
	TZ         string           `json:"tz,omitempty"`
	CreatedAt  time.Time        `json:"createdAt"`
}

type pushFile struct {
	VAPIDPrivateKey string         `json:"vapidPrivateKey"`
	Subscriptions   []Subscription `json:"subscriptions"`
}

// PushStore persists the VAPID key and subscriptions in one 0600 file.
type PushStore struct {
	path string

	mu   sync.Mutex
	key  *VAPIDKey
	subs []Subscription
}

// DefaultPushPath is $XDG_STATE_HOME/decide/webpush.json, falling back
// to ~/.local/state.
func DefaultPushPath() (string, error) {
	base := os.Getenv("XDG_STATE_HOME")
	if base == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("web push state directory: %w", err)
		}
		base = filepath.Join(home, ".local", "state")
	}
	return filepath.Join(base, "decide", "webpush.json"), nil
}

// OpenPushStore loads path, creating the VAPID key (and file) when it
// does not exist yet.
func OpenPushStore(path string) (*PushStore, error) {
	st := &PushStore{path: path}
	raw, err := os.ReadFile(path)
	switch {
	case err == nil:
		var f pushFile
		if err := json.Unmarshal(raw, &f); err != nil {
			return nil, fmt.Errorf("decode web push state: %w", err)
		}
		if st.key, err = ParseVAPIDKey(f.VAPIDPrivateKey); err != nil {
			return nil, err
		}
		st.subs = f.Subscriptions
	case errors.Is(err, os.ErrNotExist):
		if st.key, err = NewVAPIDKey(); err != nil {
			return nil, err
		}
		if err := st.saveLocked(); err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("read web push state: %w", err)
	}
	return st, nil
}

func (p *PushStore) saveLocked() error {
	if err := os.MkdirAll(filepath.Dir(p.path), 0o700); err != nil {
		return fmt.Errorf("create web push state directory: %w", err)
	}
	b, err := json.MarshalIndent(pushFile{VAPIDPrivateKey: p.key.PrivateKeyB64(), Subscriptions: p.subs}, "", "  ")
	if err != nil {
		return fmt.Errorf("encode web push state: %w", err)
	}
	if err := renameio.WriteFile(p.path, b, 0o600); err != nil {
		return fmt.Errorf("write web push state: %w", err)
	}
	return nil
}

// Key is the VAPID key.
func (p *PushStore) Key() *VAPIDKey { return p.key }

// List returns a copy of the subscriptions.
func (p *PushStore) List() []Subscription {
	p.mu.Lock()
	defer p.mu.Unlock()
	out := slices.Clone(p.subs)
	for i := range out {
		out[i].Classes = slices.Clone(out[i].Classes)
	}
	return out
}

// Find returns the subscription for endpoint.
func (p *PushStore) Find(endpoint string) (Subscription, bool) {
	for _, s := range p.List() {
		if s.Endpoint == endpoint {
			return s, true
		}
	}
	return Subscription{}, false
}

// Upsert adds a subscription or refreshes the keys of the one with the
// same endpoint, keeping its preferences. It returns the stored one.
func (p *PushStore) Upsert(endpoint string, keys SubscriptionKeys, now time.Time) (Subscription, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	for i := range p.subs {
		if p.subs[i].Endpoint == endpoint {
			p.subs[i].Keys = keys
			return p.subs[i], p.saveLocked()
		}
	}
	sub := Subscription{
		ID:        encodeB64(randomBytes(9)),
		Endpoint:  endpoint,
		Keys:      keys,
		Enabled:   true,
		Classes:   slices.Clone(DefaultPushClasses),
		CreatedAt: now.UTC(),
	}
	p.subs = append(p.subs, sub)
	return sub, p.saveLocked()
}

// Update applies fn to the subscription for endpoint and saves it. It
// reports false when there is none.
func (p *PushStore) Update(endpoint string, fn func(*Subscription)) (Subscription, bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	for i := range p.subs {
		if p.subs[i].Endpoint == endpoint {
			fn(&p.subs[i])
			return p.subs[i], true, p.saveLocked()
		}
	}
	return Subscription{}, false, nil
}

// Remove deletes the subscription for endpoint and reports whether it
// existed.
func (p *PushStore) Remove(endpoint string) (bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	before := len(p.subs)
	p.subs = slices.DeleteFunc(p.subs, func(s Subscription) bool { return s.Endpoint == endpoint })
	if len(p.subs) == before {
		return false, nil
	}
	return true, p.saveLocked()
}

func randomBytes(n int) []byte {
	b := make([]byte, n)
	_, _ = rand.Read(b)
	return b
}
