package inboxrpc

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

type outboxData struct {
	Hub     string            `json:"hub"`
	NextID  int64             `json:"nextId"`
	Entries []tui.OutboxEntry `json:"entries"`
}

// OutboxPath returns the on-disk outbox file path for hub label under
// cacheDir (or the user cache directory when cacheDir is empty).
func OutboxPath(cacheDir, label string) (string, error) {
	if cacheDir == "" {
		if v := os.Getenv("XDG_CACHE_HOME"); v != "" {
			cacheDir = v
		} else {
			var err error
			cacheDir, err = os.UserCacheDir()
			if err != nil {
				return "", fmt.Errorf("user cache dir: %w", err)
			}
		}
	}
	return filepath.Join(cacheDir, "decide", "outbox", hubFileKey(label)+".json"), nil
}

type outboxStore struct {
	label string
	path  string

	mu     sync.Mutex
	loaded bool
	data   outboxData
}

func newOutboxStore(cacheDir, label string) (*outboxStore, error) {
	path, err := OutboxPath(cacheDir, label)
	if err != nil {
		if cacheDir != "" {
			return nil, err
		}
		path = ""
	}
	return &outboxStore{label: label, path: path}, nil
}

func (o *outboxStore) ensureLoaded() {
	if o.loaded {
		return
	}
	o.loaded = true
	o.data = outboxData{Hub: o.label, Entries: []tui.OutboxEntry{}}
	if o.path == "" {
		return
	}
	raw, err := os.ReadFile(o.path) //nolint:gosec // operator-local cache file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return
		}
		return
	}
	var decoded outboxData
	if err := json.Unmarshal(raw, &decoded); err != nil {
		return
	}
	if decoded.Entries == nil {
		decoded.Entries = []tui.OutboxEntry{}
	}
	o.data = decoded
}

func (o *outboxStore) save() {
	if o.path == "" {
		return
	}
	if err := os.MkdirAll(filepath.Dir(o.path), 0o755); err != nil {
		return
	}
	data, err := json.MarshalIndent(o.data, "", "  ")
	if err != nil {
		return
	}
	data = append(data, '\n')
	_ = renameio.WriteFile(o.path, data, 0o600)
}

func (o *outboxStore) entries() []tui.OutboxEntry {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	out := make([]tui.OutboxEntry, len(o.data.Entries))
	copy(out, o.data.Entries)
	return out
}

func (o *outboxStore) append(e tui.OutboxEntry) tui.OutboxEntry {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	o.data.NextID++
	e.ID = fmt.Sprintf("OB-%03d", o.data.NextID)
	e.Status = tui.OutboxPending
	o.data.Entries = append(o.data.Entries, e)
	o.save()
	return e
}

// nextSendable returns the next pending entry that is not blocked by an
// earlier held/failed entry for the same item, or by any earlier pending entry.
func (o *outboxStore) nextSendable() (tui.OutboxEntry, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	blockedItems := make(map[string]bool)
	for _, e := range o.data.Entries {
		switch e.Status {
		case tui.OutboxPending:
			if e.ItemID != "" && blockedItems[e.ItemID] {
				continue
			}
			return e, true
		case tui.OutboxHeld, tui.OutboxFailed:
			if e.ItemID != "" {
				blockedItems[e.ItemID] = true
			}
		}
	}
	return tui.OutboxEntry{}, false
}

func (o *outboxStore) hasPending() bool {
	_, ok := o.nextSendable()
	return ok
}

func (o *outboxStore) remove(id string) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	i := slices.IndexFunc(o.data.Entries, func(e tui.OutboxEntry) bool { return e.ID == id })
	if i < 0 {
		return false
	}
	o.data.Entries = slices.Delete(o.data.Entries, i, i+1)
	o.save()
	return true
}

func (o *outboxStore) markHeld(id, reason string) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	i := slices.IndexFunc(o.data.Entries, func(e tui.OutboxEntry) bool { return e.ID == id })
	if i < 0 {
		return
	}
	o.data.Entries[i].Status = tui.OutboxHeld
	o.data.Entries[i].HoldReason = reason
	o.data.Entries[i].Error = ""
	o.save()
}

func (o *outboxStore) markFailed(id, errMsg string) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	i := slices.IndexFunc(o.data.Entries, func(e tui.OutboxEntry) bool { return e.ID == id })
	if i < 0 {
		return
	}
	o.data.Entries[i].Status = tui.OutboxFailed
	o.data.Entries[i].Error = errMsg
	o.data.Entries[i].HoldReason = ""
	o.save()
}

func (o *outboxStore) edit(id, text string, now time.Time, cur *decide.Item) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	i := slices.IndexFunc(o.data.Entries, func(e tui.OutboxEntry) bool { return e.ID == id })
	if i < 0 {
		return fmt.Errorf("outbox entry %s not found", id)
	}
	e := &o.data.Entries[i]
	e.Text = text
	e.WrittenAt = now
	e.Status = tui.OutboxPending
	e.Error = ""
	e.HoldReason = ""
	e.Force = false
	if cur != nil {
		setBaseline(e, *cur)
	}
	o.save()
	return nil
}

func (o *outboxStore) forceSend(id string, cur *decide.Item) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	i := slices.IndexFunc(o.data.Entries, func(e tui.OutboxEntry) bool { return e.ID == id })
	if i < 0 {
		return fmt.Errorf("outbox entry %s not found", id)
	}
	e := &o.data.Entries[i]
	e.Status = tui.OutboxPending
	e.HoldReason = ""
	e.Error = ""
	e.Force = true
	if e.Op == OpArchive && cur != nil {
		t := cur.LastMessageAt()
		e.Seen = &t
	}
	o.save()
	return nil
}

func (o *outboxStore) rebaselineItem(itemID string, cur decide.Item) {
	if itemID == "" {
		return
	}
	o.mu.Lock()
	defer o.mu.Unlock()
	o.ensureLoaded()
	changed := false
	for i := range o.data.Entries {
		e := &o.data.Entries[i]
		if e.ItemID == itemID && e.Status == tui.OutboxPending {
			setBaseline(e, cur)
			changed = true
		}
	}
	if changed {
		o.save()
	}
}

func setBaseline(e *tui.OutboxEntry, it decide.Item) {
	e.BaseState = it.State
	e.BaseMessageCount = len(it.Messages)
	e.BaseLastMessageAt = it.LastMessageAt()
	e.BaseAnswered = it.State == decide.StateAnswered || it.Answer != nil
}
