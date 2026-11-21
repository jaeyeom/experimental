package inboxrpc

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

type pendingDraftEdit struct {
	Draft   decide.Draft `json:"draft"`
	BaseRev int64        `json:"baseRev"`
	Gen     int64        `json:"gen,omitempty"`
}

type replicaData struct {
	Hub           string                      `json:"hub"`
	Seq           int64                       `json:"seq"`
	SyncedAt      time.Time                   `json:"syncedAt,omitempty"`
	Items         []decide.Item               `json:"items"`
	Status        tui.StatusBoard             `json:"status"`
	Drafts        []decide.Draft              `json:"drafts,omitempty"`
	DraftSeq      int64                       `json:"draftSeq,omitempty"`
	PendingDrafts map[string]pendingDraftEdit `json:"pendingDrafts,omitempty"`
}

// ReplicaPath returns the on-disk replica file path for hub label under
// cacheDir (or the user cache directory when cacheDir is empty).
func ReplicaPath(cacheDir, label string) (string, error) {
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
	return filepath.Join(cacheDir, "decide", "replicas", hubFileKey(label)+".json"), nil
}

func hubFileKey(label string) string {
	if label == "" {
		return "default"
	}
	var b strings.Builder
	for _, r := range label {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '-' || r == '_' || r == '.' || r == '@' {
			b.WriteRune(r)
		} else {
			b.WriteByte('_')
		}
	}
	sum := sha256.Sum256([]byte(label))
	return b.String() + "-" + hex.EncodeToString(sum[:6])
}

type replicaStore struct {
	label string
	path  string

	mu              sync.Mutex
	loaded          bool
	data            replicaData
	pendingMarkRead map[string]bool
}

func newReplicaStore(cacheDir, label string) (*replicaStore, error) {
	path, err := ReplicaPath(cacheDir, label)
	if err != nil {
		if cacheDir != "" {
			return nil, err
		}
		// Hermetic test environments (e.g. Bazel) may unset HOME and XDG_CACHE_HOME.
		// Fall back to an in-memory replica when no cache directory is available.
		path = ""
	}
	return &replicaStore{
		label:           label,
		path:            path,
		pendingMarkRead: make(map[string]bool),
	}, nil
}

// ensureLoaded loads the replica from disk once. Caller holds r.mu.
func (r *replicaStore) ensureLoaded() {
	if r.loaded {
		return
	}
	r.loaded = true
	r.data = replicaData{Hub: r.label, Items: []decide.Item{}}
	if r.path == "" {
		return
	}
	raw, err := os.ReadFile(r.path) //nolint:gosec // operator-local cache file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return
		}
		return
	}
	var decoded replicaData
	if err := json.Unmarshal(raw, &decoded); err != nil {
		return
	}
	if decoded.Items == nil {
		decoded.Items = []decide.Item{}
	}
	if decoded.Drafts == nil {
		decoded.Drafts = []decide.Draft{}
	}
	r.data = decoded
}

// save writes the replica atomically to disk. Caller holds r.mu.
func (r *replicaStore) save() {
	if r.path == "" {
		return
	}
	if err := os.MkdirAll(filepath.Dir(r.path), 0o755); err != nil {
		return
	}
	data, err := json.MarshalIndent(r.data, "", "  ")
	if err != nil {
		return
	}
	data = append(data, '\n')
	_ = renameio.WriteFile(r.path, data, 0o600)
}

func (r *replicaStore) seq() int64 {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	return r.data.Seq
}

func (r *replicaStore) syncedAt() time.Time {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	return r.data.SyncedAt
}

func (r *replicaStore) items() []decide.Item {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	out := make([]decide.Item, len(r.data.Items))
	copy(out, r.data.Items)
	return out
}

func (r *replicaStore) get(id string) (decide.Item, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	i := slices.IndexFunc(r.data.Items, func(it decide.Item) bool { return it.ID == id })
	if i < 0 {
		return decide.Item{}, false
	}
	return r.data.Items[i], true
}

func (r *replicaStore) status() tui.StatusBoard {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	return tui.StatusBoard{
		Agents:    append([]decide.AgentStatus(nil), r.data.Status.Agents...),
		Resources: append([]decide.ResourceStatus(nil), r.data.Status.Resources...),
	}
}

// applySync applies an initial or pushed watch response. It returns true
// when a sequence gap is detected and the caller should force a full resync.
func (r *replicaStore) applySync(resp Response, now time.Time) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	if r.applyItemSyncLocked(resp) {
		return true
	}
	if resp.Status != nil {
		r.data.Status = tui.StatusBoard{
			Agents:    append([]decide.AgentStatus(nil), resp.Status.Agents...),
			Resources: append([]decide.ResourceStatus(nil), resp.Status.Resources...),
		}
	}
	if resp.Drafts != nil || resp.DraftSeq > 0 {
		r.data.Drafts = cloneDraftsSlice(resp.Drafts)
		r.data.DraftSeq = resp.DraftSeq
	}
	r.data.SyncedAt = now
	r.save()
	return false
}

func (r *replicaStore) applyItemSyncLocked(resp Response) bool {
	switch {
	case resp.Resync:
		r.data.Items = append([]decide.Item(nil), resp.Items...)
		for i := range r.data.Items {
			if r.pendingMarkRead[r.data.Items[i].ID] {
				r.data.Items[i].Unread = false
			}
		}
		r.data.Seq = resp.Seq
	case len(resp.Changes) > 0:
		for _, ch := range resp.Changes {
			if ch.Seq <= r.data.Seq {
				continue
			}
			if ch.Seq != r.data.Seq+1 {
				return true
			}
			if ch.Deleted {
				r.data.Items = slices.DeleteFunc(r.data.Items, func(it decide.Item) bool {
					return it.ID == ch.ID
				})
			} else if ch.Item != nil {
				it := *ch.Item
				if r.pendingMarkRead[it.ID] {
					it.Unread = false
				}
				r.upsertLocked(it)
			}
			r.data.Seq = ch.Seq
		}
		if resp.Seq > r.data.Seq {
			return true
		}
	case resp.Seq > r.data.Seq:
		return true
	}
	return false
}

func (r *replicaStore) upsertLocked(it decide.Item) {
	i := slices.IndexFunc(r.data.Items, func(existing decide.Item) bool { return existing.ID == it.ID })
	if i >= 0 {
		r.data.Items[i] = it
		return
	}
	r.data.Items = append(r.data.Items, it)
}

func (r *replicaStore) upsertLocal(it decide.Item) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	r.upsertLocked(it)
	r.save()
}

func (r *replicaStore) markReadLocal(id string) (decide.Item, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	i := slices.IndexFunc(r.data.Items, func(it decide.Item) bool { return it.ID == id })
	if i < 0 {
		return decide.Item{}, false
	}
	r.data.Items[i].Unread = false
	r.pendingMarkRead[id] = true
	r.save()
	return r.data.Items[i], true
}

func (r *replicaStore) clearPendingMarkRead(id string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	delete(r.pendingMarkRead, id)
}

func cloneDraft(d decide.Draft) decide.Draft {
	if len(d.Conflicts) > 0 {
		c := make([]decide.DraftConflict, len(d.Conflicts))
		copy(c, d.Conflicts)
		d.Conflicts = c
	}
	return d
}

func cloneDraftsSlice(in []decide.Draft) []decide.Draft {
	out := make([]decide.Draft, len(in))
	for i, d := range in {
		out[i] = cloneDraft(d)
	}
	return out
}

func (r *replicaStore) drafts() []decide.Draft {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	byKey := make(map[string]decide.Draft, len(r.data.Drafts)+len(r.data.PendingDrafts))
	for _, d := range r.data.Drafts {
		byKey[d.Key] = cloneDraft(d)
	}
	for k, p := range r.data.PendingDrafts {
		byKey[k] = cloneDraft(p.Draft)
	}
	out := make([]decide.Draft, 0, len(byKey))
	for _, d := range byKey {
		out = append(out, d)
	}
	sort.Slice(out, func(i, j int) bool {
		return out[i].Key < out[j].Key
	})
	return out
}

func (r *replicaStore) upsertDraftLocked(d decide.Draft) {
	i := slices.IndexFunc(r.data.Drafts, func(existing decide.Draft) bool { return existing.Key == d.Key })
	if i >= 0 {
		r.data.Drafts[i] = cloneDraft(d)
		return
	}
	r.data.Drafts = append(r.data.Drafts, cloneDraft(d))
}

func (r *replicaStore) upsertDraftLocal(d decide.Draft) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	r.upsertDraftLocked(d)
	delete(r.data.PendingDrafts, d.Key)
	r.save()
}

func (r *replicaStore) deleteDraftLocal(key, conflictID string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	if conflictID == "" {
		r.data.Drafts = slices.DeleteFunc(r.data.Drafts, func(d decide.Draft) bool {
			return d.Key == key
		})
		delete(r.data.PendingDrafts, key)
	} else {
		for i := range r.data.Drafts {
			if r.data.Drafts[i].Key == key {
				r.data.Drafts[i].Conflicts = slices.DeleteFunc(r.data.Drafts[i].Conflicts, func(c decide.DraftConflict) bool {
					return c.ID == conflictID
				})
				break
			}
		}
	}
	r.save()
}

func (r *replicaStore) queuePendingDraft(d decide.Draft, baseRev int64) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	if r.data.PendingDrafts == nil {
		r.data.PendingDrafts = make(map[string]pendingDraftEdit)
	}
	var gen int64 = 1
	if existing, ok := r.data.PendingDrafts[d.Key]; ok {
		baseRev = existing.BaseRev
		gen = existing.Gen + 1
	}
	r.data.PendingDrafts[d.Key] = pendingDraftEdit{
		Draft:   cloneDraft(d),
		BaseRev: baseRev,
		Gen:     gen,
	}
	r.save()
}

func (r *replicaStore) hasPendingDrafts() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	return len(r.data.PendingDrafts) > 0
}

func (r *replicaStore) pendingDrafts() []pendingDraftEdit {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	out := make([]pendingDraftEdit, 0, len(r.data.PendingDrafts))
	for _, p := range r.data.PendingDrafts {
		out = append(out, pendingDraftEdit{
			Draft:   cloneDraft(p.Draft),
			BaseRev: p.BaseRev,
			Gen:     p.Gen,
		})
	}
	sort.Slice(out, func(i, j int) bool {
		return out[i].Draft.Key < out[j].Draft.Key
	})
	return out
}

func (r *replicaStore) resolvePendingDraft(key string, gen int64, saved *decide.Draft) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.ensureLoaded()
	if p, ok := r.data.PendingDrafts[key]; ok && p.Gen == gen {
		delete(r.data.PendingDrafts, key)
	}
	if saved != nil {
		r.upsertDraftLocked(*saved)
	}
	r.save()
}
