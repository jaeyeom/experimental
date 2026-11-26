package decide

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/google/renameio/v2"
	"golang.org/x/sys/unix"
)

const (
	draftsFile = "drafts.json"
	draftsLock = "drafts.lock"

	// DraftKindReply is a draft reply to an existing thread.
	DraftKindReply = "reply"
	// DraftKindAnswer is a draft free-text answer to a relay item.
	DraftKindAnswer = "answer"
	// DraftKindCompose is a draft for composing a new thread.
	DraftKindCompose = "compose"
	// DraftKindForward is a draft for forwarding an existing thread.
	DraftKindForward = "forward"

	// DraftKeyCompose is the draft key for the compose box.
	DraftKeyCompose = "compose"

	// MaxDraftTextBytes is the maximum allowed size of Draft.Text (64 KiB).
	MaxDraftTextBytes = 64 * 1024

	// MaxDraftConflicts is the maximum number of conflict copies per draft.
	MaxDraftConflicts = 10
)

// ErrDraftTooLarge is returned when a draft's text exceeds MaxDraftTextBytes.
var ErrDraftTooLarge = errors.New("draft text exceeds 64 KiB limit")

// ErrTooManyConflicts is returned when a draft already has MaxDraftConflicts entries.
var ErrTooManyConflicts = errors.New("draft has too many conflicts")

// ReplyDraftKey returns the draft key for a reply to itemID.
func ReplyDraftKey(itemID string) string {
	return DraftKindReply + ":" + itemID
}

// AnswerDraftKey returns the draft key for a free-text answer to itemID.
func AnswerDraftKey(itemID string) string {
	return DraftKindAnswer + ":" + itemID
}

// ForwardDraftKey returns the draft key for forwarding itemID.
func ForwardDraftKey(itemID string) string {
	return DraftKindForward + ":" + itemID
}

// DraftConflict records a conflicting draft version saved against a stale baseRev.
type DraftConflict struct {
	ID        string    `json:"id"`
	Text      string    `json:"text"`
	To        string    `json:"to,omitempty"`
	Bus       bool      `json:"bus,omitempty"`
	Forward   string    `json:"forward,omitempty"`
	BaseRev   int64     `json:"baseRev"`
	UpdatedAt time.Time `json:"updatedAt"`
	UpdatedBy string    `json:"updatedBy,omitempty"`
}

// Draft is one persisted unsent reply, free-text answer, or compose note.
type Draft struct {
	Key       string          `json:"key"`
	Kind      string          `json:"kind,omitempty"`
	ItemID    string          `json:"itemId,omitempty"`
	Text      string          `json:"text"`
	To        string          `json:"to,omitempty"`
	Bus       bool            `json:"bus,omitempty"`
	Forward   string          `json:"forward,omitempty"`
	Rev       int64           `json:"rev"`
	UpdatedAt time.Time       `json:"updatedAt"`
	UpdatedBy string          `json:"updatedBy,omitempty"`
	Conflicts []DraftConflict `json:"conflicts,omitempty"`
}

type draftsFileData struct {
	Seq    int64            `json:"seq"`
	Drafts map[string]Draft `json:"drafts"`
}

// DraftStore persists drafts in drafts.json alongside ledger.json.
type DraftStore struct {
	dir string
	now func() time.Time
}

// OpenDraftStore creates dir if needed and returns the draft store there.
func OpenDraftStore(dir string) (*DraftStore, error) {
	if dir == "" {
		return nil, fmt.Errorf("decide directory is empty")
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create decide directory: %w", err)
	}
	return &DraftStore{dir: dir, now: time.Now}, nil
}

// SetClock replaces the clock used for new timestamps. Tests use it.
func (s *DraftStore) SetClock(now func() time.Time) {
	if now == nil {
		s.now = time.Now
		return
	}
	s.now = now
}

// List returns the current monotonic sequence number and all drafts sorted by key.
func (s *DraftStore) List() (int64, []Draft, error) {
	var seq int64
	var out []Draft
	err := s.locked(false, func(data *draftsFileData) (bool, error) {
		seq = data.Seq
		out = make([]Draft, 0, len(data.Drafts))
		for _, d := range data.Drafts {
			out = append(out, cloneDraft(d))
		}
		sort.Slice(out, func(i, j int) bool {
			return out[i].Key < out[j].Key
		})
		return false, nil
	})
	if err != nil {
		return 0, nil, err
	}
	return seq, out, nil
}

// Get returns the draft for key, if present.
func (s *DraftStore) Get(key string) (Draft, bool, error) {
	var found Draft
	var ok bool
	err := s.locked(false, func(data *draftsFileData) (bool, error) {
		d, exists := data.Drafts[key]
		if exists {
			found = cloneDraft(d)
			ok = true
		}
		return false, nil
	})
	if err != nil {
		return Draft{}, false, err
	}
	return found, ok, nil
}

// Put saves d using optimistic concurrency against baseRev.
//
// If no draft exists for d.Key, it is created with Rev 1. If baseRev matches
// the stored Rev, the draft is updated in place and Rev is incremented. If
// baseRev is stale and the content matches, Put is a no-op; otherwise the
// stored draft is kept intact, the incoming version is appended to Conflicts
// (up to MaxDraftConflicts), Rev is incremented, and conflict is true.
func (s *DraftStore) Put(d Draft, baseRev int64) (Draft, bool, error) {
	if d.Key == "" {
		return Draft{}, false, fmt.Errorf("draft key is empty")
	}
	if len(d.Text) > MaxDraftTextBytes {
		return Draft{}, false, ErrDraftTooLarge
	}
	fillDraftKindAndItem(&d)
	if d.UpdatedAt.IsZero() {
		d.UpdatedAt = s.now().UTC()
	} else {
		d.UpdatedAt = d.UpdatedAt.UTC()
	}

	var saved Draft
	var conflict bool
	err := s.locked(true, func(data *draftsFileData) (bool, error) {
		existing, ok := data.Drafts[d.Key]
		if !ok {
			d.Rev = 1
			d.Conflicts = nil
			data.Seq++
			data.Drafts[d.Key] = cloneDraft(d)
			saved = cloneDraft(d)
			return true, nil
		}
		if existing.Rev == baseRev {
			d.Rev = existing.Rev + 1
			d.Conflicts = cloneConflicts(existing.Conflicts)
			data.Seq++
			data.Drafts[d.Key] = cloneDraft(d)
			saved = cloneDraft(d)
			return true, nil
		}
		if draftContentEqual(existing, d) {
			saved = cloneDraft(existing)
			return false, nil
		}
		if len(existing.Conflicts) >= MaxDraftConflicts {
			return false, ErrTooManyConflicts
		}
		existing.Rev++
		conflictID := fmt.Sprintf("c-%d-%d", existing.Rev, len(existing.Conflicts)+1)
		existing.Conflicts = append(existing.Conflicts, DraftConflict{
			ID:        conflictID,
			Text:      d.Text,
			To:        d.To,
			Bus:       d.Bus,
			Forward:   d.Forward,
			BaseRev:   baseRev,
			UpdatedAt: d.UpdatedAt,
			UpdatedBy: d.UpdatedBy,
		})
		data.Seq++
		data.Drafts[d.Key] = cloneDraft(existing)
		saved = cloneDraft(existing)
		conflict = true
		return true, nil
	})
	if err != nil {
		return Draft{}, false, err
	}
	return saved, conflict, nil
}

// Delete removes the entire draft for key when conflictID is empty, or removes
// only the matching conflict entry and bumps Rev when conflictID is non-empty.
func (s *DraftStore) Delete(key, conflictID string) error {
	if key == "" {
		return nil
	}
	return s.locked(true, func(data *draftsFileData) (bool, error) {
		existing, ok := data.Drafts[key]
		if !ok {
			return false, nil
		}
		if conflictID == "" {
			delete(data.Drafts, key)
			data.Seq++
			return true, nil
		}
		idx := -1
		for i, c := range existing.Conflicts {
			if c.ID == conflictID {
				idx = i
				break
			}
		}
		if idx < 0 {
			return false, nil
		}
		existing.Conflicts = append(existing.Conflicts[:idx], existing.Conflicts[idx+1:]...)
		existing.Rev++
		data.Seq++
		data.Drafts[key] = cloneDraft(existing)
		return true, nil
	})
}

// Prune removes reply:<id> and answer:<id> drafts whose itemID is not in validIDs.
// Compose drafts are preserved.
func (s *DraftStore) Prune(validIDs map[string]bool) error {
	return s.locked(true, func(data *draftsFileData) (bool, error) {
		changed := false
		for key, d := range data.Drafts {
			itemID, ok := threadDraftItemID(key, d)
			if !ok {
				continue
			}
			if !validIDs[itemID] {
				delete(data.Drafts, key)
				changed = true
			}
		}
		if changed {
			data.Seq++
		}
		return changed, nil
	})
}

func fillDraftKindAndItem(d *Draft) {
	switch {
	case strings.HasPrefix(d.Key, DraftKindReply+":"):
		if d.Kind == "" {
			d.Kind = DraftKindReply
		}
		if d.ItemID == "" {
			d.ItemID = strings.TrimPrefix(d.Key, DraftKindReply+":")
		}
	case strings.HasPrefix(d.Key, DraftKindAnswer+":"):
		if d.Kind == "" {
			d.Kind = DraftKindAnswer
		}
		if d.ItemID == "" {
			d.ItemID = strings.TrimPrefix(d.Key, DraftKindAnswer+":")
		}
	case strings.HasPrefix(d.Key, DraftKindForward+":"):
		if d.Kind == "" {
			d.Kind = DraftKindForward
		}
		if d.ItemID == "" {
			d.ItemID = strings.TrimPrefix(d.Key, DraftKindForward+":")
		}
		if d.Forward == "" {
			d.Forward = d.ItemID
		}
	case d.Key == DraftKeyCompose:
		if d.Kind == "" {
			d.Kind = DraftKindCompose
		}
	}
}

func threadDraftItemID(key string, d Draft) (string, bool) {
	for _, prefix := range []string{DraftKindReply + ":", DraftKindAnswer + ":", DraftKindForward + ":"} {
		if strings.HasPrefix(key, prefix) {
			if d.ItemID != "" {
				return d.ItemID, true
			}
			return strings.TrimPrefix(key, prefix), true
		}
	}
	return "", false
}

func draftContentEqual(a, b Draft) bool {
	return a.Text == b.Text &&
		a.To == b.To &&
		a.Bus == b.Bus &&
		a.Forward == b.Forward
}

func cloneDraft(d Draft) Draft {
	d.Conflicts = cloneConflicts(d.Conflicts)
	return d
}

func cloneConflicts(in []DraftConflict) []DraftConflict {
	if len(in) == 0 {
		return nil
	}
	out := make([]DraftConflict, len(in))
	copy(out, in)
	return out
}

func (s *DraftStore) locked(write bool, fn func(*draftsFileData) (bool, error)) error {
	f, err := s.acquire()
	if err != nil {
		return err
	}
	defer func() {
		_ = release(f)
	}()

	data, err := loadDrafts(filepath.Join(s.dir, draftsFile))
	if err != nil {
		return err
	}
	dirty, err := fn(data)
	if err != nil {
		return err
	}
	if write && dirty {
		return saveDrafts(s.dir, data)
	}
	return nil
}

func (s *DraftStore) acquire() (*os.File, error) {
	path := filepath.Join(s.dir, draftsLock)
	//nolint:gosec // operator-local state file
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open decide drafts lock: %w", err)
	}
	if err := flock(f, unix.LOCK_EX); err != nil {
		_ = f.Close()
		return nil, err
	}
	return f, nil
}

func loadDrafts(path string) (*draftsFileData, error) {
	raw, err := os.ReadFile(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return &draftsFileData{Drafts: map[string]Draft{}}, nil
		}
		return nil, fmt.Errorf("read decide drafts: %w", err)
	}
	var data draftsFileData
	if err := json.Unmarshal(raw, &data); err != nil {
		return nil, fmt.Errorf("decode decide drafts: %w", err)
	}
	if data.Drafts == nil {
		data.Drafts = map[string]Draft{}
	}
	return &data, nil
}

func saveDrafts(dir string, data *draftsFileData) error {
	if data.Drafts == nil {
		data.Drafts = map[string]Draft{}
	}
	raw, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return fmt.Errorf("encode decide drafts: %w", err)
	}
	raw = append(raw, '\n')
	path := filepath.Join(dir, draftsFile)
	if err := renameio.WriteFile(path, raw, 0o600); err != nil {
		return fmt.Errorf("write decide drafts: %w", err)
	}
	return nil
}
