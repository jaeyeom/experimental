package ghoutbox

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"
)

// Draft states. A draft is pending until the user answers its decide
// item. It is posting from just before the first GitHub call until every
// part is recorded, so a restart never sends a part twice.
const (
	StatePending      = "pending"
	StatePosting      = "posting"
	StatePosted       = "posted"
	StateNeedsRedraft = "needs-redraft"
	StateCancelled    = "cancelled"
	// StateUncertain is a draft whose poller stopped during a GitHub
	// call, so whether that part was posted is unknown. It is not
	// retried.
	StateUncertain = "uncertain"
)

// Link is one posted part: Where is path:line or "review".
type Link struct {
	Where string `json:"where"`
	URL   string `json:"url"`
}

// ReplyTarget describes the review comment and thread being answered.
type ReplyTarget struct {
	// RootID is the top-level review comment id of the thread (used by
	// the GitHub reply API). When empty, Draft.InReplyTo is used.
	RootID      string `json:"rootId,omitempty"`
	Path        string `json:"path"`
	Line        int    `json:"line"`
	Starter     string `json:"starter"`
	CommentBody string `json:"commentBody"`
	Resolved    bool   `json:"resolved,omitempty"`
}

// Record is a stored draft and its progress.
type Record struct {
	ID int64 `json:"id"`
	Draft
	State      string `json:"state"`
	HeadSHA    string `json:"headSha"`
	NewHeadSHA string `json:"newHeadSha,omitempty"`
	// OldText is the current text of the comment or review an edit
	// replaces.
	OldText  string       `json:"oldText,omitempty"`
	Reply    *ReplyTarget `json:"reply,omitempty"`
	DecideID string       `json:"decideId,omitempty"`
	// Posted are the parts already posted, in order.
	Posted []Link `json:"posted,omitempty"`
	// InFlight is the index of the part being sent. It is set before
	// the GitHub call and cleared when the result is recorded.
	InFlight *int   `json:"inFlight,omitempty"`
	Outcome  string `json:"outcome,omitempty"`
	// Notes are the user's edit notes for a redraft.
	Notes string `json:"notes,omitempty"`
	// Notified and DecideClosed record that the owner was told the
	// outcome and that the decide item was closed.
	Notified     bool      `json:"notified,omitempty"`
	DecideClosed bool      `json:"decideClosed,omitempty"`
	CreatedAt    time.Time `json:"createdAt"`
	UpdatedAt    time.Time `json:"updatedAt"`
}

// Store keeps one JSON file per record in Dir.
type Store struct{ Dir string }

// NewStore returns a store in dir.
func NewStore(dir string) *Store { return &Store{Dir: dir} }

// ErrNotFound is an unknown draft id.
var ErrNotFound = errors.New("no such draft")

func (s *Store) path(id int64) string {
	return filepath.Join(s.Dir, strconv.FormatInt(id, 10)+".json")
}

// Create stores r under a new id and returns it.
func (s *Store) Create(r Record) (Record, error) {
	if err := os.MkdirAll(s.Dir, 0o700); err != nil {
		return Record{}, fmt.Errorf("create data dir: %w", err)
	}
	ids, err := s.ids()
	if err != nil {
		return Record{}, err
	}
	next := int64(1)
	if len(ids) > 0 {
		next = ids[len(ids)-1] + 1
	}
	for ; ; next++ {
		f, err := os.OpenFile(s.path(next), os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
		if errors.Is(err, fs.ErrExist) {
			continue
		}
		if err != nil {
			return Record{}, fmt.Errorf("create draft: %w", err)
		}
		if err := f.Close(); err != nil {
			return Record{}, fmt.Errorf("create draft: %w", err)
		}
		r.ID = next
		return r, s.Save(r)
	}
}

// Save replaces r's file atomically.
func (s *Store) Save(r Record) error {
	data, err := json.MarshalIndent(r, "", "  ")
	if err != nil {
		return fmt.Errorf("encode draft %d: %w", r.ID, err)
	}
	tmp, err := os.CreateTemp(s.Dir, ".draft-*")
	if err != nil {
		return fmt.Errorf("save draft %d: %w", r.ID, err)
	}
	_, werr := tmp.Write(append(data, '\n'))
	serr := tmp.Sync()
	cerr := tmp.Close()
	if err := errors.Join(werr, serr, cerr); err != nil {
		return errors.Join(fmt.Errorf("save draft %d: %w", r.ID, err), os.Remove(tmp.Name()))
	}
	if err := os.Rename(tmp.Name(), s.path(r.ID)); err != nil {
		return errors.Join(fmt.Errorf("save draft %d: %w", r.ID, err), os.Remove(tmp.Name()))
	}
	return nil
}

// Get reads one record.
func (s *Store) Get(id int64) (Record, error) {
	data, err := os.ReadFile(s.path(id))
	if errors.Is(err, fs.ErrNotExist) {
		return Record{}, fmt.Errorf("draft %d: %w", id, ErrNotFound)
	}
	if err != nil {
		return Record{}, fmt.Errorf("read draft %d: %w", id, err)
	}
	var r Record
	if err := json.Unmarshal(data, &r); err != nil {
		return Record{}, fmt.Errorf("parse draft %d: %w", id, err)
	}
	return r, nil
}

// List returns every record, oldest first. A file Create has claimed
// but not written yet is skipped.
func (s *Store) List() ([]Record, error) {
	ids, err := s.ids()
	if err != nil {
		return nil, err
	}
	var out []Record
	for _, id := range ids {
		r, err := s.Get(id)
		if err != nil {
			if info, serr := os.Stat(s.path(id)); serr == nil && info.Size() == 0 {
				continue
			}
			return nil, err
		}
		out = append(out, r)
	}
	return out, nil
}

func (s *Store) ids() ([]int64, error) {
	entries, err := os.ReadDir(s.Dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read data dir: %w", err)
	}
	var ids []int64
	for _, e := range entries {
		name, ok := strings.CutSuffix(e.Name(), ".json")
		if !ok {
			continue
		}
		if id, err := strconv.ParseInt(name, 10, 64); err == nil {
			ids = append(ids, id)
		}
	}
	slices.Sort(ids)
	return ids, nil
}
