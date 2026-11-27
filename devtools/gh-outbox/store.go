package ghoutbox

import (
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/outbox"
)

// Draft states. A draft is pending until the user answers its decide
// item. It is posting from just before the first GitHub call until every
// part is recorded, so a restart never sends a part twice.
const (
	StatePending      = outbox.StatePending
	StatePosting      = "posting"
	StatePosted       = "posted"
	StateNeedsRedraft = outbox.StateNeedsRedraft
	StateCancelled    = outbox.StateCancelled
	// StateUncertain is a draft whose poller stopped during a GitHub
	// call, so whether that part was posted is unknown. It is not
	// retried.
	StateUncertain = outbox.StateUncertain
)

// Link is one posted part: Where is path:line or "review".
type Link = outbox.Receipt

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
	// Digest holds the SHA-256 of the rendered approval preview when submitted.
	Digest string `json:"digest,omitempty"`
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

func (s *Store) inner() *outbox.TypedFileStore[Record] {
	return outbox.NewTypedFileStore(
		s.Dir,
		func(r Record) int64 { return r.ID },
		func(r *Record, id int64) { r.ID = id },
	)
}

// Create stores r under a new id and returns it.
func (s *Store) Create(r Record) (Record, error) {
	created, err := s.inner().Create(r)
	if err != nil {
		return Record{}, fmt.Errorf("create draft: %w", err)
	}
	return created, nil
}

// Save replaces r's file atomically.
func (s *Store) Save(r Record) error {
	if err := s.inner().Save(r); err != nil {
		return fmt.Errorf("save draft %d: %w", r.ID, err)
	}
	return nil
}

// Get reads one record.
func (s *Store) Get(id int64) (Record, error) {
	r, err := s.inner().Get(id)
	if errors.Is(err, outbox.ErrNotFound) {
		return Record{}, fmt.Errorf("draft %d: %w", id, ErrNotFound)
	}
	if err != nil {
		return Record{}, fmt.Errorf("read draft %d: %w", id, err)
	}
	return r, nil
}

// List returns every record, oldest first. A file Create has claimed
// but not written yet is skipped.
func (s *Store) List() ([]Record, error) {
	recs, err := s.inner().List()
	if err != nil {
		return nil, fmt.Errorf("list drafts: %w", err)
	}
	return recs, nil
}
