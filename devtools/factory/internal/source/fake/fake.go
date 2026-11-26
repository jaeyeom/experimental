// Package fake is an in-memory source.Source for tests.
package fake

import (
	"context"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const inProgress = "In Progress"

// Call is one recorded method invocation.
//
// Args holds the arguments as strings. Sync records since in UTC RFC3339Nano.
// Create records title, body, parent, comma-joined labels, comma-joined
// blocks, and comma-joined blocked-by ids, in that order.
type Call struct {
	Method string
	Args   []string
}

// Source is an in-memory source.Source guarded by a mutex.
type Source struct {
	mu sync.Mutex

	// SourceName is returned by Name and stored on items that Create inserts.
	SourceName string

	// Account is the login Me returns. Claim sets Assignee to Account.
	Account string

	// Items is the backlog, keyed by item ID.
	Items map[string]source.Item

	// Query, when set, is the source query: Sync and IDs return only the
	// items it accepts. Get still reads any item. A nil Query matches every
	// item.
	Query func(source.Item) bool

	// Calls records each method call, in order.
	Calls []Call

	// Comments records non-blank comments posted to each item, keyed by item ID.
	Comments map[string][]string

	// ClaimComments records structured claim comments for each item, keyed by item ID.
	// Tests may pre-populate it before calling LatestClaimComment.
	ClaimComments map[string][]source.ClaimComment

	// ClaimStatus, when non-empty, is the state set on Claim/ClaimWithOptions
	// when mark_in_progress is enabled. Empty defaults to "In Progress".
	ClaimStatus string

	// ClaimLabel, when non-empty, is added on Claim/ClaimWithOptions when
	// mark_in_progress is enabled and removed on Release.
	ClaimLabel string

	// LastClaimOptions records the last ClaimOptions passed to Claim or
	// ClaimWithOptions for each item ID.
	LastClaimOptions map[string]source.ClaimOptions

	// Now supplies timestamps for PostClaimComment. When nil, time.Now().UTC() is used.
	Now func() time.Time

	// FailOn, when set, makes the named method return that error before it
	// changes Items. Names are Me, Sync, IDs, Get, Claim, ClaimWithOptions,
	// PostClaimComment, LatestClaimComment, Release, Finish, Close, Link,
	// Unlink, Create, and SetPriority.
	FailOn map[string]error

	next    int
	claimed map[string]claimSnap
}

type claimSnap struct {
	state    string
	assignee string
	labels   []string
}

// Compile-time check that Source implements source.Source.
var _ source.Source = (*Source)(nil)

// Name returns SourceName.
func (s *Source) Name() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.record("Name")
	return s.SourceName
}

// Me returns Account.
func (s *Source) Me(ctx context.Context) (string, error) {
	unlock, err := s.pre(ctx, "Me")
	if err != nil {
		return "", err
	}
	defer unlock()
	return s.Account, nil
}

// Sync returns items with Updated at or after since, ordered by Updated and then ID.
// The zero time returns every item.
func (s *Source) Sync(ctx context.Context, since time.Time) ([]source.Item, error) {
	unlock, err := s.pre(ctx, "Sync", since.UTC().Format(time.RFC3339Nano))
	if err != nil {
		return nil, err
	}
	defer unlock()

	out := make([]source.Item, 0, len(s.Items))
	for id := range s.Items {
		it := s.Items[id]
		if it.Updated.Before(since) || !s.matches(it) {
			continue
		}
		out = append(out, cloneItem(it))
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Updated.Equal(out[j].Updated) {
			return out[i].ID < out[j].ID
		}
		return out[i].Updated.Before(out[j].Updated)
	})
	return out, nil
}

// IDs returns the sorted IDs of every item Query accepts.
func (s *Source) IDs(ctx context.Context) ([]string, error) {
	unlock, err := s.pre(ctx, "IDs")
	if err != nil {
		return nil, err
	}
	defer unlock()

	out := make([]string, 0, len(s.Items))
	for id := range s.Items {
		if s.matches(s.Items[id]) {
			out = append(out, id)
		}
	}
	sort.Strings(out)
	return out, nil
}

func (s *Source) matches(it source.Item) bool {
	return s.Query == nil || s.Query(it)
}

// Get returns a copy of the item. The copy does not alias stored slices.
func (s *Source) Get(ctx context.Context, id string) (source.Item, error) {
	unlock, err := s.pre(ctx, "Get", id)
	if err != nil {
		return source.Item{}, err
	}
	defer unlock()

	it, ok := s.Items[id]
	if !ok {
		return source.Item{}, fmt.Errorf("get %q: %w", id, source.ErrNotFound)
	}
	return cloneItem(it), nil
}

// Claim sets State to "In Progress" and Assignee to Account.
// A second Claim keeps the original state, so one Release restores it.
func (s *Source) Claim(ctx context.Context, id string) error {
	unlock, err := s.pre(ctx, "Claim", id)
	if err != nil {
		return err
	}
	defer unlock()
	return s.claimLocked(id, source.ClaimOptions{})
}

// ClaimWithOptions marks the item taken according to opts.
// Nil pointer fields in opts default to true.
func (s *Source) ClaimWithOptions(ctx context.Context, id string, opts source.ClaimOptions) error {
	unlock, err := s.pre(ctx, "ClaimWithOptions", id)
	if err != nil {
		return err
	}
	defer unlock()
	return s.claimLocked(id, opts)
}

func (s *Source) claimLocked(id string, opts source.ClaimOptions) error {
	it, ok := s.Items[id]
	if !ok {
		return fmt.Errorf("claim %q: %w", id, source.ErrNotFound)
	}
	if s.claimed == nil {
		s.claimed = map[string]claimSnap{}
	}
	if _, already := s.claimed[id]; !already {
		s.claimed[id] = claimSnap{
			state:    it.State,
			assignee: it.Assignee,
			labels:   cloneStrings(it.Labels),
		}
	}
	if s.LastClaimOptions == nil {
		s.LastClaimOptions = map[string]source.ClaimOptions{}
	}
	optsCopy := opts
	optsCopy.RemoveAssignees = cloneStrings(opts.RemoveAssignees)
	s.LastClaimOptions[id] = optsCopy

	assign := opts.Assign == nil || *opts.Assign
	markInProgress := opts.MarkInProgress == nil || *opts.MarkInProgress
	if markInProgress {
		status := s.ClaimStatus
		if status == "" {
			status = inProgress
		}
		it.State = status
		if s.ClaimLabel != "" && !hasString(it.Labels, s.ClaimLabel) {
			it.Labels = append(it.Labels, s.ClaimLabel)
		}
	}
	if assign {
		it.Assignee = s.Account
	}
	s.Items[id] = it
	return nil
}

// PostClaimComment records a claim comment on the item.
func (s *Source) PostClaimComment(ctx context.Context, id, body string) error {
	unlock, err := s.pre(ctx, "PostClaimComment", id, body)
	if err != nil {
		return err
	}
	defer unlock()

	if strings.TrimSpace(body) == "" {
		return fmt.Errorf("post claim comment %q: expected a non-empty comment body, found empty", id)
	}
	if _, ok := s.Items[id]; !ok {
		return fmt.Errorf("post claim comment %q: %w", id, source.ErrNotFound)
	}
	if s.Comments == nil {
		s.Comments = map[string][]string{}
	}
	s.Comments[id] = append(s.Comments[id], body)
	if s.ClaimComments == nil {
		s.ClaimComments = map[string][]source.ClaimComment{}
	}
	now := time.Now().UTC()
	if s.Now != nil {
		now = s.Now()
	}
	s.ClaimComments[id] = append(s.ClaimComments[id], source.ClaimComment{
		Author:    s.Account,
		CreatedAt: now,
		Body:      body,
	})
	return nil
}

// LatestClaimComment returns the latest claim comment on the item whose trimmed
// body starts with source.ClaimCommentPrefix, or nil when none exists.
func (s *Source) LatestClaimComment(ctx context.Context, id string) (*source.ClaimComment, error) {
	unlock, err := s.pre(ctx, "LatestClaimComment", id)
	if err != nil {
		return nil, err
	}
	defer unlock()

	if _, ok := s.Items[id]; !ok {
		return nil, fmt.Errorf("latest claim comment %q: %w", id, source.ErrNotFound)
	}
	var latest *source.ClaimComment
	for i := range s.ClaimComments[id] {
		c := s.ClaimComments[id][i]
		if !strings.HasPrefix(strings.TrimSpace(c.Body), source.ClaimCommentPrefix) {
			continue
		}
		if latest == nil || !c.CreatedAt.Before(latest.CreatedAt) {
			cp := c
			latest = &cp
		}
	}
	return latest, nil
}

// Release restores the state, assignee, and labels from before Claim.
func (s *Source) Release(ctx context.Context, id string) error {
	unlock, err := s.pre(ctx, "Release", id)
	if err != nil {
		return err
	}
	defer unlock()

	it, ok := s.Items[id]
	if !ok {
		return fmt.Errorf("release %q: %w", id, source.ErrNotFound)
	}
	snap, ok := s.claimed[id]
	if !ok {
		return fmt.Errorf("release %q: expected a claim to reverse, found state %q assignee %q", id, it.State, it.Assignee)
	}
	it.State = snap.state
	it.Assignee = snap.assignee
	it.Labels = cloneStrings(snap.labels)
	delete(s.claimed, id)
	s.Items[id] = it
	return nil
}

// Finish sets Done.
func (s *Source) Finish(ctx context.Context, id string) error {
	unlock, err := s.pre(ctx, "Finish", id)
	if err != nil {
		return err
	}
	defer unlock()

	it, ok := s.Items[id]
	if !ok {
		return fmt.Errorf("finish %q: %w", id, source.ErrNotFound)
	}
	it.Done = true
	s.Items[id] = it
	return nil
}

// Close sets Done and records any non-blank comment.
func (s *Source) Close(ctx context.Context, id, comment string) error {
	unlock, err := s.pre(ctx, "Close", id, comment)
	if err != nil {
		return err
	}
	defer unlock()

	it, ok := s.Items[id]
	if !ok {
		return fmt.Errorf("close %q: %w", id, source.ErrNotFound)
	}
	it.Done = true
	s.Items[id] = it
	if strings.TrimSpace(comment) != "" {
		if s.Comments == nil {
			s.Comments = map[string][]string{}
		}
		s.Comments[id] = append(s.Comments[id], comment)
	}
	return nil
}

// Link appends a blocked-by ref on blocked.
// The ref's LinkID is L-<blocker>-<blocked>.
func (s *Source) Link(ctx context.Context, blocker, blocked string) error {
	unlock, err := s.pre(ctx, "Link", blocker, blocked)
	if err != nil {
		return err
	}
	defer unlock()

	if _, ok := s.Items[blocker]; !ok {
		return fmt.Errorf("link blocker %q: %w", blocker, source.ErrNotFound)
	}
	it, ok := s.Items[blocked]
	if !ok {
		return fmt.Errorf("link blocked %q: %w", blocked, source.ErrNotFound)
	}
	if err := addBlock(&it, blocker, blocked); err != nil {
		return err
	}
	s.Items[blocked] = it
	return nil
}

// Unlink removes the ref Link would add. A missing link returns ErrNotFound.
func (s *Source) Unlink(ctx context.Context, blocker, blocked string) error {
	unlock, err := s.pre(ctx, "Unlink", blocker, blocked)
	if err != nil {
		return err
	}
	defer unlock()

	it, ok := s.Items[blocked]
	if !ok {
		return fmt.Errorf("unlink blocked %q: %w", blocked, source.ErrNotFound)
	}
	linkID := fakeLinkID(blocker, blocked)
	next := make([]source.BlockRef, 0, len(it.BlockedBy))
	found := false
	for _, ref := range it.BlockedBy {
		if ref.LinkID == linkID {
			found = true
			continue
		}
		next = append(next, ref)
	}
	if !found {
		return fmt.Errorf("unlink %s: %w", linkID, source.ErrNotFound)
	}
	if len(next) == 0 {
		next = nil
	}
	it.BlockedBy = next
	s.Items[blocked] = it
	return nil
}

// Create stores a new item and returns its id.
// The first id is FAKE-1. Timestamps stay at the zero time.
// Body is recorded in Calls only, because Item has no body field.
// BlockedBy refs are stored on the new item. Blocks refs are stored on the
// named items, which must already exist in this source.
func (s *Source) Create(ctx context.Context, req source.CreateRequest) (string, error) {
	unlock, err := s.pre(ctx, "Create", createArgs(req)...)
	if err != nil {
		return "", err
	}
	defer unlock()

	if err := s.validateCreate(req); err != nil {
		return "", err
	}
	id := fmt.Sprintf("FAKE-%d", s.next+1)
	item := source.Item{
		Source: s.SourceName,
		ID:     id,
		Title:  req.Title,
		Parent: req.Parent,
		Labels: cloneStrings(req.Labels),
	}
	for _, blocker := range req.BlockedBy {
		if err := addBlock(&item, blocker, id); err != nil {
			return "", err
		}
	}
	pending := make(map[string]source.Item, len(req.Blocks))
	for _, blocked := range req.Blocks {
		other := s.Items[blocked]
		if err := addBlock(&other, id, blocked); err != nil {
			return "", err
		}
		pending[blocked] = other
	}
	if s.Items == nil {
		s.Items = map[string]source.Item{}
	}
	for blocked, other := range pending {
		s.Items[blocked] = other
	}
	s.Items[id] = item
	s.next++
	return id, nil
}

// SetPriority stores rank on the item. It does not map the rank to a name.
func (s *Source) SetPriority(ctx context.Context, id string, rank int) error {
	unlock, err := s.pre(ctx, "SetPriority", id, strconv.Itoa(rank))
	if err != nil {
		return err
	}
	defer unlock()

	it, ok := s.Items[id]
	if !ok {
		return fmt.Errorf("set priority %q: %w", id, source.ErrNotFound)
	}
	it.Priority = rank
	s.Items[id] = it
	return nil
}

// pre locks s, records the call, and applies FailOn.
// On success the caller must call unlock. On error the lock is not held.
func (s *Source) pre(ctx context.Context, method string, args ...string) (func(), error) {
	if ctx == nil {
		return nil, fmt.Errorf("%s: expected a context, found nil", method)
	}
	if err := ctx.Err(); err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	s.mu.Lock()
	s.record(method, args...)
	if err := s.fail(method); err != nil {
		s.mu.Unlock()
		return nil, err
	}
	return s.mu.Unlock, nil
}

func (s *Source) record(method string, args ...string) {
	call := Call{Method: method}
	if len(args) > 0 {
		call.Args = append([]string(nil), args...)
	}
	s.Calls = append(s.Calls, call)
}

func (s *Source) fail(method string) error {
	if s.FailOn == nil {
		return nil
	}
	return s.FailOn[method]
}

func (s *Source) validateCreate(req source.CreateRequest) error {
	if err := uniqueIDs("blocked-by", req.BlockedBy); err != nil {
		return err
	}
	if err := uniqueIDs("blocks", req.Blocks); err != nil {
		return err
	}
	for _, id := range req.Blocks {
		if _, ok := s.Items[id]; !ok {
			return fmt.Errorf("create blocks %q: expected an item in this source, found none: %w", id, source.ErrNotFound)
		}
	}
	return nil
}

func uniqueIDs(field string, ids []string) error {
	seen := make(map[string]struct{}, len(ids))
	for _, id := range ids {
		if id == "" {
			return fmt.Errorf("create %s: expected an item id, found empty", field)
		}
		if _, ok := seen[id]; ok {
			return fmt.Errorf("create %s %q: expected one id, found a duplicate", field, id)
		}
		seen[id] = struct{}{}
	}
	return nil
}

func addBlock(it *source.Item, blocker, blocked string) error {
	linkID := fakeLinkID(blocker, blocked)
	for _, ref := range it.BlockedBy {
		if ref.LinkID == linkID {
			return fmt.Errorf("link %s: expected no existing link, found one", linkID)
		}
	}
	it.BlockedBy = append(it.BlockedBy, source.BlockRef{ID: blocker, LinkID: linkID})
	return nil
}

func fakeLinkID(blocker, blocked string) string {
	return "L-" + blocker + "-" + blocked
}

func createArgs(req source.CreateRequest) []string {
	return []string{
		req.Title,
		req.Body,
		req.Parent,
		strings.Join(req.Labels, ","),
		strings.Join(req.Blocks, ","),
		strings.Join(req.BlockedBy, ","),
		req.Project,
	}
}

func cloneItem(it source.Item) source.Item {
	it.Labels = cloneStrings(it.Labels)
	if it.BlockedBy != nil {
		refs := make([]source.BlockRef, len(it.BlockedBy))
		copy(refs, it.BlockedBy)
		it.BlockedBy = refs
	}
	return it
}

func cloneStrings(in []string) []string {
	if in == nil {
		return nil
	}
	out := make([]string, len(in))
	copy(out, in)
	return out
}

func hasString(list []string, target string) bool {
	for _, item := range list {
		if item == target {
			return true
		}
	}
	return false
}
