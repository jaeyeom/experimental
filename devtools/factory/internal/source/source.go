// Package source is the work-item model and the backend adapter interface
// shared by every source implementation.
package source

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"time"
)

// BlockRef is one "blocked by" link stored on an item.
type BlockRef struct {
	ID     string // Blocker item ID, same source.
	LinkID string // Backend link handle: Jira issueLink id, or GitHub blocker node id.
}

// Item is one work item in a source.
type Item struct {
	Source   string
	ID       string
	Title    string
	URL      string
	Type     string // Issue type, such as Task, Bug, Epic, or issue.
	State    string // Raw status name from the backend.
	Done     bool   // True when the backend counts the item as done.
	Parent   string // Parent or epic ID. Empty when the item has none.
	Labels   []string
	Assignee string
	Author   string // Author login or account id. Empty when unknown or deleted.
	// AuthorAssociation is the backend's author-to-repo relationship (e.g. OWNER, MEMBER, NONE).
	AuthorAssociation string
	Locked            bool   // True when the item is locked against public edits/comments.
	ContentHash       string // Canonical SHA-256 of (title, body), when provided by the backend.
	Priority          int    // 0 is highest. 2 is normal.
	BlockedBy         []BlockRef
	Created           time.Time
	Updated           time.Time
	// Due is the due date, or nil when the backend has none.
	// A non-nil value is the UTC calendar date, at midnight.
	Due *time.Time
	// NotBefore defers the item: it is not ready before this date. Nil
	// means no deferral. A non-nil value is the UTC calendar date, at midnight.
	NotBefore *time.Time
}

// ClaimCommentPrefix is the leading prefix of a factory claim comment.
const ClaimCommentPrefix = "Claimed by "

// ClaimComment is a claim comment read from an issue.
type ClaimComment struct {
	Author    string
	CreatedAt time.Time
	Body      string
}

// ClaimOptions configures per-call overrides for Source.ClaimWithOptions.
// Nil pointer fields defer to the source's configured [claim] settings.
type ClaimOptions struct {
	// Assign overrides the source's claim.assign setting when non-nil.
	Assign *bool
	// MarkInProgress overrides the source's claim.mark_in_progress setting when non-nil.
	MarkInProgress *bool
	// CreateLabel overrides the source's claim.create_label setting when non-nil.
	CreateLabel *bool
	// RemoveAssignees lists prior assignees to remove on ownership takeover
	// for backends (such as GitHub) that allow multiple assignees.
	RemoveAssignees []string
}

// CreateRequest is the input to Source.Create.
type CreateRequest struct {
	Title  string
	Body   string
	Parent string
	// Project is the project to file in, for a source that has projects.
	// Empty picks the source's default.
	Project   string
	Labels    []string
	Blocks    []string // Item IDs this item blocks.
	BlockedBy []string // Item IDs that block this item.
}

// Source is one configured backend, such as a Jira site or a GitHub repo.
type Source interface {
	// Name is the configured source name.
	Name() string

	// Me returns the account id or login of the authenticated user.
	Me(ctx context.Context) (string, error)

	// Sync returns items updated at or after since, oldest first.
	// The zero time returns every item.
	Sync(ctx context.Context, since time.Time) ([]Item, error)

	// IDs returns the IDs of every item the source query matches now.
	// It uses the same query as Sync with the zero time, but reads IDs only.
	// Sync uses it to drop mirrored items that left the query.
	IDs(ctx context.Context) ([]string, error)

	// Get reads one item by its ID.
	Get(ctx context.Context, id string) (Item, error)

	// Claim marks the item taken by the authenticated user using the source's
	// configured [claim] settings.
	Claim(ctx context.Context, id string) error

	// ClaimWithOptions marks the item taken by the authenticated user,
	// applying per-call overrides and ownership takeover options.
	ClaimWithOptions(ctx context.Context, id string, opts ClaimOptions) error

	// PostClaimComment posts a claim comment on the item.
	PostClaimComment(ctx context.Context, id, body string) error

	// LatestClaimComment returns the latest comment whose trimmed body starts
	// with ClaimCommentPrefix, or nil when no such comment exists.
	LatestClaimComment(ctx context.Context, id string) (*ClaimComment, error)

	// Release undoes Claim.
	Release(ctx context.Context, id string) error

	// Finish marks the item done.
	Finish(ctx context.Context, id string) error

	// Close marks the item done and, when comment is non-blank, posts it.
	Close(ctx context.Context, id, comment string) error

	// Link records that blocker blocks blocked.
	Link(ctx context.Context, blocker, blocked string) error

	// Unlink removes the link that Link recorded.
	Unlink(ctx context.Context, blocker, blocked string) error

	// Create files a new item and returns its ID.
	Create(ctx context.Context, req CreateRequest) (string, error)

	// SetPriority sets the item's priority rank. Zero is highest.
	// The source maps that rank to one priority name or label.
	// A rank with no single name returns an error and writes nothing.
	SetPriority(ctx context.Context, id string, rank int) error
}

// ContentReader is an optional interface implemented by sources that can fetch
// an item's live metadata and raw body text for content-hash approval.
type ContentReader interface {
	GetContent(ctx context.Context, id string) (Item, string, error)
}

// StatusSetter is an optional interface implemented by sources that can
// transition an item's workflow status directly (used by `factory approve`).
type StatusSetter interface {
	SetStatus(ctx context.Context, id, status string) error
}

// ReadContent fetches live item metadata and raw body text via ContentReader
// when supported by src, or falls back to src.Get with an empty body.
func ReadContent(ctx context.Context, src Source, id string) (Item, string, error) {
	if cr, ok := src.(ContentReader); ok {
		return cr.GetContent(ctx, id) //nolint:wrapcheck // caller adds command context
	}
	it, err := src.Get(ctx, id)
	return it, "", err //nolint:wrapcheck // caller adds command context
}

// SetStatus transitions id to status when src implements StatusSetter, or
// returns ErrNotSupported otherwise.
func SetStatus(ctx context.Context, src Source, id, status string) error {
	setter, ok := src.(StatusSetter)
	if !ok {
		return ErrNotSupported
	}
	return setter.SetStatus(ctx, id, status) //nolint:wrapcheck // caller adds command context
}

// ContentHash returns the canonical SHA-256 digest of (title, body) formatted
// as "sha256:<hex>". Length-prefixing both strings prevents boundary collisions.
func ContentHash(title, body string) string {
	h := sha256.New()
	_, _ = fmt.Fprintf(h, "%d:%s\n%d:%s", len(title), title, len(body), body)
	return "sha256:" + hex.EncodeToString(h.Sum(nil))
}

var (
	// ErrNotFound means the item or the link is absent.
	ErrNotFound = errors.New("not found")

	// ErrNotSupported means this source cannot perform the operation.
	ErrNotSupported = errors.New("not supported by this source")
)
