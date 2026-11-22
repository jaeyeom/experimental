// Package inboxrpc runs the decide inbox on one machine against the
// ledger on another, the hub.
//
// The hub runs `decide serve --stdio`, which reads one JSON request per
// line on stdin and writes one JSON response per line on stdout. Each
// request has an id and an op; the response carries the same id, so
// requests may overlap. Client is a tui.Backend over one long-lived ssh
// session to that server. Notifications, compose sends, and hooks run
// on the hub, as they do for the CLI in hub mode.
package inboxrpc

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// Version is the protocol version hello returns. Version 2 added
// config. Version 3 added set-avatar. Version 4 added set-hidden.
// Version 5 added set-contact-aliases. Version 6 added seen on archive.
// Version 7 added bus-health. Version 8 added check-deliveries.
// Version 9 added watch for the hub-client replica.
// Version 10 added outbox writtenAt and conflict checks.
// Version 11 added persistent draft sync.
// Version 12 added org-tree.
const Version = 12

// Ops. Archive is done with the outcome "archived". Deliveries refreshes
// the pending delivery marks on the hub.
const (
	OpHello        = "hello"
	OpList         = "list"
	OpShow         = "show"
	OpMarkRead     = "mark-read"
	OpAnswer       = "answer"
	OpReply        = "reply"
	OpDone         = "done"
	OpArchive      = "archive"
	OpWithdraw     = "withdraw"
	OpSnooze       = "snooze"
	OpUnsnooze     = "unsnooze"
	OpStatus       = "status"
	OpTargets      = "targets"
	OpBusList      = "bus-list"
	OpBusHealth    = "bus-health"
	OpSend         = "send"
	OpLocate       = "locate"
	OpConfig       = "config"
	OpSetAvatar    = "set-avatar"
	OpSetHidden    = "set-hidden"
	OpSetAliases   = "set-contact-aliases"
	OpSetFavorites = "set-favorites"
	OpDeliveries   = "check-deliveries"
	OpWatch        = "watch"
	OpListDrafts   = "list-drafts"
	OpPutDraft     = "put-draft"
	OpDeleteDraft  = "delete-draft"
	OpOrgTree      = "org-tree"
)

// Request is one line from the client. Item is the item id. Text is the
// answer, reply, outcome, reason, or compose text. To, Bus, and Forward
// name a send's recipient and the item it forwards.
type Request struct {
	ID      int64      `json:"id"`
	Op      string     `json:"op"`
	Item    string     `json:"item,omitempty"`
	Text    string     `json:"text,omitempty"`
	Until   *time.Time `json:"until,omitempty"`
	To      string     `json:"to,omitempty"`
	Bus     bool       `json:"bus,omitempty"`
	Forward string     `json:"forward,omitempty"`
	// Name and Choice are a set-avatar: a built-in id, "default", or "none".
	// Text for set-hidden is a JSON array of hidden contact names.
	// Text for set-contact-aliases is a JSON object of ledger name to agent.
	// Text for set-favorites is a JSON array of starred contact names.
	Name   string `json:"name,omitempty"`
	Choice string `json:"choice,omitempty"`
	// Seen is the newest message time the client has rendered. Archive
	// refuses a message newer than Seen. Nil is older than any message.
	Seen *time.Time `json:"seen,omitempty"`
	// Route is a reply's owner, worker, or both choice. Empty is the owner.
	Route string `json:"route,omitempty"`
	// Since is the client replica's last known ledger sequence for watch.
	Since int64 `json:"since,omitempty"`
	// WrittenAt is when an outbox entry was written on the client.
	WrittenAt *time.Time `json:"writtenAt,omitempty"`
	// ConflictCheck, when true, holds the write if the item changed since
	// the recorded baseline.
	ConflictCheck     bool          `json:"conflictCheck,omitempty"`
	BaseState         string        `json:"baseState,omitempty"`
	BaseMessageCount  int           `json:"baseMessageCount,omitempty"`
	BaseLastMessageAt *time.Time    `json:"baseLastMessageAt,omitempty"`
	BaseAnswered      bool          `json:"baseAnswered,omitempty"`
	Draft             *decide.Draft `json:"draft,omitempty"`
	BaseRev           int64         `json:"baseRev,omitempty"`
	ConflictID        string        `json:"conflictId,omitempty"`
}

// Response is one line from the server. Error fails the request.
// NotifyError is a write that was stored whose notification failed.
// Output is the raw `agentbus list --json` of bus-list, or the raw
// `agentbus health --json` of bus-health. Display is the
// hub's display settings, from config.
type Response struct {
	ID             int64                   `json:"id"`
	Error          string                  `json:"error,omitempty"`
	NotifyError    string                  `json:"notifyError,omitempty"`
	Version        int                     `json:"version,omitempty"`
	Item           *decide.Item            `json:"item,omitempty"`
	Items          []decide.Item           `json:"items,omitempty"`
	Status         *tui.StatusBoard        `json:"status,omitempty"`
	Targets        []decide.ConfigTarget   `json:"targets,omitempty"`
	Agentbus       bool                    `json:"agentbus,omitempty"`
	Output         string                  `json:"output,omitempty"`
	Location       *decide.OwnerLocation   `json:"location,omitempty"`
	Display        *decide.DisplaySettings `json:"display,omitempty"`
	Seq            int64                   `json:"seq,omitempty"`
	Resync         bool                    `json:"resync,omitempty"`
	Changes        []decide.LedgerChange   `json:"changes,omitempty"`
	Conflict       string                  `json:"conflict,omitempty"`
	Draft          *decide.Draft           `json:"draft,omitempty"`
	Drafts         []decide.Draft          `json:"drafts,omitempty"`
	DraftConflict  bool                    `json:"draftConflict,omitempty"`
	DraftSeq       int64                   `json:"draftSeq,omitempty"`
	OrgSpecModTime *time.Time              `json:"orgSpecModTime,omitempty"`
}

// writeOps change the ledger or send something. While the hub is
// unreachable they fail at once and are never queued.
var writeOps = map[string]bool{
	OpMarkRead:     true,
	OpAnswer:       true,
	OpReply:        true,
	OpDone:         true,
	OpArchive:      true,
	OpWithdraw:     true,
	OpSnooze:       true,
	OpUnsnooze:     true,
	OpSend:         true,
	OpSetAvatar:    true,
	OpSetHidden:    true,
	OpSetAliases:   true,
	OpSetFavorites: true,
	OpPutDraft:     true,
	OpDeleteDraft:  true,
}
