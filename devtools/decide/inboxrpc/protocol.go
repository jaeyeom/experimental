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

// Version is the protocol version hello returns.
const Version = 1

// Ops. Archive is done with the outcome "archived".
const (
	OpHello    = "hello"
	OpList     = "list"
	OpShow     = "show"
	OpMarkRead = "mark-read"
	OpAnswer   = "answer"
	OpReply    = "reply"
	OpDone     = "done"
	OpArchive  = "archive"
	OpWithdraw = "withdraw"
	OpSnooze   = "snooze"
	OpUnsnooze = "unsnooze"
	OpStatus   = "status"
	OpTargets  = "targets"
	OpBusList  = "bus-list"
	OpSend     = "send"
	OpLocate   = "locate"
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
}

// Response is one line from the server. Error fails the request.
// NotifyError is a write that was stored whose notification failed.
// Output is the raw `agentbus list --json` of bus-list.
type Response struct {
	ID          int64                 `json:"id"`
	Error       string                `json:"error,omitempty"`
	NotifyError string                `json:"notifyError,omitempty"`
	Version     int                   `json:"version,omitempty"`
	Item        *decide.Item          `json:"item,omitempty"`
	Items       []decide.Item         `json:"items,omitempty"`
	Status      *tui.StatusBoard      `json:"status,omitempty"`
	Targets     []decide.ConfigTarget `json:"targets,omitempty"`
	Agentbus    bool                  `json:"agentbus,omitempty"`
	Output      string                `json:"output,omitempty"`
	Location    *decide.OwnerLocation `json:"location,omitempty"`
}

// writeOps change the ledger or send something. While the hub is
// unreachable they fail at once and are never queued.
var writeOps = map[string]bool{
	OpMarkRead: true,
	OpAnswer:   true,
	OpReply:    true,
	OpDone:     true,
	OpArchive:  true,
	OpWithdraw: true,
	OpSnooze:   true,
	OpUnsnooze: true,
	OpSend:     true,
}
