// Package hibernate stops idle coding agents and keeps their herdr
// panes. It holds the hibernation record the daemon keeps, the checks
// an agent must pass first, and the stop itself.
package hibernate

import (
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// Record statuses.
const (
	// StatusStopping is written before the exit input is sent.
	StatusStopping = "stopping"
	// StatusHibernated means the agent process left the pane.
	StatusHibernated = "hibernated"
	// StatusStopFailed means the agent did not exit in time. It was not
	// killed.
	StatusStopFailed = "stop-failed"
)

// Record is what the daemon keeps for a hibernated agent, keyed by
// Handle: enough to start the same session again in the same pane.
type Record struct {
	Handle string `json:"handle"`
	Pane   string `json:"pane"`
	// Kind is the agent kind herdr reports, such as claude.
	Kind string `json:"kind"`
	// Session is herdr's agent_session.value.
	Session string `json:"session"`
	// Cwd and Argv are the agent's foreground process's, from herdr
	// pane process-info.
	Cwd  string   `json:"cwd"`
	Argv []string `json:"argv"`
	// Names are the names registered on Handle when it stopped. They
	// stay registered.
	Names []string `json:"names,omitempty"`
	// At is when the stop began.
	At time.Time `json:"at"`
	// RSSBytes is the agent's process-tree RSS just before the stop.
	RSSBytes int64  `json:"rss_bytes"` //nolint:tagliatelle // record shape
	Status   string `json:"status"`
	// Reason is why a stop failed.
	Reason string `json:"reason,omitempty"`
}

// Seen is the state the daemon last saw an agent in, and for how long
// it has seen it there.
type Seen struct {
	Status string        `json:"status"`
	For    time.Duration `json:"for"`
}

// Args carries a hibernate-* request.
type Args struct {
	// Record is hibernate-begin's record.
	Record *Record `json:"record,omitempty"`
	// Handle, Status, and Reasons are hibernate-end's and
	// hibernate-refused's.
	Handle  string   `json:"handle,omitempty"`
	Status  string   `json:"status,omitempty"`
	Reasons []string `json:"reasons,omitempty"`
}

// Reply answers a hibernate-* request.
type Reply struct {
	// Names are the registered names by handle.
	Names map[string][]string `json:"names,omitempty"`
	// Seen is by handle, for the agents the daemon's inventory has.
	Seen map[string]Seen `json:"seen,omitempty"`
	// Records are the hibernation records.
	Records []Record `json:"records,omitempty"`
	// Leases is the lease table.
	Leases []lease.Status `json:"leases,omitempty"`
	// Record is the record hibernate-begin or hibernate-end wrote.
	Record *Record `json:"record,omitempty"`
}
