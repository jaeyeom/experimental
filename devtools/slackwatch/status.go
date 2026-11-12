package slackwatch

import (
	"database/sql"
	"fmt"
	"time"
)

// Server states. serve writes the first three as the connection. ConnStale
// is reported by ReadStatus when the heartbeat is too old.
const (
	ConnConnected    = "connected"
	ConnReconnecting = "reconnecting"
	ConnDown         = "down"
	ConnStale        = "stale"
)

// DefaultHeartbeatInterval is how often serve writes its heartbeat.
const DefaultHeartbeatInterval = time.Minute

// Status is the serve heartbeat row. State and Undelivered are derived:
// WriteStatus counts undelivered notifications and ReadStatus sets State.
type Status struct {
	PID               int       `json:"pid"`
	StartedAt         time.Time `json:"startedAt,omitzero"`
	HeartbeatAt       time.Time `json:"heartbeatAt,omitzero"`
	HeartbeatInterval Duration  `json:"heartbeatInterval"`
	Connection        string    `json:"connection"`
	State             string    `json:"state"`
	LastEventAt       time.Time `json:"lastEventAt,omitzero"`
	LastCatchUpAt     time.Time `json:"lastCatchUpAt,omitzero"`
	LastCatchUpError  string    `json:"lastCatchUpError"`
	LastSweepAt       time.Time `json:"lastSweepAt,omitzero"`
	LastSweepError    string    `json:"lastSweepError"`
	Undelivered       int       `json:"undelivered"`
	// PostMode is post.mode, empty when posting is off. PostingDisabled
	// is why direct posting is disabled. Outbox counts the records
	// waiting on someone. The status command fills these.
	PostMode        string       `json:"postMode"`
	PostingDisabled string       `json:"postingDisabled,omitempty"`
	Outbox          OutboxCounts `json:"outbox"`
}

// Healthy reports whether the server is up with a fresh heartbeat and
// every notification was delivered, and posting is not disabled. A
// reconnecting server is up.
func (st Status) Healthy() bool {
	return st.State != ConnDown && st.State != ConnStale && st.Undelivered == 0 && st.PostingDisabled == ""
}

// undeliveredQuery counts notifications nobody received. Each failed
// attempt is the end of its chain unless a later attempt for the same
// reply went to the same target (a retry) or to a later attempt number
// (the fallback). The last attempt of a chain counts when it failed.
const undeliveredQuery = `
SELECT COUNT(*) FROM deliveries d WHERE d.delivered = 0
AND NOT EXISTS (SELECT 1 FROM deliveries l WHERE l.id > d.id
	AND l.reply_ts = d.reply_ts AND l.watch_id IS d.watch_id
	AND (l.target = d.target OR l.attempt > d.attempt))`

func unixOrNull(t time.Time) any {
	if t.IsZero() {
		return nil
	}
	return t.Unix()
}

// WriteStatus replaces the heartbeat row with st and the current count
// of undelivered notifications.
func (s *Store) WriteStatus(st Status) error {
	if _, err := s.db.Exec(`UPDATE status SET
	pid = ?, started_at = ?, heartbeat_at = ?, heartbeat_interval = ?, connection = ?,
	last_event_at = ?, last_catchup_at = ?, last_catchup_error = ?,
	last_sweep_at = ?, last_sweep_error = ?, undelivered = (`+undeliveredQuery+`)
WHERE id = 1`,
		st.PID, unixOrNull(st.StartedAt), unixOrNull(st.HeartbeatAt), int64(st.HeartbeatInterval.Duration/time.Second),
		st.Connection, unixOrNull(st.LastEventAt), unixOrNull(st.LastCatchUpAt), st.LastCatchUpError,
		unixOrNull(st.LastSweepAt), st.LastSweepError); err != nil {
		return fmt.Errorf("write status: %w", err)
	}
	return nil
}

// ReadStatus reads the heartbeat row. State is the connection, ConnDown
// when serve never ran, or ConnStale when the heartbeat is older than
// twice its interval at now.
func (s *Store) ReadStatus(now time.Time) (Status, error) {
	var st Status
	var pid, started, heartbeat, interval, event, catchUp, sweep sql.NullInt64
	if err := s.db.QueryRow(`SELECT pid, started_at, heartbeat_at, heartbeat_interval, connection,
	last_event_at, last_catchup_at, last_catchup_error, last_sweep_at, last_sweep_error, undelivered
FROM status WHERE id = 1`).Scan(&pid, &started, &heartbeat, &interval, &st.Connection,
		&event, &catchUp, &st.LastCatchUpError, &sweep, &st.LastSweepError, &st.Undelivered); err != nil {
		return Status{}, fmt.Errorf("read status: %w", err)
	}
	st.PID = int(pid.Int64)
	st.StartedAt = unixTime(started)
	st.HeartbeatAt = unixTime(heartbeat)
	st.HeartbeatInterval = Duration{time.Duration(interval.Int64) * time.Second}
	st.LastEventAt = unixTime(event)
	st.LastCatchUpAt = unixTime(catchUp)
	st.LastSweepAt = unixTime(sweep)
	switch {
	case st.Connection == "" || st.Connection == ConnDown || st.HeartbeatAt.IsZero():
		st.State = ConnDown
	case now.Sub(st.HeartbeatAt) > 2*st.HeartbeatInterval.Duration:
		st.State = ConnStale
	default:
		st.State = st.Connection
	}
	return st, nil
}
