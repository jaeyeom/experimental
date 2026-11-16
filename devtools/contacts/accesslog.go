package contacts

import (
	"fmt"
	"time"
)

// Access is one access-log row. PersonID is empty when the command matched
// no single person. The log never stores names.
type Access struct {
	Time     string `json:"time"`
	Caller   string `json:"caller"`
	Command  string `json:"command"`
	PersonID string `json:"person"`
	Reason   string `json:"reason"`
}

// LogAccess appends one access-log row stamped with the store's clock.
func (s *Store) LogAccess(caller, command, personID, reason string) error {
	if _, err := s.db.Exec(`INSERT INTO access_log (time, caller, command, person_id, reason) VALUES (?, ?, ?, ?, ?)`,
		s.timestamp(), caller, command, personID, reason); err != nil {
		return fmt.Errorf("access log: %w", err)
	}
	return nil
}

// AccessFilter selects access-log rows. Empty fields match everything.
type AccessFilter struct {
	Caller   string
	PersonID string
	// Since is a UTC timestamp in timeLayout; rows at or after it match.
	Since string
}

// AccessLog returns matching rows, newest first.
func (s *Store) AccessLog(f AccessFilter) ([]Access, error) {
	rows, err := s.db.Query(`SELECT time, caller, command, person_id, reason FROM access_log
		WHERE (?1 = '' OR caller = ?1) AND (?2 = '' OR person_id = ?2) AND (?3 = '' OR time >= ?3)
		ORDER BY time DESC, id DESC`, f.Caller, f.PersonID, f.Since)
	if err != nil {
		return nil, fmt.Errorf("access log: %w", err)
	}
	defer rows.Close()
	out := []Access{}
	for rows.Next() {
		var a Access
		if err := rows.Scan(&a.Time, &a.Caller, &a.Command, &a.PersonID, &a.Reason); err != nil {
			return nil, fmt.Errorf("access log: %w", err)
		}
		out = append(out, a)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("access log: %w", err)
	}
	return out, nil
}

// StartOfDay parses a YYYY-MM-DD date in loc and returns the UTC timestamp
// of its first second, for AccessFilter.Since.
func StartOfDay(date string, loc *time.Location) (string, error) {
	t, err := time.ParseInLocation(dateLayout, date, loc)
	if err != nil {
		return "", fmt.Errorf("date %q: want YYYY-MM-DD", date)
	}
	return t.UTC().Format(timeLayout), nil
}
