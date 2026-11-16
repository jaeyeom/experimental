package contacts

import "fmt"

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
