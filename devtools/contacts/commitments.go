package contacts

import (
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"
)

// Commitment is a promise between the user and a person. Direction mine is
// the user's promise; theirs is the person's.
type Commitment struct {
	ID        int64  `json:"id"`
	PersonID  string `json:"person"`
	Direction string `json:"direction"`
	Text      string `json:"text"`
	Due       string `json:"due"`
	Status    string `json:"status"`
	Link      string `json:"link"`
	Reporter  string `json:"reporter"`
	Note      string `json:"note"`
	CreatedAt string `json:"createdAt"`
	ClosedAt  string `json:"closedAt"`
	// Overdue is true for an open commitment whose due date is before
	// today.
	Overdue bool `json:"overdue"`
}

// ValidateCommitment checks c before it is recorded.
func ValidateCommitment(c Commitment) error {
	if c.Direction != "mine" && c.Direction != "theirs" {
		return fmt.Errorf("direction %q: want mine or theirs", c.Direction)
	}
	if strings.TrimSpace(c.Text) == "" {
		return errors.New("text is required")
	}
	if c.Reporter == "" {
		return errors.New("reporter (--by) is required")
	}
	if c.Due != "" {
		if _, err := time.Parse(dateLayout, c.Due); err != nil {
			return fmt.Errorf("due %q: want YYYY-MM-DD", c.Due)
		}
	}
	if c.Link != "" {
		return checkLink(c.Link)
	}
	return nil
}

// Commit records an open commitment and returns its id.
func (s *Store) Commit(c Commitment) (int64, error) {
	if err := ValidateCommitment(c); err != nil {
		return 0, err
	}
	res, err := s.db.Exec(`INSERT INTO commitments (person_id, direction, text, due, link, reporter, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?)`, c.PersonID, c.Direction, c.Text, c.Due, c.Link, c.Reporter, s.timestamp())
	if err != nil {
		return 0, fmt.Errorf("commit: %w", err)
	}
	id, err := res.LastInsertId()
	if err != nil {
		return 0, fmt.Errorf("commit: %w", err)
	}
	return id, nil
}

// CloseCommitment marks open commitment id done or dropped and returns it. Closing a
// closed commitment is an error.
func (s *Store) CloseCommitment(id int64, status, note string) (Commitment, error) {
	if status != "done" && status != "dropped" {
		return Commitment{}, fmt.Errorf("status %q: want done or dropped", status)
	}
	var c Commitment
	err := s.inTx(func(tx *sql.Tx) error {
		var current string
		err := tx.QueryRow(`SELECT status FROM commitments WHERE id = ?`, id).Scan(&current)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("no commitment %d", id)
		}
		if err != nil {
			return fmt.Errorf("close: %w", err)
		}
		if current != "open" {
			return fmt.Errorf("commitment %d is already %s", id, current)
		}
		if _, err := tx.Exec(`UPDATE commitments SET status = ?, note = ?, closed_at = ? WHERE id = ?`,
			status, note, s.timestamp(), id); err != nil {
			return fmt.Errorf("close: %w", err)
		}
		cs, err := s.commitments(tx, `WHERE id = ?`, id)
		if err != nil {
			return err
		}
		c = cs[0]
		return nil
	})
	return c, err
}

// CommitmentFilter selects commitments. Empty fields match everything.
type CommitmentFilter struct {
	PersonID string
	Open     bool
	Overdue  bool
}

// Commitments lists matching commitments, open ones first, then by due
// date (undated last) and id.
func (s *Store) Commitments(f CommitmentFilter) ([]Commitment, error) {
	where := `WHERE (?1 = '' OR person_id = ?1)`
	if f.Open || f.Overdue {
		where += ` AND status = 'open'`
	}
	if f.Overdue {
		where += ` AND due != '' AND due < ?2`
	}
	return s.commitments(s.db, where+` ORDER BY status != 'open', due = '', due, id`, f.PersonID, s.Today())
}

func (s *Store) commitments(q querier, where string, args ...any) ([]Commitment, error) {
	rows, err := q.Query(`SELECT id, person_id, direction, text, due, status, link, reporter, note, created_at, closed_at
		FROM commitments `+where, args...)
	if err != nil {
		return nil, fmt.Errorf("commitments: %w", err)
	}
	defer rows.Close()
	today := s.Today()
	out := []Commitment{}
	for rows.Next() {
		var c Commitment
		if err := rows.Scan(&c.ID, &c.PersonID, &c.Direction, &c.Text, &c.Due, &c.Status, &c.Link,
			&c.Reporter, &c.Note, &c.CreatedAt, &c.ClosedAt); err != nil {
			return nil, fmt.Errorf("commitments: %w", err)
		}
		c.Overdue = c.Status == "open" && c.Due != "" && c.Due < today
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("commitments: %w", err)
	}
	return out, nil
}
