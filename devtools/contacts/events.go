package contacts

import (
	"database/sql"
	"errors"
	"fmt"
	"net/url"
	"strings"
	"time"
	"unicode/utf8"
)

// DefaultSummaryMax is the default cap, in characters, on an event summary.
const DefaultSummaryMax = 200

// Event is one thing that happened with a person: a link and a one-line
// summary, never the message text itself. Events are append-only: there is
// no way to edit or delete one, only to report a correcting event.
type Event struct {
	ID        int64  `json:"id"`
	PersonID  string `json:"person"`
	Date      string `json:"date"`
	Channel   string `json:"channel"`
	Link      string `json:"link"`
	Summary   string `json:"summary"`
	Reporter  string `json:"reporter"`
	CreatedAt string `json:"createdAt"`
}

// ValidateEvent checks e before it is reported. summaryMax caps the summary
// length in characters.
func ValidateEvent(e Event, summaryMax int) error {
	switch {
	case e.Channel == "":
		return errors.New("channel is required")
	case e.Reporter == "":
		return errors.New("reporter (--by) is required")
	}
	if err := checkLink(e.Link); err != nil {
		return err
	}
	if strings.ContainsAny(e.Summary, "\r\n") {
		return errors.New("summary must be one line")
	}
	if strings.TrimSpace(e.Summary) == "" {
		return errors.New("summary is required")
	}
	if n := utf8.RuneCountInString(e.Summary); n > summaryMax {
		return fmt.Errorf("summary is %d characters; the limit is %d", n, summaryMax)
	}
	if _, err := time.Parse(dateLayout, e.Date); err != nil {
		return fmt.Errorf("date %q: want YYYY-MM-DD", e.Date)
	}
	return nil
}

// checkLink accepts only an absolute http(s) URL.
func checkLink(link string) error {
	u, err := url.Parse(link)
	if err != nil || (u.Scheme != "http" && u.Scheme != "https") || u.Host == "" {
		return fmt.Errorf("link %q must be an absolute http(s) URL", link)
	}
	return nil
}

// Today is the store clock's local date, YYYY-MM-DD.
func (s *Store) Today() string { return s.now().Local().Format(dateLayout) }

// Report appends e and returns its id. When an event with the same person,
// link and summary exists, it adds nothing and returns that event's id with
// dup true.
func (s *Store) Report(e Event, summaryMax int) (id int64, dup bool, err error) {
	if err := ValidateEvent(e, summaryMax); err != nil {
		return 0, false, err
	}
	err = s.inTx(func(tx *sql.Tx) error {
		err := tx.QueryRow(`SELECT id FROM events WHERE person_id = ? AND link = ? AND summary = ?`,
			e.PersonID, e.Link, e.Summary).Scan(&id)
		if err == nil {
			dup = true
			return nil
		}
		if !errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("report: %w", err)
		}
		res, err := tx.Exec(`INSERT INTO events (person_id, date, channel, link, summary, reporter, created_at)
			VALUES (?, ?, ?, ?, ?, ?, ?)`, e.PersonID, e.Date, e.Channel, e.Link, e.Summary, e.Reporter, s.timestamp())
		if err != nil {
			return fmt.Errorf("report: %w", err)
		}
		id, err = res.LastInsertId()
		if err != nil {
			return fmt.Errorf("report: %w", err)
		}
		return nil
	})
	return id, dup, err
}
