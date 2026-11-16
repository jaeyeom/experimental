package contacts

import (
	"database/sql"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"golang.org/x/sys/unix"
)

// AgeReporter is the reporter of the summary events contacts age writes.
const AgeReporter = "contacts age"

// ErrAgeLocked means another contacts age run holds the lock.
var ErrAgeLocked = errors.New("another contacts age run is in progress")

// AgeOptions controls Age.
type AgeOptions struct {
	// RetentionDays is the age, in days, past which rows are aged.
	RetentionDays int
	// Drop deletes old events instead of compacting them.
	Drop bool
	// DryRun computes the changes and makes none.
	DryRun bool
}

// AgeResult counts what Age changed, or would change on a dry run.
type AgeResult struct {
	CompactedEvents    int `json:"compactedEvents"`
	Summaries          int `json:"summaries"`
	DroppedEvents      int `json:"droppedEvents"`
	DeletedCommitments int `json:"deletedCommitments"`
	DeletedAccessRows  int `json:"deletedAccessRows"`
}

// Changed reports whether r counts any change.
func (r AgeResult) Changed() bool { return r != AgeResult{} }

var errDryRun = errors.New("dry run")

// Age applies retention. Events dated before the cutoff are compacted into
// one summary event per person per month, keeping the newest link, or
// deleted with Drop. Closed commitments closed before the cutoff and
// access-log rows older than it are deleted. Open commitments are never
// touched. A second run changes nothing. Age holds a lock file in the data
// directory and returns ErrAgeLocked when another run holds it.
func (s *Store) Age(opts AgeOptions) (AgeResult, error) {
	var r AgeResult
	if opts.RetentionDays <= 0 {
		return r, errors.New("retention days must be positive")
	}
	unlock, err := lockFile(filepath.Join(s.dir, "age.lock"))
	if err != nil {
		return r, err
	}
	defer unlock()
	now := s.now()
	cutoffDate := now.Local().AddDate(0, 0, -opts.RetentionDays).Format(dateLayout)
	cutoffTime := now.UTC().AddDate(0, 0, -opts.RetentionDays).Format(timeLayout)
	err = s.inTx(func(tx *sql.Tx) error {
		if opts.Drop {
			n, err := execCount(tx, `DELETE FROM events WHERE date < ?`, cutoffDate)
			if err != nil {
				return fmt.Errorf("age: drop events: %w", err)
			}
			r.DroppedEvents = n
		} else if err := compact(tx, cutoffDate, s.timestamp(), &r); err != nil {
			return err
		}
		n, err := execCount(tx, `DELETE FROM commitments WHERE status != 'open' AND closed_at != '' AND closed_at < ?`, cutoffTime)
		if err != nil {
			return fmt.Errorf("age: commitments: %w", err)
		}
		r.DeletedCommitments = n
		if r.DeletedAccessRows, err = execCount(tx, `DELETE FROM access_log WHERE time < ?`, cutoffTime); err != nil {
			return fmt.Errorf("age: access log: %w", err)
		}
		if opts.DryRun {
			return errDryRun
		}
		return nil
	})
	if errors.Is(err, errDryRun) {
		err = nil
	}
	return r, err
}

// compact replaces each person's events of each month before cutoff with
// one summary event, unless that month already is a single summary.
func compact(tx *sql.Tx, cutoff, createdAt string, r *AgeResult) error {
	rows, err := tx.Query(`SELECT id, person_id, date, channel, link, compacted FROM events
		WHERE date < ? ORDER BY person_id, substr(date, 1, 7), date, id`, cutoff)
	if err != nil {
		return fmt.Errorf("age: %w", err)
	}
	type old struct {
		id                          int64
		person, date, channel, link string
		compacted                   int
	}
	var groups [][]old
	for rows.Next() {
		var o old
		if err := rows.Scan(&o.id, &o.person, &o.date, &o.channel, &o.link, &o.compacted); err != nil {
			rows.Close()
			return fmt.Errorf("age: %w", err)
		}
		if n := len(groups); n > 0 {
			last := groups[n-1][0]
			if last.person == o.person && last.date[:7] == o.date[:7] {
				groups[n-1] = append(groups[n-1], o)
				continue
			}
		}
		groups = append(groups, []old{o})
	}
	rows.Close()
	if err := rows.Err(); err != nil {
		return fmt.Errorf("age: %w", err)
	}
	for _, g := range groups {
		if len(g) == 1 && g[0].compacted > 0 {
			continue
		}
		total := 0
		var channels []string
		for _, o := range g {
			total += max(o.compacted, 1)
			channels = append(channels, strings.Split(o.channel, ",")...)
			if _, err := tx.Exec(`DELETE FROM events WHERE id = ?`, o.id); err != nil {
				return fmt.Errorf("age: %w", err)
			}
		}
		slices.Sort(channels)
		channels = slices.Compact(channels)
		newest := g[len(g)-1]
		if _, err := tx.Exec(`INSERT INTO events (person_id, date, channel, link, summary, reporter, created_at, compacted)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?)`, newest.person, newest.date, strings.Join(channels, ","), newest.link,
			fmt.Sprintf("%d events, channels: %s", total, strings.Join(channels, ", ")), AgeReporter,
			createdAt, total); err != nil {
			return fmt.Errorf("age: %w", err)
		}
		r.CompactedEvents += len(g)
		r.Summaries++
	}
	return nil
}

func execCount(tx *sql.Tx, query string, args ...any) (int, error) {
	res, err := tx.Exec(query, args...)
	if err != nil {
		return 0, err //nolint:wrapcheck // callers wrap
	}
	n, err := res.RowsAffected()
	return int(n), err //nolint:wrapcheck // callers wrap
}

// lockFile takes an exclusive, non-blocking lock on path. It returns
// ErrAgeLocked when another process holds it.
func lockFile(path string) (unlock func(), err error) {
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, fmt.Errorf("age lock: %w", err)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, unix.EWOULDBLOCK) {
			return nil, ErrAgeLocked
		}
		return nil, fmt.Errorf("age lock: %w", err)
	}
	return func() { f.Close() }, nil
}
