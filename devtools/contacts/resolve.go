package contacts

import (
	"errors"
	"io/fs"
	"os"
	"time"
	_ "time/tzdata" // timezones work without system zoneinfo
)

// Detail is a person plus what resolve shows about them.
type Detail struct {
	Person
	// LocalTime is the person's current local time, empty without a
	// timezone.
	LocalTime string `json:"localTime"`
	// Notes is the notes file path, empty when there is none.
	Notes string `json:"notes"`
}

// Detail returns p with its local time and notes path.
func (s *Store) Detail(p Person) Detail {
	d := Detail{Person: p}
	if loc, err := time.LoadLocation(p.TZ); err == nil && p.TZ != "" {
		d.LocalTime = s.now().In(loc).Format("2006-01-02 15:04 -07:00")
	}
	if _, err := os.Stat(s.NotesPath(p.ID)); !errors.Is(err, fs.ErrNotExist) {
		d.Notes = s.NotesPath(p.ID)
	}
	return d
}
