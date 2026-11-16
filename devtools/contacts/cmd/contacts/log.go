package main

import (
	"errors"
	"time"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// cmdLog prints the access log. Reading the log is not itself logged.
func (a *app) cmdLog(args []string) int {
	fs := a.flags("log")
	caller := fs.String("caller", "", "only rows from this caller")
	person := fs.String("person", "", "only rows about this person (an id still works after the person is deleted)")
	since := fs.String("since", "", "only rows on or after this local date, YYYY-MM-DD")
	asJSON := fs.Bool("json", false, "print JSON")
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	f := contacts.AccessFilter{Caller: *caller}
	if *since != "" {
		ts, err := contacts.StartOfDay(*since, time.Local)
		if err != nil {
			a.errorf("--since: %v", err)
			return exitUsage
		}
		f.Since = ts
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	if *person != "" {
		f.PersonID = *person
		p, err := s.Find(*person)
		var amb *contacts.AmbiguousError
		switch {
		case err == nil:
			f.PersonID = p.ID
		case errors.As(err, &amb):
			return a.fail(err)
		}
	}
	rows, err := s.AccessLog(f)
	if err != nil {
		return a.fail(err)
	}
	if *asJSON {
		a.printJSON(rows)
		return exitOK
	}
	for _, r := range rows {
		a.printRow(r.Time, r.Caller, r.Command, r.PersonID, r.Reason)
	}
	return exitOK
}
