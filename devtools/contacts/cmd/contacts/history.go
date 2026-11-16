package main

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// historyJSON is the --json output of contacts history.
type historyJSON struct {
	Person      string                `json:"person"`
	Events      []contacts.Event      `json:"events"`
	Commitments []contacts.Commitment `json:"commitments"`
	Notes       string                `json:"notes"`
}

// cmdHistory prints a person's events, newest first, then their open
// commitments. It never prints the notes file, only its path.
func (a *app) cmdHistory(args []string) int {
	fs := a.flags("history")
	since := fs.String("since", "", "only events on or after this date, YYYY-MM-DD")
	until := fs.String("until", "", "only events on or before this date, YYYY-MM-DD")
	topic := fs.String("topic", "", "only events whose summary contains this text (case-insensitive)")
	limit := fs.Int("limit", 0, "at most this many events (0 for all)")
	reason := fs.String("reason", "", "why you are reading this history (required)")
	noCommitments := fs.Bool("no-commitments", false, "do not list open commitments")
	asJSON := fs.Bool("json", false, "print JSON")
	caller := a.callerFlag(fs)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	if *reason == "" {
		a.errorf("history: --reason is required; every read of history is logged with why")
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	p, code, ok := a.find(s, pos[0])
	if !ok {
		return code
	}
	events, err := s.History(contacts.HistoryQuery{PersonID: p.ID, Since: *since, Until: *until, Topic: *topic, Limit: *limit})
	if err != nil {
		a.errorf("history: %v", err)
		return exitUsage
	}
	a.logAccess(s, caller(), "history", p.ID, *reason)
	commitments := []contacts.Commitment{}
	if !*noCommitments {
		if commitments, err = s.Commitments(contacts.CommitmentFilter{PersonID: p.ID, Open: true}); err != nil {
			return a.fail(err)
		}
	}
	notes := s.Detail(p).Notes
	if *asJSON {
		a.printJSON(historyJSON{Person: p.ID, Events: events, Commitments: commitments, Notes: notes})
		return exitOK
	}
	for _, e := range events {
		a.printRow(e.Date, e.Channel, e.Link, e.Summary, e.Reporter)
	}
	if len(commitments) > 0 {
		fmt.Fprintln(a.stdout, "\nopen commitments:")
		a.printCommitments(commitments)
	}
	if notes != "" {
		fmt.Fprintf(a.stdout, "\nnotes: %s\n", notes)
	}
	return exitOK
}
