package main

import (
	"fmt"
	"strconv"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

func (a *app) cmdCommit(args []string) int {
	fs := a.flags("commit")
	direction := fs.String("direction", "", "mine (your promise) or theirs (required)")
	text := fs.String("text", "", "what was promised (required)")
	due := fs.String("due", "", "due date, YYYY-MM-DD")
	link := fs.String("link", "", "absolute http(s) link to where it was promised")
	by := fs.String("by", "", "the recording agent (required)")
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	c := contacts.Commitment{Direction: *direction, Text: *text, Due: *due, Link: *link, Reporter: *by}
	if err := contacts.ValidateCommitment(c); err != nil {
		a.errorf("commit: %v", err)
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
	c.PersonID = p.ID
	id, err := s.Commit(c)
	if err != nil {
		return a.fail(err)
	}
	a.logAccess(s, *by, "commit", p.ID, "")
	fmt.Fprintln(a.stdout, id)
	return exitOK
}

func (a *app) cmdClose(args []string) int {
	fs := a.flags("close")
	status := fs.String("status", "", "done or dropped (required)")
	note := fs.String("note", "", "how it ended")
	caller := a.callerFlag(fs)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	id, err := strconv.ParseInt(pos[0], 10, 64)
	if err != nil {
		a.errorf("close: commitment id %q is not a number", pos[0])
		return exitUsage
	}
	if *status != "done" && *status != "dropped" {
		a.errorf("close: --status must be done or dropped")
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	c, err := s.CloseCommitment(id, *status, *note)
	if err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "close", c.PersonID, "")
	fmt.Fprintf(a.stdout, "%d %s\n", c.ID, c.Status)
	return exitOK
}

func (a *app) cmdCommitments(args []string) int {
	fs := a.flags("commitments")
	open := fs.Bool("open", false, "only open commitments")
	overdue := fs.Bool("overdue", false, "only open commitments past their due date")
	asJSON := fs.Bool("json", false, "print JSON")
	reason := fs.String("reason", "", "why you are reading them")
	caller := a.callerFlag(fs)
	pos, ok := a.parse(fs, args, 0, 1)
	if !ok {
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	f := contacts.CommitmentFilter{Open: *open, Overdue: *overdue}
	if len(pos) == 1 {
		p, code, ok := a.find(s, pos[0])
		if !ok {
			return code
		}
		f.PersonID = p.ID
	}
	cs, err := s.Commitments(f)
	if err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "commitments", f.PersonID, *reason)
	if *asJSON {
		a.printJSON(cs)
		return exitOK
	}
	a.printCommitments(cs)
	return exitOK
}

// printCommitments prints one tab-separated line per commitment: id,
// person, direction, status (flagged OVERDUE), due, text, link.
func (a *app) printCommitments(cs []contacts.Commitment) {
	for _, c := range cs {
		status := c.Status
		if c.Overdue {
			status = "OVERDUE"
		}
		due := c.Due
		if due == "" {
			due = "-"
		}
		a.printRow(strconv.FormatInt(c.ID, 10), c.PersonID, c.Direction, status, due, c.Text, c.Link)
	}
}
