package main

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

func (a *app) cmdReport(args []string) int {
	fs := a.flags("report")
	channel := fs.String("channel", "", "where it happened, such as chat or email (required)")
	link := fs.String("link", "", "absolute http(s) link to the conversation (required)")
	summary := fs.String("summary", "", "one-line summary, never the message text (required)")
	date := fs.String("date", "", "YYYY-MM-DD (default today, local date)")
	by := fs.String("by", "", "the reporting agent (required)")
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	for name, v := range map[string]string{"channel": *channel, "link": *link, "summary": *summary, "by": *by} {
		if v == "" {
			a.errorf("report: --%s is required", name)
			return exitUsage
		}
	}
	s, cfg, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	if *date == "" {
		*date = s.Today()
	}
	e := contacts.Event{Date: *date, Channel: *channel, Link: *link, Summary: *summary, Reporter: *by}
	if err := contacts.ValidateEvent(e, cfg.SummaryMax); err != nil {
		a.errorf("report: %v", err)
		return exitUsage
	}
	p, code, ok := a.find(s, pos[0])
	if !ok {
		return code
	}
	e.PersonID = p.ID
	id, dup, err := s.Report(e, cfg.SummaryMax)
	if err != nil {
		return a.fail(err)
	}
	a.logAccess(s, *by, "report", p.ID, "")
	if dup {
		a.errorf("same link and summary already reported as event %d; nothing added", id)
	}
	fmt.Fprintln(a.stdout, id)
	return exitOK
}
