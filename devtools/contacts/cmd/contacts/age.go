package main

import (
	"errors"
	"fmt"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// cmdAge applies the retention period. It is safe to run from a timer:
// when another run holds the lock it says so and exits 0. It writes no
// access-log row, so a second run changes nothing.
func (a *app) cmdAge(args []string) int {
	fs := a.flags("age")
	dryRun := fs.Bool("dry-run", false, "print what would change and change nothing")
	drop := fs.Bool("drop", false, "delete old events instead of compacting them")
	if _, ok := a.parse(fs, args, 0, 0); !ok {
		return exitUsage
	}
	s, cfg, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	r, err := s.Age(contacts.AgeOptions{RetentionDays: cfg.RetentionDays, Drop: *drop, DryRun: *dryRun})
	if errors.Is(err, contacts.ErrAgeLocked) {
		fmt.Fprintln(a.stdout, "another contacts age run holds the lock; nothing done")
		return exitOK
	}
	if err != nil {
		return a.fail(err)
	}
	verb := ""
	if *dryRun {
		verb = "would have "
	}
	if !r.Changed() {
		fmt.Fprintf(a.stdout, "nothing older than %d days\n", cfg.RetentionDays)
		return exitOK
	}
	if r.Summaries > 0 {
		fmt.Fprintf(a.stdout, "%scompacted %d events into %d summaries\n", verb, r.CompactedEvents, r.Summaries)
	}
	if r.DroppedEvents > 0 {
		fmt.Fprintf(a.stdout, "%sdropped %d events\n", verb, r.DroppedEvents)
	}
	if r.DeletedCommitments > 0 {
		fmt.Fprintf(a.stdout, "%sdeleted %d closed commitments\n", verb, r.DeletedCommitments)
	}
	if r.DeletedAccessRows > 0 {
		fmt.Fprintf(a.stdout, "%sdeleted %d access-log rows\n", verb, r.DeletedAccessRows)
	}
	return exitOK
}
